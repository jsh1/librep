/* numbers.c -- Implement the tower of numeric types

   Copyright (C) 1993-2015 John Harper <jsh@unfactored.org>

   This file is part of Librep.

   Librep is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   Librep is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Librep; see the file COPYING.  If not, write to
   the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */

#include "repint.h"

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include <float.h>
#include <ctype.h>
#include <limits.h>
#include <errno.h>
#include <time.h>
#include <inttypes.h>

#ifdef HAVE_LOCALE_H
# include <locale.h>
#endif
#ifdef HAVE_XLOCALE_H
# include <xlocale.h>
#endif

#ifdef HAVE_GMP
#include <gmp.h>
#endif

#ifdef NEED_MEMORY_H
# include <memory.h>
#endif

#ifdef HAVE_UNISTD_H
# include <unistd.h>
#endif

DEFSTRING(div_zero, "Divide by zero");
DEFSTRING(domain_error, "Domain error");

#if !defined(LONG_LONG_MIN)
# if defined(LONGLONG_MIN)
   /* AIX and IRIX use LONGLONG_ */
#  define LONG_LONG_MIN LONGLONG_MIN
#  define LONG_LONG_MAX LONGLONG_MAX
# elif defined (LLONG_MIN)
   /* Solaris uses LLONG_ */
#  define LONG_LONG_MIN LLONG_MIN
#  define LONG_LONG_MAX LLONG_MAX
# endif
#endif

/* FIXME: hmm.. */
#if !defined(LONG_LONG_MAX)
# define LONG_LONG_MAX LONG_MAX
#endif
#if !defined(LONG_LONG_MIN)
# define LONG_LONG_MIN LONG_MIN
#endif

#if !defined(HAVE_STRTOLL) && defined(HAVE_STRTOQ)
# define strtoll strtoq
# define HAVE_STRTOLL 1
#endif


/* Private type definitions */

typedef struct number_z_struct number_z;
typedef struct number_q_struct number_q;
typedef struct number_f_struct number_f;

struct number_z_struct {
  repv car;
#ifdef HAVE_GMP
  mpz_t z;
#else
  int64_t z;
#endif
};

struct number_q_struct {
  repv car;
#ifdef HAVE_GMP
  mpq_t q;
#endif
};

struct number_f_struct {
  repv car;
  double f;
};

typedef struct number_block_struct number_block;

struct number_block_struct {
  rep_ALIGN_CELL(number_block *next);
  rep_number data[1];
};

#define rep_SIZEOF_NUMBER_BLOCK(n,t) \
    (sizeof(number_block) - sizeof(rep_number) + (t) * (n))

#define rep_NUMBER(v,t) (((number_ ## t *) rep_PTR(v))->t)

#define ZEROP(x) \
    (rep_INTP(x) ? (x) == rep_MAKE_INT(0) : Fzerop(x) != rep_nil)


/* number object handling */

static number_block *number_block_list[3];
static rep_number *number_free_list[3];
static int number_allocations[3], number_sizeofs[3];
static int allocated_numbers, used_numbers;

static inline int
type_to_index(int type)
{
  return type == rep_NUMBER_BIGNUM ? 0 : type == rep_NUMBER_RATIONAL ? 1 : 2;
}

static void *
make_number(int type)
{
  int idx = type_to_index(type);

  rep_number *cn = number_free_list[idx];
  if (!cn) {
    number_block *cb
      = rep_alloc(rep_SIZEOF_NUMBER_BLOCK(number_allocations[idx],
					  number_sizeofs[idx]));
    allocated_numbers += number_allocations[idx];

    cb->next = number_block_list[idx];
    number_block_list[idx] = cb;

    rep_number *ptr = cb->data, *next;
    for (int i = 0; i < (number_allocations[idx] - 1); i++, ptr = next) {
      next = (rep_number *)(((char *)ptr) + number_sizeofs[idx]);
      ptr->car = (repv)next;
    }
    ptr->car = 0;

    number_free_list[idx] = (rep_number *)cb->data;
    cn = number_free_list[idx];
  }

  number_free_list[idx] = (rep_number *)cn->car;

  cn->car = rep_Number | type;

  used_numbers++;
  rep_data_after_gc += sizeof(rep_number);

  return cn;
}

static void
number_sweep(void)
{
  used_numbers = 0;

  for (int idx = 0; idx < 3; idx++) {

    number_block *cb = number_block_list[idx];
    number_block_list[idx] = NULL;
    number_free_list[idx] = NULL;

    while (cb) {
      number_block *next_cb = cb->next;

      int block_used = 0;
      rep_number *free_list = NULL, *free_tail = NULL;

      rep_number *ptr = cb->data;
      for (int i = 0; i < number_allocations[idx]; i++) {

	/* If on the freelist then the CELL_IS_8 bit will be unset (since
	   the pointer is long aligned). */

	if (rep_CELL_CONS_P(rep_VAL(ptr))
	    || !rep_GC_CELL_MARKEDP((repv)ptr))
	{
	  if (!free_tail) {
	    free_tail = ptr;
	  }

	  if (!rep_CELL_CONS_P(rep_VAL(ptr))) {
	    switch (idx) {
	    case 0:
#ifdef HAVE_GMP
	      mpz_clear(((number_z *)ptr)->z);
#else
	      ((number_z *)ptr)->z = 0;
#endif
	      break;
	    case 1:
#ifdef HAVE_GMP
	      mpq_clear(((number_q *)ptr)->q);
#endif
	      break;
	    }
	  }

	  ptr->car = rep_VAL(free_list);
	  free_list = ptr;
	} else {
	  rep_GC_CLR_CELL((repv)ptr);
	  block_used++;
	}

	ptr = (rep_number *)(((char *)ptr) + number_sizeofs[idx]);
      }

      if (block_used == 0) {

	/* The whole block is unused, lets get rid of it.  */

	rep_free(cb);
	allocated_numbers -= number_allocations[idx];

      } else {

	/* Link the block's freelist onto the global list.  */

	if (free_tail) {
	  free_tail->car = rep_VAL(number_free_list[idx]);
	  number_free_list[idx] = free_list;
	  used_numbers += block_used;
	}

	/* Have to rebuild the block chain as well.  */

	cb->next = number_block_list[idx];
	number_block_list[idx] = cb;
      }

      cb = next_cb;
    }
  }
}


/* Promotion */

static NOT_INLINE repv
copy_number__(repv in)
{
  switch (rep_NUMBER_TYPE(in)) {
  case rep_NUMBER_BIGNUM: {
    number_z *z = make_number(rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
    mpz_init_set(z->z, rep_NUMBER(in,z));
#else
    z->z = rep_NUMBER(in,z);
#endif
    return rep_VAL(z); }

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL: {
    number_q *q = make_number(rep_NUMBER_RATIONAL);
    mpq_init(q->q);
    mpq_set(q->q, rep_NUMBER(in,q));
    return rep_VAL(q); }
#endif

  case rep_NUMBER_FLOAT: {
    number_f *f = make_number(rep_NUMBER_FLOAT);
    f->f = rep_NUMBER(in,f);
    return rep_VAL(f); }

  default:
    abort();
  }
}

static inline repv
copy_number(repv in)
{
  if (rep_INTP(in)) {
    return in;
  } else {
    return copy_number__(in);
  }
}

static repv
promote_to(repv in, int type)
{
  int in_type = rep_NUMERIC_TYPE(in);

  if (in_type >= type) {
    return in;
  }

  switch (in_type) {
  case rep_NUMBER_INT:
    switch (type) {
    case rep_NUMBER_BIGNUM: {
      number_z *z = make_number(rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
      mpz_init_set_si(z->z, rep_INT(in));
#else
      z->z = rep_INT(in);
#endif
      return rep_VAL(z); }

    case rep_NUMBER_RATIONAL:
#ifdef HAVE_GMP
      {
	number_q *q = make_number(rep_NUMBER_RATIONAL);
	mpq_init(q->q);
	mpq_set_si(q->q, rep_INT(in), 1);
	return rep_VAL(q);
      }
#else
      /* fall through */
#endif

    case rep_NUMBER_FLOAT: {
      number_f *f = make_number(rep_NUMBER_FLOAT);
      f->f = (double) rep_INT(in);
      return rep_VAL(f);
      break; }

    default:
      abort();
    }

  case rep_NUMBER_BIGNUM:
    switch (type) {
    case rep_NUMBER_RATIONAL:
#ifdef HAVE_GMP
      {
	number_q *q = make_number(rep_NUMBER_RATIONAL);
	mpq_init(q->q);
	mpq_set_z(q->q, rep_NUMBER(in,z));
	return rep_VAL(q);
      }
#else
      /* fall through */
#endif

    case rep_NUMBER_FLOAT: {
      number_f *f = make_number(rep_NUMBER_FLOAT);
#ifdef HAVE_GMP
      f->f = mpz_get_d(rep_NUMBER(in,z));
#else
      f->f = rep_NUMBER(in,z);
#endif
      return rep_VAL(f); }

    default:
      abort();
    }

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL: {
    assert(type == rep_NUMBER_FLOAT);
    number_f *f = make_number(rep_NUMBER_FLOAT);
    f->f = mpq_get_d(rep_NUMBER(in,q));
    return rep_VAL(f); }
#endif

  default:
    abort();
  }
}

/* IN must be a non-fixnum number */

static repv
maybe_demote(repv in)
{
  assert(rep_NUMBERP(in));

  switch (rep_NUMBER_TYPE(in)) {
#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL:
    if (mpz_cmp_ui(mpq_denref(rep_NUMBER(in,q)), 1) == 0) {
      number_z *z = make_number(rep_NUMBER_BIGNUM);
      mpz_init_set(z->z, mpq_numref(rep_NUMBER(in,q)));
      in = rep_VAL(z);
      goto do_bignum;
    }
    break;
#endif

  case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
  do_bignum:
    if (mpz_cmp_si(rep_NUMBER(in,z), rep_LISP_MAX_INT) <= 0
	&& mpz_cmp_si(rep_NUMBER(in,z), rep_LISP_MIN_INT) >= 0)
    {
      in = rep_MAKE_INT(mpz_get_si(rep_NUMBER(in,z)));
    }
#else
    if (rep_NUMBER(in,z) <= rep_LISP_MAX_INT
	&& rep_NUMBER(in,z) >= rep_LISP_MIN_INT)
    {
      in = rep_MAKE_INT(rep_NUMBER(in,z));
    }
#endif
  }

  return in;
}

static repv
coerce(repv in, int type)
{
  int in_type = rep_NUMERIC_TYPE(in);

  if (in_type <= type) {
    return in;
  }

  switch (in_type) {
  case rep_NUMBER_BIGNUM:
    switch (type)
    {
    case rep_NUMBER_INT:
#ifdef HAVE_GMP
      return rep_MAKE_INT(mpz_get_si(rep_NUMBER(in,z)));
#else
      return rep_MAKE_INT(rep_NUMBER(in,z));
#endif

    default:
      abort();
    }
    break;

    /* FIXME: implement me.. */
  case rep_NUMBER_RATIONAL:
  case rep_NUMBER_FLOAT:
  default:
    abort();
  }

  /* not reached. */
  return 0;
}

static inline void
promote(repv *n1p, repv *n2p)
{
  repv n1 = *n1p;
  repv n2 = *n2p;

  int n1_type = rep_NUMERIC_TYPE(n1);
  int n2_type = rep_NUMERIC_TYPE(n2);

  if (n1_type > n2_type) {
    *n2p = promote_to(n2, n1_type);
  } else if (n1_type < n2_type) {
    *n1p = promote_to(n1, n2_type);
  }
}

static NOT_INLINE repv
promote_dup__(repv *n1p, repv *n2p)
{
  repv n1 = *n1p;
  repv n2 = *n2p;

  int n1_type = rep_NUMERIC_TYPE(n1);
  int n2_type = rep_NUMERIC_TYPE(n2);

  repv out = 0;

  if (n1_type > n2_type) {
    out = promote_to(n2, n1_type);
    *n2p = out;
  } else if (n1_type < n2_type) {
    out = promote_to(n1, n2_type);
    *n1p = out;
  } else {
    out = copy_number(*n1p);
  }

  return out;
}

static inline repv
promote_dup(repv *n1p, repv *n2p)
{
  repv n1 = *n1p;
  repv n2 = *n2p;

  if (rep_INTP_2(n1, n2)) {
    return n1;
  } else {
    return promote_dup__(n1p, n2p);
  }
}

bool
rep_long_int_p(repv v)
{
  return rep_INTEGERP(v) || (rep_CONSP(v) && rep_INTP_2(rep_CAR(v), rep_CDR(v)));
}

repv
rep_make_long_uint(uintptr_t in)
{
  if (in < rep_LISP_MAX_INT) {
    return rep_MAKE_INT(in);
  } else {
    number_z *z = make_number(rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
    mpz_init_set_ui(z->z, in);
#else
    z->z = in;
#endif
    return rep_VAL(z);
  }
}

static NOT_INLINE repv
make_long_int__(intptr_t in)
{
  number_z *z = make_number(rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
  mpz_init_set_si(z->z, in);
#else
  z->z = in;
#endif
  return rep_VAL(z);
}

static inline repv
make_long_int(intptr_t in)
{
  if (in >= rep_LISP_MIN_INT && in <= rep_LISP_MAX_INT) {
    return rep_MAKE_INT(in);
  }

  return make_long_int__(in);
}

repv
rep_make_long_int(intptr_t in)
{
  return make_long_int(in);
}

uintptr_t
rep_get_long_uint(repv in)
{
  if (rep_INTP(in)) {
    return rep_INT(in);
  }

  if (rep_NUMBERP(in)) {
    switch (rep_NUMBER_TYPE(in)) {
    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
      return mpz_get_ui(rep_NUMBER(in,z));
#else
      return rep_NUMBER(in,z);
#endif

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
      return (uintptr_t)mpq_get_d(rep_NUMBER(in,q));
#endif

    case rep_NUMBER_FLOAT:
      return (uintptr_t)rep_NUMBER(in,f);
    }
  }

  if (rep_CONSP(in) && rep_INTP(rep_CAR(in)) && rep_INTP(rep_CDR(in))) {
    return rep_INT(rep_CAR(in)) | (rep_INT(rep_CDR(in)) << 24);
  }

  return 0;
}

intptr_t
rep_get_long_int(repv in)
{
  if (rep_INTP(in)) {
    return rep_INT(in);
  }

  if (rep_NUMBERP(in)) {
    switch (rep_NUMBER_TYPE(in))
    {
    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
      return mpz_get_si(rep_NUMBER(in,z));
#else
      return rep_NUMBER(in,z);
#endif

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
      return(intptr_t) mpq_get_d(rep_NUMBER(in,q));
#endif

    case rep_NUMBER_FLOAT:
      return(intptr_t) rep_NUMBER(in,f);
    }
  }

  if (rep_CONSP(in) && rep_INTP(rep_CAR(in)) && rep_INTP(rep_CDR(in))) {
    return rep_INT(rep_CAR(in)) | (rep_INT(rep_CDR(in)) << 24);
  }

  return 0;
}

repv
rep_make_longlong_int(long long in)
{
#if LONG_LONG_MAX == LONG_MAX

  return make_long_int(in);

#else

  if (in <= rep_LISP_MAX_INT && in >= rep_LISP_MIN_INT) {
    return rep_MAKE_INT(in);
  }

# ifdef HAVE_GMP

  int sign = (in < 0) ? -1 : 1;
  unsigned long long uin = (sign < 0) ? -in : in;
  unsigned long bottom = (unsigned long) uin;
  unsigned long top = (unsigned long) (uin >> (CHAR_BIT * sizeof(long)));
  number_z *z = make_number(rep_NUMBER_BIGNUM);
  mpz_init_set_ui(z->z, bottom);

  if (top != 0) {
    mpz_t tem;
    mpz_init_set_ui(tem, top);
    mpz_mul_2exp(tem, tem, CHAR_BIT * sizeof(long));
    mpz_add(z->z, z->z, tem);
    mpz_clear(tem);
  }
  if (sign < 0) {
    mpz_neg(z->z, z->z);
  }
  return rep_VAL(z);

# else

  number_z *z = make_number(rep_NUMBER_BIGNUM);
  z->z = in;
  return rep_VAL(z);

# endif /* !HAVE_GMP */
#endif /* !LONG_LONG_MAX == LONG_MAX */
}

long long
rep_get_longlong_int(repv in)
{
#if LONG_LONG_MAX == LONG_MAX

  return rep_get_long_int(in);

#else

  if (rep_INTP(in)) {
    return rep_INT(in);
  }

  if (rep_NUMBERP(in)) {
    switch (rep_NUMBER_TYPE(in)) {
    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
      {
	int sign = mpz_sgn(rep_NUMBER(in,z));
	mpz_t tem;
	mpz_init_set(tem, rep_NUMBER(in,z));
	if (sign < 0) {
	  mpz_neg(tem, tem);
	}
	long long bottom = mpz_get_ui(tem);
	mpz_tdiv_q_2exp(tem, tem, CHAR_BIT * sizeof(long));
	long long top = mpz_get_ui(tem);
	long long out = bottom | (top << (CHAR_BIT * sizeof(long)));
	if (sign < 0) {
	  out = -out;
	}
	mpz_clear(tem);
	return out;
      }
#else
      return rep_NUMBER(in,z);
#endif

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
      return (long long) mpq_get_d(rep_NUMBER(in,q));
#endif

    case rep_NUMBER_FLOAT:
      return (long long) rep_NUMBER(in,f);
    }
  }

  if (rep_CONSP(in) && rep_INTP(rep_CAR(in)) && rep_INTP(rep_CDR(in))) {
    long long out = rep_INT(rep_CDR(in));
    out = (out << 24) | rep_INT(rep_CAR(in));
    return out;
  }

  return 0;
#endif
}

repv
rep_make_float(double in, bool force)
{
  if (!force && floor(in) == in) {
    if (in < LONG_MAX && in > LONG_MIN) {
      return make_long_int((long) in);
    }
#if LONG_LONG_MAX > LONG_MAX
    else if (in < LONG_LONG_MAX && in > LONG_LONG_MIN) {
      return rep_make_longlong_int(in);
    }
#endif
  }

  number_f *f = make_number(rep_NUMBER_FLOAT);
  f->f = in;
  return rep_VAL(f);
}

double
rep_get_float(repv in)
{
  if (rep_NUMERICP(in)) {
    switch (rep_NUMERIC_TYPE(in)) {
    case rep_NUMBER_INT:
      return rep_INT(in);

    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
      return mpz_get_d(rep_NUMBER(in,z));
#else
      return rep_NUMBER(in,z);
#endif

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
      return mpq_get_d(rep_NUMBER(in,q));
#endif

    case rep_NUMBER_FLOAT:
      return rep_NUMBER(in,f);
    }
  }

  return 0;
}

/* This ignores exactness. */

int
rep_compare_numbers(repv v1, repv v2)
{
  if (rep_INTP_2(v1, v2)) {
    /* FIMXE: can we remove the rep_INT()'s here? */
    return rep_INT(v1) - rep_INT(v2);
  }

  if (!rep_NUMERICP(v1) || !rep_NUMERICP(v2)) {
    return 1;
  }

  promote(&v1, &v2);

  switch (rep_NUMERIC_TYPE(v1)) {
  case rep_NUMBER_INT:
    return rep_INT(v1) - rep_INT(v2);

  case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
    return mpz_cmp(rep_NUMBER(v1,z), rep_NUMBER(v2,z));
#else
    return rep_NUMBER(v1,z) - rep_NUMBER(v2,z);
#endif

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL:
    return mpq_cmp(rep_NUMBER(v1,q), rep_NUMBER(v2,q));
#endif

  case rep_NUMBER_FLOAT: {
    double d = rep_NUMBER(v1,f) - rep_NUMBER(v2,f);
    return d < 0 ? -1 : d > 0 ? +1 : 0; }
  }

  return 1;
}

/* This includes exactness in the comparison. */

static int
number_cmp(repv v1, repv v2)
{
  if (rep_INTP_2(v1, v2)) {
    /* FIMXE: can we remove the rep_INT()'s here? */
    return rep_INT(v1) - rep_INT(v2);
  }

  if (!rep_NUMERICP(v1) || !rep_NUMERICP(v2)) {
    return 1;
  }

  bool i1 = rep_NUMBER_INEXACT_P(v1);
  bool i2 = rep_NUMBER_INEXACT_P(v2);

  if ((i1 && !i2) || (!i1 && i2)) {
    return 1;
  }

  promote(&v1, &v2);

  switch (rep_NUMERIC_TYPE(v1)) {
  case rep_NUMBER_INT:
    return rep_INT(v1) - rep_INT(v2);

  case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
    return mpz_cmp(rep_NUMBER(v1,z), rep_NUMBER(v2,z));
#else
    return rep_NUMBER(v1,z) - rep_NUMBER(v2,z);
#endif

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL:
    return mpq_cmp(rep_NUMBER(v1,q), rep_NUMBER(v2,q));
#endif

  case rep_NUMBER_FLOAT: {
    double d = rep_NUMBER(v1,f) - rep_NUMBER(v2,f);
    return(d < 0) ? -1 : (d > 0) ? +1 : 0; }
  }

  return 1;
}

static const signed int map[] = {
  0,  1,  2,  3,  4,  5,  6,  7,		/* 0x30 -> 0x37 */
  8,  9, -1, -1, -1, -1, -1, -1,
  -1, 10, 11, 12, 13, 14, 15, 16,		/* 0x40 -> 0x48 */
  17, 18, 19, 20, 21, 22, 23, 24,
  25, 26, 27, 28, 29, 30, 31, 32,		/* 0x50 -> 0x58 */
  33, 34, 35, 36
};

#define MAP_SIZE 0x2c

#ifndef HAVE_GMP

static bool
parse_integer_to_float(char *buf, size_t len, int radix,
		       int sign, double *output)
{
  double value = 0;

  while (len-- > 0) {
    int c = *buf++;
    int d = rep_toupper(c) - '0';
    if (d < 0 || d >= MAP_SIZE) {
      return false;
    }
    d = map [d];
    if (d < 0 || d >= radix) {
      return false;
    }
    value = value * radix + d;
  }

  *output = (sign < 0) ? -value : value;
  return true;
}

#endif /* !HAVE_GMP */

#define INSTALL_LOCALE(var, type, locale)	\
  do {						\
    char *tem = setlocale(type, 0);		\
    if (tem) {					\
      size_t len = strlen(tem);			\
      char *copy = alloca(len + 1);		\
      memcpy(copy, tem, len);			\
      copy[len] = 0;				\
      (var) = copy;				\
      setlocale(type, locale);			\
    } else {					\
      (var) = 0;				\
    }						\
  } while (0)

repv
rep_parse_number(const char *buf, size_t len, int radix,
		 int sign, unsigned int type)
{
  if (len == 0) {
    return 0;
  }

  switch (type) {

  case 0: {
    size_t bits;
    switch (radix) {
    case 2:
      bits = len;
      break;

    case 8:
      bits = len * 3;
      break;

    case 10:
      /* log_2 10 = 3.3219.. ~ 27/8 */
      bits = (len * 27) / 8;
      break;

    case 16:
      bits = len * 4;
      break;

    default:
      abort();
    }

    if (bits < rep_LISP_INT_BITS) {
      intptr_t value = 0;
      char c;
      if (radix == 10) {
	/* Optimize most common case. */
	while (len-- > 0) {
	  c = *buf++;
	  if (c < '0' || c > '9') {
	    return 0;
	  }
	  value = value * 10 + (c - '0');
	}
      } else {
	while (len-- > 0) {
	  c = *buf++;
	  int d = rep_toupper(c) - '0';
	  if (d < 0 || d >= MAP_SIZE) {
	    return 0;
	  }
	  d = map [d];
	  if (d < 0 || d >= radix) {
	    return 0;
	  }
	  value = value * radix + d;
	}
      }
      return sign > 0 ? rep_MAKE_INT(value) : rep_MAKE_INT(value * -1);

    } else {
      number_z *z = make_number(rep_NUMBER_BIGNUM);

      char copy[len + 1];
      memcpy(copy, buf, len);
      copy[len] = 0;

#ifdef HAVE_GMP

      if (mpz_init_set_str(z->z, copy, radix) == 0) {
	if (sign < 0) {
	  mpz_neg(z->z, z->z);
	}
	return maybe_demote(rep_VAL(z));
      } else {
	return 0;
      }

#else /* !HAVE_GMP */

      char *tail;
      errno = 0;
# ifdef HAVE_STRTOLL
      long long value = strtoll(copy, &tail, radix);
# else
      long value = strtol(copy, &tail, radix);
# endif

      if (errno == ERANGE) {
	/* Overflow -- parse to a double, then convert back to an int. */
	double d;
	if (parse_integer_to_float(buf, len, radix, sign, &d)) {
	  if (d > INT64_MIN && d < INT64_MAX) {
	    z->z = d;
	    return maybe_demote(rep_VAL(z));
	  } else {
	    number_f *f = make_number(rep_NUMBER_FLOAT);
	    f->f = d;
	    return rep_VAL(f);
	  }
	} else {
	  return 0;
	}
      } else if (*tail != 0 || errno != 0) {
	return 0;		/* not all characters used */
      }

      z->z = (sign < 0) ? -value : value;
      return maybe_demote(rep_VAL(z));

#endif /* !HAVE_GMP */
    }
  /* not reached */ }

  case rep_NUMBER_RATIONAL: {
    char *tem = strchr(buf, '/');
    assert(tem != 0);

#ifdef HAVE_GMP

    number_q *q = make_number(rep_NUMBER_RATIONAL);
    mpq_init(q->q);

    char copy[tem - buf + 1];
    memcpy(copy, buf, tem - buf);
    copy[tem - buf] = 0;

    if (mpz_set_str(mpq_numref(q->q), copy, radix) == 0
	&& mpz_set_str(mpq_denref(q->q), tem + 1, radix) == 0)
    {
      if (mpz_sgn(mpq_denref(q->q)) == 0) {
	return 0;
      }

      mpq_canonicalize(q->q);

      if (sign < 0) {
	mpq_neg(q->q, q->q);
      }
      return maybe_demote(rep_VAL(q));

    } else {
      return 0;
    }

#else /* HAVE_GMP */

    repv num = rep_parse_number(buf, tem - buf, radix, 1, 0);
    repv den = rep_parse_number(tem + 1, len - (tem + 1 - buf), radix, 1, 0);
    if (!num || !den) {
      return 0;
    }
    num = rep_number_div(num, den);
    if (num && sign < 0) {
      num = rep_number_neg(num);
    }
    return num;

#endif /* !HAVE_GMP */

    /* not reached */ }

  case rep_NUMBER_FLOAT: {
    double d;
    if (len == 5 && strncmp(buf, "inf.0", 5) == 0) {
      d = HUGE_VAL;
    } else if (len == 5 && strncmp(buf, "nan.0", 5) == 0) {
      d = NAN;
    } else {
      char *tem = NULL;
#ifdef HAVE_STRTOD_L
      d = strtod_l(buf, &tem, NULL);
#else
# ifdef HAVE_SETLOCALE
      char *old_locale = NULL;
      INSTALL_LOCALE(old_locale, LC_NUMERIC, "C");
# endif
      d = strtod(buf, &tem);
# ifdef HAVE_SETLOCALE
      if (old_locale != 0) {
	setlocale(LC_NUMERIC, old_locale);
      }
# endif
#endif
      if (tem - buf != len) {
	return 0;
      }
    }
    number_f *f = make_number(rep_NUMBER_FLOAT);
    f->f = d * sign;
    return rep_VAL(f); }
  }

  abort();
}

char *
rep_print_number_to_string(repv obj, int radix, int prec)
{
  char *out = 0;

  if (!rep_NUMERICP(obj)) {
    return strdup("#<non-number>");
  }

  switch (rep_NUMERIC_TYPE(obj)) {
    char buf[128], fmt[8], *tem;

  case rep_NUMBER_INT:
    if (radix == 10) {
      tem = "%" PRIdPTR;
    } else if (radix == 16) {
      tem = "%" PRIxPTR;
    } else if (radix == 8) {
      tem = "%" PRIoPTR;
    } else {
      /* FIXME: implement properly..? */
      obj = promote_to(obj, rep_NUMBER_BIGNUM);
      goto do_bignum;
    }
    if (tem) {
#if defined(HAVE_SNPRINTF_L)
      snprintf_l(buf, sizeof(buf), NULL, tem, rep_INT(obj));
#elif defined(HAVE_SNPRINTF)
      snprintf(buf, sizeof(buf), tem, rep_INT(obj));
#else
      sprintf(buf, tem, rep_INT(obj));
#endif
      out = strdup(buf);
    }
    break;

  case rep_NUMBER_BIGNUM:
  do_bignum:
#ifdef HAVE_GMP
    out = mpz_get_str(0, radix, rep_NUMBER(obj,z));
#else
    {
      static const char *map = "0123456789abcdefghijklmnopqrstuvwxyz";
      char *ptr = buf, *optr;
      int64_t value = rep_NUMBER(obj,z);
      int sign = (value < 0) ? -1 : +1;
      while (value != 0) {
	int digit = value % radix;
	*ptr++ = map[ABS(digit)];
	value = value / radix;
      }
      if (sign < 0) {
	*ptr++ = '-';
      }
      out = malloc((ptr - buf) + 1);
      for (optr = out; ptr > buf;) {
	*optr++ = *(--ptr);
      }
      *optr = 0;
    }
#endif
    break;

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL: {
    size_t len = (mpz_sizeinbase(mpq_numref(rep_NUMBER(obj, q)), radix)
		  + mpz_sizeinbase(mpq_denref(rep_NUMBER(obj, q)), radix) + 4);
    out = malloc(len);
    mpz_get_str(out, radix, mpq_numref(rep_NUMBER(obj,q)));
    len = strlen(out);
    out[len++] = '/';
    mpz_get_str(out + len, radix, mpq_denref(rep_NUMBER(obj,q)));
    break; }
#endif

  case rep_NUMBER_FLOAT: {		/* FIXME: handle radix arg */
    if (isnan(rep_NUMBER(obj, f))) {
      strcpy(buf, "+nan.0");
    } else if (isinf(rep_NUMBER(obj, f))) {
      strcpy(buf, rep_NUMBER(obj, f) > 0 ? "+inf.0" : "-inf.0");
    } else {
#ifdef HAVE_SNPRINTF_L
      /* Allows us to pass in null (i.e. C) locale directly. */
      snprintf_l(fmt, sizeof(fmt), NULL, "%%.%dg", prec < 0 ? 16 : prec);
      snprintf_l(buf, sizeof(buf), NULL, fmt, rep_NUMBER(obj, f));
#else
      sprintf(fmt, "%%.%dg", prec < 0 ? 16 : prec);
# ifdef HAVE_SETLOCALE
      char *old_locale;
      INSTALL_LOCALE(old_locale, LC_NUMERIC, "C");
# endif
# ifdef HAVE_SNPRINTF
      snprintf(buf, sizeof(buf), fmt, rep_NUMBER(obj, f));
# else
      sprintf(buf, fmt, rep_NUMBER(obj, f));
# endif
# ifdef HAVE_SETLOCALE
      if (old_locale != 0) {
	setlocale(LC_NUMERIC, old_locale);
      }
# endif
#endif /* !HAVE_SNPRINTF_L */
      /* Libc doesn't always add a point */
      if (!strchr(buf, '.') && !strchr(buf, 'e') && !strchr(buf, 'E')) {
	strcat(buf, ".");
      }
    }
    out = strdup(buf);
    break; }
  }

  return out;
}

static void
number_prin(repv stream, repv obj)
{
  if (rep_INTP(obj)) {
    char buf[64];
#if defined(HAVE_SNPRINTF_L)
    snprintf_l(buf, sizeof(buf), NULL, "%" PRIdPTR, rep_INT(obj));
#elif defined(HAVE_SNPRINTF)
    snprintf(buf, sizeof(buf), "%" PRIdPTR, rep_INT(obj));
#else
    sprintf(buf, "%" PRIdPTR, rep_INT(obj));
#endif
    rep_stream_puts(stream, buf, -1, false);
  } else {
    char *string = rep_print_number_to_string(obj, 10, -1);
    if (string != 0) {
      rep_stream_puts(stream, string, -1, false);
      free(string);
    } else {
      rep_stream_puts(stream, "#<unprintable number>", -1, false);
    }
  }
}


/* Arithmetic primitives. */

static inline repv
number_foldv(int argc, repv *argv, repv(*op) (repv, repv))
{
  if (argc < 1) {
    return rep_signal_missing_arg(1);
  }
  if (!rep_NUMERICP(argv[0])) {
    return rep_signal_arg_error(argv[0], 1);
  }

  repv sum = argv[0];

  for (int i = 1; i < argc; i++) {
    if (!rep_NUMERICP(argv[i])) {
      return rep_signal_arg_error(argv[i], i + 1);
    }

    sum = op(sum, argv[i]);
  }

  return sum;
}

static inline repv
integer_foldv(int argc, repv *argv, repv(*op) (repv, repv))
{
  if (argc < 1) {
    return rep_signal_missing_arg(1);
  }
  if (!rep_INTEGERP(argv[0])) {
    return rep_signal_arg_error(argv[0], 1);
  }

  repv sum = argv[0];

  for (int i = 1; i < argc; i++) {
    if (!rep_INTEGERP(argv[i])) {
      return rep_signal_arg_error(argv[i], i + 1);
    }

    sum = op(sum, argv[i]);
  }

  return sum;
}

static inline repv
foldv(int argc, repv *argv, repv(*op) (repv, repv))
{
  if (argc < 1) {
    return rep_signal_missing_arg(1);
  }

  repv sum = argv[0];

  for (int i = 1; i < argc; i++) {
    sum = op(sum, argv[i]);
  }

  return sum;
}

static inline intptr_t
modulo_int(intptr_t a, intptr_t b)
{
  /* This code from GNU Emacs */

  intptr_t c = a % b;

  /* If the "remainder" comes out with the wrong sign, fix it.  */

  if (b < 0 ? c > 0 : c < 0) {
    c += b;
  }

  return c;
}

static inline double
round_even(double x)
{
  /* From Guile. */

  double plus_half = x + 0.5;
  double result = floor(plus_half);

  /* Adjust so that the round is towards even.  */

  return ((plus_half == result && plus_half / 2 != floor(plus_half / 2))
	  ? result - 1 : result);
}

/* "Slow" arithmetic functions. These are only called if both
    arguments are fixnums. (Or possibly if the divisor is zero).
    They're marked non-inline to avoid making the prolog of the
    calling fast-path function slower. */

static NOT_INLINE repv
rep_number_add__(repv x, repv y)
{
  rep_DECLARE1(x, rep_NUMERICP);
  rep_DECLARE2(y, rep_NUMERICP);

  repv out = promote_dup__(&x, &y);

  switch (rep_NUMERIC_TYPE(out)) {
  case rep_NUMBER_INT:
    out = rep_make_long_int(rep_INT(x) + rep_INT(y));
    break;

  case rep_NUMBER_BIGNUM: {
#ifdef HAVE_GMP
    mpz_add(rep_NUMBER(out,z), rep_NUMBER(x,z), rep_NUMBER(y,z));
#else
    double t = (double) rep_NUMBER(x,z) + (double) rep_NUMBER(y,z);
    if (t > INT64_MIN && t < INT64_MAX) {
      rep_NUMBER(out,z) = rep_NUMBER(x,z) + rep_NUMBER(y,z);
    } else {
      out = rep_make_float(t, true);
    }
#endif
    out = maybe_demote(out);
    break; }

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL:
    mpq_add(rep_NUMBER(out,q), rep_NUMBER(x,q), rep_NUMBER(y,q));
    out = maybe_demote(out);
    break;
#endif
    
  case rep_NUMBER_FLOAT:
    rep_NUMBER(out,f) = rep_NUMBER(x,f) + rep_NUMBER(y,f);
    break;
  }

  return out;
}

static NOT_INLINE repv
rep_number_add1__(repv x)
{
  rep_DECLARE1(x, rep_NUMERICP);

  switch (rep_NUMERIC_TYPE(x)) {
  case rep_NUMBER_INT:
    return rep_make_long_int(rep_INT(x) + 1);

  case rep_NUMBER_BIGNUM:
    x = copy_number(x);
#ifdef HAVE_GMP
    mpz_add_ui(rep_NUMBER(x,z), rep_NUMBER(x,z), 1);
#else
    rep_NUMBER(x,z) = rep_NUMBER(x,z) + 1;
#endif
    return maybe_demote(x);

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL: {
    x = copy_number(x);
    mpq_t temq;
    mpq_init(temq);
    mpq_set_ui(temq, 1, 1);
    mpq_add(rep_NUMBER(x,q), rep_NUMBER(x,q), temq);
    mpq_clear(temq);
    return maybe_demote(x); }
#endif

  case rep_NUMBER_FLOAT:
    x = copy_number(x);
    rep_NUMBER(x,f) = rep_NUMBER(x,f) + 1;
    return x;

  default:
    return rep_signal_arg_error(x, 1);
  }

  /* not reached */
}

static NOT_INLINE repv
rep_number_neg__(repv x)
{
  rep_DECLARE1(x, rep_NUMERICP);

  repv out = copy_number(x);

  switch (rep_NUMERIC_TYPE(out)) {
  case rep_NUMBER_INT:
    out = rep_make_long_int(-rep_INT(x));
    break;

  case rep_NUMBER_BIGNUM: {
#ifdef HAVE_GMP
    mpz_neg(rep_NUMBER(out,z), rep_NUMBER(x,z));
#else
    double t = - (double) rep_NUMBER(x,z);
    if (t > INT64_MIN && t < INT64_MAX) {
      rep_NUMBER(out,z) = - rep_NUMBER(x,z);
    } else {
      out = rep_make_float(t, true);
    }
#endif
    break; }

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL:
    mpq_neg(rep_NUMBER(out,q), rep_NUMBER(x,q));
    break;
#endif

  case rep_NUMBER_FLOAT:
    rep_NUMBER(out,f) = -rep_NUMBER(x,f);
    break;
  }

  return out;
}

static NOT_INLINE repv
rep_number_sub__(repv x, repv y)
{
  rep_DECLARE1(x, rep_NUMERICP);
  rep_DECLARE2(y, rep_NUMERICP);

  repv out = promote_dup__(&x, &y);

  switch (rep_NUMERIC_TYPE(out)) {
  case rep_NUMBER_INT:
    out = rep_make_long_int(rep_INT(x) - rep_INT(y));
    break;

  case rep_NUMBER_BIGNUM: {
#ifdef HAVE_GMP
    mpz_sub(rep_NUMBER(out,z), rep_NUMBER(x,z), rep_NUMBER(y,z));
#else
    double t = (double) rep_NUMBER(x,z) - (double) rep_NUMBER(y,z);
    if (t > INT64_MIN && t < INT64_MAX) {
      rep_NUMBER(out,z) = rep_NUMBER(x,z) - rep_NUMBER(y,z);
    } else {
      out = rep_make_float(t, true);
    }
#endif
    out = maybe_demote(out);
    break; }

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL:
    mpq_sub(rep_NUMBER(out,q), rep_NUMBER(x,q), rep_NUMBER(y,q));
    out = maybe_demote(out);
    break;
#endif
    
  case rep_NUMBER_FLOAT:
    rep_NUMBER(out,f) = rep_NUMBER(x,f) - rep_NUMBER(y,f);
    break;
  }

  return out;
}

static NOT_INLINE repv
rep_number_sub1__(repv x)
{
  rep_DECLARE1(x, rep_NUMERICP);

  switch (rep_NUMERIC_TYPE(x)) {
  case rep_NUMBER_INT:
    return rep_make_long_int(rep_INT(x) - 1);

  case rep_NUMBER_BIGNUM:
    x = copy_number(x);
#ifdef HAVE_GMP
    mpz_sub_ui(rep_NUMBER(x,z), rep_NUMBER(x,z), 1);
#else
    rep_NUMBER(x,z) = rep_NUMBER(x,z) - 1;
#endif
    return maybe_demote(x);

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL: {
    x = copy_number(x);
    mpq_t temq;
    mpq_init(temq);
    mpq_set_si(temq, 1, 1);
    mpq_sub(rep_NUMBER(x,q), rep_NUMBER(x,q), temq);
    mpq_clear(temq);
    return maybe_demote(x); }
#endif

  case rep_NUMBER_FLOAT:
    x = copy_number(x);
    rep_NUMBER(x,f) = rep_NUMBER(x,f) - 1;
    return x;

  default:
    return rep_signal_arg_error(x, 1);
  }
}

static NOT_INLINE repv
rep_number_mul__(repv x, repv y)
{
  rep_DECLARE1(x, rep_NUMERICP);
  rep_DECLARE2(y, rep_NUMERICP);

  repv out = promote_dup__(&x, &y);

  switch (rep_NUMERIC_TYPE(out)) {
  case rep_NUMBER_INT: {
    /* Only got here because rep_number_mul() overflowed. */
#ifdef HAVE_GMP
    number_z *z = make_number(rep_NUMBER_BIGNUM);
    mpz_init_set_si(z->z, rep_INT(x));
    mpz_t tem;
    mpz_init_set_si(tem, rep_INT(y));
    mpz_mul(z->z, z->z, tem);
    mpz_clear(tem);
    out = maybe_demote(rep_VAL(z));
#else
    out = rep_make_float((double)a * (double)b, false);
#endif
    break; }

  case rep_NUMBER_BIGNUM: {
#ifdef HAVE_GMP
    mpz_mul(rep_NUMBER(out,z), rep_NUMBER(x,z), rep_NUMBER(y,z));
#else
    double t = (double) rep_NUMBER(x,z) * (double) rep_NUMBER(y,z);
    if (t > INT64_MIN && t < INT64_MAX) {
      rep_NUMBER(out,z) = rep_NUMBER(x,z) * rep_NUMBER(y,z);
    } else {
      out = rep_make_float(t, true);
    }
#endif
    out = maybe_demote(out);
    break; }

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL:
    mpq_mul(rep_NUMBER(out,q), rep_NUMBER(x,q), rep_NUMBER(y,q));
    out = maybe_demote(out);
    break;
#endif
    
  case rep_NUMBER_FLOAT:
    rep_NUMBER(out,f) = rep_NUMBER(x,f) * rep_NUMBER(y,f);
    break;
  }

  return out;
}

static NOT_INLINE repv
rep_number_div__(repv x, repv y)
{
  rep_DECLARE1(x, rep_NUMERICP);
  rep_DECLARE2(y, rep_NUMERICP);

  if (!rep_NUMBER_INEXACT_P(y) && ZEROP(y)) {
    return Fsignal(Qarith_error, rep_LIST_1(rep_VAL(&div_zero)));
  }

  repv out = promote_dup__(&x, &y);

  switch (rep_NUMERIC_TYPE(out)) {
  case rep_NUMBER_INT:
    if (rep_INT(x) % rep_INT(y) == 0) {
      out = rep_MAKE_INT(rep_INT(x) / rep_INT(y));
    } else {
#ifdef HAVE_GMP
      /* FIXME: rep_INT() is wider than long in 64-bit. */
      unsigned long uy = rep_INT(y) < 0 ? - rep_INT(y) : rep_INT(y);
      number_q *q = make_number(rep_NUMBER_RATIONAL);
      mpq_init(q->q);
      mpq_set_si(q->q, rep_INT(x), uy);
      mpq_canonicalize(q->q);
      if (rep_INT(y) < 0) {
	mpq_neg(q->q, q->q);
      }
      out = rep_VAL(q);
#else
      number_f *f = make_number(rep_NUMBER_FLOAT);
      f->f = ((double) rep_INT(x)) / ((double) rep_INT(y));
      out = rep_VAL(f);
#endif
    }
    break;

  case rep_NUMBER_BIGNUM: {
#ifdef HAVE_GMP
    mpz_t rem;
    mpz_init(rem);
    mpz_tdiv_r(rem, rep_NUMBER(x,z), rep_NUMBER(y,z));
    int sign = mpz_sgn(rem);
    mpz_clear(rem);
    if (sign == 0) {
      mpz_tdiv_q(rep_NUMBER(out,z), rep_NUMBER(x,z), rep_NUMBER(y,z));
      out = maybe_demote(out);
    } else {
      number_q *q = make_number(rep_NUMBER_RATIONAL);
      mpq_init(q->q);
      mpq_set_z(q->q, rep_NUMBER(x,z));
      mpq_t div;
      mpq_init(div);
      mpq_set_z(div, rep_NUMBER(y,z));
      mpq_div(q->q, q->q, div);
      mpq_clear(div);
      out = rep_VAL(q);
    }
#else
    if (rep_NUMBER(x,z) % rep_NUMBER(y,z) == 0) {
      number_z *z = make_number(rep_NUMBER_BIGNUM);
      z->z = rep_NUMBER(x,z) / rep_NUMBER(y,z);
      out = rep_VAL(z);
    } else {
      number_f *f = make_number(rep_NUMBER_FLOAT);
      f->f = ((double) rep_NUMBER(x,z)) / ((double) rep_NUMBER(y,z));
      out = rep_VAL(f);
    }
#endif
    break; }

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL:
    mpq_div(rep_NUMBER(out,q), rep_NUMBER(x,q), rep_NUMBER(y,q));
    out = maybe_demote(out);
    break;
#endif
    
  case rep_NUMBER_FLOAT:
    rep_NUMBER(out,f) = rep_NUMBER(x,f) / rep_NUMBER(y,f);
    break;
  }

  return out;
}

static NOT_INLINE repv
rep_number_lognot__(repv x)
{
  rep_DECLARE1(x, rep_NUMERICP);

  repv out;
  switch (rep_NUMERIC_TYPE(x)) {
  case rep_NUMBER_INT:
    out = rep_MAKE_INT(~rep_INT(x));
    break;

  case rep_NUMBER_BIGNUM: {
    number_z *z = make_number(rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
    mpz_init(z->z);
    mpz_com(z->z, rep_NUMBER(x,z));
#else
    z->z = ~rep_NUMBER(x,z);
#endif
    out = rep_VAL(z);
    break; }

  default:
    out = rep_signal_arg_error(x, 1);
  }

  return out;
}

static NOT_INLINE repv
rep_number_logior__(repv x, repv y)
{
  rep_DECLARE1(x, rep_NUMERICP);
  rep_DECLARE2(y, rep_NUMERICP);

  repv out = promote_dup__(&x, &y);

  switch (rep_NUMERIC_TYPE(out)) {
  case rep_NUMBER_INT:
    out = rep_MAKE_INT(rep_INT(x) | rep_INT(y));
    break;

  case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
    mpz_ior(rep_NUMBER(out,z), rep_NUMBER(x,z), rep_NUMBER(y,z));
#else
    rep_NUMBER(out,z) = rep_NUMBER(x,z) | rep_NUMBER(y,z);
#endif
    break;

  default:
    out = rep_signal_arg_error(x, 1);
  }

  return out;
}

static NOT_INLINE repv
rep_number_logxor__(repv x, repv y)
{
  rep_DECLARE1(x, rep_NUMERICP);
  rep_DECLARE2(y, rep_NUMERICP);

  repv out = promote_dup__(&x, &y);

  switch (rep_NUMERIC_TYPE(out)) {
  case rep_NUMBER_INT:
    out = rep_MAKE_INT(rep_INT(x) ^ rep_INT(y));
    break;

  case rep_NUMBER_BIGNUM: {
#ifdef HAVE_GMP
    /* XXX is this correct: x^y = x|y & ~(x&y) */
    mpz_t tem;
    mpz_init(tem);
    mpz_ior(tem, rep_NUMBER(x,z), rep_NUMBER(y,z));
    mpz_and(rep_NUMBER(out,z), rep_NUMBER(x,z), rep_NUMBER(y,z));
    mpz_com(rep_NUMBER(out,z), rep_NUMBER(out,z));
    mpz_and(rep_NUMBER(out,z), rep_NUMBER(out,z), tem);
    mpz_clear(tem);
#else
    rep_NUMBER(out,z) = rep_NUMBER(x,z) ^ rep_NUMBER(y,z);
#endif
    break; }

  default:
    out = rep_signal_arg_error(x, 1);
  }

  return out;
}

static NOT_INLINE repv
rep_number_logand__(repv x, repv y)
{
  rep_DECLARE1(x, rep_NUMERICP);
  rep_DECLARE2(y, rep_NUMERICP);

  repv out = promote_dup__(&x, &y);

  switch (rep_NUMERIC_TYPE(out)) {
  case rep_NUMBER_INT:
    out = rep_MAKE_INT(rep_INT(x) & rep_INT(y));
    break;

  case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
    mpz_and(rep_NUMBER(out,z), rep_NUMBER(x,z), rep_NUMBER(y,z));
#else
    rep_NUMBER(out,z) = rep_NUMBER(x,z) & rep_NUMBER(y,z);
#endif
    break;

  default:
    out = rep_signal_arg_error(x, 1);
  }

  return out;
}

static NOT_INLINE repv
rep_number_max__(repv x, repv y)
{
  repv max;

  if (rep_NUMBERP(x) || rep_NUMBERP(y)) {
    max = (rep_compare_numbers(x, y) >= 0) ? x : y;
    if (rep_NUMBER_INEXACT_P(x) || rep_NUMBER_INEXACT_P(y)) {
      max = Fexact_to_inexact(max);
    }
  } else {
    max = (rep_value_cmp(x, y) >= 0) ? x : y;
  }

  return max;
}

static NOT_INLINE repv
rep_number_min__(repv x, repv y)
{
  repv min;

  if (rep_NUMBERP(x) || rep_NUMBERP(y)) {
    min = (rep_compare_numbers(x, y) <= 0) ? x : y;
    if (rep_NUMBER_INEXACT_P(x) || rep_NUMBER_INEXACT_P(y)) {
      min = Fexact_to_inexact(min);
    }
  } else {
    min = (rep_value_cmp(x, y) <= 0) ? x : y;
  }

  return min;
}

static NOT_INLINE repv
Ffloor__(repv x)
{
  rep_DECLARE1(x, rep_NUMBERP);

  switch (rep_NUMBER_TYPE(x)) {
  case rep_NUMBER_BIGNUM:
    return x;

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL: {
    number_z *z = make_number(rep_NUMBER_BIGNUM);
    mpz_init(z->z);
    mpz_fdiv_q(z->z, mpq_numref(rep_NUMBER(x,q)),
	       mpq_denref(rep_NUMBER(x, q)));
    return maybe_demote(rep_VAL(z)); }
#endif

  case rep_NUMBER_FLOAT:
    return rep_make_float(floor(rep_NUMBER(x,f)), true);

  default:
    return rep_signal_arg_error(x, 1);
  }
}

static NOT_INLINE repv
Fceiling__(repv x)
{
  rep_DECLARE1(x, rep_NUMBERP);

  switch (rep_NUMBER_TYPE(x)) {
  case rep_NUMBER_BIGNUM:
    return x;

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL: {
    number_z *z = make_number(rep_NUMBER_BIGNUM);
    mpz_init(z->z);
    mpz_cdiv_q(z->z, mpq_numref(rep_NUMBER(x,q)),
	       mpq_denref(rep_NUMBER(x, q)));
    return maybe_demote(rep_VAL(z)); }
#endif

  case rep_NUMBER_FLOAT:
    return rep_make_float(ceil(rep_NUMBER(x,f)), true);

  default:
    return rep_signal_arg_error(x, 1);
  }
}

static NOT_INLINE repv
Ftruncate__(repv x)
{
  rep_DECLARE1(x, rep_NUMBERP);

  switch (rep_NUMBER_TYPE(x)) {
  case rep_NUMBER_BIGNUM:
    return x;

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL: {
    number_z *z = make_number(rep_NUMBER_BIGNUM);
    mpz_init(z->z);
    mpz_tdiv_q(z->z, mpq_numref(rep_NUMBER(x,q)),
	       mpq_denref(rep_NUMBER(x, q)));
    return maybe_demote(rep_VAL(z)); }
#endif

  case rep_NUMBER_FLOAT: {
    double d = rep_NUMBER(x,f);
    d = d < 0 ? -floor(-d) : floor(d);
    return rep_make_float(d, true); }

  default:
    return rep_signal_arg_error(x, 1);
  }
}

static NOT_INLINE repv
Fround__(repv x)
{
  rep_DECLARE1(x, rep_NUMBERP);

  switch (rep_NUMBER_TYPE(x)) {
  case rep_NUMBER_INT:
  case rep_NUMBER_BIGNUM:
    return x;

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL:
    return rep_make_long_int((intptr_t)round_even
			     (mpq_get_d(rep_NUMBER(x,q))));
#endif

  case rep_NUMBER_FLOAT:
    return rep_make_float(round_even(rep_NUMBER(x,f)), true);

  default:
    return rep_signal_arg_error(x, 1);
  }
}

static NOT_INLINE repv
Fremainder__(repv n1, repv n2)
{
  rep_DECLARE1(n1, rep_NUMERICP);
  rep_DECLARE2(n2, rep_NUMERICP);

  if (!rep_NUMBER_INEXACT_P(n2) && ZEROP(n2)) {
    return Fsignal(Qarith_error, rep_LIST_1(rep_VAL(&div_zero)));
  }

  repv out = promote_dup__(&n1, &n2);

  switch (rep_NUMERIC_TYPE(out)) {
  case rep_NUMBER_INT:
    out = rep_MAKE_INT(rep_INT(n1) % rep_INT(n2));
    break;

  case rep_NUMBER_BIGNUM: {
#ifdef HAVE_GMP
    mpz_tdiv_r(rep_NUMBER(out,z), rep_NUMBER(n1,z), rep_NUMBER(n2,z));
#else
    rep_NUMBER(out,z) = rep_NUMBER(n1,z) % rep_NUMBER(n2,z);
#endif
    out = maybe_demote(out);
    break; }

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL: {
    /* See modulo. */
    mpz_t d1n2, d2n1;
    mpz_init(d1n2);
    mpz_init(d2n1);
    mpz_mul(d1n2, mpq_denref(rep_NUMBER(n1,q)), mpq_numref(rep_NUMBER(n2,q)));
    mpz_mul(d2n1, mpq_denref(rep_NUMBER(n2,q)), mpq_numref(rep_NUMBER(n1,q)));
    mpz_mul(mpq_denref(rep_NUMBER(out,q)),
	    mpq_denref(rep_NUMBER(n1,q)), mpq_denref(rep_NUMBER(n2,q)));
    mpz_tdiv_q(mpq_numref(rep_NUMBER(out,q)), d2n1, d1n2);
    mpz_mul(mpq_numref(rep_NUMBER(out,q)),
	    mpq_numref(rep_NUMBER(out,q)), d1n2);
    mpz_sub(mpq_numref(rep_NUMBER(out,q)),
	    d2n1, mpq_numref(rep_NUMBER(out,q)));
    mpz_clear(d1n2);
    mpz_clear(d2n1);
    mpq_canonicalize(rep_NUMBER(out,q));
    out = maybe_demote(out);
    break; }
#endif

  case rep_NUMBER_FLOAT:
    rep_NUMBER(out,f) = fmod(rep_NUMBER(n1,f), rep_NUMBER(n2,f));
    break;

  default:
    return rep_signal_arg_error(n1, 1);
  }

  return out;
}

static NOT_INLINE repv
Fmodulo__(repv n1, repv n2)
{
  rep_DECLARE1(n1, rep_NUMERICP);
  rep_DECLARE2(n2, rep_NUMERICP);

  if (!rep_NUMBER_INEXACT_P(n2) && ZEROP(n2)) {
    return Fsignal(Qarith_error, rep_LIST_1(rep_VAL(&div_zero)));
  }

  repv out = promote_dup__(&n1, &n2);

  switch (rep_NUMERIC_TYPE(out)) {
  case rep_NUMBER_INT: {
    out = rep_MAKE_INT(modulo_int(rep_INT(n1), rep_INT(n2)));
    break; }

  case rep_NUMBER_BIGNUM: {
#ifdef HAVE_GMP
    mpz_tdiv_r(rep_NUMBER(out,z), rep_NUMBER(n1,z), rep_NUMBER(n2,z));
    /* If the "remainder" comes out with the wrong sign, fix it.  */
    int sign = mpz_sgn(rep_NUMBER(out,z));
    if (mpz_sgn(rep_NUMBER(n2,z)) < 0 ? sign > 0 : sign < 0) {
      mpz_add(rep_NUMBER(out,z), rep_NUMBER(out,z), rep_NUMBER(n2,z));
    }
#else
    long long z = rep_NUMBER(n1,z) % rep_NUMBER(n2,z);
    if (rep_NUMBER(n2,z) < 0 ? z > 0 : z < 0) {
      z += rep_NUMBER(n2,z);
    }
    rep_NUMBER(out,z) = z;
#endif
    out = maybe_demote(out);
    break; }

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL: {
    /* From the definition above: modulo(n1/d1, n2/d2)
         = (d2*n1 - floor(d2*n1 / d1*n2) * d1*n2) / (d1*d2). */
    mpz_t d1n2, d2n1;
    mpz_init(d1n2);
    mpz_init(d2n1);
    mpz_mul(d1n2, mpq_denref(rep_NUMBER(n1,q)), mpq_numref(rep_NUMBER(n2,q)));
    mpz_mul(d2n1, mpq_denref(rep_NUMBER(n2,q)), mpq_numref(rep_NUMBER(n1,q)));
    mpz_mul(mpq_denref(rep_NUMBER(out,q)),
	    mpq_denref(rep_NUMBER(n1,q)), mpq_denref(rep_NUMBER(n2,q)));
    mpz_fdiv_q(mpq_numref(rep_NUMBER(out,q)), d2n1, d1n2);
    mpz_mul(mpq_numref(rep_NUMBER(out,q)),
	    mpq_numref(rep_NUMBER(out,q)), d1n2);
    mpz_sub(mpq_numref(rep_NUMBER(out,q)),
	    d2n1, mpq_numref(rep_NUMBER(out,q)));
    mpz_clear(d1n2);
    mpz_clear(d2n1);
    mpq_canonicalize(rep_NUMBER(out,q));
    out = maybe_demote(out);
    break; }
#endif

  case rep_NUMBER_FLOAT: {
    double f1 = rep_NUMBER(n1,f);
    double f2 = rep_NUMBER(n2,f);
    double d = fmod(f1, f2);
    if (f2 < 0 ? d > 0 : d < 0) {
      d += f2;
    }
    rep_NUMBER(out,f) = d;
    break; }

  default:
    out = rep_signal_arg_error(n1, 1);
  }

  return out;
}

static NOT_INLINE repv
Fquotient__(repv n1, repv n2)
{
  rep_DECLARE1(n1, rep_NUMERICP);
  rep_DECLARE2(n2, rep_NUMERICP);

  if (!rep_NUMBER_INEXACT_P(n2) && ZEROP(n2)) {
    return Fsignal(Qarith_error, rep_LIST_1(rep_VAL(&div_zero)));
  }

  promote(&n1, &n2);

  repv out;

  switch (rep_NUMERIC_TYPE(n1)) {
  case rep_NUMBER_INT:
    out = rep_MAKE_INT(rep_INT(n1) / rep_INT(n2));
    break;

  case rep_NUMBER_BIGNUM: {
    number_z *z = make_number(rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
    mpz_init(z->z);
    mpz_tdiv_q(z->z, rep_NUMBER(n1,z), rep_NUMBER(n2,z));
#else
    rep_NUMBER(z,z) = rep_NUMBER(n1,z) / rep_NUMBER(n2,z);
#endif
    out = maybe_demote(rep_VAL(z));
    break; }

#ifdef HAVE_GMP
  case rep_NUMBER_RATIONAL: {
    number_z *z = make_number(rep_NUMBER_BIGNUM);
    mpz_t d1n2, d2n1;
    mpz_init(d1n2);
    mpz_init(d2n1);
    mpz_mul(d1n2, mpq_denref(rep_NUMBER(n1,q)), mpq_numref(rep_NUMBER(n2,q)));
    mpz_mul(d2n1, mpq_denref(rep_NUMBER(n2,q)), mpq_numref(rep_NUMBER(n1,q)));
    mpz_init(z->z);
    mpz_tdiv_q(z->z, d2n1, d1n2);
    mpz_clear(d1n2);
    mpz_clear(d2n1);
    out = maybe_demote(rep_VAL(z));
    break; }
#endif

  case rep_NUMBER_FLOAT: {
    double d = rep_NUMBER(n1,f) / rep_NUMBER(n2,f);
    d = d < 0 ? -floor(-d) : floor(d);
    out = rep_make_float(d, true);
    break; }

  default:
    return rep_signal_arg_error(n1, 1);
  }

  return out;
}

/* Fast path functions. These assume rep_MAKE_INT(x) == 4x + 2. */

repv
rep_number_add(repv x, repv y)
{
  if (rep_INTP_2(x, y)) {
#if defined(HAVE_OVERFLOW_BUILTINS) && INTPTR_MAX == LONG_MAX
    long z;
    if (!__builtin_saddl_overflow(x, y - 2, &z)) {
      return (repv)z;
    }
#else
    return make_long_int(rep_INT(x) + rep_INT(y));
#endif
  }

  return rep_number_add__(x, y);
}

repv
rep_number_neg(repv x)
{
  if (rep_INTP(x)) {
#if defined(HAVE_OVERFLOW_BUILTINS) && INTPTR_MAX == LONG_MAX
    long y;
    if (!__builtin_ssubl_overflow(4, x, &y)) {
      return (repv)y;
    }
#else
    return make_long_int(-rep_INT(x));
#endif
  }

  return rep_number_neg__(x);
}

repv
rep_number_sub(repv x, repv y)
{
  if (rep_INTP_2(x, y)) {
#if defined(HAVE_OVERFLOW_BUILTINS) && INTPTR_MAX == LONG_MAX
    long z;
    if (!__builtin_ssubl_overflow(x, y - 2, &z)) {
      return (repv)z;
    }
#else
    return make_long_int(rep_INT(x) - rep_INT(y));
#endif
  }

  return rep_number_sub__(x, y);
}

repv
rep_number_mul(repv x, repv y)
{
  if (rep_INTP_2(x, y)) {
#if defined(HAVE_OVERFLOW_BUILTINS) && INTPTR_MAX == LONG_MAX
    long z;
    /* ((4x+2)-2) * y + 2 == 4xy + 2. */
    if (!__builtin_smull_overflow(x - 2, rep_INT(y), &z)
	&& !__builtin_saddl_overflow(z, 2, &z)) {
      return z;
    }
#elif INTPTR_MAX < LONG_LONG_MAX
    long long z = (long long)rep_INT(x) * (long long)rep_INT(y);
    return rep_make_longlong_int(z);
#else
    /* No larger integral type to cast input values to. */
    long long a = rep_INT(x);
    long long b = rep_INT(y);
    long long c = a * b;
    if (b == 0 || a <= INT64_MAX / b) {
      return rep_make_longlong_int(c);
    }
#endif
  }

  return rep_number_mul__(x, y);
}

repv
rep_number_div(repv x, repv y)
{
  if (rep_INTP_2(x, y) && y != rep_MAKE_INT(0)) {
    return rep_MAKE_INT(rep_INT(x) / rep_INT(y));
  }

  return rep_number_div__(x, y);
}

repv
rep_number_lognot(repv x)
{
  if (rep_INTP(x)) {
    return rep_MAKE_INT(~rep_INT(x));
  }

  return rep_number_lognot__(x);
}

repv
rep_number_logior(repv x, repv y)
{
  if (rep_INTP_2(x, y)) {
    return rep_MAKE_INT(rep_INT(x) | rep_INT(y));
  }

  return rep_number_logior__(x, y);
}

repv
rep_number_logxor(repv x, repv y)
{
  if (rep_INTP_2(x, y)) {
    return rep_MAKE_INT(rep_INT(x) ^ rep_INT(y));
  }

  return rep_number_logxor__(x, y);
}

repv
rep_number_logand(repv x, repv y)
{
  if (rep_INTP_2(x, y)) {
    return rep_MAKE_INT(rep_INT(x) & rep_INT(y));
  }

  return rep_number_logand__(x, y);
}

repv
rep_number_max(repv x, repv y)
{
  if (rep_INTP_2(x, y)) {
    return rep_INT(x) > rep_INT(y) ? x : y;
  }

  return rep_number_max__(x, y);
}

repv
rep_number_min(repv x, repv y)
{
  if (rep_INTP_2(x, y)) {
    return rep_INT(x) < rep_INT(y) ? x : y;
  }

  return rep_number_min__(x, y);
}

repv
rep_integer_gcd(repv x, repv y)
{
  repv out = promote_dup(&x, &y);

  if (rep_INTP(x)) {
    /* Euclid's algorithm */

    intptr_t m = rep_INT(x);
    intptr_t n = rep_INT(y);
    m = ABS(m); n = ABS(n);

    while (m != 0) {
      intptr_t t = n % m;
      n = m;
      m = t;
    }

    out = rep_MAKE_INT(n);

  } else {
#ifdef HAVE_GMP
    mpz_gcd(rep_NUMBER(out,z), rep_NUMBER(x,z), rep_NUMBER(y,z));
#else
    /* Same as above */

    long long m = rep_NUMBER(x,z);
    long long n = rep_NUMBER(y,z);
    m = ABS(m); n = ABS(n);

    while (m != 0) {
      long long t = n % m;
      n = m;
      m = t;
    }

    rep_NUMBER(out,z) = n;
#endif
  }

  return out;
}


/* Lisp functions. */

DEFUN("+", Fplus, Splus, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#+::
+ NUMBERS...

Adds all NUMBERS together. If no arguments are given returns 0.
::end:: */
{
  if (argc == 2) {
    return rep_number_add(argv[0], argv[1]);
  } else if (argc == 0) {
    return rep_MAKE_INT(0);
  } else {
    return number_foldv(argc, argv, rep_number_add);
  }
}

DEFUN("-", Fminus, Sminus, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#-::
- NUMBER [NUMBERS...]

Either returns the negation of NUMBER or the value of NUMBER minus
NUMBERS
::end:: */
{
  if (argc == 2) {
    return rep_number_sub(argv[0], argv[1]);
  } else if (argc == 1) {
    return rep_number_neg(argv[0]);
  } else if (argc == 0) {
    return rep_signal_missing_arg(1);
  } else {
    return number_foldv(argc, argv, rep_number_sub);
  }
}

DEFUN("*", Fproduct, Sproduct, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#*::
* NUMBERS...

Multiplies all NUMBERS together. If no numbers are given returns 1.
::end:: */
{
  if (argc == 2) {
    return rep_number_mul(argv[0], argv[1]);
  } else   if (argc == 0) {
    return rep_MAKE_INT(1);
  } else {
    return number_foldv(argc, argv, rep_number_mul);
  }
}

DEFUN("/", Fdivide, Sdivide, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#/::
/ NUMBERS...

Divides NUMBERS(in left-to-right order).
::end:: */
{
  if (argc == 2) {
    return rep_number_div(argv[0], argv[1]);
  } else if (argc == 1) {
    return rep_number_div(rep_MAKE_INT(1), argv[0]);
  } else if (argc == 0) {
    return rep_signal_missing_arg(1);
  } else {
    return number_foldv(argc, argv, rep_number_div);
  }
}

DEFUN("remainder", Fremainder, Sremainder, (repv n1, repv n2), rep_Subr2) /*
::doc:rep.lang.math#remainder::
remainder DIVIDEND DIVISOR

Returns the remainder after dividing DIVIDEND by DIVISOR, that is:

  (remainder X Y) == (- X (* Y (truncate (/ X Y)))),

for Y not equal to zero.
::end:: */
{
  if (rep_INTP_2(n1, n2) && n2 != rep_MAKE_INT(0)) {
    return rep_MAKE_INT(rep_INT(n1) % rep_INT(n2));
  }

  return Fremainder__(n1, n2);
}

DEFUN("modulo", Fmod, Smod, (repv n1, repv n2), rep_Subr2) /*
::doc:rep.lang.math#modulo::
modulo DIVIDEND DIVISOR

Returns the value of DIVIDEND modulo DIVISOR, that is:

  (modulo X Y) == (- X (* Y (floor (/ X Y)))),

for Y not equal to zero.
::end:: */
{
  if (rep_INTP_2(n1, n2) && n2 != rep_MAKE_INT(0)) {
    return rep_MAKE_INT(modulo_int(rep_INT(n1), rep_INT(n2)));
  }

  return Fmodulo__(n1, n2);
}

DEFUN("quotient", Fquotient, Squotient, (repv n1, repv n2), rep_Subr2) /*
::doc:rep.lang.math#quotient::
quotient DIVIDEND DIVISOR

Returns the quotient from dividing numbers DIVIDEND and DIVISOR.
::end:: */
{
  if (rep_INTP_2(n1, n2) && n2 != rep_MAKE_INT(0)) {
    return rep_MAKE_INT(rep_INT(n1) / rep_INT(n2));
  }

  return Fquotient__(n1, n2);
}

DEFUN("lognot", Flognot, Slognot, (repv num), rep_Subr1) /*
::doc:rep.lang.math#lognot::
lognot NUMBER

Returns the bitwise logical `not' of NUMBER.
::end:: */
{
  return rep_number_lognot(num);
}

DEFUN("logior", Flogior, Slogior, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#logior::
logior NUMBERS...

Returns the bitwise logical `inclusive-or' of its arguments.
::end:: */
{
  if (argc == 2) {
    return rep_number_logior(argv[0], argv[1]);
  } else if (argc == 0) {
    return rep_MAKE_INT(0);
  } else {
    return number_foldv(argc, argv, rep_number_logior);
  }
}

DEFUN("logxor", Flogxor, Slogxor, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#logxor::
logxor NUMBERS...

Returns the bitwise logical `exclusive-or' of its arguments.
::end:: */
{
  if (argc == 2) {
    return rep_number_logxor(argv[0], argv[1]);
  } else {
    return number_foldv(argc, argv, rep_number_logxor);
  }
}

DEFUN("logand", Flogand, Slogand, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#logand::
logand NUMBERS...

Returns the bitwise logical `and' of its arguments.
::end:: */
{
  if (argc == 2) {
    return rep_number_logand(argv[0], argv[1]);
  } else {
    return number_foldv(argc, argv, rep_number_logand);
  }
}

DEFUN("eqv?", Feql, Seql, (repv arg1, repv arg2), rep_Subr2) /*
::doc:rep.data#eqv?::
eqv? ARG1 ARG2

Similar to `eq?' except that numbers with the same value will always be
considered `eqv?' (this may or may not be the case with `eq?').

Note however that exact and inexact versions of the same number are not
considered the same value. As a rule of thumb, if two numbers print the
same, they will be considered `eqv?'.
::end:: */
{
  if (rep_NUMERICP(arg1) && rep_NUMERICP(arg2)) {
    return number_cmp(arg1, arg2) == 0 ? Qt : rep_nil;
  } else {
    return arg1 == arg2 ? Qt : rep_nil;
  }
}

DEFUN("zero?", Fzerop, Szerop, (repv num), rep_Subr1) /*
::doc:rep.lang.math#zero?::
zero? NUMBER

Return t if NUMBER is zero.
::end:: */
{
  if (rep_INTP(num)) {
    return num == rep_MAKE_INT(0) ? Qt : rep_nil;
  } else if (rep_NUMBERP(num)) {
    switch (rep_NUMBER_TYPE(num)) {
    case rep_NUMBER_BIGNUM:
#ifdef HAVE_GMP
      return mpz_sgn(rep_NUMBER(num,z)) == 0 ? Qt : rep_nil;
#else
      return rep_NUMBER(num,z) == 0 ? Qt : rep_nil;
#endif

#ifdef HAVE_GMP
    case rep_NUMBER_RATIONAL:
      return mpq_sgn(rep_NUMBER(num,q)) == 0 ? Qt : rep_nil;
#endif

    case rep_NUMBER_FLOAT:
      return rep_NUMBER(num,f) == 0 ? Qt : rep_nil;
    }
  }

  return rep_nil;
}

DEFUN("1+", Fplus1, Splus1, (repv x), rep_Subr1) /*
::doc:rep.lang.math#1+::
1+ NUMBER

Return NUMBER plus 1.
::end:: */
{
  if (rep_INTP(x)) {
#if defined(HAVE_OVERFLOW_BUILTINS) && INTPTR_MAX == LONG_MAX
    long z;
    if (!__builtin_saddl_overflow(x, 4, &z)) {
      return (repv)z;
    }
#else
    return make_long_int(rep_INT(x) + 1);
#endif
  }

  return rep_number_add1__(x);
}

DEFUN("1-", Fsub1, Ssub1, (repv x), rep_Subr1) /*
::doc:rep.lang.math#1-::
1- NUMBER

Return NUMBER minus 1.
::end:: */
{
  if (rep_INTP(x)) {
#if defined(HAVE_OVERFLOW_BUILTINS) && INTPTR_MAX == LONG_MAX
    long z;
    if (!__builtin_ssubl_overflow(x, 4, &z)) {
      return (repv)z;
    }
#else
    return make_long_int(rep_INT(x) + 1);
#endif
  }

  return rep_number_sub1__(x);
}

DEFUN("ash", Fash, Sash, (repv num, repv shift), rep_Subr2) /*
::doc:rep.lang.math#ash::
ash NUMBER COUNT

Use an arithmetic shift to shift the bits in NUMBER by COUNT bits to
the left, a negative COUNT means shift right.

Both NUMBER and COUNT must be integers.
::end:: */
{
  rep_DECLARE1(num, rep_INTEGERP);
  rep_DECLARE2(shift, rep_INTEGERP);

  shift = coerce(shift, rep_NUMBER_INT);

  switch (rep_NUMERIC_TYPE(num)) {
  case rep_NUMBER_INT:
    if (rep_INT(shift) >= rep_LISP_INT_BITS) {
      num = promote_to(num, rep_NUMBER_BIGNUM);
      goto do_bignum;
    } else {
      long long tot;
      if (rep_INT(shift) > 0) {
	tot = ((long long) rep_INT(num)) << rep_INT(shift);
      } else {
	tot = ((long long) rep_INT(num)) >> -rep_INT(shift);
      }
      return rep_make_longlong_int(tot);
    }
    /* not reached */

  case rep_NUMBER_BIGNUM:
  do_bignum: {
    number_z *z = make_number(rep_NUMBER_BIGNUM);
#ifdef HAVE_GMP
    mpz_init(z->z);
    if (rep_INT(shift) > 0) {
      mpz_mul_2exp(z->z, rep_NUMBER(num,z), rep_INT(shift));
    } else {
      mpz_div_2exp(z->z, rep_NUMBER(num,z), - rep_INT(shift));
    }
#else
    if (rep_INT(shift) > 0) {
      double factor = 1;
      for (intptr_t i = rep_INT(shift); i > 0; i -= this) {
	intptr_t tem = MIN(sizeof(long) * CHAR_BIT - 1, i);
	factor = factor * (1L << tem);
      }
      double t = (double) rep_NUMBER(num,z) * factor;
      if (t > INT64_MIN && t < INT64_MAX) {
	z->z = rep_NUMBER(num,z) << rep_INT(shift);
      } else {
	return rep_make_float(t, true);
      }
    }
    else {
      z->z = rep_NUMBER(num,z) >> -rep_INT(shift);
    }
#endif
    return maybe_demote(rep_VAL(z)); }

  default:
    return rep_signal_arg_error(num, 1);
  }
}

DEFUN("floor", Ffloor, Sfloor, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#floor::
floor NUMBER

Round NUMBER downwards to the nearest integer less than or equal to
NUMBER.
::end:: */
{
  if (rep_INTP(arg)) {
    return arg;
  }

  return Ffloor__(arg);
}

DEFUN("ceiling", Fceiling, Sceiling, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#ceiling::
ceiling NUMBER

Round NUMBER upwards to the nearest integer greater than or equal to
NUMBER.
::end:: */
{
  if (rep_INTP(arg)) {
    return arg;
  }

  return Fceiling__(arg);
}

DEFUN("truncate", Ftruncate, Struncate, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#truncate::
truncate NUMBER

Round NUMBER to the nearest integer between NUMBER and zero.
::end:: */
{
  if (rep_INTP(arg)) {
    return arg;
  }

  return Ftruncate__(arg);
}

DEFUN("round", Fround, Sround, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#round::
round NUMBER

Round NUMBER to the nearest integer. Halfway cases are rounded to the
nearest even integer.
::end:: */
{
  if (rep_INTP(arg)) {
    return arg;
  }

  return Fround__(arg);
}

DEFUN("exp", Fexp, Sexp, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#exp::
exp X

Return `e' (the base of natural logarithms) raised to the power X.
::end:: */
{
  rep_DECLARE1(arg, rep_NUMERICP);

  return rep_make_float(exp(rep_get_float(arg)), true);
}

DEFUN("log", Flog_, Slog, (repv arg, repv base), rep_Subr2) /*
::doc:rep.lang.math#log::
log X [BASE]

Return the logarithm of X in base BASE. An arithmetic error is
signalled if X is less than zero. If BASE isn't defined, return the
natural logarithm of X.
::end:: */
{
  rep_DECLARE1(arg, rep_NUMERICP);
  rep_DECLARE2_OPT(base, rep_NUMERICP);

  double d = rep_get_float(arg);

  if (base != rep_nil) {
    double b = rep_get_float(base);
    if (d >= 0 && b >= 0) {
      return rep_make_float(log(d) / log(b), true);
    }
  } else {
    if (d >= 0) {
      return rep_make_float(log(d), true);
    }
  }

  return Fsignal(Qarith_error, rep_LIST_1(rep_VAL(&domain_error)));
}

/* Called by bytecode VM. */

repv
Flog(repv x)
{
  return Flog_(x, rep_nil);
}

DEFUN("sin", Fsin, Ssin, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#sin::
sin X

Returns the sine of X, in radians.
::end:: */
{
  rep_DECLARE1(arg, rep_NUMERICP);

  return rep_make_float(sin(rep_get_float(arg)), true);
}

DEFUN("cos", Fcos, Scos, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#cos::
cos X

Returns the cosine of X, in radians.
::end:: */
{
  rep_DECLARE1(arg, rep_NUMERICP);

  return rep_make_float(cos(rep_get_float(arg)), true);
}

DEFUN("tan", Ftan, Stan, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#tan::
tan X

Returns the tangent of X, in radians.
::end:: */
{
  rep_DECLARE1(arg, rep_NUMERICP);

  return rep_make_float(tan(rep_get_float(arg)), true);
}

DEFUN("asin", Fasin, Sasin, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#asin::
asin X

Return the arc sine of X (the value whose sine is X), in radians.
::end:: */
{
  rep_DECLARE1(arg, rep_NUMERICP);

  double d = rep_get_float(arg);

  if (d < -1 || d > 1) {
    return Fsignal(Qarith_error, rep_LIST_1(rep_VAL(&domain_error)));
  }

  return rep_make_float(asin(d), true);
}

DEFUN("acos", Facos, Sacos, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#acos::
acos X

Return the arc cosine of X (the value whose cosine is X), in radians.
::end:: */
{
  rep_DECLARE1(arg, rep_NUMERICP);

  double d = rep_get_float(arg);

  if (d < -1 || d > 1) {
    return Fsignal(Qarith_error, rep_LIST_1(rep_VAL(&domain_error)));
  }

  return rep_make_float(acos(d), true);
}

DEFUN("atan", Fatan, Satan, (repv y, repv x), rep_Subr2) /*
::doc:rep.lang.math#atan::
atan X

Returns the arc tangent of X(the value whose tangent is X), in
radians.

atan Y X

Returns the arc tangent of Y/X, in radians. The signs of both arguments
are used to determine the quadrant of the result, and X is permitted to
be zero.
::end:: */
{
  rep_DECLARE1(y, rep_NUMERICP);

  if (!rep_NUMERICP(x)) {
    return rep_make_float(atan(rep_get_float(y)), true);
  } else {
    return rep_make_float(atan2(rep_get_float(y), rep_get_float(x)), true);
  }
}

DEFUN("sqrt", Fsqrt, Ssqrt, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#sqrt::
sqrt X

Returns the nonnegative square root of X. If X is negative, signals an
arithmetic error(should return a complex number).
::end:: */
{
  rep_DECLARE1(arg, rep_NUMERICP);

  double d = rep_get_float(arg);

  if (d < 0) {
    return Fsignal(Qarith_error, rep_LIST_1(rep_VAL(&domain_error)));
  }

  return rep_make_float(sqrt(d), rep_NUMBER_INEXACT_P(arg));
}

DEFUN("expt", Fexpt, Sexpt, (repv arg1, repv arg2), rep_Subr2) /*
::doc:rep.lang.math#expt::
expt X Y

Returns X raised to the power Y.

If X is negative and Y is a non-integer, then an arithmetic error is
signalled(mathematically should return a complex number).
::end:: */
{
  rep_DECLARE1(arg1, rep_NUMERICP);
  rep_DECLARE1(arg2, rep_NUMERICP);

  repv out;
  if (rep_INTEGERP(arg1) && rep_INTP(arg2)) {
    if (rep_INTP(arg1)) {
      arg1 = promote_to(arg1, rep_NUMBER_BIGNUM);
      out = arg1;
    } else {
      out = copy_number(arg1);
    }
#ifdef HAVE_GMP
    int neg = rep_INT(arg2) < 0;
    mpz_pow_ui(rep_NUMBER(out,z), rep_NUMBER(arg1,z),
	       neg ? -rep_INT(arg2) : rep_INT(arg2));
    if (neg) {
      out = rep_number_div(rep_MAKE_INT(1), out);
    }
#else
    double t = pow(rep_NUMBER(arg1,z), rep_INT(arg2));
    out = rep_make_float(t, false);
#endif
  } else {
    double x = rep_get_float(arg1);
    double y = rep_get_float(arg2);
    if (x >= 0 || ceil(y) == y) {
      out = rep_make_float(pow(x, y), rep_NUMBER_INEXACT_P(arg1)
			   || rep_NUMBER_INEXACT_P(arg2));
    } else {
      out = Fsignal(Qarith_error, rep_LIST_1(rep_VAL(&domain_error)));
    }
  }

  return out;
}

DEFUN("gcd", Fgcd, Sgcd, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#gcd::
gcd ...

Return the greatest common divisor of the integer arguments. The result
is always non-negative. Returns 0 with arguments.
::end:: */
{
  if (argc == 0) {
    return rep_MAKE_INT(0);
  } else if (argc == 1) {
    rep_DECLARE1(argv[0], rep_INTEGERP);
    return rep_integer_gcd(argv[0], argv[0]);
  } else {
    return integer_foldv(argc, argv, rep_integer_gcd);
  }
}

DEFUN("number?", Fnumberp, Snumberp, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#number?::
number? ARG

Return t if ARG is a number.
::end:: */
{
  return rep_NUMERICP(arg) ? Qt : rep_nil;
}

DEFUN("integer?", Fintegerp, Sintegerp, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#integer?::
integer? ARG

Return t if ARG is a integer.
::end:: */
{
  if (rep_INTP(arg)) {
    return Qt;
  }

  if (!rep_NUMBERP(arg)) {
    return rep_nil;
  }

  switch (rep_NUMBER_TYPE(arg)) {
  case rep_NUMBER_BIGNUM:
    return Qt;

  case rep_NUMBER_FLOAT:
    return floor(rep_NUMBER(arg,f)) == rep_NUMBER(arg,f) ? Qt : rep_nil;

  case rep_NUMBER_RATIONAL:
  default:
    return rep_nil;
  }
}

DEFUN("fixnum?", Ffixnump, Sfixnump, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#fixnum?::
fixnum? ARG

Return t if ARG is a fixnum (i.e. an integer that fits in a Lisp
pointer).
::end:: */
{
  return rep_INTP(arg) ? Qt : rep_nil;
}

DEFUN("exact?", Fexactp, Sexactp, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#exact?::
exact? ARG

Return t if ARG is an exact number.
::end:: */
{
  return rep_INTP(arg) || (rep_NUMBERP(arg) && !rep_NUMBER_FLOAT_P(arg)) ? Qt : rep_nil;
}

DEFUN("inexact?", Finexactp, Sinexactp, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#exact?::
exact? ARG

Return t if ARG is an exact number.
::end:: */
{
  return rep_NUMBERP(arg) && rep_NUMBER_FLOAT_P(arg) ? Qt : rep_nil;
}

DEFUN("infinite?", Finfinitep, Sinfinitep, (repv arg), rep_Subr1) /*
::doc::rep.lang.math#infinite?::
infinite? ARG

Return true if ARG is an infinite number.
::end:: */
{
  if (rep_NUMBERP(arg) && rep_NUMBER_FLOAT_P(arg)) {
    return isinf(rep_NUMBER(arg,f)) ? Qt : rep_nil;
  } else {
    return rep_nil;
  }
}

DEFUN("finite?", Ffinitep, Sfinitep, (repv arg), rep_Subr1) /*
::doc::rep.lang.math#finite?::
finite? ARG

Return true if ARG is a finite number.
::end:: */
{
  if (rep_NUMBERP(arg)) {
    if (rep_NUMBER_FLOAT_P(arg)) {
      return isfinite(rep_NUMBER(arg,f)) ? Qt : rep_nil;
    } else {
      return Qt;
    }
  } else if (rep_INTP(arg)) {
    return Qt;
  } else {
    return rep_nil;
  }
}

DEFUN("nan?", Fnanp, Snanp, (repv arg), rep_Subr1) /*
::doc::rep.lang.math#nan?::
nan? ARG

Return true if ARG is a NaN number.
::end:: */
{
  if (rep_NUMBERP(arg) && rep_NUMBER_FLOAT_P(arg)) {
    return isnan(rep_NUMBER(arg,f)) ? Qt : rep_nil;
  } else {
    return rep_nil;
  }
}

DEFUN("exact->inexact", Fexact_to_inexact,
      Sexact_to_inexact, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#exact->inexact::
exact->inexact X

Returns an inexact(i.e. floating point) representation of X.
::end:: */
{
  rep_DECLARE1(arg, rep_NUMERICP);

  if (!rep_INTP(arg) && rep_NUMBER_FLOAT_P(arg)) {
    return arg;
  } else {
    return rep_make_float(rep_get_float(arg), true);
  }
}

static void
rationalize(repv arg, double *numerator, double *denominator)
{
  /* X/Y always equals the input value. Tactic is to iteratively
     multiply both X and Y by 2 until X is an integer. We bound the
     number of iterations to the size of the mantissa by starting with
     the normalized value... */

  int expt;
  double x = frexp(rep_get_float(arg), &expt);
  double y = pow(2.0, -expt);

  while (x - floor(x) > DBL_EPSILON) {
    x = x * 2;
    y = y * 2;
  }

  if (numerator != NULL) {
    *numerator = x;
  }
  if (denominator != NULL) {
    *denominator = y;
  }
}

DEFUN("inexact->exact", Finexact_to_exact,
      Sinexact_to_exact, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#inexact->exact::
inexact->exact X

Returns an exact representation of X. This may involve a loss of
accuracy.
::end:: */
{
  rep_DECLARE1(arg, rep_NUMERICP);

  if (rep_INTP(arg) || !rep_NUMBER_FLOAT_P(arg)) {
    return arg;
  } else {
#ifdef HAVE_GMP
    number_q *q = make_number(rep_NUMBER_RATIONAL);
    mpq_init(q->q);
    mpq_set_d(q->q, rep_get_float(arg));
    return maybe_demote(rep_VAL(q));
#else
    double x, y;
    rationalize(arg, &x, &y);
    number_z *z = make_number(rep_NUMBER_BIGNUM);
    z->z = x / y;
    return maybe_demote(rep_VAL(z));
#endif
  }
}

DEFUN("numerator", Fnumerator, Snumerator, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#numerator::
numerator X

Return the numerator of rational number X.
::end:: */
{
  rep_DECLARE1(arg, rep_NUMERICP);

#ifdef HAVE_GMP
  if (rep_NUMBER_RATIONAL_P(arg)) {
    number_z *z = make_number(rep_NUMBER_BIGNUM);
    mpz_init_set(z->z, mpq_numref(rep_NUMBER(arg,q)));
    return maybe_demote(rep_VAL(z));
  }
#endif

  bool inexact = rep_NUMBER_INEXACT_P(arg);

  double x;
  rationalize(arg, &x, NULL);

  return rep_make_float(x, inexact);
}

DEFUN("denominator", Fdenominator, Sdenominator, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#denominator::
denominator X

Return the denominator of rational number X.
::end:: */
{
  rep_DECLARE1(arg, rep_NUMERICP);

#ifdef HAVE_GMP
  if (rep_NUMBER_RATIONAL_P(arg)) {
    number_z *z = make_number(rep_NUMBER_BIGNUM);
    mpz_init_set(z->z, mpq_denref(rep_NUMBER(arg,q)));
    return maybe_demote(rep_VAL(z));
  }
#endif

  bool inexact = rep_NUMBER_INEXACT_P(arg);
  inexact = true;

  double y;
  rationalize(arg, NULL, &y);

  return rep_make_float(y, inexact);
}

DEFUN("max", Fmax, Smax, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#max::
max ARGS...

Returns the greatest of its arguments. There must be at least two
arguments. When comparing numbers, any inexact arguments cause the
result to be inexact.
::end:: */
{
  return foldv(argc, argv, rep_number_max);
}

DEFUN("min", Fmin, Smin, (int argc, repv *argv), rep_SubrV) /*
::doc:rep.lang.math#min::
min ARGS...

Returns the smallest of its arguments. There must be at least two
arguments. When comparing numbers, any inexact arguments cause the
result to be inexact.
::end:: */
{
  return foldv(argc, argv, rep_number_min);
}

DEFUN("string->number", Fstring_to_number,
      Sstring_to_number, (repv string, repv radix_), rep_Subr2) /*
::doc:rep.lang.math#string->number::
string->number STRING [RADIX]

Return the number represented by STRING. If RADIX is specified, the
number is parsed from that base, otherwise base 10 is assumed.
::end:: */
{
  rep_DECLARE1(string, rep_STRINGP);

  if (radix_ == rep_nil) {
    radix_ = rep_MAKE_INT(10);
  }

  rep_DECLARE(2, radix_, rep_INTP(radix_) && rep_INT(radix_) > 0);

  const char *ptr = rep_STR(string);
  int radix = rep_INT(radix_);
  int force_exactness = 0;
  int sign = 1;

  while (*ptr == '#') {
    switch (ptr[1]) {
    case 'b': case 'B':
      radix = 2;
      break;

    case 'o': case 'O':
      radix = 8;
      break;

    case 'd': case 'D':
      radix = 10;
      break;

    case 'x': case 'X':
      radix = 16;
      break;

    case 'e': case 'E':
      force_exactness = +1;
      break;

    case 'i': case 'I':
      force_exactness = -1;
      break;

    default:
      return rep_nil;
    }
    ptr += 2;
  }

  if (*ptr == '-' || *ptr == '+') {
    if (*ptr == '-') {
      sign = -1;
    }
    ptr++;
  }

  int type = 0;
  if (strchr(ptr, '/')) {
    type = rep_NUMBER_RATIONAL;
  } else if (radix == 10) {
    if (strchr(ptr, '.') || strchr(ptr, 'e') || strchr(ptr, 'E')) {
      type = rep_NUMBER_FLOAT;
    }
  }

  repv ret = rep_parse_number(ptr, rep_STRING_LEN(string)
			      - (ptr - rep_STR(string)), radix, sign, type);
  if (!ret) {
    ret = rep_nil;
  } else if (force_exactness > 0) {
    ret = Finexact_to_exact(ret);
  } else if (force_exactness < 0) {
    ret = Fexact_to_inexact(ret);
  }

  return ret;
}

DEFUN("number->string", Fnumber_to_string,
      Snumber_to_string, (repv z, repv radix), rep_Subr2) /*
::doc:rep.lang.math#number->string::
number->string Z [RADIX]

Return a string containing a printed representation of the number Z. If
RADIX is specified, print the number in that base, otherwise print it
in base 10.
::end:: */
{
  rep_DECLARE1(z, rep_NUMERICP);

  if (radix == rep_nil) {
    radix = rep_MAKE_INT(10);
  }

  rep_DECLARE(2, radix, rep_INTP(radix) && rep_INT(radix) > 0);

  char *out = rep_print_number_to_string(z, rep_INT(radix), -1);
  return out ? rep_box_string(out, strlen(out)) : rep_nil;
}


/* Random number generation */

#if defined(HAVE_GMP) && defined(HAVE_GMP_RANDINIT) && __GNU_MP__ >= 4

static gmp_randstate_t random_state;

static void
ensure_random_state(void)
{
  static bool initialized;

  if (!initialized) {
    /* Generate the best random numbers up to 128 bits, the maximum
       allowed by gmp */

    gmp_randinit(random_state, GMP_RAND_ALG_DEFAULT, 128);

    /* Initialize to a known seed */

    gmp_randseed_ui(random_state, 0);

    initialized = true;
  }
}

static void
random_seed(u_long seed)
{
  ensure_random_state();
  gmp_randseed_ui(random_state, seed);
}

static repv
random_new(repv limit_)
{
  number_z *z = make_number(rep_NUMBER_BIGNUM);
  repv limit = promote_to(limit_, rep_NUMBER_BIGNUM);

  ensure_random_state();
  mpz_init(z->z);
  mpz_urandomm(z->z, random_state, rep_NUMBER(limit, z));

  return maybe_demote(rep_VAL(z));
}

#else /* HAVE_GMP */

/* Try to work out how many bits of randomness rand() will give.. */
#ifdef HAVE_LRAND48
# define RAND_BITS 31
# define rand lrand48
# define srand srand48
#else
# if RAND_MAX == 32768
#  define RAND_BITS 15
# elif RAND_MAX == 2147483647
#  define RAND_BITS 31
# else
#  define RAND_BITS 63
# endif
#endif

static void
random_seed(u_long seed)
{
  srand(seed);
}

static repv
random_new(repv limit_)
{
  intptr_t limit = rep_get_long_int(limit_);

  if (limit <= 0 || limit > rep_LISP_MAX_INT) {
    return rep_signal_arg_error(limit_, 1);
  }

  intptr_t divisor = rep_LISP_MAX_INT / limit;

  intptr_t val;

  do {
    val = rand();
    if (rep_LISP_INT_BITS-1 > RAND_BITS) {
      val = (val << RAND_BITS) | rand();
      if (rep_LISP_INT_BITS-1 > 2*RAND_BITS) {
	val = (val << RAND_BITS) | rand();
	if (rep_LISP_INT_BITS-1 > 3*RAND_BITS) {
	  val = (val << RAND_BITS) | rand();
	  if (rep_LISP_INT_BITS-1 > 4*RAND_BITS) {
	    val = (val << RAND_BITS) | rand();
	  }
	}
      }
    }

    /* Ensure VAL is positive(assumes twos-complement) */

    val &= ~(((intptr_t)-1) << (rep_LISP_INT_BITS-1));
    val /= divisor;

  } while (val >= limit);

  return rep_make_long_int(val);
}

#endif /* !HAVE_GMP */

DEFUN("random", Frandom, Srandom, (repv arg), rep_Subr1) /*
::doc:rep.lang.math#random::
random [LIMIT]

Produce a pseudo-random number between zero and LIMIT(or the largest
positive integer representable). If LIMIT is the symbol `t' the
generator is seeded with the current time of day.
::end:: */
{
  if (arg == Qt) {
    uintptr_t seed = time(0);
    seed = (seed << 8) | (getpid() & 0xff);
    random_seed(seed);
    return Qt;
  }

  rep_DECLARE1_OPT(arg, rep_INTEGERP);

  repv limit;
  if (rep_INTEGERP(arg)) {
    limit = arg;
  } else {
    limit = rep_MAKE_INT(rep_LISP_MAX_INT);
  }

  if (rep_compare_numbers(limit, rep_MAKE_INT(0)) <= 0) {
    return rep_signal_arg_error(limit, 1);
  }

  return random_new(limit);
}


/* init */

void
rep_numbers_init(void)
{
  repv tem;

  static rep_type fixnum = {
    .car = rep_Int,
    .name = "fixnum",
    .compare = number_cmp,
    .print = number_prin,
  };

  static rep_type number = {
    .car = rep_Number,
    .name = "number",
    .compare = number_cmp,
    .print = number_prin,
    .sweep = number_sweep,
  };

  rep_define_type(&fixnum);
  rep_define_type(&number);
  
  number_sizeofs[0] = sizeof(number_z);
  number_sizeofs[1] = sizeof(number_q);
  number_sizeofs[2] = sizeof(number_f);
  for (int i = 0; i < 3; i++) {
    number_allocations[i] = ((2040 - sizeof(number_block))
			     / number_sizeofs[i]);
  }
  
  tem = rep_push_structure("rep.lang.math");
  rep_ADD_SUBR(Splus);
  rep_ADD_SUBR(Sminus);
  rep_ADD_SUBR(Sproduct);
  rep_ADD_SUBR(Sdivide);
  rep_ADD_SUBR(Sremainder);
  rep_ADD_SUBR(Smod);
  rep_ADD_SUBR(Squotient);
  rep_ADD_SUBR(Slognot);
  rep_ADD_SUBR(Slogior);
  rep_ADD_SUBR(Slogxor);
  rep_ADD_SUBR(Slogand);
  rep_ADD_SUBR(Szerop);
  rep_ADD_SUBR(Splus1);
  rep_ADD_SUBR(Ssub1);
  rep_ADD_SUBR(Sash);
  rep_ADD_SUBR(Sfloor);
  rep_ADD_SUBR(Sceiling);
  rep_ADD_SUBR(Struncate);
  rep_ADD_SUBR(Sround);
  rep_ADD_SUBR(Sexp);
  rep_ADD_SUBR(Slog);
  rep_ADD_SUBR(Ssin);
  rep_ADD_SUBR(Scos);
  rep_ADD_SUBR(Stan);
  rep_ADD_SUBR(Sasin);
  rep_ADD_SUBR(Sacos);
  rep_ADD_SUBR(Satan);
  rep_ADD_SUBR(Ssqrt);
  rep_ADD_SUBR(Sexpt);
  rep_ADD_SUBR(Sgcd);
  rep_ADD_SUBR(Snumberp);
  rep_ADD_SUBR(Sintegerp);
  rep_ADD_SUBR(Sfixnump);
  rep_ADD_SUBR(Sexactp);
  rep_ADD_SUBR(Sinexactp);
  rep_ADD_SUBR(Snanp);
  rep_ADD_SUBR(Sinfinitep);
  rep_ADD_SUBR(Sfinitep);
  rep_ADD_SUBR(Sexact_to_inexact);
  rep_ADD_SUBR(Sinexact_to_exact);
  rep_ADD_SUBR(Snumerator);
  rep_ADD_SUBR(Sdenominator);
  rep_ADD_SUBR(Smax);
  rep_ADD_SUBR(Smin);
  rep_ADD_SUBR(Sstring_to_number);
  rep_ADD_SUBR(Snumber_to_string);
  rep_ADD_SUBR(Srandom);
  rep_pop_structure(tem);
  
  tem = rep_push_structure("rep.data");
  rep_ADD_SUBR(Seql);
  rep_pop_structure(tem);
}

void
rep_numbers_kill(void)
{
  for (int idx = 0; idx < 3; idx++) {
    number_block *b = number_block_list[idx];
    while (b) {
      number_block *next = b->next;
      rep_free(b);
      b = next;
    }
    number_block_list[idx] = NULL;
    number_free_list[idx] = NULL;
  }
}
