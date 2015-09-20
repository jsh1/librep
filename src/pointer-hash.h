/* pointer-hash.h -- pointer-sized hash functions

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

#ifndef POINTER_HASH_H
#define POINTER_HASH_H

#include <stdint.h>

/* Hash functions from Thomas Wang.
   http://www.cris.com/~Ttwang/tech/inthash.htm */

static inline uintptr_t
pointer_hash(uintptr_t v)
{
  if (sizeof(v) == sizeof(uint32_t)) {
    v += ~(v << 15);
    v ^= v >> 10;
    v += v << 3;
    v ^= v >> 6;
    v += ~(v << 11);
    v ^= v >> 16;
    return v;
  } else {
    v += ~(v << 32);
    v ^= v >> 22;
    v += ~(v << 13);
    v ^= v >> 8;
    v += v << 3;
    v ^= v >> 15;
    v += ~(v << 27);
    v ^= v >> 31;
    return v;
  }
}

#endif /* POINTER_HASH_H */
