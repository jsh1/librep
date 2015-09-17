/* utf8-utils.c -- UTF-8 functions

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

#include "utf8-utils.h"

/* Size of UTF-8 sequences whose first byte is greater than 0xc0. */

static const uint8_t utf8_table[64] = {
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,6,6,1,1
};

size_t
utf8_code_point_size(uint8_t c)
{
  return c < 0xc0 ? 1 : utf8_table[c - 0xc0];
}

size_t
utf8_string_size(const uint8_t *ptr, size_t len)
{
  size_t count = 0;
  intptr_t remaining = len;

  while (remaining > 0) {
    uint8_t c = *ptr;
    if (c < 0xc0) {
      ptr++;
      remaining--;
    } else {
      uint8_t l = utf8_table[c - 0xc0];
      ptr += l;
      remaining -= l;
    }
    count++;
  }

  if (remaining < 0) {
    /* Last code point was incomplete. */
    count--;
  }

  return count;
}

uint8_t *
utf8_skip_characters(const uint8_t *ptr, size_t len, size_t count)
{
  intptr_t remaining = len;

  while (count != 0 && remaining > 0) {
    uint8_t c = *ptr;
    if (c < 0xc0) {
      ptr++;
      remaining--;
    } else {
      uint8_t l = utf8_table[c - 0xc0];
      ptr += l;
      remaining -= l;
    }
    count--;
  }

  return count == 0 ? (uint8_t *)ptr : NULL;
}

void
utf8_to_utf32(uint32_t *dst, const uint8_t *src, size_t len)
{
  while (len != 0) {
    uint32_t c = src[0];
    if (c < 0xc0) {
      *dst++ = c;
      src++;
      len--;
    } else {
      unsigned int l = utf8_table[c - 0xc0];
      if (l > len) {
	break;
      }
      c = c & ~(0xff << (7 - l));
      for (unsigned int i = 1; i < l; i++) {
	c = (c << 6) | (src[i] & 0x3f);
      }
      *dst++ = c;
      src += l;
      len -= l;
    }
  }
}

size_t
utf32_to_utf8_size(const uint32_t *src, size_t len)
{
  size_t size = 0;

  for (size_t i = 0; i < len; i++) {
    size += utf32_to_utf8_size_1(src[i]);
  }

  return size;
}

static inline size_t
inline_utf32_to_utf8_1(uint8_t *dst, uint32_t c)
{
  if (c < 0x80) {
    dst[0] = c;
    return 1;
  }

  if (c >= 0x200000) {
    return 0;
  }

  uint32_t u = c;
  uint8_t tag = 0xc0;
  unsigned int size = 2;

  if (c >= 0x10000) {
    dst[3] = (u & 0x3f) | 0x80; u >>= 6;
    tag |= 0x10;
    size++;
  }

  if (c >= 0x800) {
    dst[2] = (u & 0x3f) | 0x80; u >>= 6;
    tag |= 0x20;
    size++;
  }

  dst[1] = (u & 0x3f) | 0x80; u >>= 6;
  dst[0] = u | tag;

  return size;
}

size_t
utf32_to_utf8_1(uint8_t *dst, uint32_t c)
{
  return inline_utf32_to_utf8_1(dst, c);
}

void
utf32_to_utf8(uint8_t *dst, const uint32_t *src, size_t len)
{
  for (size_t i = 0; i < len; i++) {
    dst += inline_utf32_to_utf8_1(dst, src[i]);
  }
}
