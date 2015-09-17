/* utf8-utils.h -- UTF-8 functions

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

#ifndef UTF8_UTILS_H
#define UTF8_UTILS_H

#include <stdint.h>
#include <stddef.h>

extern size_t utf8_code_point_size(uint8_t c);

extern size_t utf8_string_size(const uint8_t *ptr, size_t len);

extern uint8_t *utf8_skip_characters(const uint8_t *ptr, size_t len,
  size_t count);

extern void utf8_to_utf32(uint32_t *dst, const uint8_t *src, size_t len);

static inline size_t
utf32_to_utf8_size_1(uint32_t c)
{
  size_t size = 1;
  size += c >= 0x80;
  size += c >= 0x800;
  size += c >= 0x10000;
  return c < 0x200000 ? size : 0;
}

extern size_t utf32_to_utf8_size(const uint32_t *src, size_t len);

extern size_t utf32_to_utf8_1(uint8_t *dst, uint32_t c);

extern void utf32_to_utf8(uint8_t *dst, const uint32_t *src, size_t len);

#endif /* UTF8_UTILS_H */
