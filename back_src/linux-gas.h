/* Definitions of target machine for UNICORE Linux-based GNU systems.
   Copyright (C) 1993, 1994, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
   2005, 2006, 2007
   Free Software Foundation, Inc.
   Contributed by Star <tanmingxing@mprc.pku.edu.cn>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* Unsigned chars produces much better code than signed.  */
#define DEFAULT_SIGNED_CHAR 1

#undef  SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC  "%{posix:-D_POSIX_SOURCE} %{pthread:-D_REENTRANT}"

/*
// huangping modify
#undef  SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

// huangping modify
#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

#undef  WCHAR_TYPE
#define WCHAR_TYPE "long int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE BITS_PER_WORD
*/
#undef  SIZE_TYPE
#define SIZE_TYPE "long unsigned int"

// huangping modify
#undef  PTRDIFF_TYPE
#define PTRDIFF_TYPE "long int"

#undef  WCHAR_TYPE
#define WCHAR_TYPE "unsigned int"

#undef  WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE 32 

/* Clear the instruction cache from `beg' to `end'.  This makes an
   inline system call to SYS_cacheflush.  */
//#define CLEAR_INSN_CACHE(BEG, END)					\
{									\
  register unsigned long _beg __asm ("r0") = (unsigned long) (BEG);	\
  register unsigned long _end __asm ("r1") = (unsigned long) (END);	\
  register unsigned long _flg __asm ("r2") = 0;				\
/*FIXME: this is really an illegal syscall*/\
  __asm __volatile ("jepriv 0x666		@ sys_cacheflush"	\
		    : "=r" (_beg)					\
		    : "0" (_beg), "r" (_end), "r" (_flg));		\
}
