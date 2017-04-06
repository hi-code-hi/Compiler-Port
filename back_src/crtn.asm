#   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2004, 2007,2008
#   Free Software Foundation, Inc.
#   Contributed by Star (tanmingxing@mprc.pku.edu.cn).
#   
#   This file is part of GCC.
#
#   GCC is free software; you can redistribute it and/or modify it
#   under the terms of the GNU General Public License as published
#   by the Free Software Foundation; either version 3, or (at your
#   option) any later version.
#
#   GCC is distributed in the hope that it will be useful, but WITHOUT
#   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
#   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
#   License for more details.
#
#   You should have received a copy of the GNU General Public License
#   along with GCC; see the file COPYING3.  If not see
#   <http://www.gnu.org/licenses/>.  */
#   Copyright (C) 2001 Free Software Foundation, Inc.
# 
#
# This file just makes sure that the .fini and .init sections do in
# fact return.  Users may put any desired instructions in those sections.
# This file is the last thing linked into any executable.

	# Note - this macro is complimented by the FUNC_START macro
	# in crti.asm.  If you change this macro you must also change
	# that macro match.
.macro FUNC_END
	.unicore64
	ldw r16, [fp-], #56
	ldw r17, [fp-], #52
	ldw r18, [fp-], #48
	ldw r19, [fp-], #44
	ldw r20, [fp-], #40
	ldw r21, [fp-], #36
	ldw r22, [fp-], #32
	ldw r23, [fp-], #28
	ldw r24, [fp-], #24
	ldw r25, [fp-], #20
	ldw sl, [fp-], #16
	ldw fp, [fp-], #12
	ldw sp, [fp-], #8
	ldw lr, [fp-], #4
	return
.endm
		
	
	.file		"crtn.asm"

	.section	".init"
	;;
	FUNC_END
	
	.section	".fini"
	;;
	FUNC_END
	
# end of crtn.asm
