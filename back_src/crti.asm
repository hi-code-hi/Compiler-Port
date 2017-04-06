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
#
#   This file just make a stack frame for the contents of the .fini and
#   .init sections.  Users may put any desired instructions in those
#   sections.

	# Note - this macro is complimented by the FUNC_END macro
	# in crtn.asm.  If you change this macro you must also change
	# that macro match.
.macro FUNC_START
	.unicore64
	#  Create a stack frame and save any call-preserved registers
	mov	ip, sp
	stw.w pc, [sp-], #4
	stw.w lr, [sp-], #4
	stw.w ip, [sp-], #4
	stw.w fp, [sp-], #4
	stw.w sl, [sp-], #4
	stw.w r25, [sp-], #4
	stw.w r24, [sp-], #4
	stw.w r23, [sp-], #4
	stw.w r22, [sp-], #4
	stw.w r21, [sp-], #4
	stw.w r20, [sp-], #4
	stw.w r19, [sp-], #4
	stw.w r18, [sp-], #4
	stw.w r17, [sp-], #4
	stw.w r16, [sp-], #4
	sub	fp, ip, #4
.endm
		
	.file		"crti.asm"

	.section	".init"
	.align 2
	.global	_init
_init:
	FUNC_START
	
		
	.section	".fini"
	.align	2
	.global	_fini
_fini:
	FUNC_START
	
# end of crti.asm
