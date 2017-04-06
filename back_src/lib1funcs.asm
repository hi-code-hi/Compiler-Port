@ libgcc routines for UNICORE64 cpu.
@ Division routines, written by Richard Earnshaw, (rearnsha@armltd.co.uk)

/* Copyright 1995, 1996, 1998, 1999, 2000, 2003, 2004, 2005
   Free Software Foundation, Inc.

This file is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2, or (at your option) any
later version.

In addition to the permissions in the GNU General Public License, the
Free Software Foundation gives you unlimited permission to link the
compiled version of this file into combinations with other programs,
and to distribute those combinations without any restriction coming
from the use of this file.  (The General Public License restrictions
do apply in other respects; for example, they cover modification of
the file, and distribution when not linked into a combine
executable.)

This file is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; see the file COPYING.  If not, write to
the Free Software Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  */
/* ------------------------------------------------------------------------ */

/* We need to know what prefix to add to function names.  */

#ifndef __USER_LABEL_PREFIX__
#error  __USER_LABEL_PREFIX__ not defined
#endif

/* ANSI concatenation macros.  */

#define CONCAT1(a, b) CONCAT2(a, b)
#define CONCAT2(a, b) a ## b

/* Use the right prefix for global labels.  */

#define SYM(x) CONCAT1 (__USER_LABEL_PREFIX__, x)

#ifdef __ELF__
#define __PLT__ (PLT)
#define TYPE(x) .type SYM(x),function
#define SIZE(x) .size SYM(x), . - SYM(x)
#define LSYM(x) .x
#else
#define __PLT__
#define TYPE(x)
#define SIZE(x)
#define LSYM(x) x
#endif

/* Function end macros.  Variants for interworking.  */

/* Define how to return from a function call.  */

# define RET	return	
# define RETc(x)	\
     b##x	11f;	\
     b		21f;	\
11:			\
     return;		\
21:

.macro	cfi_pop		advance, reg, cfa_offset
#ifdef __ELF__
	.pushsection	.debug_frame
	.byte	0x4		/* DW_CFA_advance_loc4 */
	.4byte	\advance
	.byte	(0xc0 | \reg)	/* DW_CFA_restore */
	.byte	0xe		/* DW_CFA_def_cfa_offset */
	.uleb128 \cfa_offset
	.popsection
#endif
.endm
.macro	cfi_push	advance, reg, offset, cfa_offset
#ifdef __ELF__
	.pushsection	.debug_frame
	.byte	0x4		/* DW_CFA_advance_loc4 */
	.4byte	\advance
	.byte	(0x80 | \reg)	/* DW_CFA_offset */
	.uleb128 (\offset / -4)
	.byte	0xe		/* DW_CFA_def_cfa_offset */
	.uleb128 \cfa_offset
	.popsection
#endif
.endm
.macro cfi_start	start_label, end_label
#ifdef __ELF__
	.pushsection	.debug_frame
LSYM(Lstart_frame):
	.4byte	LSYM(Lend_cie) - LSYM(Lstart_cie) @ Length of CIE
LSYM(Lstart_cie):
        .4byte	0xffffffff	@ CIE Identifier Tag
        .byte	0x1	@ CIE Version
        .ascii	"\0"	@ CIE Augmentation
        .uleb128 0x1	@ CIE Code Alignment Factor
        .sleb128 -4	@ CIE Data Alignment Factor
        .byte	0xe	@ CIE RA Column
        .byte	0xc	@ DW_CFA_def_cfa
        .uleb128 0xd
        .uleb128 0x0

	.align 2
LSYM(Lend_cie):
	.4byte	LSYM(Lend_fde)-LSYM(Lstart_fde)	@ FDE Length
LSYM(Lstart_fde):
	.4byte	LSYM(Lstart_frame)	@ FDE CIE offset
	.4byte	\start_label	@ FDE initial location
	.4byte	\end_label-\start_label	@ FDE address range
	.popsection
#endif
.endm
.macro cfi_end	end_label
#ifdef __ELF__
	.pushsection	.debug_frame
	.align	2
LSYM(Lend_fde):
	.popsection
\end_label:
#endif
.endm

.macro FUNC_END name
	SIZE (__\name)
.endm

.macro FUNC_START name
	.text
	.globl SYM (__\name)
	TYPE (__\name)
	.align 0
SYM (__\name):
.endm

/* Special function that will always be coded in UNICORE64 assembly, even if
   in Thumb-only compilation.  */

.macro	UNICORE64_FUNC_START name
	.text
	.globl SYM (__\name)
	TYPE (__\name)
	.align 0
	.arm
SYM (__\name):
.endm
#define EQUIV .set
.macro  UNICORE64_CALL name
	call	__\name
.endm

.macro	FUNC_ALIAS new old
	.globl	SYM (__\new)
	.set	SYM (__\new), SYM (__\old)
.endm

.macro	UNICORE64_FUNC_ALIAS new old
	.globl	SYM (__\new)
	EQUIV	SYM (__\new), SYM (__\old)
.endm

/* ------------------------------------------------------------------------ */
#ifdef L_dvmd_tls

	FUNC_START div0

	RET

	FUNC_END div0
	
#endif /* L_divmodsi_tools */
/* ------------------------------------------------------------------------ */
#ifdef L_dvmd_lnx
@ GNU/Linux division-by zero handler.  Used in place of L_dvmd_tls

/* Constant taken from <asm/signal.h>.  */
#define SIGFPE	8

	.code	32
	FUNC_START div0

	stw.w	r1, [sp-], #4
	stw.w	lr, [sp-], #4
	mov	r0, #SIGFPE
	call	SYM(raise) __PLT__
	ldw.w	r1, [sp]+, #4
	ldw.w	ip, [sp]+, #4
	jump	ip

	FUNC_END div0
	
#endif /* L_dvmd_lnx */
