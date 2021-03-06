/* Definitions of target machine for GNU compiler.
   For UNICORE64 with ELF obj format.
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2001, 2004, 2005,
   2007 Free Software Foundation, Inc.
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

#ifndef OBJECT_FORMAT_ELF
#error elf.h included before elfos.h
#endif

#ifndef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."
#endif

#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC  "-D__ELF__"
#endif

#ifndef ASM_SPEC
#define ASM_SPEC "\
%{mbig-endian:-EB} \
%{mcpu=*:-m%*} \
%{march=*:-m%*} \
%{municore16-interwork:-municore16-interwork} \
%(subtarget_extra_asm_spec)"
#endif

/* The UNICORE64 uses @ are a comment character so we need to redefine
   TYPE_OPERAND_FMT.  */
#undef  TYPE_OPERAND_FMT
#define TYPE_OPERAND_FMT	"%s"

/* We might need a UNICORE64 specific header to function declarations.  */
/* star modify */
#undef  ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(FILE, NAME, DECL)	\
  do							\
    {							\
      UNICORE64_DECLARE_FUNCTION_NAME (FILE, NAME, DECL);     \
      fprintf (FILE, "%s", TYPE_ASM_OP);		\
      assemble_name (FILE, NAME);			\
      putc (',', FILE);					\
      fprintf (FILE, TYPE_OPERAND_FMT, "function");	\
      putc ('\n', FILE);				\
      ASM_DECLARE_RESULT (FILE, DECL_RESULT (DECL));	\
      ASM_OUTPUT_LABEL(FILE, NAME);			\
    }							\
  while (0)

/* We might need an UNICORE64 specific trailer for function declarations.  */
/* star modify */
#undef  ASM_DECLARE_FUNCTION_SIZE
#define ASM_DECLARE_FUNCTION_SIZE(FILE, FNAME, DECL)		\
  do								\
    {								\
      UNICORE64_DECLARE_FUNCTION_SIZE (FILE, FNAME, DECL);	\
      if (!flag_inhibit_size_directive)				\
        {							\
	ASM_OUTPUT_MEASURED_SIZE (FILE, FNAME);			\
        }							\
    }								\
  while (0)

/* Define this macro if jump tables (for `tablejump' insns) should be
   output in the text section, along with the assembler instructions.
   Otherwise, the readonly data section is used.  */
/* We put UNICORE64 jump tables in the text section, because it makes the code
   more efficient, but for Unicore16 it's better to put them out of band.  */
#define JUMP_TABLES_IN_TEXT_SECTION (TARGET_UNICORE64)

#ifndef LINK_SPEC
#define LINK_SPEC "%{mbig-endian:-EB} -X"
#endif

/* Run-time Target Specification.  */
#ifndef TARGET_VERSION
#define TARGET_VERSION fputs (" (UNICORE64/elf)", stderr)
#endif

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT (UNICORE64_FLAG_SOFT_FLOAT )
#endif

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS \
  { "municore64", "mlittle-endian", "mhard-float",  "mno-unicore16-interwork", "fno-leading-underscore" }
#endif

#undef  TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION  unicore64_elf_asm_named_section

#undef  ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(STREAM, NAME, SIZE, ALIGN)	\
  do								\
    {								\
      fprintf (STREAM, "\t.comm\t");				\
      assemble_name (STREAM, NAME);				\
      fprintf (STREAM, ", %d, %ld\n", SIZE, ALIGN / BITS_PER_UNIT);\
    }								\
  while (0)

/* For PIC code we need to explicitly specify (PLT) and (GOT) relocs.  */
#define NEED_PLT_RELOC	flag_pic
#define NEED_GOT_RELOC	flag_pic

/* The ELF assembler handles GOT addressing differently to NetBSD.  */
#define GOT_PCREL	0

/* Biggest alignment supported by the object file format of this
   machine.  Use this macro to limit the alignment which can be
   specified using the `__attribute__ ((aligned (N)))' construct.  If
   not defined, the default value is `BIGGEST_ALIGNMENT'.  */
// star : defined in elfos.h
// #define MAX_OFILE_ALIGNMENT (32768 * 8)

/* Align output to a power of two.  Note ".align 0" is redundant,
   and also GAS will treat it as ".align 2" which we do not want.  */
#define ASM_OUTPUT_ALIGN(STREAM, POWER)			\
  do							\
    {							\
      if ((POWER) > 0)					\
	fprintf (STREAM, "\t.align\t%d\n", POWER);	\
    }							\
  while (0)

#define SUPPORTS_INIT_PRIORITY 1
