/* Definitions of target machine for UNICORE64 with a.out
   Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2004, 2007,2008
   Free Software Foundation, Inc.
   Contributed by Star (tanmingxing@mprc.pku.edu.cn).
   
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

#ifndef ASM_APP_ON
#define ASM_APP_ON  		""
#endif
#ifndef ASM_APP_OFF
#define ASM_APP_OFF  		""
#endif

/* Switch to the text or data segment.  */
#define TEXT_SECTION_ASM_OP  	"\t.text"
#define DATA_SECTION_ASM_OP  	"\t.data"
#define BSS_SECTION_ASM_OP   	"\t.bss"

#ifndef REGISTER_PREFIX
#define REGISTER_PREFIX 	""
#endif

#ifndef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX 	"_"
#endif

#ifndef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX 	""
#endif


/* The assembler's names for the registers.  */
#ifndef REGISTER_NAMES
#define REGISTER_NAMES  			   \
{				                   \
  "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",  \
  "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15", \
  "r16", "r17", "r18", "r19", "r20", "r21", "r22", "r23", \
  "r24", "r25", "sl", "fp", "ip", "sp", "lr", "pc",  \
  "f0", "f2", "f4", "f6", "f8", "f10", "f12", "f14",  \
  "f16", "f18", "f20", "f22", "f24", "f26", "f28", "f30",  \
  "cc", "sfp", "afp"		   		   \
}
#endif

#ifndef ADDITIONAL_REGISTER_NAMES
#define ADDITIONAL_REGISTER_NAMES               \
{                                               \
  {"a1", 0},                                    \
  {"a2", 1},                                    \
  {"a3", 2},                                    \
  {"a4", 3},                                    \
  {"v1", 17},                                   \
  {"v2", 18},                                   \
  {"v3", 19},                                   \
  {"v4", 20},                                   \
  {"v5", 21},                                   \
  {"v6", 22},                                   \
  {"v7", 23},                                   \
  {"v8", 24},                                   \
  {"v9", 25},                                   \
  {"v10",26},                                   \
  {"rfp",26}, /* Gcc used to call it this */    \
  {"sb", 26},                                   \
  {"r26", 26},  /* sl */                        \
  {"r27", 27},  /* fp */                        \
  {"r28", 28},  /* ip */                        \
  {"r29", 29},  /* sp */                        \
  {"r30", 30},  /* lr */                        \
  {"r31", 31}   /* pc */                        \
}
#endif


#ifndef NO_DOLLAR_IN_LABEL
#define NO_DOLLAR_IN_LABEL 1
#endif

/* Generate DBX debugging information.  riscix.h will undefine this because
   the native assembler does not support stabs. */
#ifndef DBX_DEBUGGING_INFO
#define DBX_DEBUGGING_INFO  1
#endif

/* Acorn dbx moans about continuation chars, so don't use any.  */
#ifndef DBX_CONTIN_LENGTH
#define DBX_CONTIN_LENGTH  0
#endif

/* Output a function label definition.  */
#ifndef ASM_DECLARE_FUNCTION_NAME
#define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL)	\
  do							\
    {							\
      UNICORE64_DECLARE_FUNCTION_NAME (STREAM, NAME, DECL);   \
      ASM_OUTPUT_LABEL (STREAM, NAME);			\
    }							\
  while (0)
#endif

#ifndef ASM_OUTPUT_LABEL
#define ASM_OUTPUT_LABEL(STREAM, NAME)		\
  do						\
    {						\
      assemble_name (STREAM,NAME);		\
      fputs (":\n", STREAM);			\
    }						\
  while (0)
#endif

/* Globalizing directive for a label.  */
#define GLOBAL_ASM_OP "\t.global\t"

/* Make an internal label into a string.  */
#ifndef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM)  \
  sprintf (STRING, "*%s%s%u", LOCAL_LABEL_PREFIX, PREFIX, (unsigned int)(NUM))
#endif

/* Construct a private name.  */
#define ASM_FORMAT_PRIVATE_NAME(OUTVAR, NAME, NUMBER)  \
  ((OUTVAR) = (char *) alloca (strlen (NAME) + 10),  \
   sprintf (OUTVAR, "%s.%d", NAME, NUMBER))

// huangping: FIXME 2011-07-28
/* Output an element of a dispatch table.  */
#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)  \
  asm_fprintf (STREAM, "\t.dword\t%LL%d\n", VALUE)

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)		\
  do									\
    {									\
      if (TARGET_UNICORE64)							\
	asm_fprintf (STREAM, "\tb\t%LL%d\n", VALUE);			\
      else								\
	asm_fprintf (STREAM, "\t.word\t%LL%d-%LL%d\n", VALUE, REL);	\
    }									\
  while (0)


#undef  ASM_OUTPUT_ASCII
#define ASM_OUTPUT_ASCII(STREAM, PTR, LEN)  \
  output_ascii_pseudo_op (STREAM, (const unsigned char *)(PTR), LEN)

/* Output a gap.  In fact we fill it with nulls.  */
#undef  ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(STREAM, NBYTES) 	\
  fprintf (STREAM, "\t.space\t%ld\n", NBYTES)

/* Align output to a power of two.   */
#ifndef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(STREAM, POWER)			\
  do							\
    {							\
      register int amount = 1 << (POWER);		\
							\
      if (amount == 2)					\
	fprintf (STREAM, "\t.even\n");			\
      else if (amount != 1)				\
	fprintf (STREAM, "\t.align\t%d\n", amount - 4);	\
    }							\
  while (0)
#endif

/* Output a common block */
#ifndef ASM_OUTPUT_COMMON
#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED)	\
  do							\
    {							\
      fprintf (STREAM, "\t.comm\t");			\
      assemble_name (STREAM, NAME);			\
      asm_fprintf (STREAM, ", %d\t%@ %d\n", 		\
	           ROUNDED, SIZE);			\
    }							\
  while (0)
#endif

/* Output a local common block.  /bin/as can't do this, so hack a
   `.space' into the bss segment.  Note that this is *bad* practice,
   which is guaranteed NOT to work since it doesn't define STATIC
   COMMON space but merely STATIC BSS space.  */
#ifndef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGN)		\
  do									\
    {									\
      bss_section ();							\
      ASM_OUTPUT_ALIGN (STREAM, floor_log2 (ALIGN / BITS_PER_UNIT));	\
      ASM_OUTPUT_LABEL (STREAM, NAME);					\
      fprintf (STREAM, "\t.space\t%ld\n", SIZE);				\
    }									\
  while (0)
#endif

/* Output a zero-initialized block.  */
#ifndef ASM_OUTPUT_ALIGNED_BSS
#define ASM_OUTPUT_ALIGNED_BSS(STREAM, DECL, NAME, SIZE, ALIGN) \
  asm_output_aligned_bss (STREAM, DECL, NAME, SIZE, ALIGN)
#endif

/* Output a #ident directive.  */
#ifndef ASM_OUTPUT_IDENT
#define ASM_OUTPUT_IDENT(STREAM,STRING)  \
  asm_fprintf (STREAM, "%@ - - - ident %s\n", STRING)
#endif

#ifndef ASM_COMMENT_START
#define ASM_COMMENT_START 	"@"
#endif
