/* Definitions of target machine for GNU compiler, for UNICORE64.
   Copyright (C) 1991, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000,
   2001, 2002, 2003, 2004, 2005, 2006, 2007 Free Software Foundation, Inc.
   Contributed by Gaoyi and Star (tanmingxing@mprc.pku.edu.cn)

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


/***************************level 1************************/
/************************target macros*********************/
#ifndef GCC_UNICORE64_H
#define GCC_UNICORE64_H
#define TARGET_CPU_unicore1      0x0000
/* Configure didn't specify.  */
#define TARGET_CPU_generic	0x8000

/* star add for SYSV_PRAGMA support, 2008.7.1 */
#define HANDLE_SYSV_PRAGMA 1

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()			\
  do							\
    {							\
	builtin_define ("__unicore64__");		\
	if (TARGET_UNICORE16)				\
	  builtin_define ("__unicore16__");		\
							\
	  builtin_define ("__UNICORE64EL__");		\
	  if (TARGET_UNICORE16)				\
	    builtin_define ("__UNICORE16EL__");		\
							\
	if (TARGET_SOFT_FLOAT)				\
	  builtin_define ("__SOFTFP__");		\
							\
	builtin_assert ("cpu=unicore64");			\
	builtin_assert ("machine=unicore64");			\
    } while (0)

typedef enum unicore64_cond_code
{
  UNICORE64_EQ =
    0, UNICORE64_NE, UNICORE64_CS, UNICORE64_CC, UNICORE64_MI, UNICORE64_PL,
  UNICORE64_VS, UNICORE64_VC, UNICORE64_HI, UNICORE64_LS, UNICORE64_GE,
  UNICORE64_LT, UNICORE64_GT, UNICORE64_LE, UNICORE64_AL, UNICORE64_NV
}
unicore64_cc;

extern unicore64_cc unicore64_current_cc;

enum cmp_type
{ CMP_INT, CMP_FP };
extern enum cmp_type unicore64_compare_type;
extern int is_lwf_swf;
#define UNICORE64_INVERSE_CONDITION_CODE(X)  ((unicore64_cc) (((int)X) ^ 1))

extern int unicore64_target_label;
extern int unicore64_ccfsm_state;
extern GTY (()) rtx unicore64_target_insn;
/* Run-time compilation parameters selecting different hardware subsets.  */
extern int target_flags;
/* The floating point instruction architecture, can be 2 or 3 */
extern const char *target_fp_name;
/* Define the information needed to generate branch insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */
extern GTY (()) rtx unicore64_compare_op0;
extern GTY (()) rtx unicore64_compare_op1;
/* The label of the current constant pool.  */
extern struct rtx_def *pool_vector_label;
/* Set to 1 when a return insn is output, this means that the epilogue
   is not needed. */
extern int return_used_this_function;

/* Just in case configure has failed to define anything. */
#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT TARGET_CPU_generic
#endif

/* If the configuration file doesn't specify the cpu, the subtarget may
   override it.  If it doesn't, then default to an UNICORE64v1.  */
#if TARGET_CPU_DEFAULT == TARGET_CPU_generic
#undef TARGET_CPU_DEFAULT

#ifdef SUBTARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT SUBTARGET_CPU_DEFAULT
#else
#define TARGET_CPU_DEFAULT TARGET_CPU_unicore1
#endif
#endif

#if TARGET_CPU_DEFAULT == TARGET_CPU_unicore1
#define CPP_ARCH_DEFAULT_SPEC "-D__UNICORE_ARCH_1__"
#else
Unrecognized value in TARGET_CPU_DEFAULT.
#endif
#undef  CPP_SPEC
#define CPP_SPEC "\
%(cpp_cpu_arch) %(cpp_float) \
%(cpp_endian) %(subtarget_cpp_spec) %(cpp_isa) %(cpp_interwork)"
#define CPP_ISA_SPEC "%{municore16:-D__unicore16__} %{!municore16:-D__unicore64__}"
/* Set the architecture define -- if -march= is set, then it overrides
   the -mcpu= setting.  */
#define CPP_CPU_ARCH_SPEC "\
-Acpu=unicore1 -Amachine=unity \
%{march=unicore1:-D__UNICORE_ARCH_1__} \
%{!march=*: \
 %{mcpu=unicore1:-D__UNICORE_ARCH_1__} \
 %{!mcpu*:%(cpp_cpu_arch_default)}} \
"
#define CPP_FLOAT_SPEC "\
%{msoft-float:\
  %{mhard-float:%e-msoft-float and -mhard_float may not be used together} \
  -D__SOFTFP__} \
%{!mhard-float:%{!msoft-float:%(cpp_float_default)}} \
"
/* Default is hard float, which doesn't define anything */
#define CPP_FLOAT_DEFAULT_SPEC ""
#define CPP_ENDIAN_SPEC "\
%{mbig-endian:								\
  %{mlittle-endian:							\
    %e-mbig-endian and -mlittle-endian may not be used together}	\
  -D__UNICORE64EB__ %{mwords-little-endian:-D__UNICORE64WEL__} %{municore16:-D__UNICORE16EB__}}\
%{mlittle-endian:-D__UNICORE64EL__ %{municore16:-D__UNICORE16EL__}}			\
%{!mlittle-endian:%{!mbig-endian:%(cpp_endian_default)}}		\
"
/* Default is little endian.  */
#define CPP_ENDIAN_DEFAULT_SPEC "-D__UNICORE64EL__ %{municore16:-D__UNICORE16EL__}"
/* Add a define for interworking.  Needed when building libgcc.a.  
   This must define __UNICORE16_INTERWORK__ to the pre-processor if
   interworking is enabled by default.  */
#ifndef CPP_INTERWORK_DEFAULT_SPEC
#define CPP_INTERWORK_DEFAULT_SPEC ""
#endif
#define CPP_INTERWORK_SPEC "						\
%{municore16-interwork:							\
  %{mno-unicore16-interwork: %eincompatible interworking options}	\
  -D__UNICORE16_INTERWORK__}						\
%{!municore16-interwork:%{!mno-unicore16-interwork:%(cpp_interwork_default)}}	\
"
/* star modify cc1_SPECS for profile, 2008.7.1 */
#ifndef CC1_SPEC
#define CC1_SPEC ""
#endif
//#undef  CC1_SPEC
//#define CC1_SPEC "%{profile:-p} %{pg|p|profile:%{!fno-omit-frame-pointer: -fno-omit-frame-pointer}}"
/* This macro defines names of additional specifications to put in the specs
   that can be used in various specifications like CC1_SPEC.  Its definition
   is an initializer with a subgrouping for each command option.

   Each subgrouping contains a string constant, that defines the
   specification name, and a string constant that used by the GNU CC driver
   program.

   Do not define this macro if it does not need to do anything.  */
#define EXTRA_SPECS						\
  { "cpp_cpu_arch",		CPP_CPU_ARCH_SPEC },		\
  { "cpp_cpu_arch_default",	CPP_ARCH_DEFAULT_SPEC },	\
  { "cpp_float",		CPP_FLOAT_SPEC },		\
  { "cpp_float_default",	CPP_FLOAT_DEFAULT_SPEC },	\
  { "cpp_endian",		CPP_ENDIAN_SPEC },		\
  { "cpp_endian_default",	CPP_ENDIAN_DEFAULT_SPEC },	\
  { "cpp_isa",			CPP_ISA_SPEC },			\
  { "cpp_interwork",		CPP_INTERWORK_SPEC },		\
  { "cpp_interwork_default",	CPP_INTERWORK_DEFAULT_SPEC },	\
  { "subtarget_cpp_spec",	SUBTARGET_CPP_SPEC },           \

#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC      ""
#endif
/* Run-time Target Specification.  */
#ifndef TARGET_VERSION
#define TARGET_VERSION fputs (" (UNICORE64/generic)", stderr);
#endif
/* Nonzero if the MMU will trap unaligned word accesses, so shorts must
   be loaded using either LDRH or LDRB instructions.  */
#define UNICORE64_FLAG_MMU_TRAPS	(1 << 27)
/* Nonzero if all floating point instructions are missing (and there is no
   emulator either).  Generate function calls for all ops in this case.  */
#define UNICORE64_FLAG_SOFT_FLOAT	(1 << 28)
/* Nonzero if we need to protect the prolog from scheduling */
#define UNICORE64_FLAG_NO_SCHED_PRO	(1 <<29)
/* Set if a TPCS style stack frame should be generated, for non-leaf
   functions, even if they do not need one.  */
#define UNICORE16_FLAG_BACKTRACE	(1 << 30)
/* Set if a TPCS style stack frame should be generated, for leaf
   functions, even if they do not need one.  */
#define UNICORE16_FLAG_LEAF_BACKTRACE    		(1 << 31)
#define TARGET_MMU_TRAPS		(target_flags & UNICORE64_FLAG_MMU_TRAPS)
#define TARGET_SOFT_FLOAT		(unicore64_float_abi == UNICORE64_FLOAT_ABI_SOFT)
#define TARGET_HARD_FLOAT		(unicore64_float_abi != UNICORE64_FLOAT_ABI_SOFT)
#define TARGET_NO_SCHED_PRO		(target_flags & UNICORE64_FLAG_NO_SCHED_PRO)
#define TARGET_UNICORE64                      (! TARGET_UNICORE16)
#define TARGET_EITHER			1	/* (TARGET_UNICORE64 | TARGET_UNICORE16) */
#define TARGET_BACKTRACE	        (leaf_function_p ()	      			\
				         ? (target_flags & UNICORE16_FLAG_LEAF_BACKTRACE)	\
				         : (target_flags & UNICORE16_FLAG_BACKTRACE))

#define TARGET_HARD_TP			(target_thread_pointer == TP_REG16)
#define TARGET_SOFT_TP			(target_thread_pointer == TP_SOFT)
/* SUBTARGET_SWITCHES is used to add flags on a per-config basis.
   Bit 31 is reserved.  See riscix.h.  */
#ifndef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES
#endif
  struct unicore64_cpu_select
{
  const char *string;
  const char *name;
  const struct processors *processors;
};

/* This is a magic array.  If the user specifies a command line switch
   which matches one of the entries in TARGET_OPTIONS then the corresponding
   string pointer will be set to the value specified by the user.  */
extern struct unicore64_cpu_select unicore64_select[];

enum float_abi_type
{
    UNICORE64_FLOAT_ABI_SOFT,
    UNICORE64_FLOAT_ABI_HARD
};

extern enum float_abi_type unicore64_float_abi;
 
/* What sort of floating point unit do we have? Hardware or software.
   If software, is it issue 2 or issue 3?  */
enum floating_point_type
{
  FP_HARD,
  FP_SOFT2,
  FP_SOFT3
};

/* Recast the floating point class to be the floating point attribute.  */
#define unicore64_fpu_attr ((enum attr_fpu) unicore64_fpu)

/* What type of floating point to tune for */
extern enum floating_point_type unicore64_fpu;

/* What type of floating point instructions are available */
extern enum floating_point_type unicore64_fpu_arch;

/* Default floating point architecture.  Override in sub-target if
   necessary.  */
#ifndef FP_DEFAULT
#define FP_DEFAULT FP_SOFT3
#endif

/* Which thread pointer access sequence to use.  */
enum unicore64_tp_type {
  TP_AUTO,
  TP_SOFT,
  TP_REG16
};

extern enum unicore64_tp_type target_thread_pointer;

/* Nonzero if the processor has a fast multiply insn, and one that does
   a 64-bit multiply of two 32-bit values.  */
extern int unicore64_fast_multiply;

/* Nonzero if this chip supports the UNICORE64 Architecture 1 extensions */
extern int unicore64_arch1;

/* Nonzero if this chip can benefit from load scheduling.  */
extern int unicore64_ld_sched;

/* Nonzero if generating unicore16 code.  */
extern int unicore16_code;

#ifndef TARGET_DEFAULT
#define TARGET_DEFAULT  (MASK_APCS_FRAME)
#endif

/* The frame pointer register used in gcc has nothing to do with debugging */
#define CAN_DEBUG_WITHOUT_FP


#define OVERRIDE_OPTIONS  unicore64_override_options ()

/* Nonzero if PIC code requires explicit qualifiers to generate
   PLT and GOT relocs rather than the assembler doing so implicitly.
   Subtargets can override these if required.  */
#ifndef NEED_GOT_RELOC
#define NEED_GOT_RELOC	0
#endif
#ifndef NEED_PLT_RELOC
#define NEED_PLT_RELOC	0
#endif

/* Nonzero if we need to refer to the GOT with a PC-relative
   offset.  In other words, generate
   .word	_GLOBAL_OFFSET_TABLE_ - [. - (.Lxx + 8)]  
   rather than
   .word	_GLOBAL_OFFSET_TABLE_ - (.Lxx + 8)
   The default is true, which matches NetBSD.  Subtargets can 
   override this if required.  */
#ifndef GOT_PCREL
#define GOT_PCREL   1
#endif

// huangping: FIXME 2011-10-28
// If we are crossing build from 32 bits host to 64 bits targets,
// then define CROSS_32HOST_64TARGET used in function host_integerp in tree.c.
//#define CROSS_32HOST_64TARGET


/*****************************level 2*****************************/
/******************* Layout of Source Language Data Types ******************/

/* Define the size of `int'.  The default is the same as the word size.  */
#define INT_TYPE_SIZE 32

// huangping add
/* Define the size of `short'.  The default is the same as half of a word size.  */
#define SHORT_TYPE_SIZE 16

// huangping add
/* Define the size of `char'.  The default is the same as BITS_PER_UNIT.  */
#define CHAR_TYPE_SIZE BITS_PER_UNIT

/* Define the size of `long'.  */
#define LONG_TYPE_SIZE 64

/* Define the size of `long long'.  The default is the twice the word size.  */
#define LONG_LONG_TYPE_SIZE 64

/* The two floating-point formats we support are S-floating, which is
   4 bytes, and T-floating, which is 8 bytes.  `float' is S and `double'
   and `long double' are T.  */

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

#define	WCHAR_TYPE "unsigned int"
#define	WCHAR_TYPE_SIZE 32


/************************** Storage Layout *****************************/

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

/* It is far faster to zero extend chars than to sign extend them */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)	\
  if (GET_MODE_CLASS (MODE) == MODE_INT		\
      && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)      	\
    {						\
      if (MODE == SImode)			\
	UNSIGNEDP = 1;				\
      (MODE) = DImode;				\
    }

/* Disable XFmode patterns in md file */
#define ENABLE_XF_PATTERNS 0

/* Define this if most significant bit is lowest numbered
   in instructions that operate on numbered bit-fields.  */
#define BITS_BIG_ENDIAN  0

/* Define this if most significant byte of a word is the lowest numbered.  
   Most UNICORE64 processors are run in little endian mode, so that is the default.
   If you want to have it run-time selectable, change the definition in a
   cover file to be TARGET_BIG_ENDIAN.  */
#define BYTES_BIG_ENDIAN 0 

/* Define this if most significant word of a multiword number is the lowest
   numbered.  */
#define WORDS_BIG_ENDIAN 0 

/* Define this if most significant word of doubles is the lowest numbered.
   This is always true, even when in little-endian mode.  */
#define FLOAT_WORDS_BIG_ENDIAN 0

/* Number of bits in an addressable storage unit */
#define BITS_PER_UNIT  8

// huangping: FIXME
#define BITS_PER_WORD  64

/* Width of a word, in units (bytes).  */
#define UNITS_PER_WORD 8

//#ifdef IN_LIBGCC2
//#define HOST_BITS_PER_WIDE_INT 64
//#define HOST_WIDE_INT long long
//
//#define MIN_UNITS_PER_WORD UNITS_PER_WORD
//#else
//#define MIN_UNITS_PER_WORD 4
//#endif

/* Width of a word, in units (bytes).  */
// huangping: FIXME
#ifdef IN_LIBGCC2 
#define MIN_UNITS_PER_WORD 4
//#define TARGET_64BIT	1
//#define LIBGCC2_UNITS_PER_WORD 8
#endif

#define POINTER_SIZE  64

#define PARM_BOUNDARY  	64

#define STACK_BOUNDARY  64

#define FUNCTION_BOUNDARY  64

/* The lowest bit is used to indicate Unicore16-mode functions, so the
   vbit must go into the delta field of pointers to member
   functions.  */
#define TARGET_PTRMEMFUNC_VBIT_LOCATION ptrmemfunc_vbit_in_delta

// huangping: 2011-08-01 32=>64
#define EMPTY_FIELD_BOUNDARY  64 

#define BIGGEST_ALIGNMENT  64

/* Make strings word-aligned so strcpy from constants will be faster.  */
#define CONSTANT_ALIGNMENT_FACTOR 2

#define CONSTANT_ALIGNMENT(EXP, ALIGN)				\
  ((TREE_CODE (EXP) == STRING_CST				\
    && (ALIGN) < BITS_PER_WORD * CONSTANT_ALIGNMENT_FACTOR)	\
   ? BITS_PER_WORD * CONSTANT_ALIGNMENT_FACTOR : (ALIGN))

/* If defined, a C expression to compute the alignment for a static
   variable.  TYPE is the data type, and ALIGN is the alignment that
   the object would ordinarily have.  The value of this macro is used
   instead of that alignment to align the object.

   If this macro is not defined, then ALIGN is used.

   One use of this macro is to increase alignment of medium-size
   data to make it all fit in fewer cache lines.  Another is to
   cause character arrays to be word-aligned so that `strcpy' calls
   that copy constants to character arrays can be done inline.  */

#undef DATA_ALIGNMENT
#define DATA_ALIGNMENT(TYPE, ALIGN)					\
  ((((ALIGN) < BITS_PER_WORD)						\
    && (TREE_CODE (TYPE) == ARRAY_TYPE					\
	|| TREE_CODE (TYPE) == UNION_TYPE				\
	|| TREE_CODE (TYPE) == RECORD_TYPE)) ? BITS_PER_WORD : (ALIGN))

/* We need this for the same reason as DATA_ALIGNMENT, namely to cause
   character arrays to be word-aligned so that `strcpy' calls that copy
   constants to character arrays can be done inline, and 'strcmp' can be
   optimised to use word loads. */
#define LOCAL_ALIGNMENT(TYPE, ALIGN) \
  DATA_ALIGNMENT (TYPE, ALIGN)
  
/* Setting STRUCTURE_SIZE_BOUNDARY to 32 produces more efficient code, but the
   value set in previous versions of this toolchain was 8, which produces more
   compact structures.  The command line option -mstructure_size_boundary=<n>
   can be used to change this value.  For compatibility with the UNICORE64 SDK
   however the value should be left at 32.
   UNICORE64 ABI says "Structures are aligned on word boundaries".  */
#define STRUCTURE_SIZE_BOUNDARY unicore64_structure_size_boundary
extern int unicore64_structure_size_boundary;

/* This is the value used to initialise unicore64_structure_size_boundary.  If a
   particular unicore64 target wants to change the default value it should change
   the definition of this macro, not STRUCTRUE_SIZE_BOUNDARY.  See netbsd.h
   for an example of this.  */
// huangping: 2011-08-01
#ifndef DEFAULT_STRUCTURE_SIZE_BOUNDARY
#define DEFAULT_STRUCTURE_SIZE_BOUNDARY 32 
#endif

/* Used when parsing command line option -mstructure_size_boundary.  */
extern const char *structure_size_string;

/* Non-zero if move instructions will actually fail to work
   when given unaligned data.  */
#define STRICT_ALIGNMENT 1

/* long double is not a fixed mode, but the idea is that, if we
   support long double, we also want a 128-bit integer type.  */
#define MAX_FIXED_MODE_SIZE LONG_DOUBLE_TYPE_SIZE

#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT


/******************************level 3**************************************/
/************************* Standard Register Usage ***************************/

/* Register allocation in UNICORE64 Procedure Call Standard (as used on RISCiX):
   (S - saved over call).

	r0	   *	argument word/integer result
	r1-r3		argument word

	r4-r8	     S	register variable
	r9	     S	(rfp) register variable (real frame pointer)
	
	r10  	   F S	(sl) stack limit 
	r11 	   F S	(fp) argument pointer
	r12		(ip) temp workspace
	r13  	   F S	(sp) lower end of current stack frame
	r14		(lr) link address/workspace
	r15	   F	(pc) program counter

	f0		floating point result
	f1-f3		floating point scratch

	f4-f7	     S	floating point variable

	cc		This is NOT a real register, but is used internally
	                to represent things that use or set the condition
			codes.
	sfp             This isn't either.  It is used during rtl generation
	                since the offset between the frame pointer and the
			auto's isn't known until after register allocation.
	afp		Nor this, we only need this because of non-local
	                goto.  Without it fp appears to be used and the
			elimination code won't get rid of sfp.  It tracks
			fp exactly at all times.

   *: See CONDITIONAL_REGISTER_USAGE  */

/* The stack backtrace structure is as follows:
  fp points to here:  |  save code pointer  |      [fp]
                      |  return link value  |      [fp, #-4]
                      |  return sp value    |      [fp, #-8]
                      |  return fp value    |      [fp, #-12]
                     [|  saved r10 value    |]
                     [|  saved r9 value     |]
                     [|  saved r8 value     |]
                     [|  saved r7 value     |]
                     [|  saved r6 value     |]
                     [|  saved r5 value     |]
                     [|  saved r4 value     |]
                     [|  saved r3 value     |]
                     [|  saved r2 value     |]
                     [|  saved r1 value     |]
                     [|  saved r0 value     |]
                     [|  saved f7 value     |]     three words
                     [|  saved f6 value     |]     three words
                     [|  saved f5 value     |]     three words
                     [|  saved f4 value     |]     three words
  r0-r3 are not normally saved in a C function.  */

/* 1 for registers that have pervasive standard uses
   and are not available for the register allocator.  */
#define FIXED_REGISTERS  \
{                        \
  0,0,0,0,0,0,0,0,       \
  0,0,0,0,0,0,0,0,       \
  1,0,0,0,0,0,0,0,       \
  0,0,0,0,0,1,0,1,       \
  0,0,0,0,0,0,0,0,       \
  0,0,0,0,0,0,0,0,       \
  1,1,1                  \
}
/* 1 for registers not available across function calls.
   These must include the FIXED_REGISTERS and also any
   registers that can be used without being saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.
   Aside from that, you can include as many other registers as you like.
   The CC is not preserved over function calls on the UNICORE64 6, so it is 
   easier to assume this for all.  SFP is preserved, since FP is. */
#define CALL_USED_REGISTERS  \
{                            \
  1,1,1,1,1,1,1,1,           \
  1,1,1,1,1,1,1,1,   	     \
  1,0,0,0,0,0,0,0,           \
  0,0,0,0,1,1,1,1,           \
  1,1,1,1,1,1,1,1,           \
  0,0,0,0,0,0,0,0,           \
  1,1,1                      \
}

#define CONDITIONAL_REGISTER_USAGE                              \
{                                                               \
  int regno;                                                    \
  if (TARGET_SOFT_FLOAT || TARGET_UNICORE16)                    \
    {                                                           \
      for (regno = FIRST_UNICORE64_FP_REGNUM;                   \
           regno <= LAST_UNICORE64_FP_REGNUM; ++regno)          \
        fixed_regs[regno] = call_used_regs[regno] = 1;          \
    }                                                           \
  if (PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM)                \
    {                                                           \
      fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;                  \
      call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;              \
    }                                                           \
}

/* These are a couple of extensions to the formats accecpted
   by asm_fprintf:
     %@ prints out ASM_COMMENT_START
     %r prints out REGISTER_PREFIX reg_names[arg]  */
#define ASM_FPRINTF_EXTENSIONS(FILE, ARGS, P)		\
  case '@':						\
    fputs (ASM_COMMENT_START, FILE);			\
    break;						\
							\
  case 'r':						\
    fputs (REGISTER_PREFIX, FILE);			\
    fputs (reg_names [va_arg (ARGS, int)], FILE);	\
    break;

// huangping: FIXME 2011-09-19
/* Round X up to the nearest word.  */
#define ROUND_UP_WORD(X) (((X) + 7) & ~7)

/* Convert fron bytes to longs.  */
#define NUM_LONGS(X) (((X) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* The number of (integer) registers required to hold a quantity of type MODE.  */
#define NUM_REGS(MODE)				\
  NUM_LONGS (GET_MODE_SIZE (MODE))

/* The number of (integer) registers required to hold a quantity of TYPE MODE.  */
#define NUM_REGS2(MODE, TYPE)                   \
  NUM_LONGS ((MODE) == BLKmode ? 		\
  int_size_in_bytes (TYPE) : GET_MODE_SIZE (MODE))

/* The number of (integer) argument register available.  */
#define NUM_ARG_REGS		4

/* Return the regiser number of the N'th (integer) argument.  */
#define ARG_REGISTER(N) 	(N - 1)

/* Specify the registers used for certain standard purposes.
   The values of these macros are register numbers.  */

/* The number of the last argument register.  */
#define LAST_ARG_REGNUM 	ARG_REGISTER (NUM_ARG_REGS)

/* The register that holds the return address in exception handlers.  */
#define EXCEPTION_LR_REGNUM	2

/* star add it */
/* Use r0 and r1 to pass exception handling information.  */
#define EH_RETURN_DATA_REGNO(N) (((N) < 4) ? N : INVALID_REGNUM)

/* star add it */
/* The register that holds the return address in exception handlers.  */
#define UNICORE64_EH_STACKADJ_REGNUM	2
#define EH_RETURN_STACKADJ_RTX	gen_rtx_REG (DImode, UNICORE64_EH_STACKADJ_REGNUM)

/* The native (Norcroft) Pascal compiler for the UNICORE64 passes the static chain
   as an invisible last argument (possible since varargs don't exist in
   Pascal), so the following is not true.  */
#define STATIC_CHAIN_REGNUM	(TARGET_UNICORE64 ? IP_REGNUM : 9)

/* Define this to be where the real frame pointer is if it is not possible to
   work out the offset between the frame pointer and the automatic variables
   until after register allocation has taken place.  FRAME_POINTER_REGNUM
   should point to a special register that we will make sure is eliminated.

   For the Unicore16 we have another problem.  The TPCS defines the frame pointer
   as r11, and GCC belives that it is always possible to use the frame pointer
   as base register for addressing purposes.  (See comments in
   find_reloads_address()).  But - the Unicore16 does not allow high registers,
   including r11, to be used as base address registers.  Hence our problem.

   The solution used here, and in the old unicore16 port is to use r7 instead of
   r11 as the hard frame pointer and to have special code to generate
   backtrace structures on the stack (if required to do so via a command line
   option) using r11.  This is the only 'user visable' use of r11 as a frame
   pointer.  */
/* gaoyi */
#define PIC_REGISTER_NUM (TARGET_UNICORE64 ? 25 : 9)
#define SL_REGISTER_NUM  (TARGET_UNICORE64 ? 26	:10)
#define UNICORE64_HARD_FRAME_POINTER_REGNUM	27
#define UNICORE64_FIRST_PRESERVED_REGNUM 4
#define UNICORE64_LAST_PRESERVED_REGNUM 11

#define HARD_FRAME_POINTER_REGNUM	UNICORE64_HARD_FRAME_POINTER_REGNUM		

#define FP_REGNUM	                HARD_FRAME_POINTER_REGNUM

/* Register to use for pushing function arguments.  */
#define STACK_POINTER_REGNUM	SP_REGNUM

/* UNICORE64 floating pointer registers.  */
#define FIRST_UNICORE64_FP_REGNUM 	32
#define LAST_UNICORE64_FP_REGNUM  	47

/* Base register for access to local variables of the function.  */
#define FRAME_POINTER_REGNUM	49

/* Base register for access to arguments of the function.  */
#define ARG_POINTER_REGNUM	50

/* The number of hard registers is 32 UNICORE  + 16 FPU + 1 CC + 1 SFP.  */
#define FIRST_PSEUDO_REGISTER	51

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be accessed
   via the stack pointer) in functions that seem suitable.  
   If we have to have a frame pointer we might as well make use of it.*/
/* 2008.7.1 star modify for -pg&-O2, always use frame pointer for profiler 
   linux-elf.h will redefine SUBTARGET_FRAME_POINTER_REQUIRED */

/* The GNU/Linux profiler needs a frame pointer.  */
#define SUBTARGET_FRAME_POINTER_REQUIRED crtl->profile

#define FRAME_POINTER_REQUIRED					\
  (cfun->has_nonlocal_label 					\
   || SUBTARGET_FRAME_POINTER_REQUIRED)				\
//   || (TARGET_UNICORE64 && ! leaf_function_p ()))

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.

   On the UNICORE64 regs are UNITS_PER_WORD bits wide; FPU regs can hold any FP
   mode.  */
#define HARD_REGNO_NREGS(REGNO, MODE)  	\
  ((TARGET_UNICORE64 				\
    && REGNO >= FIRST_UNICORE64_FP_REGNUM	\
    && REGNO != FRAME_POINTER_REGNUM	\
    && REGNO != ARG_POINTER_REGNUM)	\
   ? 1 : NUM_REGS (MODE))

/* Return true if REGNO is suitable for holding a quantity of type MODE.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE)					\
  unicore64_hard_regno_mode_ok ((REGNO), (MODE))

/* Value is 1 if it is a good idea to tie two pseudo registers
   when one has mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */
#define MODES_TIEABLE_P(MODE1, MODE2)  \
  (GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2))

// huangping: FIXME 2011-12-15
/* Due to the UNICORE64 discrepancy above we must override this next
   macro too.  */
#define REGMODE_NATURAL_SIZE(MODE) \
  ((TARGET_UNICORE64) ? 4 : UNITS_PER_WORD)

/* The order in which register should be allocated.  It is good to use ip
   since no saving is required (though calls clobber it) and it never contains
   function parameters.  It is quite good to use lr since other calls may
   clobber it anyway.  Allocate r0 through r3 in reverse order since r3 is 
   least likely to contain a function parameter; in addition results are
   returned in r0.  */
/* gaoyi */
#define REG_ALLOC_ORDER             \
{                                   \
    15, 14, 13, 12, 11, 10,  9,  8, \
     7,  6,  5,  4,  3,  2,  1,  0, \
    28, 30, 26, 24, 23, 22, 21, 20, \
    19, 18, 17, 16, 25, 27, 29, 31, \
    32, 33, 34, 35, 36, 37, 38, 39, \
    40, 41, 42, 43, 44, 45, 46, 47, \
    48, 49, 50                      \
}

/* Interrupt functions can only use registers that have already been
   saved by the prologue, even if they would normally be
   call-clobbered.  */
#define HARD_REGNO_RENAME_OK(SRC, DST)					\
	(! IS_INTERRUPT (cfun->machine->func_type) ||			\
	 df_regs_ever_live_p (DST))

/* Register and constant classes.  */

/* Register classes: used to be simple, just all UNICORE64 regs or all FPU regs
   Now that the Unicore16 is involved it has become more complicated.  */
enum reg_class
{
  NO_REGS,
  FPU_REGS,
  LO_REGS,
  STACK_REG,
  BASE_REGS,
  HI_REGS,
  CC_REG,
  GENERAL_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

#define N_REG_CLASSES  (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.   */
#define REG_CLASS_NAMES  \
{			\
  "NO_REGS",		\
  "FPU_REGS",		\
  "LO_REGS",		\
  "STACK_REG",		\
  "BASE_REGS",		\
  "HI_REGS",		\
  "CC_REG",		\
  "GENERAL_REGS",	\
  "ALL_REGS",		\
}

/* Define which registers fit in which classes.
   This is an initializer for a vector of HARD_REG_SET
   of length N_REG_CLASSES.  */
/* gaoyi */
#define REG_CLASS_CONTENTS              \
{                                       \
  { 0x00000000,0x00000000 }, /* NO_REGS  */         \
  { 0x00000000,0x0000FFFF }, /* FPU_REGS */         \
  { 0x000000FF,0x00000000 }, /* LO_REGS */          \
  { 0x20000000,0x00000000 }, /* STACK_REG */        \
  { 0x200000FF,0x00000000 }, /* BASE_REG */         \
  { 0xF0000F00,0x00000000 }, /* HI_REGS */          \
  { 0x00000000,0x00010000 }, /* CC_REG */           \
  { 0xFFFFFFFF,0x00020000 }, /* GENERAL_REGS */    \
  { 0xFFFFFFFF,0x0002FFFF }  /* ALL_REGS */                \
}
/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */
#define REGNO_REG_CLASS(REGNO)  unicore64_regno_class (REGNO)

/* The following macro defines cover classes for Integrated Register
   Allocator.  Cover classes is a set of non-intersected register
   classes covering all hard registers used for register allocation
   purpose.  Any move between two registers of a cover class should be
   cheaper than load or store of the registers.  The macro value is
   array of register classes with LIM_REG_CLASSES used as the end
   marker.  */

#define IRA_COVER_CLASSES						     \
{									     \
  GENERAL_REGS, FPU_REGS, 						     \
  LIM_REG_CLASSES							     \
}

/* The class value for index registers, and the one for base regs.  */
#define INDEX_REG_CLASS  (TARGET_UNICORE16 ? LO_REGS : GENERAL_REGS)
#define BASE_REG_CLASS   (TARGET_UNICORE16 ? BASE_REGS : GENERAL_REGS)

/* For the Unicore16 the high registers cannot be used as base
   registers when addressing quanitities in QI or HI mode.  
   if we don't know themode, then we must be conservative.  
   After reload we must also be conservative, since we can't 
   support SP+reg addressing, and we can't fix up any bad substitutions.  */
#define MODE_BASE_REG_CLASS(MODE)		GENERAL_REGS


/*********************************level 4*************************/
/*****************************constraints masros******************/
/* Get reg_class from a letter such as appears in the machine description.
   We only need constraint `f' for FPU_REGS (`r' == GENERAL_REGS) for the
   UNICORE64, but several more letters for the Unicore16.  */
#define REG_CLASS_FROM_LETTER(C)  	\
  (  (C) == 'f' ? FPU_REGS		\
   : (C) == 'l' ? (TARGET_UNICORE64 ? GENERAL_REGS : LO_REGS)	\
   : TARGET_UNICORE64 ? NO_REGS		\
   : (C) == 'h' ? HI_REGS		\
   : (C) == 'b' ? BASE_REGS		\
   : (C) == 'k' ? STACK_REG		\
   : (C) == 'c' ? CC_REG		\
   : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string
   can be used to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.
	I: immediate arithmetic operand (i.e. 8 bits shifted as required).
	J: valid indexing constants.  
	K: ~value ok in rhs argument of data operand.
	L: -value ok in rhs argument of data operand. 
        M: 0..32, or a power of 2  (for shifts, or mult done by shift).  */
/* gaoyi:
 * in UNICORE64, the valid index is (-16384, 16384)
 */
#define CONST_OK_FOR_UNICORE64_LETTER(VALUE, C)  		\
  ((C) == 'I' ? const_ok_for_unicore64 (VALUE) :		\
   (C) == 'J' ? ((VALUE) < 16384 && (VALUE) > -16384) :	\
   (C) == 'K' ? (const_ok_for_unicore64 (~(VALUE))) :		\
   (C) == 'L' ? (const_ok_for_unicore64 (-(VALUE))) :		\
   (C) == 'M' ? (((VALUE >= 0 && VALUE <= 32))		\
		 || (((VALUE) & ((VALUE) - 1)) == 0))	\
   : 0)

#define CONST_OK_FOR_LETTER_P(VALUE, C)					\
   CONST_OK_FOR_UNICORE64_LETTER (VALUE, C)

/* Constant letter 'G' for the FPU immediate constants. 
   'H' means the same constant negated.  */
#define CONST_DOUBLE_OK_FOR_UNICORE64_LETTER(X, C)			\
    ((C) == 'G' ? const_double_rtx_ok_for_fpu (X) :		\
     (C) == 'H' ? neg_const_double_rtx_ok_for_fpu (X) : 0)

#define CONST_DOUBLE_OK_FOR_LETTER_P(X, C)			\
  (TARGET_UNICORE64 ?							\
   CONST_DOUBLE_OK_FOR_UNICORE64_LETTER (X, C) : 0)

/* For the UNICORE64, `Q' means that this is a memory operand that is just
   an offset from a register.  
   `S' means any symbol that has the SYMBOL_REF_FLAG set or a CONSTANT_POOL
   address.  This means that the symbol is in the text segment and can be
   accessed without using a load. */

#define EXTRA_CONSTRAINT_UNICORE64(OP, C)					    \
  ((C) == 'Q' ? GET_CODE (OP) == MEM && GET_CODE (XEXP (OP, 0)) == REG :    \
   (C) == 'R' ? (GET_CODE (OP) == MEM					    \
		 && GET_CODE (XEXP (OP, 0)) == SYMBOL_REF		    \
		 && CONSTANT_POOL_ADDRESS_P (XEXP (OP, 0))) :		    \
   (C) == 'S' ? (optimize > 0 && CONSTANT_ADDRESS_P (OP))		    \
   : 0)

#define EXTRA_CONSTRAINT(X, C)						\
   EXTRA_CONSTRAINT_UNICORE64 (X, C) 

/* Given an rtx X being reloaded into a reg required to be
   in class CLASS, return the class of reg to actually use.
   In general this is just CLASS, but for the Unicore16 we prefer
   a LO_REGS class or a subset.  */
#define PREFERRED_RELOAD_CLASS(X, CLASS)	\
  (TARGET_UNICORE64 ? (CLASS) :			\
   ((CLASS) == BASE_REGS ? (CLASS) : LO_REGS))


/* Return the register class of a scratch register needed to copy IN into
   or out of a register in CLASS in MODE.  If it can be done directly,
   NO_REGS is returned.  */
#define SECONDARY_OUTPUT_RELOAD_CLASS(CLASS, MODE, X)		\
   (((MODE) == HImode && ! unicore64_arch1 && true_regnum (X) == -1)	\
    ? GENERAL_REGS : NO_REGS)					

/* If we need to load shorts byte-at-a-time, then we need a scratch. */
#define SECONDARY_INPUT_RELOAD_CLASS(CLASS, MODE, X)		\
   (((MODE) == HImode && ! unicore64_arch1 && TARGET_MMU_TRAPS	\
     && (GET_CODE (X) == MEM					\
	 || ((GET_CODE (X) == REG || GET_CODE (X) == SUBREG)	\
	     && true_regnum (X) == -1)))			\
    ? GENERAL_REGS : NO_REGS)					

/* Try a machine-dependent way of reloading an illegitimate address
   operand.  If we find one, push the reload and jump to WIN.  This
   macro is used in only one place: `find_reloads_address' in reload.c.

   For the UNICORE64, we wish to handle large displacements off a base
   register by splitting the addend across a MOV and the mem insn.
   This can cut the number of reloads needed.  */
#define UNICORE64_LEGITIMIZE_RELOAD_ADDRESS(X, MODE, OPNUM, TYPE, IND, WIN)	   \
  do									   \
    {									   \
      if (GET_CODE (X) == PLUS						   \
	  && GET_CODE (XEXP (X, 0)) == REG				   \
	  && REGNO (XEXP (X, 0)) < FIRST_PSEUDO_REGISTER		   \
	  && REG_MODE_OK_FOR_BASE_P (XEXP (X, 0), MODE)			   \
	  && GET_CODE (XEXP (X, 1)) == CONST_INT)			   \
	{								   \
	  HOST_WIDE_INT val = INTVAL (XEXP (X, 1));			   \
	  HOST_WIDE_INT low, high;					   \
									   \
	  if (MODE == DImode || MODE == SImode			   \
		   || ((MODE == HImode || MODE == QImode) && ! unicore64_arch1)) \
	    /* Need to be careful, 					   \
	     * -16384 is not a valid offset.  */	   	           \
	    low = val >= 0 ? (val & 0x7ff) : -((-val) & 0x7ff);	   \
	  else if (GET_MODE_CLASS (MODE) == MODE_FLOAT			   \
		   && TARGET_HARD_FLOAT)				   \
	    /* Need to be careful, -2048 is not a valid offset.  */	   \
	  low = val >= 0 ? ((val & 0x7ff == 2044)? 2040: (val & 0x7ff))    \
	          : -((-val) & 0x7ff); 					   \
	  else								   \
	    break;							   \
									   \
	  high = ((((val - low) & (unsigned HOST_WIDE_INT) 0xffffffff)	   \
		   ^ (unsigned HOST_WIDE_INT) 0x80000000)		   \
		  - (unsigned HOST_WIDE_INT) 0x80000000);		   \
	  /* Check for overflow or zero */				   \
	  if (low == 0 || high == 0 || (high + low != val))		   \
	    break;							   \
									   \
	  /* Reload the high part into a base reg; leave the low part	   \
	     in the mem.  */						   \
	  X = gen_rtx_PLUS (GET_MODE (X),				   \
			    gen_rtx_PLUS (GET_MODE (X), XEXP (X, 0),	   \
					  GEN_INT (high)),		   \
			    GEN_INT (low));				   \
	  push_reload (XEXP (X, 0), NULL_RTX, &XEXP (X, 0), NULL,	   \
		       MODE_BASE_REG_CLASS (MODE), GET_MODE (X), 	   \
		       VOIDmode, 0, 0, OPNUM, TYPE);			   \
	  goto WIN;							   \
	}								   \
    }									   \
  while (0)


#define LEGITIMIZE_RELOAD_ADDRESS(X, MODE, OPNUM, TYPE, IND_LEVELS, WIN)   \
    UNICORE64_LEGITIMIZE_RELOAD_ADDRESS (X, MODE, OPNUM, TYPE, IND_LEVELS, WIN) 

/* Return the maximum number of consecutive registers
   needed to represent mode MODE in a register of class CLASS.
   UNICORE64 regs are UNITS_PER_WORD bits while FPU regs can hold any FP mode */
#define CLASS_MAX_NREGS(CLASS, MODE)  \
  ((CLASS) == FPU_REGS ? 1 : NUM_REGS (MODE))

/* Moves between FPU_REGS and GENERAL_REGS are two memory insns.  */
#define REGISTER_MOVE_COST(MODE, FROM, TO)		\
  (TARGET_UNICORE64 ?						\
   ((FROM) == FPU_REGS && (TO) != FPU_REGS ? 20 :	\
    (FROM) != FPU_REGS && (TO) == FPU_REGS ? 20 : 2)	\
   :							\
   ((FROM) == HI_REGS || (TO) == HI_REGS) ? 4 : 2)


/********************************level 5************************/

/************** Stack layout; function entry, exit and calling.**************/

/* Define this if pushing a word on the stack
   makes the stack pointer a smaller address.  */
#define STACK_GROWS_DOWNWARD  1

/* Define this if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD 1

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */
#define STARTING_FRAME_OFFSET  0

/* If we generate an insn to push BYTES bytes,
   this says how many the stack pointer really advances by.  */
/* The push insns do not do this rounding implicitly.
   So don't define this. */
/* #define PUSH_ROUNDING(NPUSHED)  ROUND_UP_WORD (NPUSHED) */

/* Define this if the maximum size of all the outgoing args is to be
   accumulated and pushed during the prologue.  The amount can be
   found in the variable crtl->outgoing_args_size.  */
#define ACCUMULATE_OUTGOING_ARGS 1

/* Offset of first parameter from the argument pointer register value.  */
// huangping: 2011-07-22
#define FIRST_PARM_OFFSET(FNDECL)   8

/* Value is the number of byte of arguments automatically
   popped when returning from a subroutine call.
   FUNDECL is the declaration node of the function (as a tree),
   FUNTYPE is the data type of the function (as a tree),
   or for a library call it is an identifier node for the subroutine name.
   SIZE is the number of bytes of arguments passed on the stack.

   On the UNICORE64, the caller does not pop any of its arguments that were passed
   on the stack.  */
#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, SIZE)  0

/* Define how to find the value returned by a library function
   assuming the value has mode MODE.  */
#define LIBCALL_VALUE(MODE)  \
  (TARGET_UNICORE64 && TARGET_HARD_FLOAT && GET_MODE_CLASS (MODE) == MODE_FLOAT \
   ? gen_rtx_REG (MODE, FIRST_UNICORE64_FP_REGNUM) \
   : gen_rtx_REG (MODE, ARG_REGISTER (1)))

/* Define how to find the value returned by a function.
   VALTYPE is the data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 0.  */
#define FUNCTION_VALUE(VALTYPE, FUNC) \
  LIBCALL_VALUE (TYPE_MODE (VALTYPE))

/* 1 if N is a possible register number for a function value.
   On the UNICORE64, only r0 and f0 can return results.  */
#define FUNCTION_VALUE_REGNO_P(REGNO)  \
  ((REGNO) == ARG_REGISTER (1) \
   || (TARGET_UNICORE64 && ((REGNO) == FIRST_UNICORE64_FP_REGNUM) && TARGET_HARD_FLOAT))

/* Amount of memory needed for an untyped call to save all possible return
   registers.  */
/* star add */
#define APPLY_RESULT_SIZE unicore64_apply_result_size()

/* Define DEFAULT_PCC_STRUCT_RETURN to 1 if all structure and union return
   values must be in memory.  On the UNICORE64, they need only do so if larger
   than a word, or if they contain elements offset from zero in the struct. */
#define DEFAULT_PCC_STRUCT_RETURN 0

/* Flags for the call/call_value rtl operations set up by function_arg.  */
#define CALL_NORMAL		0x00000000	/* No special processing.  */
#define CALL_LONG		0x00000001	/* Always call indirect.  */
#define CALL_SHORT		0x00000002	/* Never call indirect.  */

/* These bits describe the different types of function supported
   by the UNICORE64 backend.  They are exclusive.  ie a function cannot be both a
   normal function and an interworked function, for example.  Knowing the
   type of a function is important for determining its prologue and
   epilogue sequences.
   Note value 7 is currently unassigned.  Also note that the interrupt
   function types all have bit 2 set, so that they can be tested for easily.
   Note that 0 is deliberately chosen for UNICORE64_FT_UNKNOWN so that when the
   machine_function structure is initialised (to zero) func_type will
   default to unknown.  This will force the first use of unicore64_current_func_type
   to call unicore64_compute_func_type.  */
#define UNICORE64_FT_UNKNOWN		 0	/* Type has not yet been determined.  */
#define UNICORE64_FT_NORMAL		 1	/* Your normal, straightforward function.  */
#define UNICORE64_FT_INTERWORKED	 2	/* A function that supports interworking.  */
#define UNICORE64_FT_EXCEPTION_HANDLER 	 3	/* A C++ exception handler.  */
#define UNICORE64_FT_ISR		 4	/* An interrupt service routine.  */
#define UNICORE64_FT_FIQ		 5	/* A fast interrupt service routine.  */
#define UNICORE64_FT_EXCEPTION	 	 6	/* An UNICORE64 exception handler (subcase of ISR).  */

#define UNICORE64_FT_TYPE_MASK	((1 << 3) - 1)

/* In addition functions can have several type modifiers,
   outlined by these bit masks:  */
#define UNICORE64_FT_INTERRUPT		(1 << 2)	/* Note overlap with FT_ISR and above.  */
#define UNICORE64_FT_NAKED		(1 << 3)	/* No prologue or epilogue.  */
#define UNICORE64_FT_VOLATILE		(1 << 4)	/* Does not return.  */
#define UNICORE64_FT_NESTED		(1 << 5)	/* Embedded inside another func. */

/* Some macros to test these flags.  */
#define UNICORE64_FUNC_TYPE(t)	(t & UNICORE64_FT_TYPE_MASK)
#define IS_INTERRUPT(t)		(t & UNICORE64_FT_INTERRUPT)
#define IS_VOLATILE(t)     	(t & UNICORE64_FT_VOLATILE)
#define IS_NAKED(t)        	(t & UNICORE64_FT_NAKED)
#define IS_NESTED(t)       	(t & UNICORE64_FT_NESTED)

/* A C structure for machine-specific, per-function data.
   This is added to the cfun structure.  */
typedef struct machine_function GTY (())
{
  /* Additionsl stack adjustment in __builtin_eh_throw.  */
  struct rtx_def *eh_epilogue_sp_ofs;
  /* Records if LR has to be saved for far jumps.  */
  int far_jump_used;
  /* Records if ARG_POINTER was ever live.  */
  int arg_pointer_live;
  /* Records if the save of LR has been eliminated.  */
  int lr_save_eliminated;
  /* Records the type of the current function.  */
  unsigned long func_type;
  /* Record if the function has a variable argument list.  */
  int uses_anonymous_args;
}machine_function;

/* A C type for declaring a variable that is used as the first argument of
   `FUNCTION_ARG' and other related values.  For some target machines, the
   type `int' suffices and can hold the number of bytes of argument so far.  */
typedef struct
{
  /* This is the number of registers of arguments scanned so far.  */
  int nregs;
  /* One of CALL_NORMAL, CALL_LONG or CALL_SHORT . */
  int call_cookie;
} CUMULATIVE_ARGS;

/* Define where to put the arguments to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).

   On the UNICORE64, normally the first 32 bytes are passed in registers r0-r3; all
   other arguments are passed on the stack.  If (NAMED == 0) (which happens
   only in assign_parms, since SETUP_INCOMING_VARARGS is defined), say it is
   passed in the stack (function_prologue will indeed make it pass in the
   stack if necessary).  */
#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  unicore64_function_arg (&(CUM), (MODE), (TYPE), (NAMED))

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.
   On the UNICORE64, the offset starts at 0.  */
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  unicore64_init_cumulative_args (&(CUM), (FNTYPE), (LIBNAME), (FNDECL))

/* Update the data in CUM to advance over an argument
   of mode MODE and data type TYPE.
   (TYPE is null for libcalls where that information may not be available.)  */
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)	\
  (CUM).nregs += NUM_REGS2 (MODE, TYPE)

/* 1 if N is a possible register number for function argument passing.
   On the UNICORE64, r0-r3 are used to pass args.  */
#define FUNCTION_ARG_REGNO_P(REGNO)	(IN_RANGE ((REGNO), 0, 3))

/* If your target environment doesn't prefix user functions with an
   underscore, you may wish to re-define this to prevent any conflicts.
   e.g. AOF may prefix mcount with an underscore.  */
#ifndef UNICORE64_MCOUNT_NAME
#define UNICORE64_MCOUNT_NAME "*mcount"
#endif

/* Call the function profiler with a given profile label.  The Acorn
   compiler puts this BEFORE the prolog but gcc puts it afterwards.
   On the UNICORE64 the full profile code will look like:
	.data
	LP1
		.word	0
	.text
		mov	ip, lr
		call	mcount
		.word	LP1

   profile_function() in final.c outputs the .data section, FUNCTION_PROFILER
   will output the .text section.

   The ``mov ip,lr'' seems like a good idea to stick with cc convention.
   ``prof'' doesn't seem to mind about this!  */
#ifndef UNICORE64_FUNCTION_PROFILER
// huangping: 2011-07-25 mov => dmov
#define UNICORE64_FUNCTION_PROFILER(STREAM, LABELNO)  	\
{							\
  char temp[20];					\
  rtx sym;						\
							\
  asm_fprintf (STREAM, "\tdmov\t%r, %r\n\tcall\t",		\
	   IP_REGNUM, LR_REGNUM);			\
  assemble_name (STREAM, UNICORE64_MCOUNT_NAME);		\
  fputc ('\n', STREAM);					\
  ASM_GENERATE_INTERNAL_LABEL (temp, "LP", LABELNO);	\
  sym = gen_rtx_SYMBOL_REF (Pmode, temp);		\
  assemble_aligned_integer (UNITS_PER_WORD, sym);	\
}
#endif


#define FUNCTION_PROFILER(STREAM, LABELNO)		\
    UNICORE64_FUNCTION_PROFILER (STREAM, LABELNO)

/* EXIT_IGNORE_STACK should be nonzero if, when returning from a function,
   the stack pointer does not matter.  The value is tested only in
   functions that have frame pointers.
   No definition is equivalent to always zero.

   On the UNICORE64, the function epilogue recovers the stack pointer from the
   frame.  */
#define EXIT_IGNORE_STACK 1

#define EPILOGUE_USES(REGNO) (reload_completed && (REGNO) == LR_REGNUM)

/* Determine if the epilogue should be output as RTL.
   You should override this if you define FUNCTION_EXTRA_EPILOGUE.  */
#define USE_RETURN_INSN(ISCOND)				\
  (TARGET_UNICORE64 ? use_return_insn (ISCOND) : 0)

/* Definitions for register eliminations.

   This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.

   We have two registers that can be eliminated on the UNICORE64.  First, the
   arg pointer register can often be eliminated in favor of the stack
   pointer register.  Secondly, the pseudo frame pointer register can always
   be eliminated; it is replaced with either the stack or the real frame
   pointer.  Note we have to use {UNICORE64|UNICORE16}_HARD_FRAME_POINTER_REGNUM
   because the definition of HARD_FRAME_POINTER_REGNUM is not a constant.  */

#define ELIMINABLE_REGS						\
{{ ARG_POINTER_REGNUM,        STACK_POINTER_REGNUM            },\
 { ARG_POINTER_REGNUM,        FRAME_POINTER_REGNUM            },\
 { ARG_POINTER_REGNUM,        UNICORE64_HARD_FRAME_POINTER_REGNUM   },\
 { FRAME_POINTER_REGNUM,      STACK_POINTER_REGNUM            },\
 { FRAME_POINTER_REGNUM,      UNICORE64_HARD_FRAME_POINTER_REGNUM   }}\

/* Given FROM and TO register numbers, say whether this elimination is
   allowed.  Frame pointer elimination is automatically handled.

   All eliminations are permissible.  Note that ARG_POINTER_REGNUM and
   HARD_FRAME_POINTER_REGNUM are in fact the same thing.  If we need a frame
   pointer, we must eliminate FRAME_POINTER_REGNUM into
   HARD_FRAME_POINTER_REGNUM and not into STACK_POINTER_REGNUM or
   ARG_POINTER_REGNUM.  */
#define CAN_ELIMINATE(FROM, TO)						\
  (((TO) == FRAME_POINTER_REGNUM && (FROM) == ARG_POINTER_REGNUM) ? 0 :	\
   ((TO) == STACK_POINTER_REGNUM && frame_pointer_needed) ? 0 :		\
   ((TO) == UNICORE64_HARD_FRAME_POINTER_REGNUM && TARGET_UNICORE16) ? 0 :	\
   1)

/* Define the offset between two registers, one to be eliminated, and the
   other its replacement, at the start of a routine.  */
#define UNICORE64_INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)		\
  do									\
    {									\
      (OFFSET) = unicore64_compute_initial_elimination_offset (FROM, TO);	\
    }									\
  while (0)

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)			\
    UNICORE64_INITIAL_ELIMINATION_OFFSET (FROM, TO, OFFSET)

/* Special case handling of the location of arguments passed on the stack.  */
#define DEBUGGER_ARG_OFFSET(value, addr) value ? value : unicore64_debugger_arg_offset (value, addr)

/* Initialize data used by insn expanders.  This is called from insn_emit,
   once for every function before code is generated.  */
#define INIT_EXPANDERS  unicore64_init_expanders ()


/************************level 6******************************/
/**********************trampoline for nested functions*********/
//FIXME: no unicore32 suport here, TARGET_XXX is useless
#define TRAMPOLINE_REG	15
/* Output assembler code for a block containing the constant parts
   of a trampoline, leaving space for the variable parts.
RK: 20140517
		ldd 	r15, [pc+], #24
		ldd		ip, [pc+], #12
		jump	r15
		nop	
		.word	static chain value
		.word	function's address
*/
#define TRAMPOLINE_TEMPLATE(FILE)				\
{								\
  asm_fprintf (FILE, "\tldd\t%r, [%r+], #24\n",			\
	       TRAMPOLINE_REG, PC_REGNUM);			\
  asm_fprintf (FILE, "\tldd\t%r, [%r+], #12\n",			\
	       STATIC_CHAIN_REGNUM, PC_REGNUM);			\
  asm_fprintf (FILE, "\tjump\t%r\n", TRAMPOLINE_REG);		\
  assemble_aligned_integer (UNITS_PER_WORD, const0_rtx);	\
  assemble_aligned_integer (UNITS_PER_WORD, const0_rtx);	\
}

/* Length in units of the trampoline for entering a nested function.  */
#define TRAMPOLINE_SIZE  32

/* Alignment required for a trampoline in bits.  */
#define TRAMPOLINE_ALIGNMENT  64

/* Emit RTL insns to initialize the variable parts of a trampoline.
   FNADDR is an RTX for the address of the function's pure code.
   CXT is an RTX for the static chain value for the function.  */
//#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)  					\
{											\
  emit_move_insn (gen_rtx_MEM (DImode, plus_constant ((TRAMP), 12)),	\
		  (CXT));						\
  emit_move_insn (gen_rtx_MEM (DImode, plus_constant ((TRAMP), 16)),	\
		  (FNADDR));						\
  emit_library_call (gen_rtx_SYMBOL_REF (Pmode, "__clear_cache"),	\
		     0, VOIDmode, 2, TRAMP, Pmode,			\
		     plus_constant (TRAMP, TRAMPOLINE_SIZE), Pmode);	\
}
#define INITIALIZE_TRAMPOLINE(TRAMP, FNADDR, CXT)  					\
{											\
  emit_move_insn (gen_rtx_MEM (DImode, plus_constant ((TRAMP), 16)),	\
		  (CXT));						\
  emit_move_insn (gen_rtx_MEM (DImode, plus_constant ((TRAMP), 24)),	\
		  (FNADDR));						\
}


/*****************************level 7****************************/
/*********** Addressing modes, and classification of registers for them.******/
#define HAVE_POST_INCREMENT  1
#define HAVE_PRE_INCREMENT   TARGET_UNICORE64
#define HAVE_POST_DECREMENT  TARGET_UNICORE64
#define HAVE_PRE_DECREMENT   TARGET_UNICORE64

/* Macros to check register numbers against specific register classes.  */

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c. */
#define TEST_REGNO(R, TEST, VALUE) \
  ((R TEST VALUE) || ((unsigned) reg_renumber[R] TEST VALUE))

/*   On the UNICORE64, don't allow the pc to be used.  */
#define UNICORE64_REGNO_OK_FOR_BASE_P(REGNO)			\
  (TEST_REGNO (REGNO, <, PC_REGNUM)			\
   || TEST_REGNO (REGNO, ==, FRAME_POINTER_REGNUM)	\
   || TEST_REGNO (REGNO, ==, ARG_POINTER_REGNUM))

#define REGNO_MODE_OK_FOR_BASE_P(REGNO, MODE)		\
   UNICORE64_REGNO_OK_FOR_BASE_P (REGNO)

/* For UNICORE64 code, we don't care about the mode, but for Unicore16, the index
   must be suitable for use in a QImode load.  */
#define REGNO_OK_FOR_INDEX_P(REGNO)	\
  REGNO_MODE_OK_FOR_BASE_P (REGNO, QImode)

/* Maximum number of registers that can appear in a valid memory address.
   Shifts in addresses can't be by a register. */
#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */
/* XXX We can address any constant, eventually...  */

#ifdef AOF_ASSEMBLER

#define CONSTANT_ADDRESS_P(X)		\
  (GET_CODE (X) == SYMBOL_REF && CONSTANT_POOL_ADDRESS_P (X))

#else

#define CONSTANT_ADDRESS_P(X)  			\
  (GET_CODE (X) == SYMBOL_REF 			\
   && (CONSTANT_POOL_ADDRESS_P (X)		\
       || (TARGET_UNICORE64 && optimize > 0 && SYMBOL_REF_FLAG (X))))

#endif /* AOF_ASSEMBLER */

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.

   On the UNICORE64, allow any integer (invalid ones are removed later by insn
   patterns), nice doubles and symbol_refs which refer to the function's
   constant pool XXX.
   
   When generating pic allow anything.  */
#define UNICORE64_LEGITIMATE_CONSTANT_P(X)	\
   (flag_pic || ! label_mentioned_p (X))

#define LEGITIMATE_CONSTANT_P(X)		\
  (!unicore64_cannot_force_const_mem (X)	\
   && UNICORE64_LEGITIMATE_CONSTANT_P (X))

/* Special characters prefixed to function names
   in order to encode attribute like information.
   Note, '@' and '*' have already been taken.  */
#define SHORT_CALL_FLAG_CHAR	'^'
#define LONG_CALL_FLAG_CHAR	'#'

#define ENCODED_SHORT_CALL_ATTR_P(SYMBOL_NAME)	\
  (*(SYMBOL_NAME) == SHORT_CALL_FLAG_CHAR)

#define ENCODED_LONG_CALL_ATTR_P(SYMBOL_NAME)	\
  (*(SYMBOL_NAME) == LONG_CALL_FLAG_CHAR)

#ifndef SUBTARGET_NAME_ENCODING_LENGTHS
#define SUBTARGET_NAME_ENCODING_LENGTHS
#endif

/* This is a C fragement for the inside of a switch statement.
   Each case label should return the number of characters to
   be stripped from the start of a function's name, if that
   name starts with the indicated character.  */
#define UNICORE64_NAME_ENCODING_LENGTHS		\
  case SHORT_CALL_FLAG_CHAR: return 1;		\
  case LONG_CALL_FLAG_CHAR:  return 1;		\
  case '*':  return 1;				\
  SUBTARGET_NAME_ENCODING_LENGTHS

/* This is how to output a reference to a user-level label named NAME.
   `assemble_name' uses this.  */
#undef  ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(FILE, NAME)		\
  asm_fprintf (FILE, "%U%s", unicore64_strip_name_encoding (NAME))

#define UNICORE64_DECLARE_FUNCTION_SIZE(STREAM, NAME, DECL)	\
  unicore64_encode_call_attribute (DECL, SHORT_CALL_FLAG_CHAR)

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx
   and check its validity for a certain class.
   We have two alternate definitions for each of them.
   The usual definition accepts all pseudo regs; the other rejects
   them unless they have been allocated suitable hard regs.
   The symbol REG_OK_STRICT causes the latter definition to be used.  */
#ifndef REG_OK_STRICT

#define UNICORE64_REG_OK_FOR_BASE_P(X)		\
  (REGNO (X) <= LAST_UNICORE64_REGNUM			\
   || REGNO (X) >= FIRST_PSEUDO_REGISTER	\
   || REGNO (X) == FRAME_POINTER_REGNUM		\
   || REGNO (X) == ARG_POINTER_REGNUM)

#else /* REG_OK_STRICT */

#define UNICORE64_REG_OK_FOR_BASE_P(X) 		\
  UNICORE64_REGNO_OK_FOR_BASE_P (REGNO (X))

#endif /* REG_OK_STRICT */

/* Now define some helpers in terms of the above.  */

#define REG_MODE_OK_FOR_BASE_P(X, MODE)		\
    UNICORE64_REG_OK_FOR_BASE_P (X)

#define UNICORE64_REG_OK_FOR_INDEX_P(X) UNICORE64_REG_OK_FOR_BASE_P (X)

/* Nonzero if X is a hard reg that can be used as an index
   or if it is a pseudo reg.  On the Unicore16, the stack pointer
   is not suitable.  */
#define REG_OK_FOR_INDEX_P(X)			\
    UNICORE64_REG_OK_FOR_INDEX_P (X)


/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression
   that is a valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS. */

/* --------------------------------unicore64 version----------------------------- */
#define UNICORE64_BASE_REGISTER_RTX_P(X)  \
  (GET_CODE (X) == REG && UNICORE64_REG_OK_FOR_BASE_P (X))

#define UNICORE64_INDEX_REGISTER_RTX_P(X)  \
  (GET_CODE (X) == REG && UNICORE64_REG_OK_FOR_INDEX_P (X))

/* A C statement (sans semicolon) to jump to LABEL for legitimate index RTXs
   used by the macro GO_IF_LEGITIMATE_ADDRESS.  Floating point indices can
   only be small constants. */
#define UNICORE64_GO_IF_LEGITIMATE_INDEX(MODE, BASE_REGNO, INDEX, LABEL)	\
  do									\
    {									\
      HOST_WIDE_INT range;						\
      enum rtx_code code = GET_CODE (INDEX);				\
									\
      if (TARGET_HARD_FLOAT && GET_MODE_CLASS (MODE) == MODE_FLOAT)	\
	{								\
	  if (code == CONST_INT && INTVAL (INDEX) < 2048		\
	      && INTVAL (INDEX) > -2048					\
	      && (INTVAL (INDEX) & 3) == 0)				\
	    goto LABEL;							\
	}								\
      else								\
	{								\
	  if (UNICORE64_INDEX_REGISTER_RTX_P (INDEX)				\
	      && GET_MODE_SIZE (MODE) <= 8)				\
	    goto LABEL;							\
	  range = 2048;							\
	  if (code == CONST_INT && INTVAL (INDEX) < range		\
	      && INTVAL (INDEX) > -range)				\
	    goto LABEL;							\
	}								\
    }									\
  while (0)

/* Jump to LABEL if X is a valid address RTX.  This must take
   REG_OK_STRICT into account when deciding about valid registers.

   Allow REG, REG+REG, REG+INDEX, INDEX+REG, REG-INDEX, and non
   floating SYMBOL_REF to the constant pool.  Allow REG-only and
   AUTINC-REG if handling TImode or HImode.  Other symbol refs must be
   forced though a static cell to ensure addressability.  */
#define UNICORE64_GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)			\
{									\
  if (UNICORE64_BASE_REGISTER_RTX_P (X))					\
    goto LABEL;								\
  else if ((GET_CODE (X) == POST_INC || GET_CODE (X) == PRE_DEC)	\
	   && GET_CODE (XEXP (X, 0)) == REG				\
	   && UNICORE64_REG_OK_FOR_BASE_P (XEXP (X, 0)))		\
    goto LABEL;								\
  else if (reload_completed						\
	   && (GET_CODE (X) == LABEL_REF				\
	       || (GET_CODE (X) == CONST				\
		   && GET_CODE (XEXP ((X), 0)) == PLUS			\
		   && GET_CODE (XEXP (XEXP ((X), 0), 0)) == LABEL_REF	\
		   && GET_CODE (XEXP (XEXP ((X), 0), 1)) == CONST_INT)))\
    goto LABEL;								\
  else if ((MODE) == TImode)						\
    ;									\
  else if (GET_CODE (X) == PLUS)					\
    {									\
      rtx xop0 = XEXP (X, 0);						\
      rtx xop1 = XEXP (X, 1);						\
									\
      if (UNICORE64_BASE_REGISTER_RTX_P (xop0))				\
	UNICORE64_GO_IF_LEGITIMATE_INDEX (MODE, REGNO (xop0), xop1, LABEL);	\
      else if (UNICORE64_BASE_REGISTER_RTX_P (xop1))				\
	UNICORE64_GO_IF_LEGITIMATE_INDEX (MODE, REGNO (xop1), xop0, LABEL);	\
    }									\
  else if (GET_MODE_CLASS (MODE) != MODE_FLOAT				\
	   && GET_CODE (X) == SYMBOL_REF				\
	   && CONSTANT_POOL_ADDRESS_P (X)				\
	   && ! (flag_pic						\
		 && symbol_mentioned_p (get_pool_constant (X))		\
                 && ! pcrel_constant_p (get_pool_constant (X))))	\
    goto LABEL;								\
  else if ((GET_CODE (X) == PRE_INC || GET_CODE (X) == POST_DEC)	\
	   && (GET_MODE_SIZE (MODE) <= 4)				\
	   && GET_CODE (XEXP (X, 0)) == REG				\
	   && UNICORE64_REG_OK_FOR_BASE_P (XEXP (X, 0)))			\
    goto LABEL;								\
}

/* ------------------------------------------------------------------- */
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, WIN)				\
  if (TARGET_UNICORE64)							\
    UNICORE64_GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN)  			\
/* ------------------------------------------------------------------- */

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.

   OLDX is the address as it was before break_out_memory_refs was called.
   In some cases it is useful to look at this to decide what needs to be done.

   MODE and WIN are passed so that this macro can use
   GO_IF_LEGITIMATE_ADDRESS.

   It is always safe for this macro to do nothing.  It exists to recognize
   opportunities to optimize the output.

   On the UNICORE64, try to convert [REG, #BIGCONST]
   into ADD BASE, REG, #UPPERCONST and [BASE, #VALIDCONST],
   where VALIDCONST == 0 in case of TImode.  */
#define UNICORE64_LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)		 \
{									 \
  if (unicore64_tls_symbol_p (x))					 \
    return legitimize_tls_address (x, NULL_RTX);			 \
  if (GET_CODE (X) == PLUS)						 \
    {									 \
      rtx xop0 = XEXP (X, 0);						 \
      rtx xop1 = XEXP (X, 1);						 \
									 \
      if (CONSTANT_P (xop0) && ! symbol_mentioned_p (xop0))		 \
	xop0 = force_reg (DImode, xop0);				 \
      if (CONSTANT_P (xop1) && ! symbol_mentioned_p (xop1))		 \
	xop1 = force_reg (DImode, xop1);				 \
      if (UNICORE64_BASE_REGISTER_RTX_P (xop0)				 \
	  && GET_CODE (xop1) == CONST_INT)				 \
	{								 \
	  HOST_WIDE_INT n, low_n;					 \
	  rtx base_reg, val;						 \
	  n = INTVAL (xop1);						 \
									 \
	    {								 \
	      /* gaoyi:*/						 \
	      if ((MODE) == HImode || (MODE) == QImode )		 \
	      low_n = n & 0x3ff;				 \
	      else							 \
	      low_n = ((MODE) == TImode ? 0				 \
		       : n >= 0 ? (n & 0x3fff) : -((-n) & 0x3fff));	 \
	      n -= low_n;						 \
	    }								 \
	  base_reg = gen_reg_rtx (DImode);				 \
	  val = force_operand (gen_rtx_PLUS (DImode, xop0,		 \
					     GEN_INT (n)), NULL_RTX);	 \
	  emit_move_insn (base_reg, val);				 \
	  (X) = (low_n == 0 ? base_reg					 \
		 : gen_rtx_PLUS (DImode, base_reg, GEN_INT (low_n)));	 \
	}								 \
      else if (xop0 != XEXP (X, 0) || xop1 != XEXP (x, 1))		 \
	(X) = gen_rtx_PLUS (DImode, xop0, xop1);			 \
    }									 \
  else if (GET_CODE (X) == MINUS)					 \
    {									 \
      rtx xop0 = XEXP (X, 0);						 \
      rtx xop1 = XEXP (X, 1);						 \
									 \
      if (CONSTANT_P (xop0))						 \
	xop0 = force_reg (DImode, xop0);				 \
      if (CONSTANT_P (xop1) && ! symbol_mentioned_p (xop1))		 \
	xop1 = force_reg (DImode, xop1);				 \
      if (xop0 != XEXP (X, 0) || xop1 != XEXP (X, 1))			 \
	(X) = gen_rtx_MINUS (DImode, xop0, xop1);			 \
    }									 \
  if (flag_pic)								 \
    (X) = legitimize_pic_address (OLDX, MODE, NULL_RTX);		 \
  if (memory_address_p (MODE, X))					 \
    goto WIN;								 \
}


#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)	\
  if (TARGET_UNICORE64)				\
    UNICORE64_LEGITIMIZE_ADDRESS (X, OLDX, MODE, WIN)	

/* Go to LABEL if ADDR (a legitimate address expression)
   has an effect that depends on the machine mode it is used for.  */
#define UNICORE64_GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)  			\
{									\
  if (   GET_CODE (ADDR) == PRE_DEC || GET_CODE (ADDR) == POST_DEC	\
      || GET_CODE (ADDR) == PRE_INC || GET_CODE (ADDR) == POST_INC)	\
    goto LABEL;								\
}

/* Nothing helpful to do for the Unicore16 */
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)	\
  if (TARGET_UNICORE64)					\
    UNICORE64_GO_IF_MODE_DEPENDENT_ADDRESS (ADDR, LABEL)


/**********************************level 8***************************/
/**********************miscellaneous parameters macros**************/

/* Specify the machine mode that this machine uses
   for the index in the tablejump instruction.  */
#define CASE_VECTOR_MODE Pmode

/* Define as C expression which evaluates to nonzero if the tablejump
   instruction expects the table to contain offsets from the address of the
   table.
   Do not define this if the table should contain absolute addresses. */
/* #define CASE_VECTOR_PC_RELATIVE 1 */

/* signed 'char' is most compatible, but RISC OS wants it unsigned.
   unsigned is probably best, but may break some code.  */
#ifndef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR  1
#endif

/* Max number of bytes we can move from memory to memory
   in one reasonably fast instruction.  */
#define MOVE_MAX 8

#undef  MOVE_RATIO
// star , 20091020
#define MOVE_RATIO(speed) 2

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */
// huangping: FIXME
//#define LOAD_EXTEND_OP(MODE)	ZERO_EXTEND

/* Nonzero if access to memory by bytes is slow and undesirable.  */
#define SLOW_BYTE_ACCESS 0

#define SLOW_UNALIGNED_ACCESS(MODE, ALIGN) 1

/* Immediate shift counts are truncated by the output routines (or was it
   the assembler?).  Shift counts in a register are truncated by UNICORE64.  Note
   that the native compiler puts too large (> 32) immediate shift counts
   into a register and shifts by the register, letting the UNICORE64 decide what
   to do instead of doing that itself.  */
/* This is all wrong.  Defining SHIFT_COUNT_TRUNCATED tells combine that
   code like (X << (Y % 32)) for register X, Y is equivalent to (X << Y).
   On the unicore64, Y in a register is used modulo 256 for the shift. Only for
   rotates is modulo 32 used. */
/* #define SHIFT_COUNT_TRUNCATED 1 */

/* All integers have the same format so truncation is easy.  */
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC)  1

/* Calling from registers is a massive pain.  */
#define NO_FUNCTION_CSE 1

/* The machine modes of pointers and functions */
#define Pmode  DImode
#define FUNCTION_MODE  Pmode

// huangping: FIXME
#define POINTERS_EXTEND_UNSIGNED 1

#define UNICORE64_FRAME_RTX(X)					\
  (   (X) == frame_pointer_rtx || (X) == stack_pointer_rtx	\
   || (X) == arg_pointer_rtx)

/* Moves to and from memory are quite expensive */
#define MEMORY_MOVE_COST(M, CLASS, IN)			\
  (TARGET_UNICORE64 ? 10 :					\
   ((GET_MODE_SIZE (M) < 4 ? 8 : 2 * GET_MODE_SIZE (M))	\
    * (CLASS == LO_REGS ? 1 : 2)))

/* Try to generate sequences that don't involve branches, we can then use
   conditional instructions */
///* UNICORE64 has no conditional instructions , so set BRAHCN_COST as default */
//#define BRANCH_COST (optimize > 1 ? 1 : 0)
// star , 20091020
#define BRANCH_COST(speed_p, predictable_p) (optimize > 0 ? 2 : 0)

/* Position Independent Code.  */
extern unsigned unicore64_pic_register;

/* Used when parsing command line option -mpic-register=.  */
extern const char *unicore64_pic_register_string;

/* The register number of the register used to address a table of static
   data addresses in memory.  */
#define PIC_OFFSET_TABLE_REGNUM unicore64_pic_register

/* We can't directly access anything that contains a symbol,
   nor can we indirect via the constant pool.  */
/*
#define LEGITIMATE_PIC_OPERAND_P(X)					\
	(   ! symbol_mentioned_p (X)					\
	 && ! label_mentioned_p (X)					\
	 && (! CONSTANT_POOL_ADDRESS_P (X)				\
	     || (   ! symbol_mentioned_p (get_pool_constant (X))  	\
	         && ! label_mentioned_p (get_pool_constant (X)))))
*/
#define LEGITIMATE_PIC_OPERAND_P(X)					\
	(!(symbol_mentioned_p (X)					\
	   || label_mentioned_p (X)					\
	   || (GET_CODE (X) == SYMBOL_REF				\
	       && CONSTANT_POOL_ADDRESS_P (X)				\
	       && (symbol_mentioned_p (get_pool_constant (X))		\
		   || label_mentioned_p (get_pool_constant (X)))))	\
	 || tls_mentioned_p (X))


/* We need to know when we are making a constant pool; this determines
   whether data needs to be in the GOT or can be referenced via a GOT
   offset.  */
extern int making_const_table;
/* /\* Handle pragmas for compatibility with Intel's compilers.  *\/ */
/* #define REGISTER_TARGET_PRAGMAS(PFILE) do { \ */
/*   cpp_register_pragma (PFILE, 0, "long_calls", unicore64_pr_long_calls); \ */
/*   cpp_register_pragma (PFILE, 0, "no_long_calls", unicore64_pr_no_long_calls); \ */
/*   cpp_register_pragma (PFILE, 0, "long_calls_off", unicore64_pr_long_calls_off); \ */
/* } while (0) */

#define SELECT_CC_MODE(OP, X, Y)  unicore64_select_cc_mode (OP, X, Y)

#define REVERSIBLE_CC_MODE(MODE) ((MODE) != CCFPEmode)

#define CANONICALIZE_COMPARISON(CODE, OP0, OP1)				\
  do									\
    {									\
      if (GET_CODE (OP1) == CONST_INT					\
          && ! (const_ok_for_unicore64 (INTVAL (OP1))			\
	        || (const_ok_for_unicore64 (- INTVAL (OP1)))))		\
        {								\
          rtx const_op = OP1;						\
          CODE = unicore64_canonicalize_comparison ((CODE), &const_op);	\
          OP1 = const_op;						\
        }								\
    }									\
  while (0)

#define STORE_FLAG_VALUE 1



/**************************level 9**********************************/
/*****************output assembler language*************************/
#undef  ASM_APP_OFF
#define ASM_APP_OFF (TARGET_UNICORE16 ? "\t.code\t16\n" : "")

/* Output a push or a pop instruction (only used when profiling).  */
#define ASM_OUTPUT_REG_PUSH(STREAM, REGNO)		\
  do							\
    {							\
      if (TARGET_UNICORE64)				\
	asm_fprintf (STREAM,"\tstd.w\t%r, [%r-], #8\n", 	\
		     REGNO, STACK_POINTER_REGNUM);	\
    } while (0)


#define ASM_OUTPUT_REG_POP(STREAM, REGNO)		\
  do							\
    {							\
      if (TARGET_UNICORE64)				\
	asm_fprintf (STREAM, "\tldd.w\t%r, [%r]+, #8\n",	\
		     REGNO, STACK_POINTER_REGNUM);	\
    } while (0)

/* This is how to output a label which precedes a jumptable.  Since
   Unicore16 instructions are 2 bytes, we may need explicit alignment here.  */
#undef  ASM_OUTPUT_CASE_LABEL
#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, JUMPTABLE)	\
  do								\
    {								\
      if (TARGET_UNICORE16)					\
        ASM_OUTPUT_ALIGN (FILE, 2);				\
      (*targetm.asm_out.internal_label) (FILE, PREFIX, NUM);	\
    }								\
  while (0)

#define UNICORE64_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL) 	\
  do							\
    {							\
      if (TARGET_POKE_FUNCTION_NAME)			\
        unicore64_poke_function_name (STREAM, (char *) NAME);	\
    }							\
  while (0)

/* For aliases of functions we use .unicore16_set instead.  */
#define ASM_OUTPUT_DEF_FROM_DECLS(FILE, DECL1, DECL2)		\
  do						   		\
    {								\
      const char *const LABEL1 = XSTR (XEXP (DECL_RTL (decl), 0), 0); \
      const char *const LABEL2 = IDENTIFIER_POINTER (DECL2);	\
								\
      if (TARGET_UNICORE16 && TREE_CODE (DECL1) == FUNCTION_DECL)	\
	{							\
	  fprintf (FILE, "\t.unicore16_set ");			\
	  assemble_name (FILE, LABEL1);			   	\
	  fprintf (FILE, ",");			   		\
	  assemble_name (FILE, LABEL2);		   		\
	  fprintf (FILE, "\n");					\
	}							\
      else							\
	ASM_OUTPUT_DEF (FILE, LABEL1, LABEL2);			\
    }								\
  while (0)

#ifdef HAVE_GAS_MAX_SKIP_P2ALIGN
/* To support -falign-* switches we need to use .p2align so
   that alignment directives in code sections will be padded
   with no-op instructions, rather than zeroes.  */
#define ASM_OUTPUT_MAX_SKIP_ALIGN(FILE,LOG,MAX_SKIP)		\
  if ((LOG) != 0)						\
    {								\
      if ((MAX_SKIP) == 0)					\
        fprintf ((FILE), "\t.p2align %d\n", (LOG));		\
      else							\
        fprintf ((FILE), "\t.p2align %d,,%d\n",			\
                 (LOG), (MAX_SKIP));				\
    }
#endif

/* Only perform branch elimination (by making instructions conditional) if
   we're optimising.  Otherwise it's of no use anyway.  */
#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS)	\
  if (TARGET_UNICORE64 && optimize)				\
    unicore64_final_prescan_insn (INSN);			

#define PRINT_OPERAND_PUNCT_VALID_P(CODE)	\
  (CODE == '@' || CODE == '|'			\
   || (TARGET_UNICORE64   && (CODE == '?'))		\
   || (TARGET_UNICORE16 && (CODE == '_')))

/* Output an operand of an instruction.  */
#define PRINT_OPERAND(STREAM, X, CODE)  \
  unicore64_print_operand (STREAM, X, CODE)

/* star : FIXME64 */
// huangping: FIXME 2012-02-10
#define UNICORE64_SIGN_EXTEND(x)  ((HOST_WIDE_INT)			\
  (HOST_BITS_PER_WIDE_INT == 64 ? (unsigned HOST_WIDE_INT) (x)		\
   : ((((unsigned HOST_WIDE_INT)(x)) & (unsigned HOST_WIDE_INT) 0xffffffff) |\
      ((((unsigned HOST_WIDE_INT)(x)) & (unsigned HOST_WIDE_INT) 0x80000000) \
       ? (~ (unsigned HOST_WIDE_INT) 0xffffffff)			\
       : 0))))

/* Output the address of an operand.  */
#define UNICORE64_PRINT_OPERAND_ADDRESS(STREAM, X)  		\
{								\
    int is_minus = GET_CODE (X) == MINUS;			\
								\
    if (GET_CODE (X) == REG)					\
      asm_fprintf (STREAM, "[%r+], #0", REGNO (X));		\
    else if (GET_CODE (X) == PLUS || is_minus)			\
      {								\
	rtx base = XEXP (X, 0);					\
	rtx index = XEXP (X, 1);				\
	HOST_WIDE_INT offset = 0;				\
	if (GET_CODE (base) != REG)				\
	  {							\
	    /* Ensure that BASE is a register */ 		\
            /* (one of them must be). */			\
	    rtx temp = base;					\
	    base = index;					\
	    index = temp;					\
	  }							\
	switch (GET_CODE (index))				\
	  {							\
	  case CONST_INT:					\
	    offset = INTVAL (index);				\
	    if (is_minus)					\
	      offset = -offset;					\
	    asm_fprintf (STREAM, "[%r+], #%d", 			\
		         REGNO (base), offset);			\
	    break;						\
								\
	  case REG:						\
	    asm_fprintf (STREAM, "[%r%s], %r", 			\
		     REGNO (base), is_minus ? "-" : "+",	\
		     REGNO (index));				\
	    break;						\
								\
	  case MULT:						\
	  case ASHIFTRT:					\
	  case LSHIFTRT:					\
	  case ASHIFT:						\
	  case ROTATERT:					\
	  {							\
	    asm_fprintf (STREAM, "[%r%s], %r", 			\
		         REGNO (base), is_minus ? "-" : "+", 	\
                         REGNO (XEXP (index, 0)));		\
	    unicore64_print_operand (STREAM, index, 'S');	\
	    break;						\
	  }							\
	    							\
	  default:						\
	    abort();						\
	}							\
    }							        \
  else if (   GET_CODE (X) == PRE_INC || GET_CODE (X) == POST_INC\
	   || GET_CODE (X) == PRE_DEC || GET_CODE (X) == POST_DEC)\
    {								\
      extern int output_memory_reference_mode;			\
      								\
      if (GET_CODE (XEXP (X, 0)) != REG)			\
	abort ();						\
								\
      if (GET_CODE (X) == PRE_DEC || GET_CODE (X) == PRE_INC)	\
	asm_fprintf (STREAM, "[%r%s], #%d",/*FIXME, star, write_back */ 	\
		     REGNO (XEXP (X, 0)),			\
		     GET_CODE (X) == PRE_DEC ? "-" : "+",	\
		     GET_MODE_SIZE (output_memory_reference_mode));\
      else{							\
	if(is_lwf_swf)						\
	     asm_fprintf (STREAM, "[%r]%s, #%d", /*FIXME, star, write_back */          \
		     REGNO (XEXP (X, 0)),                       \
		     GET_CODE (X) == POST_DEC ? "-" : "+",       \
		  GET_MODE_SIZE (output_memory_reference_mode));\
	else asm_fprintf (STREAM, "[%r]%s, #%d", 		\
		     REGNO (XEXP (X, 0)),			\
		     GET_CODE (X) == POST_DEC ? "-" : "+",	\
		     GET_MODE_SIZE (output_memory_reference_mode));}\
    }								\
  else output_addr_const (STREAM, X);				\
}


#define PRINT_OPERAND_ADDRESS(STREAM, X)	\
  if (TARGET_UNICORE64)				\
    UNICORE64_PRINT_OPERAND_ADDRESS (STREAM, X)	

#define OUTPUT_ADDR_CONST_EXTRA(file, x, fail)		\
  if (unicore64_output_addr_const_extra (file, x) == FALSE)	\
    goto fail

/* A C expression whose value is RTL representing the value of the return
   address for the frame COUNT steps up from the current frame.  */

#define RETURN_ADDR_RTX(COUNT, FRAME) \
  unicore64_return_addr (COUNT, FRAME)

/* Mask of the bits in the PC that contain the real return address 
   when running in 26-bit mode.  */
#define RETURN_ADDR_MASK26 (0x03fffffc)

/* Pick up the return address upon entry to a procedure. Used for
   dwarf2 unwind information.  This also enables the table driven
   mechanism.  */
#define INCOMING_RETURN_ADDR_RTX	gen_rtx_REG (Pmode, LR_REGNUM)
#define DWARF_FRAME_RETURN_COLUMN	DWARF_FRAME_REGNUM (LR_REGNUM)

/* Used to mask out junk bits from the return address, such as
   processor state, interrupt status, condition codes and the like.  */
#define MASK_RETURN_ADDR \
  /* If we are generating code for an UNICORE642/UNICORE643 machine or for an UNICORE646	\
     in 26 bit mode, the condition codes must be masked out of the	\
     return address.  This does not apply to UNICORE646 and later processors	\
     when running in 32 bit mode.  */					\
  ((GEN_INT (~(unsigned long)0)))
//fixed by RK 20131111

enum unicore64_builtins
{
  UNICORE64_BUILTIN_CLZ,
  UNICORE64_BUILTIN_MAX,
  UNICORE64_BUILTIN_THREAD_POINTER
};
#endif /* ! GCC_UNICORE64_H */
