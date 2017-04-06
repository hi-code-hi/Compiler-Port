/* Output routines for GCC for UNICORE64.
   Copyright (C) 1991, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000, 
   2001, 2002,2003,2004,2005,2006,2007
   Free Software Foundation, Inc.
   Contributed by Gaoyi and Star (tanmingxing@mprc.pku.edu.cn)

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "obstack.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "toplev.h"
#include "recog.h"
#include "ggc.h"
#include "except.h"
#include "c-pragma.h"
#include "integrate.h"
#include "tm_p.h"
#include "target.h"
#include "target-def.h"
#include "debug.h"
#include "langhooks.h"
#include "df.h"

/* Forward definitions of types.  */
typedef struct minipool_node Mnode;
typedef struct minipool_fixup Mfix;

/* In order to improve the layout of the prototypes below
   some short type abbreviations are defined here.  */
#define Hint     HOST_WIDE_INT
#define Mmode    enum machine_mode
#define Ulong    unsigned long
#define Ccstar   const char *

const struct attribute_spec unicore64_attribute_table[];

/* Forward function declarations.  */
static void unicore64_add_gc_roots (void);
static int unicore64_gen_constant (enum rtx_code, Mmode, Hint, rtx, rtx, int, int);
static Ulong bit_count (signed int);
static int const_ok_for_op (Hint, enum rtx_code);
static int eliminate_lr2ip (rtx *);
static rtx emit_multi_reg_push (int);
static rtx emit_sfm (int, int);
static bool unicore64_assemble_integer (rtx, unsigned int, int);
static Ccstar fp_const_from_val (REAL_VALUE_TYPE *);
static unicore64_cc get_unicore64_condition_code (rtx);
static void init_fpa_table (void);
static rtx is_jump_table (rtx);
static Ccstar output_multi_immediate (rtx *, Ccstar, int, Hint);
static void print_multi_reg (FILE *, Ccstar, Ccstar, int, int);
static Mmode select_dominance_cc_mode (rtx, rtx, Hint);
static struct machine_function *unicore64_init_machine_status (void);
static int number_of_first_bit_set (int);
static void replace_symbols_in_block (tree, rtx, rtx);
static rtx is_jump_table (rtx);
static Hint get_jump_table_size (rtx);
static Mnode *move_minipool_fix_forward_ref (Mnode *, Mnode *, Hint);
static Mnode *add_minipool_forward_ref (Mfix *);
static Mnode *move_minipool_fix_backward_ref (Mnode *, Mnode *, Hint);
static Mnode *add_minipool_backward_ref (Mfix *);
static void assign_minipool_offsets (Mfix *);
static void unicore64_print_value (FILE *, rtx);
static void dump_minipool (rtx);
static int unicore64_barrier_cost (rtx);
static Mfix *create_fix_barrier (Mfix *, Hint);
static void push_minipool_barrier (rtx, Hint);
static void push_minipool_fix (rtx, Hint, rtx *, Mmode, rtx);
static void unicore64_reorg (void);
static void note_invalid_constants (rtx, Hint);
static int current_file_function_operand (rtx);
static Ulong unicore64_compute_save_reg0_reg28_mask (void);
static Ulong unicore64_compute_save_reg_mask (void);
static Ulong unicore64_isr_value (tree);
static Ulong unicore64_compute_func_type (void);
static tree unicore64_handle_fndecl_attribute (tree *, tree, tree, int,
					       bool *);
static tree unicore64_handle_isr_attribute (tree *, tree, tree, int, bool *);
static void unicore64_output_function_epilogue (FILE *, Hint);
static void unicore64_output_function_prologue (FILE *, Hint);
static int unicore64_comp_type_attributes (tree, tree);
static void unicore64_set_default_type_attributes (tree);
static int unicore64_adjust_cost (rtx, rtx, rtx, int);
#ifdef OBJECT_FORMAT_ELF
static void unicore64_elf_asm_named_section (const char *, unsigned int);
#endif
static void unicore64_output_mi_thunk (FILE *, tree, HOST_WIDE_INT,
				       HOST_WIDE_INT, tree);
/* star add for static */
static bool unicore64_function_ok_for_sibcall (tree, tree);

// huangping: FIXME 2011-09-01
static bool unicore64_split_complex_arg (const_tree type);

/* star delete it*/
/* static rtx unicore64_struct_value_rtx (tree, int);*/
static int unicore64_arg_partial_bytes (CUMULATIVE_ARGS *, enum machine_mode,
					tree, bool);
static void unicore64_setup_incoming_varargs (CUMULATIVE_ARGS *,
					      enum machine_mode, tree, int *,
					      int);
static void unicore64_encode_section_info (tree, rtx, int);
static int unicore64_rtx_costs_1 (rtx, enum rtx_code, enum rtx_code);
static bool unicore64_rtx_costs (rtx, int, int, int *);
static int unicore64_address_cost (rtx);
/* star add it*/
static enum machine_mode unicore64_apply_result_mode (unsigned regno);
static bool unicore64_handle_option (size_t, const char *, int);
static rtx unicore64_load_tp (rtx target);
//static bool unicore64_tls_symbol_p (rtx x);
static int unicore64_issue_rate (void);

/* star add for tls, 20091118 */
static bool unicore64_cannot_copy_insn_p (rtx);
static void unicore64_output_dwarf_dtprel (FILE *file, int size, rtx x);

/* Build the SYMBOL_REF for __tls_get_addr.  */

static GTY(()) int pic_labelno;
static GTY(()) rtx tls_get_addr_libfunc;



#undef  TARGET_CANNOT_COPY_INSN_P
#define TARGET_CANNOT_COPY_INSN_P unicore64_cannot_copy_insn_p

#undef TARGET_CANNOT_FORCE_CONST_MEM
#define TARGET_CANNOT_FORCE_CONST_MEM unicore64_cannot_force_const_mem

#undef TARGET_APPLY_RESULT_MODE
#define TARGET_APPLY_RESULT_MODE unicore64_apply_result_mode

#undef Hint
#undef Mmode
#undef Ulong
#undef Ccstar

/* Initialize the GCC target structure.  */
#if TARGET_DLLIMPORT_DECL_ATTRIBUTES
#undef  TARGET_MERGE_DECL_ATTRIBUTES
#define TARGET_MERGE_DECL_ATTRIBUTES merge_dllimport_decl_attributes
#endif

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE unicore64_attribute_table

#ifdef AOF_ASSEMBLER
#undef  TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP "\tDCB\t"
#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\tDCW\t"
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\tDCD\t"
#else
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP NULL
#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER unicore64_assemble_integer
#endif

#undef  TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE unicore64_output_function_prologue

#undef  TARGET_ASM_FUNCTION_EPILOGUE
#define TARGET_ASM_FUNCTION_EPILOGUE unicore64_output_function_epilogue

#undef  TARGET_HANDLE_OPTION
#define TARGET_HANDLE_OPTION unicore64_handle_option

#undef  TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES unicore64_comp_type_attributes

#undef  TARGET_SET_DEFAULT_TYPE_ATTRIBUTES
#define TARGET_SET_DEFAULT_TYPE_ATTRIBUTES unicore64_set_default_type_attributes

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS unicore64_init_builtins

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN unicore64_expand_builtin

#undef  TARGET_SCHED_ADJUST_COST
#define TARGET_SCHED_ADJUST_COST unicore64_adjust_cost

#undef	TARGET_SCHED_ISSUE_RATE
#define	TARGET_SCHED_ISSUE_RATE unicore64_issue_rate

#undef  TARGET_PROMOTE_FUNCTION_ARGS
#define TARGET_PROMOTE_FUNCTION_ARGS hook_bool_tree_true
#undef  TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_tree_true

/* star delete it*/
/*
#undef  TARGET_STRUCT_VALUE_RTX
#define TARGET_STRUCT_VALUE_RTX unicore64_struct_value_rtx
*/

// huangping: FIXME 2011-09-01
#undef TARGET_SPLIT_COMPLEX_ARG
#define TARGET_SPLIT_COMPLEX_ARG unicore64_split_complex_arg

// huangping: FIXME 2011-09-01
//#undef TARGET_PROMOTE_FUNCTION_RETURN
//#define TARGET_PROMOTE_FUNCTION_RETURN hook_bool_const_tree_true

#undef  TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES unicore64_arg_partial_bytes

#undef  TARGET_SETUP_INCOMING_VARARGS
#define TARGET_SETUP_INCOMING_VARARGS unicore64_setup_incoming_varargs

#undef TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING unicore64_strip_name_encoding

/* star add */
#undef  TARGET_FUNCTION_OK_FOR_SIBCALL
#define TARGET_FUNCTION_OK_FOR_SIBCALL unicore64_function_ok_for_sibcall

// huangping: FIXME 2011-09-15
#undef TARGET_HAVE_TLS
#define TARGET_HAVE_TLS HAVE_AS_TLS

#ifdef HAVE_AS_TLS
#undef TARGET_ASM_OUTPUT_DWARF_DTPREL
#define TARGET_ASM_OUTPUT_DWARF_DTPREL unicore64_output_dwarf_dtprel
#endif

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO  unicore64_encode_section_info

#undef  TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK unicore64_output_mi_thunk
#undef  TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK default_can_output_mi_thunk_no_vcall

#undef TARGET_RTX_COSTS
#define TARGET_RTX_COSTS unicore64_rtx_costs

#undef TARGET_ADDRESS_COST
#define TARGET_ADDRESS_COST unicore64_address_cost

/* How large values are returned */
#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY unicore64_return_in_memory

#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG unicore64_reorg

struct gcc_target targetm = TARGET_INITIALIZER;

/* Obstack for minipool constant handling.  */
static struct obstack minipool_obstack;
static char *minipool_startobj;
int is_lwf_swf;

/* The maximum number of insns skipped which
   will be conditionalised if possible.  */
static int max_insns_skipped = 5;

extern FILE *asm_out_file;

/* True if we are currently building a constant table.  */
int making_const_table;

/* Define the information needed to generate branch insns.  This is
   stored from the compare operation.  */
rtx unicore64_compare_op0, unicore64_compare_op1;

/* What type of floating point are we tuning for?  */
enum floating_point_type unicore64_fpu;

/* What type of floating point instructions are available?  */
enum floating_point_type unicore64_fpu_arch;

/* Whether to use floating point hardware.  */
enum float_abi_type unicore64_float_abi=UNICORE64_FLOAT_ABI_HARD;

/* Which thread pointer model to use.  */
enum unicore64_tp_type target_thread_pointer = TP_REG16;

/* Set by the -mfp=... option.  */
const char *target_fp_name = NULL;

/* Used to parse -mstructure_size_boundary command line option.  */
const char *structure_size_string = NULL;
int unicore64_structure_size_boundary = DEFAULT_STRUCTURE_SIZE_BOUNDARY;

/* Bit values used to identify processor capabilities.  */
#define FL_CO_PROC    (1 << 0)	/* Has external co-processor bus */
#define FL_FAST_MULT  (1 << 1)	/* Fast multiply */
#define FL_ARCH1      (1 << 2)	/* Architecture rel 5 */
#define FL_UNICORE16  (1 << 3)	/* Unicore16 aware */
#define FL_LDSCHED    (1 << 4)	/* Load scheduling necessary */
#define FL_MODE32     (1 << 5)
#define FL_MODE26     (1 << 6)

enum cmp_type unicore64_compare_type;

/* The bits in this mask specify which
   instructions we are allowed to generate.  */
static int insn_flags = 0;

/* The bits in this mask specify which instruction scheduling options should
   be used.  Note - there is an overlap with the FL_FAST_MULT.  For some
   hardware we want to be able to generate the multiply instructions, but to
   tune as if they were not present in the architecture.  */
static int tune_flags = 0;

/* The following are used in the unicore64.md file as equivalents to bits
   in the above two flag variables.  */

/* Nonzero if this is an "M" variant of the processor.  */
int unicore64_fast_multiply = 0;

/* Nonzero if this chip supports the UNICORE64 Architecture 4 extensions.  */
int unicore64_arch1 = 0;


/* Nonzero if this chip can benefit from load scheduling.  */
int unicore64_ld_sched = 0;

/* In case of a PRE_INC, POST_INC, PRE_DEC, POST_DEC memory reference, we
   must report the mode of the memory reference from PRINT_OPERAND to
   PRINT_OPERAND_ADDRESS.  */
enum machine_mode output_memory_reference_mode;

/* The register number to be used for the PIC offset register.  */
const char *unicore64_pic_register_string = NULL;
const char *unicore64_const_pool = NULL;
unsigned unicore64_pic_register = INVALID_REGNUM;

/* Supported TLS relocations.  */

enum tls_reloc {
  TLS_GD32,
  TLS_LDM32,
  TLS_LDO32,
  TLS_IE32,
  TLS_LE32
};


/* Set to 1 when a return insn is output, this means that the epilogue
   is not needed.  */
int return_used_this_function;

/* Set to 1 after unicore64_reorg has started.  Reset to start at the start of
   the next function.  */
static int after_unicore64_reorg = 0;

/* The maximum number of insns to be used when loading a constant.  */
static int unicore64_constant_limit = 1;

/* For an explanation of these variables, see final_prescan_insn below.  */
int unicore64_ccfsm_state;
enum unicore64_cond_code unicore64_current_cc;
rtx unicore64_target_insn;
int unicore64_target_label;

/* The condition codes of the UNICORE64, and the inverse function.  */
static const char *const unicore64_condition_codes[] = {
  "eq", "ne", "ea", "ub", "fs", "ns", "fv", "nv",
  "ua", "eb", "eg", "sl", "sg", "el", "al"
};

#define streq(string1, string2) (strcmp (string1, string2) == 0)

/* Initialization code.  */

struct processors
{
  const char *const name;
  const unsigned int flags;
};

/* Not all of these give usefully different compilation alternatives,
   but there is no simple way of generalizing them.  */
static const struct processors all_cores[] = {
  /* UNICORE64 Cores */

  {"unicore1",
   FL_FAST_MULT | FL_ARCH1 | FL_UNICORE16 | FL_LDSCHED | FL_MODE32},
  {NULL, 0}
};

static const struct processors all_architectures[] = {
  /* UNICORE64 Architectures */

  {"unicore1",
   FL_CO_PROC | FL_FAST_MULT | FL_ARCH1 | FL_UNICORE16 | FL_MODE32},
  {NULL, 0}
};

/* This is a magic stucture.  The 'string' field is magically filled in
   with a pointer to the value specified by the user on the command line
   assuming that the user has specified such a value.  */

struct unicore64_cpu_select unicore64_select[] = {
  /* string       name            processors  */
  {NULL, "-mcpu=", all_cores},
  {NULL, "-march=", all_architectures},
  {NULL, "-mtune=", all_cores}
};

/* Return the number of bits set in value' */
static unsigned long
bit_count (signed int value)
{
  unsigned long count = 0;

  while (value)
    {
      value &= ~(value & -value);
      ++count;
    }

  return count;
}

/* Implement TARGET_HANDLE_OPTION.  */

static bool
unicore64_handle_option (size_t code, const char *arg, int value ATTRIBUTE_UNUSED)
{
  switch (code)
    {
    case OPT_march_:
      unicore64_select[1].string = arg;
      return true;

    case OPT_mcpu_:
      unicore64_select[0].string = arg;
      return true;

    case OPT_mhard_float:
      unicore64_float_abi = UNICORE64_FLOAT_ABI_HARD;
      return true;

    case OPT_msoft_float:
      unicore64_float_abi = UNICORE64_FLOAT_ABI_SOFT;
      return true;

    case OPT_mtune_:
      unicore64_select[2].string = arg;
      return true;

    default:
      return true;
    }
}

/* Fix up any incompatible options that the user has specified.
   This has now turned into a maze.  */
void
unicore64_override_options (void)
{
  unsigned i;

  /* Set up the flags based on the cpu/architecture selected by the user.  */
  for (i = ARRAY_SIZE (unicore64_select); i--;)
    {
      struct unicore64_cpu_select *ptr = unicore64_select + i;

      if (ptr->string != NULL && ptr->string[0] != '\0')
	{
	  const struct processors *sel;

	  for (sel = ptr->processors; sel->name != NULL; sel++)
	    if (streq (ptr->string, sel->name))
	      {
		if (i == 2)
		  tune_flags = sel->flags;
		else
		  {
		    /* If we have been given an architecture and a processor
		       make sure that they are compatible.  We only generate
		       a warning though, and we prefer the CPU over the
		       architecture.  */
		    if (insn_flags != 0 && (insn_flags ^ sel->flags))
		      warning (0,
			       "switch -mcpu=%s conflicts with -march= switch",
			       ptr->string);

		    insn_flags = sel->flags;
		  }

		break;
	      }

	  if (sel->name == NULL)
	    error ("bad value (%s) for %s switch", ptr->string, ptr->name);
	}
    }

  /* If the user did not specify a processor, choose one for them.  */
  if (insn_flags == 0)
    {
      const struct processors *sel;
      unsigned int sought;
      static const struct cpu_default
      {
	const int cpu;
	const char *const name;
      }
      cpu_defaults[] =
      {

	{
	TARGET_CPU_unicore1, "unicore1"},
	{
	TARGET_CPU_generic, "unicore"},
	{
	0, 0}
      };
      const struct cpu_default *def;

      /* Find the default.  */
      for (def = cpu_defaults; def->name; def++)
	if (def->cpu == TARGET_CPU_DEFAULT)
	  break;

      /* Make sure we found the default CPU.  */
      if (def->name == NULL)
	abort ();

      /* Find the default CPU's flags.  */
      for (sel = all_cores; sel->name != NULL; sel++)
	if (streq (def->name, sel->name))
	  break;

      if (sel->name == NULL)
	abort ();

      insn_flags = sel->flags;

      /* Now check to see if the user has specified some command line
         switch that require certain abilities from the cpu.  */
      sought = 0;

      if (TARGET_INTERWORK || TARGET_UNICORE16)
	{
	  sought |= (FL_UNICORE16 | FL_MODE32);

	  /* There are no UNICORE64 processors that support both lr and
	     interworking.  Therefore we force FL_MODE26 to be removed
	     from insn_flags here (if it was set), so that the search
	     below will always be able to find a compatible processor.  */
	  insn_flags &= ~FL_MODE26;
	}

      if (sought != 0 && ((sought & insn_flags) != sought))
	{
	  /* Try to locate a CPU type that supports all of the abilities
	     of the default CPU, plus the extra abilities requested by
	     the user.  */
	  for (sel = all_cores; sel->name != NULL; sel++)
	    if ((sel->flags & sought) == (sought | insn_flags))
	      break;

	  if (sel->name == NULL)
	    {
	      unsigned int current_bit_count = 0;
	      const struct processors *best_fit = NULL;

	      /* Ideally we would like to issue an error message here
	         saying that it was not possible to find a CPU compatible
	         with the default CPU, but which also supports the command
	         line options specified by the programmer, and so they
	         ought to use the -mcpu=<name> command line option to
	         override the default CPU type.

	         Instead if we cannot find a cpu that has both the
	         characteristics of the default cpu and the given command line
	         options we scan the array again looking for a best match.  */
	      for (sel = all_cores; sel->name != NULL; sel++)
		if ((sel->flags & sought) == sought)
		  {
		    unsigned int count;

		    count = bit_count (sel->flags & insn_flags);

		    if (count >= current_bit_count)
		      {
			best_fit = sel;
			current_bit_count = count;
		      }
		  }

	      if (best_fit == NULL)
		abort ();
	      else
		sel = best_fit;
	    }

	  insn_flags = sel->flags;
	}
    }

  /* If tuning has not been specified, tune for whichever processor or
     architecture has been selected.  */
  if (tune_flags == 0)
    tune_flags = insn_flags;

  /* Make sure that the processor choice does not conflict with any of the
     other command line choices.  */

  if (TARGET_INTERWORK && !(insn_flags & FL_UNICORE16))
    {
      warning (0, "target CPU does not support interworking");
      target_flags &= ~MASK_INTERWORK;
    }

  if (TARGET_UNICORE16 && !(insn_flags & FL_UNICORE16))
    {
      warning (0, "target CPU does not support UNICORE16 instructions");
      target_flags &= ~MASK_UNICORE16;
    }

  /* TARGET_BACKTRACE calls leaf_function_p, which causes a crash if done
     from here where no function is being compiled currently.  */
  if ((target_flags &
       (UNICORE16_FLAG_LEAF_BACKTRACE | UNICORE16_FLAG_BACKTRACE))
      && TARGET_UNICORE64)
    warning (0,
	     "enabling backtrace support is only meaningful when compiling for the Unicore16");

  if (TARGET_UNICORE64 && TARGET_CALLEE_INTERWORKING)
    warning (0,
	     "enabling callee interworking support is only meaningful when compiling for the Unicore16");

  if (TARGET_UNICORE64 && TARGET_CALLER_INTERWORKING)
    warning (0,
	     "enabling caller interworking support is only meaningful when compiling for the Unicore16");

  /* If stack checking is disabled, we can use r26(sl) as the PIC register,
     which keeps r25 available.  */
  if (flag_pic)
    unicore64_pic_register = SL_REGISTER_NUM;

  /* Initialise boolean versions of the flags, for use in the unicore64.md file.  */
  unicore64_fast_multiply = (insn_flags & FL_FAST_MULT) != 0;
  unicore64_arch1 = (insn_flags & FL_ARCH1) != 0;
  unicore64_ld_sched = (tune_flags & FL_LDSCHED) != 0;

  /* Default value for floating point code... if no co-processor
     bus, then schedule for emulated floating point.  Otherwise,
     assume the user has an FPA.
     Note: this does not prevent use of floating point instructions,
     -msoft-float does that.  */
  unicore64_fpu = (tune_flags & FL_CO_PROC) ? FP_HARD : FP_SOFT3;

  if (target_fp_name)
    {
      if (streq (target_fp_name, "2"))
	unicore64_fpu_arch = FP_SOFT2;
      else if (streq (target_fp_name, "3"))
	unicore64_fpu_arch = FP_SOFT3;
      else
	error ("invalid floating point emulation option: -mfpe-%s",
	       target_fp_name);
    }
  else
    unicore64_fpu_arch = FP_DEFAULT;

  if (TARGET_FPE && unicore64_fpu != FP_HARD)
    unicore64_fpu = FP_SOFT2;

  /* For unicore642/3 there is no need to do any scheduling if there is only
     a floating point emulator, or we are doing software floating-point.  */
  if ((TARGET_SOFT_FLOAT || unicore64_fpu != FP_HARD)
      && (tune_flags & FL_MODE32) == 0)
    flag_schedule_insns = flag_schedule_insns_after_reload = 0;

  if (target_thread_switch)
    {
      if (strcmp (target_thread_switch, "soft") == 0)
	target_thread_pointer = TP_SOFT;
      else if (strcmp (target_thread_switch, "auto") == 0)
	target_thread_pointer = TP_AUTO;
      else if (strcmp (target_thread_switch, "reg16") == 0)
	target_thread_pointer = TP_REG16;
      else
	error ("invalid thread pointer option: -mtp=%s", target_thread_switch);
    }

  /* Use the cp15 method if it is available.  */
  if (target_thread_pointer == TP_AUTO)
    {
      if (TARGET_UNICORE64)
	target_thread_pointer = TP_REG16;
      else
	target_thread_pointer = TP_SOFT;
    }

  if (TARGET_HARD_TP && TARGET_UNICORE16)
    error ("can not use -mtp=cp15 with unicore16");

  if (structure_size_string != NULL)
    {
      int size = strtol (structure_size_string, NULL, 0);

      if (size == 8 || size == 32)
	unicore64_structure_size_boundary = size;
      else
	warning (0, "structure size boundary can only be set to 8 or 32");
    }

  if (unicore64_pic_register_string != NULL)
    {
      int pic_register = decode_reg_name (unicore64_pic_register_string);

      if (!flag_pic)
	warning (0, "-mpic-register= is useless without -fpic");

      /* Prevent the user from choosing an obviously stupid PIC register.  */
      else if (pic_register < 0 || call_used_regs[pic_register]
	       || pic_register == HARD_FRAME_POINTER_REGNUM
	       || pic_register == STACK_POINTER_REGNUM
	       || pic_register >= PC_REGNUM)
	error ("unable to use '%s' for PIC register",
	       unicore64_pic_register_string);
      else
	unicore64_pic_register = pic_register;
    }

  if (TARGET_UNICORE16 && flag_schedule_insns)
    {
      /* Don't warn since it's on by default in -O2.  */
      flag_schedule_insns = 0;
    }

  if (unicore64_const_pool != NULL)
    {
      int const_pool_limit = atoi (unicore64_const_pool);
      if (const_pool_limit > 0 && const_pool_limit < 21)
	unicore64_constant_limit = const_pool_limit;
      else
	error ("can not set const_pool_limit to %d."
	       " it must be [1,20].\n",
	       const_pool_limit);
    }
  else
    {
      /* If optimizing for space, don't synthesize constants.
	 For processors with load scheduling, it never costs more than 2 cycles
	 to load a constant, and the load scheduler may well reduce that to 1.  */
      if (optimize_size || (tune_flags & FL_LDSCHED))
        unicore64_constant_limit = 1;

    }

  /* If optimizing for size, bump the number of instructions that we
     are prepared to conditionally execute. */ 
  if (optimize_size)
    max_insns_skipped = 6;

  /* Register global variables with the garbage collector.  */
  unicore64_add_gc_roots ();
}

static void
unicore64_add_gc_roots (void)
{
  gcc_obstack_init (&minipool_obstack);
  minipool_startobj = (char *) obstack_alloc (&minipool_obstack, 0);
}
/* A table of known UNICORE64 exception types.
   For use with the interrupt function attribute.  */

typedef struct
{
  const char *const arg;
  const unsigned long return_value;
}
isr_attribute_arg;

static const isr_attribute_arg isr_attribute_args[] = {
  {"IRQ", UNICORE64_FT_ISR},
  {"irq", UNICORE64_FT_ISR},
  {"FIQ", UNICORE64_FT_FIQ},
  {"fiq", UNICORE64_FT_FIQ},
  {"ABORT", UNICORE64_FT_ISR},
  {"abort", UNICORE64_FT_ISR},
  {"ABORT", UNICORE64_FT_ISR},
  {"abort", UNICORE64_FT_ISR},
  {"UNDEF", UNICORE64_FT_EXCEPTION},
  {"undef", UNICORE64_FT_EXCEPTION},
  {"SWI", UNICORE64_FT_EXCEPTION},
  {"swi", UNICORE64_FT_EXCEPTION},
  {NULL, UNICORE64_FT_NORMAL}
};

/* Returns the (interrupt) function type of the current
   function, or UNICORE64_FT_UNKNOWN if the type cannot be determined.  */

static unsigned long
unicore64_isr_value (tree argument)
{
  const isr_attribute_arg *ptr;
  const char *arg;

  /* No argument - default to IRQ.  */
  if (argument == NULL_TREE)
    return UNICORE64_FT_ISR;

  /* Get the value of the argument.  */
  if (TREE_VALUE (argument) == NULL_TREE
      || TREE_CODE (TREE_VALUE (argument)) != STRING_CST)
    return UNICORE64_FT_UNKNOWN;

  arg = TREE_STRING_POINTER (TREE_VALUE (argument));

  /* Check it against the list of known arguments.  */
  for (ptr = isr_attribute_args; ptr->arg != NULL; ptr++)
    if (streq (arg, ptr->arg))
      return ptr->return_value;

  /* An unrecognised interrupt type.  */
  return UNICORE64_FT_UNKNOWN;
}

/* Computes the type of the current function.  */

static unsigned long
unicore64_compute_func_type (void)
{
  unsigned long type = UNICORE64_FT_UNKNOWN;
  tree a;
  tree attr;

  if (TREE_CODE (current_function_decl) != FUNCTION_DECL)
    abort ();

  /* Decide if the current function is volatile.  Such functions
     never return, and many memory cycles can be saved by not storing
     register values that will never be needed again.  This optimization
     was added to speed up context switching in a kernel application.  */
  if (optimize > 0
      && TREE_NOTHROW (current_function_decl)
      && TREE_THIS_VOLATILE (current_function_decl))
    type |= UNICORE64_FT_VOLATILE;

  if (cfun->static_chain_decl)
    type |= UNICORE64_FT_NESTED;

  attr = DECL_ATTRIBUTES (current_function_decl);

  a = lookup_attribute ("naked", attr);
  if (a != NULL_TREE)
    type |= UNICORE64_FT_NAKED;

  if (cfun->machine->eh_epilogue_sp_ofs != NULL_RTX)
    type |= UNICORE64_FT_EXCEPTION_HANDLER;
  else
    {
      a = lookup_attribute ("isr", attr);
      if (a == NULL_TREE)
	a = lookup_attribute ("interrupt", attr);

      if (a == NULL_TREE)
	type |=
	  TARGET_INTERWORK ? UNICORE64_FT_INTERWORKED : UNICORE64_FT_NORMAL;
      else
	type |= unicore64_isr_value (TREE_VALUE (a));
    }

  return type;
}

/* Returns the type of the current function.  */

unsigned long
unicore64_current_func_type (void)
{
  if (UNICORE64_FUNC_TYPE (cfun->machine->func_type) == UNICORE64_FT_UNKNOWN)
    cfun->machine->func_type = unicore64_compute_func_type ();

  return cfun->machine->func_type;
}

/* Return 1 if it is possible to return using a single instruction.  */
/* star */
int
use_return_insn (int iscond ATTRIBUTE_UNUSED)
{
  int regno;
  unsigned int func_type;
  unsigned long saved_int_regs;

  /* Never use a return instruction before reload has run.  */
  if (!reload_completed)
    return 0;

  func_type = unicore64_current_func_type ();

  /* Naked functions and volatile functions need special
     consideration.  */
  if (func_type & (UNICORE64_FT_VOLATILE | UNICORE64_FT_NAKED))
    return 0;

//  /* As do variadic functions.  */
//  if (crtl->args.pretend_args_size || cfun->machine->uses_anonymous_args
//      /* Of if the function calls __builtin_eh_return () */
//      || UNICORE64_FUNC_TYPE (func_type) == UNICORE64_FT_EXCEPTION_HANDLER
//      /* Or if there is no frame pointer and there is a stack adjustment.  */
//      || ((get_frame_size () + crtl->outgoing_args_size != 0)
//	  && !frame_pointer_needed))
//    return 0;

  /* As do variadic functions.  */
  if (crtl->args.pretend_args_size || cfun->machine->uses_anonymous_args
      /* Of if the function calls __builtin_eh_return () */
      || UNICORE64_FUNC_TYPE (func_type) == UNICORE64_FT_EXCEPTION_HANDLER
      /* Or if there is no frame pointer and there is a stack adjustment.  */
      || ((get_frame_size () + crtl->outgoing_args_size != 0)
	  && !frame_pointer_needed))
    return 0;

  saved_int_regs = unicore64_compute_save_reg_mask ();

  /* Can't be done if interworking with Unicore16, and any registers have been
     stacked. */
  if (TARGET_INTERWORK && saved_int_regs != 0)
    return 0;

  /* If there are saved registers but the LR isn't saved, then we need
   * two instructions for the return.  */
  if (saved_int_regs && !(saved_int_regs & (1 << LR_REGNUM)))
    return 0;
  /* Can't be done if any of the FPU regs are pushed,
     since this also requires an insn.  */
  if (TARGET_HARD_FLOAT)
    for (regno = FIRST_UNICORE64_FP_REGNUM; regno <= LAST_UNICORE64_FP_REGNUM;
	 regno++)
      if (df_regs_ever_live_p (regno) && !call_used_regs[regno])
	return 0;

  return 1;
}

// huangping: start!
/* Return TRUE if int I is a valid immediate UNICORE64 constant.  */
/* star modify for UniCore-3. */
int
const_ok_for_unicore64 (HOST_WIDE_INT i)
{
  /* 11 bits signed imm. */
  HOST_WIDE_INT imm_max = ((HOST_WIDE_INT)1) << 11;

  if (i >= 0 && i < imm_max)
    return TRUE;


  return FALSE;
}

/* Return true if I is a valid constant for the operation CODE.  */
static int
const_ok_for_op (HOST_WIDE_INT i, enum rtx_code code)
{
  if (const_ok_for_unicore64 (i))
    return 1;

  switch (code)
    {
    case PLUS:
      return const_ok_for_unicore64 (UNICORE64_SIGN_EXTEND (-i));

    case MINUS:		/* Should only occur with (MINUS I reg) => rsb */
    case XOR:
    case IOR:
      return 0;

    case AND:
      return const_ok_for_unicore64 (UNICORE64_SIGN_EXTEND (~i));

    default:
      abort ();
    }
}

/* Emit a sequence of insns to handle a large constant.
   CODE is the code of the operation required, it can be any of SET, PLUS,
   IOR, AND, XOR, MINUS;
   MODE is the mode in which the operation is being performed;
   VAL is the integer to operate on;
   SOURCE is the other operand (a register, or a null-pointer for SET);
   SUBTARGETS means it is safe to create scratch registers if that will
   either produce a simpler sequence, or we will want to cse the values.
   Return value is the number of insns emitted.  */

int
unicore64_split_constant (enum rtx_code code, enum machine_mode mode,
			  rtx insn, HOST_WIDE_INT val, rtx target, rtx source,
			  int subtargets)
{
  rtx cond;
  if (insn && GET_CODE (PATTERN (insn)) == COND_EXEC)
    cond = COND_EXEC_TEST (PATTERN (insn));
  else
    cond = NULL_RTX;

//  if (subtargets || code == SET
//      || (GET_CODE (target) == REG && GET_CODE (source) == REG
//	  && REGNO (target) != REGNO (source)))
//    {
//      /* After unicore64_reorg has been called, we can't fix up expensive
//         constants by pushing them into memory so we must synthesize
//         them in-line, regardless of the cost.  This is only likely to
//         be more costly on chips that have load delay slots and we are
//         compiling without running the scheduler (so no splitting
//         occurred before the final instruction emission).
//
//         Ref: gcc -O1 gcc.c-torture/compile/980506-2.c
//       */
//
//      if (!after_unicore64_reorg
//	  && !cond
//	  && (unicore64_gen_constant (code, mode, val, target, source, 1, 0)
//	      > unicore64_constant_limit + (code != SET)))
//	{
//	  if (code == SET)
//	    {
//	      /* Currently SET is the only monadic value for CODE, all
//	         the rest are diadic.  */
//	      emit_insn (gen_rtx_SET (VOIDmode, target, GEN_INT (val)));
//	      return 1;
//	    }
//	  else
//	    {
//	      rtx temp = subtargets ? gen_reg_rtx (mode) : target;
//
//	      emit_insn (gen_rtx_SET (VOIDmode, temp, GEN_INT (val)));
//	      /* For MINUS, the value is subtracted from, since we never
//	         have subtraction of a constant.  */
//	      if (code == MINUS)
//		emit_insn (gen_rtx_SET (VOIDmode, target,
//					gen_rtx_MINUS (mode, temp, source)));
//	      else
//		emit_insn (gen_rtx_SET (VOIDmode, target,
//					gen_rtx_fmt_ee (code, mode, source,
//							temp)));
//	      return 2;
//	    }
//	}
//    }

  return unicore64_gen_constant (code, mode, val, target, source, subtargets,
				 1);
}

/* As above, but extra parameter GENERATE which, if clear, suppresses
   RTL generation.  */
int
unicore64_gen_constant (enum rtx_code code,
			enum machine_mode mode,
			HOST_WIDE_INT val,
			rtx target, rtx source, int subtargets, int generate)
{
  int i;
  int set_sign_bit_copies = 0;
  int clear_sign_bit_copies = 0;
  int clear_zero_bit_copies = 0;
  int set_zero_bit_copies = 0;

  /*
  if (const_ok_for_unicore64 (val))
  {
    if (generate)
    {
      if (code == MINUS)
	  emit_insn (gen_rtx_SET (VOIDmode, target,
		gen_rtx_MINUS (mode, GEN_INT(val), source)));
	else
	  emit_insn (gen_rtx_SET (VOIDmode, target,
		gen_rtx_fmt_ee (code, mode, source, GEN_INT (val))));
      }
      return 1;
  }
  else if (const_ok_for_unicore64 (-val))
  {
    switch (code)
    {
      case PLUS:
      case SET:
	emit_insn (gen_rtx_SET (VOIDmode, target,
	      gen_rtx_fmt_ee (code, mode, source, GEN_INT (val))));
	break;
      case MINUS:
	emit_insn (gen_rtx_SET (VOIDmode, target,
	      gen_rtx_MINUS (mode, GEN_INT(val), source)));
	break;
    }
    return 1;
  }
  */


  
  switch (code)
  {
    case SET:
      if (const_ok_for_unicore64 (val)
	  ||const_ok_for_unicore64 (-val)
	  || const_ok_for_unicore64 (~val))
      {
	if (generate)
	  emit_insn (gen_rtx_SET (VOIDmode, target, GEN_INT (val)));
	return 1;
      }
      break;

    case PLUS:
      if (const_ok_for_unicore64 (val)
	  ||const_ok_for_unicore64 (-val))
      {
	if (generate)
	  emit_insn (gen_rtx_SET (VOIDmode, target,
		gen_rtx_fmt_ee (code, mode, source, GEN_INT (val))));
	return 1;
      }
      break;

    case MINUS:
      /* We treat MINUS as (val - source), since (source - val) is always
         passed as (source + (-val)).  */
      if (val == (HOST_WIDE_INT)0)
	{
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target,
				    gen_rtx_NEG (mode, source)));
	  return 1;
	}

      if (const_ok_for_unicore64 (val)
	  ||const_ok_for_unicore64 (-val))
      {
	if (generate)
	  emit_insn (gen_rtx_SET (VOIDmode, target,
		gen_rtx_MINUS (mode, GEN_INT (val), source)));
	return 1;
      }
      break;


    case IOR:
      if (val == (HOST_WIDE_INT)(-1))
	{
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target,
				    GEN_INT (UNICORE64_SIGN_EXTEND (val))));
	  return 1;
	}
      if (val == (HOST_WIDE_INT)0)
	{
	  if (reload_completed && rtx_equal_p (target, source))
	    return 0;
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target, source));
	  return 1;
	}
      break;

    case AND:
      if (val == (HOST_WIDE_INT)0)
	{
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target, const0_rtx));
	  return 1;
	}
      if (val == (HOST_WIDE_INT)(-1))
	{
	  if (reload_completed && rtx_equal_p (target, source))
	    return 0;
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target, source));
	  return 1;
	}
      break;

    case XOR:
      if (val == (HOST_WIDE_INT)0)
	{
	  if (reload_completed && rtx_equal_p (target, source))
	    return 0;
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target, source));
	  return 1;
	}
      if (val == (HOST_WIDE_INT)(-1))
	{
	  if (generate)
	    emit_insn (gen_rtx_SET (VOIDmode, target,
				    gen_rtx_NOT (mode, source)));
	  return 1;
	}
      break;

    default:
      abort ();
    }

  if (generate)
  {
    if (code == SET)
    {
      /* Currently SET is the only monadic value for CODE, all
	 the rest are diadic.  */
      emit_insn (gen_rtx_SET (VOIDmode, target, GEN_INT (val)));
    }
    else
    {
      rtx temp = subtargets ? gen_reg_rtx (mode) : target;

      emit_insn (gen_rtx_SET (VOIDmode, temp, GEN_INT (val)));
      /* For MINUS, the value is subtracted from, since we never
	 have subtraction of a constant.  */
      if (code == MINUS)
	emit_insn (gen_rtx_SET (VOIDmode, target,
	      gen_rtx_MINUS (mode, temp, source)));
      else
	emit_insn (gen_rtx_SET (VOIDmode, target,
	      gen_rtx_fmt_ee (code, mode, source,
		temp)));
    }
  }
  return 100;

  //optimize for AND/IOR
  /* Calculate a few attributes that may be useful for specific
     optimizations.  */
//  for (i = (8*sizeof(HOST_WIDE_INT)-1); i >= 0; i--)
//    {
//      if ((val & (1 << i)) == 0)
//	clear_sign_bit_copies++;
//      else
//	break;
//    }
//
//  for (i = (8*sizeof(HOST_WIDE_INT)-1); i >= 0; i--)
//    {
//      if ((val & (1 << i)) != 0)
//	set_sign_bit_copies++;
//      else
//	break;
//    }
//
//  for (i = 0; i <= (8*sizeof(HOST_WIDE_INT)-1); i++)
//    {
//      if ((val & (1 << i)) == 0)
//	clear_zero_bit_copies++;
//      else
//	break;
//    }
//
//  for (i = 0; i <= (8*sizeof(HOST_WIDE_INT)-1); i++)
//    {
//      if ((val & (1 << i)) != 0)
//	set_zero_bit_copies++;
//      else
//	break;
//    }
//
//  if (code == AND)
//  {
//    if ((set_sign_bit_copies + clear_zero_bit_copies) == (8*sizeof(HOST_WIDE_INT)))
//	{
//	  if (generate)
//	 	 {
//	    		 rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
//	    		 rtx shift = GEN_INT (clear_sign_bit_copies);
// 
//	    		 emit_insn (gen_lshrdi3 (new_src, source, clear_zero_bit_copies));
//	    		 emit_insn (gen_ashldi3 (target, new_src, clear_zero_bit_copies));
//		 }
//	  return 2;
//	}
//    else if ((clear_sign_bit_copies + set_zero_bit_copies) == (8*sizeof(HOST_WIDE_INT)))
//    	{
//	  if (generate) 
//	  	{	                               
//		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
//		  rtx shift = GEN_INT (clear_sign_bit_copies);
//		  emit_insn (gen_ashldi3 (new_src, source, set_zero_bit_copies)):
//		  emit_insn (gen_lshrdi3 (target, new_src, set_zero_bit_copies)); 
//		}
//	  return 2;
//	}
//  }

  /**********FIXME: Maybe we should optimize this const_pool mechanism ********/
  return 100;
}

//int
//unicore64_gen_constant (enum rtx_code code,
//			enum machine_mode mode,
//			HOST_WIDE_INT val,
//			rtx target, rtx source, int subtargets, int generate)
//{
//  int can_invert = 0;
//  int can_negate = 0;
//  int can_negate_initial = 0;
//  int can_shift = 0;
//  int i;
//  int num_bits_set = 0;
//  int set_sign_bit_copies = 0;
//  int clear_sign_bit_copies = 0;
//  int clear_zero_bit_copies = 0;
//  int set_zero_bit_copies = 0;
//  int insns = 0;
//  unsigned HOST_WIDE_INT temp1, temp2;
//  unsigned HOST_WIDE_INT remainder = val & -1ULL;
//
//
//  /* Find out which operations are safe for a given CODE.  Also do a quick
//     check for degenerate cases; these can occur when DImode operations
//     are split.  */
//  switch (code)
//    {
//    case SET:
//      can_invert = 1;
//      can_shift = 1;
//      can_negate = 1;
//      break;
//
//    case PLUS:
//      can_negate = 1;
//      can_negate_initial = 1;
//      break;
//
//    case IOR:
//      if (remainder == -1ULL)
//	{
//	  if (generate)
//	    emit_insn (gen_rtx_SET (VOIDmode, target,
//				    GEN_INT (val)));
//	  return 1;
//	}
//      if (remainder == 0ULL)
//	{
//	  if (reload_completed && rtx_equal_p (target, source))
//	    return 0;
//	  if (generate)
//	    emit_insn (gen_rtx_SET (VOIDmode, target, source));
//	  return 1;
//	}
//      break;
//
//    case AND:
//      if (remainder == 0ULL)
//	{
//	  if (generate)
//	    emit_insn (gen_rtx_SET (VOIDmode, target, const0_rtx));
//	  return 1;
//	}
//      if (remainder == -1ULL)
//	{
//	  if (reload_completed && rtx_equal_p (target, source))
//	    return 0;
//	  if (generate)
//	    emit_insn (gen_rtx_SET (VOIDmode, target, source));
//	  return 1;
//	}
//      can_invert = 1;
//      break;
//
//    case XOR:
//      if (remainder == 0ULL)
//	{
//	  if (reload_completed && rtx_equal_p (target, source))
//	    return 0;
//	  if (generate)
//	    emit_insn (gen_rtx_SET (VOIDmode, target, source));
//	  return 1;
//	}
//      if (remainder == -1ULL)
//	{
//	  if (generate)
//	    emit_insn (gen_rtx_SET (VOIDmode, target,
//				    gen_rtx_NOT (mode, source)));
//	  return 1;
//	}
//
//      /* We don't know how to handle this yet below.  */
//      abort ();
//
//    case MINUS:
//      /* We treat MINUS as (val - source), since (source - val) is always
//         passed as (source + (-val)).  */
//      if (remainder == 0ULL)
//	{
//	  if (generate)
//	    emit_insn (gen_rtx_SET (VOIDmode, target,
//				    gen_rtx_NEG (mode, source)));
//	  return 1;
//	}
//      if (const_ok_for_unicore64 (val))
//	{
//	  if (generate)
//	    emit_insn (gen_rtx_SET (VOIDmode, target,
//				    gen_rtx_MINUS (mode, GEN_INT (val),
//						   source)));
//	  return 1;
//	}
//      can_negate = 1;
//
//      break;
//
//    default:
//      abort ();
//    }
//
//  /* If we can do it in one insn get out quickly.  */
//  if (const_ok_for_unicore64 (val)
//      || (can_negate_initial && const_ok_for_unicore64 (-val))
//      || (can_invert && const_ok_for_unicore64 (~val)))
//    {
//      if (generate)
//	emit_insn (gen_rtx_SET (VOIDmode, target,
//				(source ? gen_rtx_fmt_ee (code, mode, source, GEN_INT (val)) : GEN_INT (val))));
//      return 1;
//    }
//
//  /* Calculate a few attributes that may be useful for specific
//     optimizations.  */
//  for (i = 63; i >= 0; i--)
//    {
//      if ((remainder & (1 << i)) == 0)
//	clear_sign_bit_copies++;
//      else
//	break;
//    }
//
//  for (i = 63; i >= 0; i--)
//    {
//      if ((remainder & (1 << i)) != 0)
//	set_sign_bit_copies++;
//      else
//	break;
//    }
//
//  for (i = 0; i <= 63; i++)
//    {
//      if ((remainder & (1 << i)) == 0)
//	clear_zero_bit_copies++;
//      else
//	break;
//    }
//
//  for (i = 0; i <= 63; i++)
//    {
//      if ((remainder & (1 << i)) != 0)
//	set_zero_bit_copies++;
//      else
//	break;
//    }
//
//  switch (code)
//    {
//    case SET:
//      /* See if we can do this by sign_extending a constant that is known
//         to be negative.  This is a good, way of doing it, since the shift
//         may well merge into a subsequent insn.  */
//      if (set_sign_bit_copies > 1)
//	{
//	  if (const_ok_for_unicore64
//	      (temp1 = UNICORE64_SIGN_EXTEND (remainder
//					      << (set_sign_bit_copies - 1))))
//	    {
//	      if (generate)
//		{
//		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
//		  emit_insn (gen_rtx_SET (VOIDmode, new_src,
//					  GEN_INT (temp1)));
//		  emit_insn (gen_ashrdi3 (target, new_src,
//					  GEN_INT (set_sign_bit_copies - 1)));
//		}
//	      return 2;
//	    }
//	  /* For an inverted constant, we will need to set the low bits,
//	     these will be shifted out of harm's way.  */
//	  temp1 |= (1 << (set_sign_bit_copies - 1)) - 1;
//	  if (const_ok_for_unicore64 (~temp1))
//	    {
//	      if (generate)
//		{
//		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
//		  emit_insn (gen_rtx_SET (VOIDmode, new_src,
//					  GEN_INT (temp1)));
//		  emit_insn (gen_ashrdi3 (target, new_src,
//					  GEN_INT (set_sign_bit_copies - 1)));
//		}
//	      return 2;
//	    }
//	}
//
//      break;
//
//    case IOR:
//    case XOR:
//      /* If we have IOR or XOR, and the constant can be loaded in a
//         single instruction, and we can find a temporary to put it in,
//         then this can be done in two instructions instead of 3-4.  */
//      if (subtargets
//	  /* TARGET can't be NULL if SUBTARGETS is 0 */
//	  || (reload_completed && !reg_mentioned_p (target, source)))
//	{
//	  if (const_ok_for_unicore64 (UNICORE64_SIGN_EXTEND (~val)))
//	    {
//	      if (generate)
//		{
//		  rtx sub = subtargets ? gen_reg_rtx (mode) : target;
//
//		  emit_insn (gen_rtx_SET (VOIDmode, sub, GEN_INT (val)));
//		  emit_insn (gen_rtx_SET (VOIDmode, target,
//					  gen_rtx_fmt_ee (code, mode, source,
//							  sub)));
//		}
//	      return 2;
//	    }
//	}
//
//      if (code == XOR)
//	break;
//      if (const_ok_for_unicore64 (temp1 = UNICORE64_SIGN_EXTEND (~val)))
//	{
//	  if (generate)
//	    {
//	      rtx sub = subtargets ? gen_reg_rtx (mode) : target;
//	      emit_insn (gen_rtx_SET (VOIDmode, sub,
//				      gen_rtx_NOT (mode, source)));
//	      source = sub;
//	      if (subtargets)
//		sub = gen_reg_rtx (mode);
//	      emit_insn (gen_rtx_SET (VOIDmode, sub,
//				      gen_rtx_AND (mode, source,
//						   GEN_INT (temp1))));
//	      emit_insn (gen_rtx_SET (VOIDmode, target,
//				      gen_rtx_NOT (mode, sub)));
//	    }
//	  return 3;
//	}
//      break;
//    
//    case AND:
//      /* See if two shifts will do 2 or more insn's worth of work.  */
//      if (clear_sign_bit_copies >= 16 && clear_sign_bit_copies < 24)
//	{
//	  HOST_WIDE_INT shift_mask = ((0xffffffff
//				       << (32 - clear_sign_bit_copies))
//				      & 0xffffffff);
//
//	  if ((remainder | shift_mask) != 0xffffffff)
//	    {
//	      if (generate)
//		{
//		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
//		  insns =
//		    unicore64_gen_constant (AND, mode, remainder | shift_mask,
//					    new_src, source, subtargets, 1);
//		  source = new_src;
//		}
//	      else
//		{
//		  rtx targ = subtargets ? NULL_RTX : target;
//		  insns =
//		    unicore64_gen_constant (AND, mode, remainder | shift_mask,
//					    targ, source, subtargets, 0);
//		}
//	    }
//
//	  if (generate)
//	    {
//	      rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
//	      rtx shift = GEN_INT (clear_sign_bit_copies);
//
//	      emit_insn (gen_ashlsi3 (new_src, source, shift));
//	      emit_insn (gen_lshrsi3 (target, new_src, shift));
//	    }
//
//	  return insns + 2;
//	}
//
//      if (clear_zero_bit_copies >= 16 && clear_zero_bit_copies < 24)
//	{
//	  HOST_WIDE_INT shift_mask = (1 << clear_zero_bit_copies) - 1;
//
//	  if ((remainder | shift_mask) != 0xffffffff)
//	    {
//	      if (generate)
//		{
//		  rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
//
//		  insns =
//		    unicore64_gen_constant (AND, mode, remainder | shift_mask,
//					    new_src, source, subtargets, 1);
//		  source = new_src;
//		}
//	      else
//		{
//		  rtx targ = subtargets ? NULL_RTX : target;
//
//		  insns =
//		    unicore64_gen_constant (AND, mode, remainder | shift_mask,
//					    targ, source, subtargets, 0);
//		}
//	    }
//
//	  if (generate)
//	    {
//	      rtx new_src = subtargets ? gen_reg_rtx (mode) : target;
//	      rtx shift = GEN_INT (clear_zero_bit_copies);
//
//	      emit_insn (gen_lshrsi3 (new_src, source, shift));
//	      emit_insn (gen_ashlsi3 (target, new_src, shift));
//	    }
//
//	  return insns + 2;
//	}
//
//      break;
//
//    default:
//      break;
//    }
//
///*
//  for (i = 0; i < 32; i++)
//    if (remainder & (1 << i))
//      num_bits_set++;
//  if (code == AND || (can_invert && num_bits_set > 16))
//    remainder = (~remainder) & 0xffffffff;
//  else if (code == PLUS && num_bits_set > 16)
//    remainder = (-remainder) & 0xffffffff;
//  
//  else
//  */
//    {
//      can_invert = 0;
//      can_negate = 0;
//    }
//
//
//  /* star add for UniCore-3 ISA. */
//  {
//    int H = (remainder & (0x7ff << 22)) >> 22; 
//    int M = (remainder & (0x7ff << 11)) >> 11; 
//    int L = remainder & 0x7ff; 
//    rtx tmp1_rtx, tmp2_rtx;
//    rtx local_rtx, new_rtx = NULL;
//    rtx shift_rtx = GEN_INT (11);
//
//    /*
//    fprintf (stderr, "star : remainder=%x, H=%x, M=%x, L=%x, subtargets=%d, code=%d\n",
//	     remainder, H, M, L, subtargets, code);
//	     */
//    
//    // only count the number of instructions. *DO NOT* generate insn. 
//    // huangping: FIXME 2012-02-14
//    if (1)
//    //if (!generate)
//      {
//	if (H && M && L)
//	  insns+=5;
//	else if (H && M)
//	  insns+=4;
//	else if ((H && L) || (M && L))
//	  insns+=3;
//	else if (H || M)
//	  insns+=2;
//	else
//	  insns+=1;
//	return insns;
//      }
//
//    gcc_assert (subtargets || code==SET);
//
//    if (H)
//      {
//	tmp1_rtx = GEN_INT (H);
//	new_rtx = (subtargets ? gen_reg_rtx (mode) : target);
//
//	if (!M)
//	  shift_rtx = GEN_INT (22);
//
//	emit_insn (gen_rtx_SET (VOIDmode, new_rtx, tmp1_rtx));
//	emit_insn (gen_ashlsi3 (new_rtx, new_rtx, shift_rtx));
//	insns+=2;
//      }
//    if (M)
//      {
//	tmp1_rtx = GEN_INT (M);
//	if (!new_rtx)
//	  {
//	    new_rtx = (subtargets ? gen_reg_rtx (mode) : target);
//	    emit_insn (gen_rtx_SET (VOIDmode, new_rtx, tmp1_rtx));
//	  }
//	else
//	  {
//	    rtx tmp2_rtx = gen_rtx_PLUS (mode, new_rtx, tmp1_rtx);
//	    emit_insn (gen_rtx_SET (VOIDmode, new_rtx, tmp2_rtx));
//	  }
//
//	emit_insn (gen_ashlsi3 (new_rtx, new_rtx, shift_rtx));
//	insns+=2;
//      }
//    if (L)
//      {
//	tmp1_rtx = GEN_INT (L);
//	if (!new_rtx)
//	  {
//	    new_rtx = (subtargets ? gen_reg_rtx (mode) : target);
//	    emit_insn (gen_rtx_SET (VOIDmode, new_rtx, tmp1_rtx));
//	  }
//	else
//	  {
//	    rtx tmp2_rtx = gen_rtx_PLUS (mode, new_rtx, tmp1_rtx);
//	    emit_insn (gen_rtx_SET (VOIDmode, new_rtx, tmp2_rtx));
//	  }
//
//	insns++;
//      }
//    
//    if (!H && !M && !L)
//      {
//	tmp1_rtx = GEN_INT (0);
//      }
//
//    if (code == SET)
//      tmp1_rtx = new_rtx;
//    else if (code == MINUS)
//      tmp1_rtx = gen_rtx_MINUS (mode, new_rtx, source);
//    else
//      tmp1_rtx = gen_rtx_fmt_ee (code, mode, source, new_rtx);
//
//    emit_insn (gen_rtx_SET (VOIDmode, target, tmp1_rtx));
//  }
//  return insns;
//}

/* Canonicalize a comparison so that we are more likely to recognize it.
   This can be done for a few constant compares, where we can make the
   immediate value easier to load.  */

// huangping: used in unicore64.h

enum rtx_code
unicore64_canonicalize_comparison (enum rtx_code code, rtx * op1)
{
  unsigned HOST_WIDE_INT i = INTVAL (*op1);

  switch (code)
    {
    case EQ:
    case NE:
      return code;

    case GT:
    case LE:
      if (i !=
	  ((((unsigned HOST_WIDE_INT) 1) << (HOST_BITS_PER_WIDE_INT - 1)) - 1)
	  && (const_ok_for_unicore64 (i + 1)
	      || const_ok_for_unicore64 (-(i + 1))))
	{
	  *op1 = GEN_INT (i + 1);
	  return code == GT ? GE : LT;
	}
      break;

    case GE:
    case LT:
      if (i != (((unsigned HOST_WIDE_INT) 1) << (HOST_BITS_PER_WIDE_INT - 1))
	  && (const_ok_for_unicore64 (i - 1)
	      || const_ok_for_unicore64 (-(i - 1))))
	{
	  *op1 = GEN_INT (i - 1);
	  return code == GE ? GT : LE;
	}
      break;

    case GTU:
    case LEU:
      if (i != ~((unsigned HOST_WIDE_INT) 0)
	  && (const_ok_for_unicore64 (i + 1)
	      || const_ok_for_unicore64 (-(i + 1))))
	{
	  *op1 = GEN_INT (i + 1);
	  return code == GTU ? GEU : LTU;
	}
      break;

    case GEU:
    case LTU:
      if (i != 0
	  && (const_ok_for_unicore64 (i - 1)
	      || const_ok_for_unicore64 (-(i - 1))))
	{
	  *op1 = GEN_INT (i - 1);
	  return code == GEU ? GTU : LEU;
	}
      break;

    default:
      abort ();
    }

  return code;
}

/* Decide whether a type should be returned in memory (true)
   or in a register (false).  This is called by the macro
   TARGET_RETURN_IN_MEMORY.  */

int
unicore64_return_in_memory (tree type)
{
  // huangping: FIXME 2011-09-01
  if (COMPLEX_FLOAT_TYPE_P (type) || (TREE_CODE (type) == COMPLEX_TYPE && TREE_CODE (TREE_TYPE (type)) == INTEGER_TYPE))
    return 1;

  if (!AGGREGATE_TYPE_P (type))
    /* All simple types are returned in registers.  */
    return 0;

  /* For the unicore64-wince targets we choose to be compitable with Microsoft's
     UNICORE64 and Unicore16 compilers, which always return aggregates in memory.  */
#ifndef UNICORE64_WINCE
  /* All structures/unions bigger than one word are returned in memory.
     Also catch the case where int_size_in_bytes returns -1.  In this case
     the aggregate is either huge or of varaible size, and in either case
     we will want to return it via memory and not in a register.  */
  if (((unsigned int) int_size_in_bytes (type)) > UNITS_PER_WORD)
    return 1;

  if (TREE_CODE (type) == RECORD_TYPE)
    {
      tree field;

      /* For a struct the UPCS says that we only return in a register
         if the type is 'integer like' and every addressable element
         has an offset of zero.  For practical purposes this means
         that the structure can have at most one non bit-field element
         and that this element must be the first one in the structure.  */

      /* Find the first field, ignoring non FIELD_DECL things which will
         have been created by C++.  */
      for (field = TYPE_FIELDS (type);
	   field && TREE_CODE (field) != FIELD_DECL;
	   field = TREE_CHAIN (field))
	continue;

      if (field == NULL)
	return 0;		/* An empty structure.  Allowed by an extension to ANSI C.  */

      /* Check that the first field is valid for returning in a register.  */

      /* ... Floats are not allowed */
      if (FLOAT_TYPE_P (TREE_TYPE (field)))
	return 1;

      /* ... Aggregates that are not themselves valid for returning in
         a register are not allowed.  */
      if (TARGET_RETURN_IN_MEMORY (TREE_TYPE (field)))
	return 1;

      /* Now check the remaining fields, if any.  Only bitfields are allowed,
         since they are not addressable.  */
      for (field = TREE_CHAIN (field); field; field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  if (!DECL_BIT_FIELD_TYPE (field))
	    return 1;
	}

      return 0;
    }

  if (TREE_CODE (type) == UNION_TYPE)
    {
      tree field;

      /* Unions can be returned in registers if every element is
         integral, or can be returned in an integer register.  */
      for (field = TYPE_FIELDS (type); field; field = TREE_CHAIN (field))
	{
	  if (TREE_CODE (field) != FIELD_DECL)
	    continue;

	  if (FLOAT_TYPE_P (TREE_TYPE (field)))
	    return 1;

	  if (TARGET_RETURN_IN_MEMORY (TREE_TYPE (field)))
	    return 1;
	}

      return 0;
    }
#endif /* not UNICORE64_WINCE */

  /* Return all other types in memory.  */
  return 1;
}

/* Initialize a variable CUM of type CUMULATIVE_ARGS
   for a call to a function whose data type is FNTYPE.
   For a library call, FNTYPE is NULL.  */
// huangping: used in unicore64.h
void
unicore64_init_cumulative_args (CUMULATIVE_ARGS * pcum,
				tree fntype,
				rtx libname ATTRIBUTE_UNUSED,
				tree indirect ATTRIBUTE_UNUSED)
{
  /* On the UNICORE64, the offset starts at 0.  */
  pcum->nregs = 0;

/* star delete it*/
/*  pcum->nregs = (( fntype && aggregate_value_p (TREE_TYPE  (fntype), fntype) )? 1 : 0); */

  pcum->call_cookie = CALL_NORMAL;

  if (TARGET_LONG_CALLS)
    pcum->call_cookie = CALL_LONG;

  /* Check for long call/short call attributes.  The attributes
     override any command line option.  */
  if (fntype)
    {
      if (lookup_attribute ("short_call", TYPE_ATTRIBUTES (fntype)))
	pcum->call_cookie = CALL_SHORT;
      else if (lookup_attribute ("long_call", TYPE_ATTRIBUTES (fntype)))
	pcum->call_cookie = CALL_LONG;
    }
}

/* Determine where to put an argument to a function.
   Value is zero to push the argument on the stack,
   or a hard register in which to store the argument.

   MODE is the argument's machine mode.
   TYPE is the data type of the argument (as a tree).
    This is null for libcalls where that information may
    not be available.
   CUM is a variable of type CUMULATIVE_ARGS which gives info about
    the preceding args and about the function being called.
   NAMED is nonzero if this argument is a named parameter
    (otherwise it is an extra parameter matching an ellipsis).  */

rtx
unicore64_function_arg (CUMULATIVE_ARGS * pcum,
			enum machine_mode mode,
			tree type ATTRIBUTE_UNUSED, int named)
{
  if (mode == VOIDmode)
    /* Compute operand 2 of the call insn.  */
    return GEN_INT (pcum->call_cookie);

  if (!named || pcum->nregs >= NUM_ARG_REGS)
    return NULL_RTX;

  return gen_rtx_REG (mode, pcum->nregs);
}

/* Encode the current state of the #pragma [no_]long_calls.  */
typedef enum
{
  OFF,				/* No #pramgma [no_]long_calls is in effect.  */
  LONG,				/* #pragma long_calls is in effect.  */
  SHORT				/* #pragma no_long_calls is in effect.  */
} unicore64_pragma_enum;

static unicore64_pragma_enum unicore64_pragma_long_calls = OFF;

void
unicore64_pr_long_calls (cpp_reader * pfile ATTRIBUTE_UNUSED)
{
  unicore64_pragma_long_calls = LONG;
}

void
unicore64_pr_no_long_calls (cpp_reader * pfile ATTRIBUTE_UNUSED)
{
  unicore64_pragma_long_calls = SHORT;
}

void
unicore64_pr_long_calls_off (cpp_reader * pfile ATTRIBUTE_UNUSED)
{
  unicore64_pragma_long_calls = OFF;
}

/* Table of machine attributes.  */
const struct attribute_spec unicore64_attribute_table[] = {
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  /* Function calls made to this symbol must be done indirectly, because
     it may lie outside of the 26 bit addressing range of a normal function
     call.  */
  {"long_call", 0, 0, false, true, true, NULL},
  /* Whereas these functions are always known to reside within the 26 bit
     addressing range.  */
  {"short_call", 0, 0, false, true, true, NULL},
  /* Interrupt Service Routines have special prologue and epilogue requirements.  */
  {"isr", 0, 1, false, false, false, unicore64_handle_isr_attribute},
  {"interrupt", 0, 1, false, false, false, unicore64_handle_isr_attribute},
  {"naked", 0, 0, true, false, false, unicore64_handle_fndecl_attribute},
#ifdef UNICORE64_PE
  /* UNICORE64/PE has three new attributes:
     interfaceunicore64 - ?
     dllexport - for exporting a function/variable that will live in a dll
     dllimport - for importing a function/variable from a dll

     Microsoft allows multiple declspecs in one __declspec, separating
     them with spaces.  We do NOT support this.  Instead, use __declspec
     multiple times.
   */
  {"dllimport", 0, 0, true, false, false, NULL},
  {"dllexport", 0, 0, true, false, false, NULL},
  {"interfaceunicore64", 0, 0, true, false, false,
   unicore64_handle_fndecl_attribute},
#endif
  {NULL, 0, 0, false, false, false, NULL}
};

/* Handle an attribute requiring a FUNCTION_DECL;
   arguments as in struct attribute_spec.handler.  */

static tree
unicore64_handle_fndecl_attribute (tree * node,
				   tree name,
				   tree args ATTRIBUTE_UNUSED,
				   int flags ATTRIBUTE_UNUSED,
				   bool * no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (0, "`%s' attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}

/* Handle an "interrupt" or "isr" attribute;
   arguments as in struct attribute_spec.handler.  */

static tree
unicore64_handle_isr_attribute (tree * node,
				tree name,
				tree args, int flags, bool * no_add_attrs)
{
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) != FUNCTION_DECL)
	{
	  warning (0, "`%s' attribute only applies to functions",
		   IDENTIFIER_POINTER (name));
	  *no_add_attrs = true;
	}
      /* FIXME: the argument if any is checked for type attributes;
         should it be checked for decl ones?  */
    }
  else
    {
      if (TREE_CODE (*node) == FUNCTION_TYPE
	  || TREE_CODE (*node) == METHOD_TYPE)
	{
	  if (unicore64_isr_value (args) == UNICORE64_FT_UNKNOWN)
	    {
	      warning (0, "`%s' attribute ignored",
		       IDENTIFIER_POINTER (name));
	      *no_add_attrs = true;
	    }
	}
      else if (TREE_CODE (*node) == POINTER_TYPE
	       && (TREE_CODE (TREE_TYPE (*node)) == FUNCTION_TYPE
		   || TREE_CODE (TREE_TYPE (*node)) == METHOD_TYPE)
	       && unicore64_isr_value (args) != UNICORE64_FT_UNKNOWN)
	{
	  *node = build_variant_type_copy (*node);
	  TREE_TYPE (*node) = build_type_attribute_variant
	    (TREE_TYPE (*node),
	     tree_cons (name, args, TYPE_ATTRIBUTES (TREE_TYPE (*node))));
	  *no_add_attrs = true;
	}
      else
	{
	  /* Possibly pass this attribute on from the type to a decl.  */
	  if (flags & ((int) ATTR_FLAG_DECL_NEXT
		       | (int) ATTR_FLAG_FUNCTION_NEXT
		       | (int) ATTR_FLAG_ARRAY_NEXT))
	    {
	      *no_add_attrs = true;
	      return tree_cons (name, args, NULL_TREE);
	    }
	  else
	    {
	      warning (0, "`%s' attribute ignored",
		       IDENTIFIER_POINTER (name));
	    }
	}
    }

  return NULL_TREE;
}

/* Return 0 if the attributes for two types are incompatible, 1 if they
   are compatible, and 2 if they are nearly compatible (which causes a
   warning to be generated).  */

static int
unicore64_comp_type_attributes (tree type1, tree type2)
{
  int l1, l2, s1, s2;

  /* Check for mismatch of non-default calling convention.  */
  if (TREE_CODE (type1) != FUNCTION_TYPE)
    return 1;

  /* Check for mismatched call attributes.  */
  l1 = lookup_attribute ("long_call", TYPE_ATTRIBUTES (type1)) != NULL;
  l2 = lookup_attribute ("long_call", TYPE_ATTRIBUTES (type2)) != NULL;
  s1 = lookup_attribute ("short_call", TYPE_ATTRIBUTES (type1)) != NULL;
  s2 = lookup_attribute ("short_call", TYPE_ATTRIBUTES (type2)) != NULL;

  /* Only bother to check if an attribute is defined.  */
  if (l1 | l2 | s1 | s2)
    {
      /* If one type has an attribute, the other must have the same attribute.  */
      if ((l1 != l2) || (s1 != s2))
	return 0;

      /* Disallow mixed attributes.  */
      if ((l1 & s2) || (l2 & s1))
	return 0;
    }

  /* Check for mismatched ISR attribute.  */
  l1 = lookup_attribute ("isr", TYPE_ATTRIBUTES (type1)) != NULL;
  if (!l1)
    l1 = lookup_attribute ("interrupt", TYPE_ATTRIBUTES (type1)) != NULL;
  l2 = lookup_attribute ("isr", TYPE_ATTRIBUTES (type2)) != NULL;
  if (!l2)
    l1 = lookup_attribute ("interrupt", TYPE_ATTRIBUTES (type2)) != NULL;
  if (l1 != l2)
    return 0;

  return 1;
}

/*  Encode long_call or short_call attribute by prefixing
    symbol name in DECL with a special character FLAG.  */

void
unicore64_encode_call_attribute (tree decl, int flag)
{
  const char *str = XSTR (XEXP (DECL_RTL (decl), 0), 0);
  int len = strlen (str);
  char *newstr;

  /* Do not allow weak functions to be treated as short call.  */
  if (DECL_WEAK (decl) && flag == SHORT_CALL_FLAG_CHAR)
    return;

  newstr = alloca (len + 2);
  newstr[0] = flag;
  strcpy (newstr + 1, str);

  newstr = (char *) ggc_alloc_string (newstr, len + 1);
  XSTR (XEXP (DECL_RTL (decl), 0), 0) = newstr;
}

/*  Assigns default attributes to newly defined type.  This is used to
    set short_call/long_call attributes for function types of
    functions defined inside corresponding #pragma scopes.  */

static void
unicore64_set_default_type_attributes (tree type)
{
  /* Add __attribute__ ((long_call)) to all functions, when
     inside #pragma long_calls or __attribute__ ((short_call)),
     when inside #pragma no_long_calls.  */
  if (TREE_CODE (type) == FUNCTION_TYPE || TREE_CODE (type) == METHOD_TYPE)
    {
      tree type_attr_list, attr_name;
      type_attr_list = TYPE_ATTRIBUTES (type);

      if (unicore64_pragma_long_calls == LONG)
	attr_name = get_identifier ("long_call");
      else if (unicore64_pragma_long_calls == SHORT)
	attr_name = get_identifier ("short_call");
      else
	return;

      type_attr_list = tree_cons (attr_name, NULL_TREE, type_attr_list);
      TYPE_ATTRIBUTES (type) = type_attr_list;
    }
}

/* Return 1 if the operand is a SYMBOL_REF for a function known to be
   defined within the current compilation unit.  If this cannot be
   determined, then 0 is returned.  */

static int
current_file_function_operand (rtx sym_ref)
{
  /* This is a bit of a fib.  A function will have a short call flag
     applied to its name if it has the short call attribute, or it has
     already been defined within the current compilation unit.  */
  if (ENCODED_SHORT_CALL_ATTR_P (XSTR (sym_ref, 0)))
    return 1;

  /* The current function is always defined within the current compilation
     unit.  if it s a weak definition however, then this may not be the real
     definition of the function, and so we have to say no.  */
  if (sym_ref == XEXP (DECL_RTL (current_function_decl), 0)
      && !DECL_WEAK (current_function_decl))
    return 1;

  /* We cannot make the determination - default to returning 0.  */
  return 0;
}

/* Return non-zero if a 32 bit "long_call" should be generated for
   this call.  We generate a long_call if the function:

        a.  has an __attribute__((long call))
     or b.  is within the scope of a #pragma long_calls
     or c.  the -mlong-calls command line switch has been specified

   However we do not generate a long call if the function:
   
        d.  has an __attribute__ ((short_call))
     or e.  is inside the scope of a #pragma no_long_calls
     or f.  has an __attribute__ ((section))
     or g.  is defined within the current compilation unit.
   
   This function will be called by C fragments contained in the machine
   description file.  CALL_REF and CALL_COOKIE correspond to the matched
   rtl operands.  CALL_SYMBOL is used to distinguish between
   two different callers of the function.  It is set to 1 in the
   "call_symbol" and "call_symbol_value" patterns and to 0 in the "call"
   and "call_value" patterns.  This is because of the difference in the
   SYM_REFs passed by these patterns.  */

int
unicore64_is_longcall_p (rtx sym_ref, int call_cookie, int call_symbol)
{
  if (!call_symbol)
    {
      if (GET_CODE (sym_ref) != MEM)
	return 0;

      sym_ref = XEXP (sym_ref, 0);
    }

  if (GET_CODE (sym_ref) != SYMBOL_REF)
    return 0;

  if (call_cookie & CALL_SHORT)
    return 0;

  if (TARGET_LONG_CALLS && flag_function_sections)
    return 1;

  if (current_file_function_operand (sym_ref))
    return 0;

  return (call_cookie & CALL_LONG)
    || ENCODED_LONG_CALL_ATTR_P (XSTR (sym_ref, 0)) || TARGET_LONG_CALLS;
}

/* Return non-zero if it is ok to make a tail-call to DECL.  */

static bool
unicore64_function_ok_for_sibcall (tree decl, tree exp ATTRIBUTE_UNUSED)
{
  int call_type = TARGET_LONG_CALLS ? CALL_LONG : CALL_NORMAL;

  /* Never tailcall something for which we have no decl, or if we
     are in Unicore16 mode.  */
  if (decl == NULL || TARGET_UNICORE16)
    return false;

  /* Get the calling method.  */
  if (lookup_attribute ("short_call", TYPE_ATTRIBUTES (TREE_TYPE (decl))))
    call_type = CALL_SHORT;
  else if (lookup_attribute ("long_call", TYPE_ATTRIBUTES (TREE_TYPE (decl))))
    call_type = CALL_LONG;

  /* Cannot tail-call to long calls, since these are out of range of
     a branch instruction.  However, if not compiling PIC, we know
     we can reach the symbol if it is in this compilation unit.  */
  if (call_type == CALL_LONG && (flag_pic || !TREE_ASM_WRITTEN (decl)))
    return false;

  /* If we are interworking and the function is not declared static
     then we can't tail-call it unless we know that it exists in this 
     compilation unit (since it might be a Unicore16 routine).  */
  if (TARGET_INTERWORK && TREE_PUBLIC (decl) && !TREE_ASM_WRITTEN (decl))
    return false;

  /* Never tailcall from an ISR routine - it needs a special exit sequence.  */
  if (IS_INTERRUPT (unicore64_current_func_type ()))
    return false;

  /* Everything else is ok.  */
  return true;
}


int
legitimate_pic_operand_p (rtx x)
{
  if (CONSTANT_P (x)
      && flag_pic
      && (GET_CODE (x) == SYMBOL_REF
	  || (GET_CODE (x) == CONST
	      && GET_CODE (XEXP (x, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (x, 0), 0)) == SYMBOL_REF)))
    return 0;

  return 1;
}

/* star */
rtx
legitimize_pic_address (rtx orig, enum machine_mode mode, rtx reg)
{
  if (GET_CODE (orig) == SYMBOL_REF || GET_CODE (orig) == LABEL_REF)
    {
      rtx pic_ref, address;
      rtx insn;
      int subregs = 0;

      /* If this function doesn't have a pic register, create one now.
         A lot of the logic here is made obscure by the fact that this
         routine gets called as part of the rtx cost estimation
         process.  We don't want those calls to affect any assumptions
         about the real function; and further, we can't call
         entry_of_function() until we start the real expansion
         process.  */
      if (!crtl->uses_pic_offset_table)
	{
	  gcc_assert (can_create_pseudo_p ());
	  if (unicore64_pic_register != INVALID_REGNUM)
	    {
	      if (current_ir_type () != IR_GIMPLE)
		crtl->uses_pic_offset_table = 1;
	    }
	  else
	    {
	      rtx seq;
	      if (current_ir_type () != IR_GIMPLE)
		{
		  crtl->uses_pic_offset_table = 1;
		  start_sequence ();

		  unicore64_finalize_pic (0);

		  seq = get_insns ();
		  end_sequence ();
		  emit_insn_after (seq, entry_of_function ());
		}
	    }
	}
      if (reg == 0)
	{
	  if (!can_create_pseudo_p ())
	    abort ();
	  else
	    reg = gen_reg_rtx (Pmode);

	  subregs = 1;
	}

#ifdef AOF_ASSEMBLER
      /* The AOF assembler can generate relocations for these directly, and
         understands that the PIC register has to be added into the offset.  */
      insn = emit_insn (gen_pic_load_addr_based (reg, orig));
#else
      if (subregs)
	address = gen_reg_rtx (Pmode);
      else
	address = reg;

      if (TARGET_UNICORE64)
	emit_insn (gen_pic_load_addr_unicore64 (address, orig));
      else
	;

      if ((GET_CODE (orig) == LABEL_REF
	   || (GET_CODE (orig) == SYMBOL_REF &&
	       ENCODED_SHORT_CALL_ATTR_P (XSTR (orig, 0)))) && NEED_GOT_RELOC)
	pic_ref = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, address);
      /* hidden symbol: GOTOFF, Paul, 20090615 */
      else if ((GET_CODE (orig) == SYMBOL_REF)
	       && SYMBOL_REF_DECL (orig) && DECL_P (SYMBOL_REF_DECL (orig))
	       && (DECL_VISIBILITY (SYMBOL_REF_DECL (orig)) == VISIBILITY_HIDDEN)
	       && NEED_GOT_RELOC && ! DECL_EXTERNAL (SYMBOL_REF_DECL (orig)))
	pic_ref = gen_rtx_PLUS (Pmode, pic_offset_table_rtx, address);
      else
	{
	  pic_ref = gen_rtx_MEM (Pmode,
				 gen_rtx_PLUS (Pmode, pic_offset_table_rtx,
					       address));
	  MEM_READONLY_P (pic_ref) = 1;
	}

      insn = emit_move_insn (reg, pic_ref);
#endif
/*	star modify for -fpic,20080423	*/
/*      crtl->uses_pic_offset_table = 1; */
      /* Put a REG_EQUAL note on this insn, so that it can be optimized
         by loop.  */
      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_EQUAL, orig,
					    REG_NOTES (insn));
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, offset;

      if (GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      /* Handle the case where we have: const (UNSPEC_TLS).  */
      if (GET_CODE (XEXP (orig, 0)) == UNSPEC
	  && XINT (XEXP (orig, 0), 1) == UNSPEC_TLS)
	return orig;

      /* Handle the case where we have:
         const (plus (UNSPEC_TLS) (ADDEND)).  The ADDEND must be a
         CONST_INT.  */
      if (GET_CODE (XEXP (orig, 0)) == PLUS
          && GET_CODE (XEXP (XEXP (orig, 0), 0)) == UNSPEC
          && XINT (XEXP (XEXP (orig, 0), 0), 1) == UNSPEC_TLS)
        {
	  gcc_assert (GET_CODE (XEXP (XEXP (orig, 0), 1)) == CONST_INT);
	  return orig;
	}

      if (reg == 0)
	{
	  if (!can_create_pseudo_p ())
	    abort ();
	  else
	    reg = gen_reg_rtx (Pmode);
	}

      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	{
	  base =
	    legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode, reg);
	  offset =
	    legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
				    base == reg ? 0 : reg);
	}
      else
	abort ();

      if (GET_CODE (offset) == CONST_INT)
	{
	  /* The base register doesn't really matter, we only want to
	     test the index for the appropriate mode.  */
	  UNICORE64_GO_IF_LEGITIMATE_INDEX (mode, 0, offset, win);

	  if (can_create_pseudo_p ())
	    offset = force_reg (Pmode, offset);
	  else
	    abort ();

	win:
	  if (GET_CODE (offset) == CONST_INT)
	    return plus_constant (base, INTVAL (offset));
	}

      if (GET_MODE_SIZE (mode) > 4
	  && (GET_MODE_CLASS (mode) == MODE_INT || TARGET_SOFT_FLOAT))
	{
	  emit_insn (gen_adddi3 (reg, base, offset));
	  return reg;
	}

      return gen_rtx_PLUS (Pmode, base, offset);
    }

  return orig;
}

/* Generate code to load the PIC register.  PROLOGUE is true if
   called from unicore64_expand_prologue (in which case we want the 
   generated insns at the start of the function);  false if called
   by an exception receiver that needs the PIC register reloaded
   (in which case the insns are just dumped at the current location).  */
// star 
void
unicore64_finalize_pic (int prologue ATTRIBUTE_UNUSED)
{
  rtx l1, pic_tmp, pic_tmp2, seq, pic_rtx, labelno;
  rtx global_offset_table;

  if (crtl->uses_pic_offset_table == 0 || TARGET_SINGLE_PIC_BASE)
    return;

  gcc_assert (flag_pic);
  
  labelno = GEN_INT (pic_labelno++);
  l1 = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, labelno), UNSPEC_PIC_LABEL);
  l1 = gen_rtx_CONST (VOIDmode, l1);
  pic_rtx = plus_constant (l1, TARGET_UNICORE64 ? 0 : 2);
  pic_rtx = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, pic_rtx),
			    UNSPEC_GOTSYM_OFF);
  pic_rtx = gen_rtx_CONST (Pmode, pic_rtx);

  if (TARGET_UNICORE64)
    {
      emit_insn (gen_pic_load_addr_unicore64 (pic_offset_table_rtx, pic_rtx));
      emit_insn (gen_pic_add_dot_plus_four (pic_offset_table_rtx,
					    pic_offset_table_rtx, labelno));
    }
  emit_use (pic_offset_table_rtx);
}

#define REG_OR_SUBREG_REG(X)						\
  (GET_CODE (X) == REG							\
   || (GET_CODE (X) == SUBREG && GET_CODE (SUBREG_REG (X)) == REG))

#define REG_OR_SUBREG_RTX(X)			\
   (GET_CODE (X) == REG ? (X) : SUBREG_REG (X))

#ifndef COSTS_N_INSNS
#define COSTS_N_INSNS(N) ((N) * 4 - 2)
#endif
static inline int
unicore64_rtx_costs_1 (rtx x, enum rtx_code code, enum rtx_code outer)
{
  enum machine_mode mode = GET_MODE (x);
  enum rtx_code subcode;
  int extra_cost;

  switch (code)
    {
    case MEM:
      /* Memory costs quite a lot for the first word, but subsequent words
         load at the equivalent of a single insn each.  */
      return (10 + 4 * ((GET_MODE_SIZE (mode) - 1) / UNITS_PER_WORD)
	      + (CONSTANT_POOL_ADDRESS_P (x) ? 4 : 0));

    case DIV:
    case MOD:
      return 100;

    case ROTATE:
      if (mode == SImode && GET_CODE (XEXP (x, 1)) == REG)
	return 4;
      /* Fall through */
    case ROTATERT:
      if (mode != SImode)
	return 8;
      /* Fall through */
    case ASHIFT:
    case LSHIFTRT:
    case ASHIFTRT:
      if (mode == DImode)
	return (8 + (GET_CODE (XEXP (x, 1)) == CONST_INT ? 0 : 8)
		+ ((GET_CODE (XEXP (x, 0)) == REG
		    || (GET_CODE (XEXP (x, 0)) == SUBREG
			&& GET_CODE (SUBREG_REG (XEXP (x, 0))) == REG))
		   ? 0 : 8));
      return (1 + ((GET_CODE (XEXP (x, 0)) == REG
		    || (GET_CODE (XEXP (x, 0)) == SUBREG
			&& GET_CODE (SUBREG_REG (XEXP (x, 0))) == REG))
		   ? 0 : 4)
	      + ((GET_CODE (XEXP (x, 1)) == REG
		  || (GET_CODE (XEXP (x, 1)) == SUBREG
		      && GET_CODE (SUBREG_REG (XEXP (x, 1))) == REG)
		  || (GET_CODE (XEXP (x, 1)) == CONST_INT)) ? 0 : 4));

    case MINUS:
      if (mode == DImode)
	return (4 + (REG_OR_SUBREG_REG (XEXP (x, 1)) ? 0 : 8)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 0))
		    || (GET_CODE (XEXP (x, 0)) == CONST_INT
			&& const_ok_for_unicore64 (INTVAL (XEXP (x, 0)))))
		   ? 0 : 8));

      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	return (2 + ((REG_OR_SUBREG_REG (XEXP (x, 1))
		      || (GET_CODE (XEXP (x, 1)) == CONST_DOUBLE
			  && const_double_rtx_ok_for_fpu (XEXP (x, 1))))
		     ? 0 : 8)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 0))
		    || (GET_CODE (XEXP (x, 0)) == CONST_DOUBLE
			&& const_double_rtx_ok_for_fpu (XEXP (x, 0))))
		   ? 0 : 8));

      if (((GET_CODE (XEXP (x, 0)) == CONST_INT
	    && const_ok_for_unicore64 (INTVAL (XEXP (x, 0)))
	    && REG_OR_SUBREG_REG (XEXP (x, 1))))
	  || (((subcode = GET_CODE (XEXP (x, 1))) == ASHIFT
	       || subcode == ASHIFTRT || subcode == LSHIFTRT
	       || subcode == ROTATE || subcode == ROTATERT
	       || (subcode == MULT
		   && GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT
		   && ((INTVAL (XEXP (XEXP (x, 1), 1)) &
			(INTVAL (XEXP (XEXP (x, 1), 1)) - 1)) == 0)))
	      && REG_OR_SUBREG_REG (XEXP (XEXP (x, 1), 0))
	      && (REG_OR_SUBREG_REG (XEXP (XEXP (x, 1), 1))
		  || GET_CODE (XEXP (XEXP (x, 1), 1)) == CONST_INT)
	      && REG_OR_SUBREG_REG (XEXP (x, 0))))
	return 1;
      /* Fall through */

    case PLUS:
      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	return (2 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 8)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 1))
		    || (GET_CODE (XEXP (x, 1)) == CONST_DOUBLE
			&& const_double_rtx_ok_for_fpu (XEXP (x, 1))))
		   ? 0 : 8));

      /* Fall through */
    case AND:
    case XOR:
    case IOR:
      extra_cost = 0;

      /* Normally the frame registers will be spilt into reg+const during
         reload, so it is a bad idea to combine them with other instructions,
         since then they might not be moved outside of loops.  As a compromise
         we allow integration with ops that have a constant as their second
         operand.  */
      if ((REG_OR_SUBREG_REG (XEXP (x, 0))
	   && UNICORE64_FRAME_RTX (REG_OR_SUBREG_RTX (XEXP (x, 0)))
	   && GET_CODE (XEXP (x, 1)) != CONST_INT)
	  || (REG_OR_SUBREG_REG (XEXP (x, 0))
	      && UNICORE64_FRAME_RTX (REG_OR_SUBREG_RTX (XEXP (x, 0)))))
	extra_cost = 4;

      if (mode == DImode)
	return (4 + extra_cost + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 8)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 1))
		    || (GET_CODE (XEXP (x, 1)) == CONST_INT
			&& const_ok_for_op (INTVAL (XEXP (x, 1)), code)))
		   ? 0 : 8));

      if (REG_OR_SUBREG_REG (XEXP (x, 0)))
	return (1 + (GET_CODE (XEXP (x, 1)) == CONST_INT ? 0 : extra_cost)
		+ ((REG_OR_SUBREG_REG (XEXP (x, 1))
		    || (GET_CODE (XEXP (x, 1)) == CONST_INT
			&& const_ok_for_op (INTVAL (XEXP (x, 1)), code)))
		   ? 0 : 4));

      else if (REG_OR_SUBREG_REG (XEXP (x, 1)))
	return (1 + extra_cost
		+ ((((subcode = GET_CODE (XEXP (x, 0))) == ASHIFT
		     || subcode == LSHIFTRT || subcode == ASHIFTRT
		     || subcode == ROTATE || subcode == ROTATERT
		     || (subcode == MULT
			 && GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT
			 && ((INTVAL (XEXP (XEXP (x, 0), 1)) &
			      (INTVAL (XEXP (XEXP (x, 0), 1)) - 1)) == 0)))
		    && (REG_OR_SUBREG_REG (XEXP (XEXP (x, 0), 0)))
		    && ((REG_OR_SUBREG_REG (XEXP (XEXP (x, 0), 1)))
			|| GET_CODE (XEXP (XEXP (x, 0), 1)) == CONST_INT))
		   ? 0 : 4));

      return 8;

    case MULT:
      /* There is no point basing this on the tuning, since it is always the
         fast variant if it exists at all.  */
      if (unicore64_fast_multiply && mode == DImode
	  && (GET_CODE (XEXP (x, 0)) == GET_CODE (XEXP (x, 1)))
	  && (GET_CODE (XEXP (x, 0)) == ZERO_EXTEND
	      || GET_CODE (XEXP (x, 0)) == SIGN_EXTEND))
	return 8;

      if (GET_MODE_CLASS (mode) == MODE_FLOAT || mode == DImode)
	return 30;

      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	{
	  unsigned HOST_WIDE_INT i = (INTVAL (XEXP (x, 1))
				      & (unsigned HOST_WIDE_INT) -1);
	  int add_cost = 0;
	  int j;
	  int booth_unit_size = 9;
	  if (const_ok_for_unicore64 (i))
	    return 3;
	  for (j = 0; i && j < 32; j += booth_unit_size)
	    {
	      i >>= booth_unit_size;
	      add_cost += 1;
	      if (const_ok_for_unicore64 (i))
		{
		  add_cost += 1;
		  break;
		}
	    }
	  return add_cost + 2;
	}

      return (((tune_flags & FL_FAST_MULT) ? 8 : 30)
	      + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 4)
	      + (REG_OR_SUBREG_REG (XEXP (x, 1)) ? 0 : 4));

    case TRUNCATE:
      if (unicore64_fast_multiply && mode == SImode
	  && GET_CODE (XEXP (x, 0)) == LSHIFTRT
	  && GET_CODE (XEXP (XEXP (x, 0), 0)) == MULT
	  && (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0))
	      == GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 1)))
	  && (GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == ZERO_EXTEND
	      || GET_CODE (XEXP (XEXP (XEXP (x, 0), 0), 0)) == SIGN_EXTEND))
	return 8;
      return 99;

    case NEG:
      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	return 4 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 6);
      /* Fall through */
    case NOT:
      if (mode == DImode)
	return 4 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 4);

      return 1 + (REG_OR_SUBREG_REG (XEXP (x, 0)) ? 0 : 4);

    case IF_THEN_ELSE:
      if (GET_CODE (XEXP (x, 1)) == PC || GET_CODE (XEXP (x, 2)) == PC)
	return 14;
      return 2;

    case COMPARE:
      return 1;

    case ABS:
      return 4 + (mode == DImode ? 4 : 0);

    case SIGN_EXTEND:
      if (GET_MODE (XEXP (x, 0)) == QImode)
	return (4 + (mode == DImode ? 4 : 0)
		+ (GET_CODE (XEXP (x, 0)) == MEM ? 10 : 0));
      /* Fall through */
    case ZERO_EXTEND:
      switch (GET_MODE (XEXP (x, 0)))
	{
	case QImode:
	  return (1 + (mode == DImode ? 4 : 0)
		  + (GET_CODE (XEXP (x, 0)) == MEM ? 10 : 0));

	case HImode:
	  return (4 + (mode == DImode ? 4 : 0)
		  + (GET_CODE (XEXP (x, 0)) == MEM ? 10 : 0));

	case SImode:
	  return (1 + (GET_CODE (XEXP (x, 0)) == MEM ? 10 : 0));

	default:
	  break;
	}
      abort ();

    case CONST_INT:
      if (const_ok_for_unicore64 (INTVAL (x)))
	return outer == SET ? 2 : -1;
      else if (outer == AND && const_ok_for_unicore64 (~INTVAL (x)))
	return -1;
      else if ((outer == COMPARE
		|| outer == PLUS || outer == MINUS)
	       && const_ok_for_unicore64 (-INTVAL (x)))
	return -1;
      else
	return 5;

    case CONST:
    case LABEL_REF:
    case SYMBOL_REF:
      return 6;

    case CONST_DOUBLE:
      if (const_double_rtx_ok_for_fpu (x))
	return outer == SET ? 2 : -1;
      else if ((outer == COMPARE || outer == PLUS)
	       && neg_const_double_rtx_ok_for_fpu (x))
	return -1;
      return 7;

    default:
      return 99;
    }
}

static bool
unicore64_rtx_costs (rtx x, int code, int outer_code, int *total)
{
  *total = unicore64_rtx_costs_1 (x, code, outer_code);
  return true;
}

/* All address computations that can be done are free, but rtx cost returns
   the same for practically all of them.  So we weight the different types
   of address here in the order (most pref first):
   PRE/POST_INC/DEC, SHIFT or NON-INT sum, INT sum, REG, MEM or LABEL. */

static int
unicore64_address_cost (rtx X)
{
#define UNICORE64_ADDRESS_COST(X)						     \
  (10 - ((GET_CODE (X) == MEM || GET_CODE (X) == LABEL_REF		     \
	  || GET_CODE (X) == SYMBOL_REF)				     \
	 ? 0								     \
	 : ((GET_CODE (X) == PRE_INC || GET_CODE (X) == PRE_DEC		     \
	     || GET_CODE (X) == POST_INC || GET_CODE (X) == POST_DEC)	     \
	    ? 10							     \
	    : (((GET_CODE (X) == PLUS || GET_CODE (X) == MINUS)		     \
		? 6 + (GET_CODE (XEXP (X, 1)) == CONST_INT ? 2 		     \
		       : ((GET_RTX_CLASS (GET_CODE (XEXP (X, 0))) == '2'     \
			   || GET_RTX_CLASS (GET_CODE (XEXP (X, 0))) == 'c'  \
			   || GET_RTX_CLASS (GET_CODE (XEXP (X, 1))) == '2'  \
			   || GET_RTX_CLASS (GET_CODE (XEXP (X, 1))) == 'c') \
			  ? 1 : 0))					     \
		: 4)))))

#define UNICORE16_ADDRESS_COST(X) 					\
  ((GET_CODE (X) == REG 					\
    || (GET_CODE (X) == PLUS && GET_CODE (XEXP (X, 0)) == REG	\
	&& GET_CODE (XEXP (X, 1)) == CONST_INT))		\
   ? 1 : 2)

  return (TARGET_UNICORE64 ? UNICORE64_ADDRESS_COST (X) :
	  UNICORE16_ADDRESS_COST (X));
}

static int
unicore64_adjust_cost (rtx insn, rtx link, rtx dep, int cost)
{
  rtx i_pat, d_pat;


  /* XXX This is not strictly true for the FPA.  */
  if (REG_NOTE_KIND (link) == REG_DEP_ANTI
      || REG_NOTE_KIND (link) == REG_DEP_OUTPUT)
    return 0;

  /* Call insns don't incur a stall, even if they follow a load.  */
  if (REG_NOTE_KIND (link) == 0 && GET_CODE (insn) == CALL_INSN)
    return 1;

  if ((i_pat = single_set (insn)) != NULL
      && GET_CODE (SET_SRC (i_pat)) == MEM
      && (d_pat = single_set (dep)) != NULL
      && GET_CODE (SET_DEST (d_pat)) == MEM)
    {
      /* This is a load after a store, there is no conflict if the load reads
         from a cached area.  Assume that loads from the stack, and from the
         constant pool are cached, and that others will miss.  This is a 
         hack.  */

      if (CONSTANT_POOL_ADDRESS_P (XEXP (SET_SRC (i_pat), 0))
	  || reg_mentioned_p (stack_pointer_rtx, XEXP (SET_SRC (i_pat), 0))
	  || reg_mentioned_p (frame_pointer_rtx, XEXP (SET_SRC (i_pat), 0))
	  || reg_mentioned_p (hard_frame_pointer_rtx,
			      XEXP (SET_SRC (i_pat), 0)))
	return 1;
    }

  return cost;
}

/* This code has been fixed for cross compilation.  */

static int fpa_consts_inited = 0;

static const char *const strings_fpa[8] = {
  "0", "1", "2", "3",
  "4", "5", "0.5", "10"
};

static REAL_VALUE_TYPE values_fpa[8];

static void
init_fpa_table (void)
{
  int i;
  REAL_VALUE_TYPE r;

  for (i = 0; i < 8; i++)
    {
      r = REAL_VALUE_ATOF (strings_fpa[i], DFmode);
      values_fpa[i] = r;
    }

  fpa_consts_inited = 1;
}

/* Return TRUE if rtx X is a valid immediate FPU constant.  */
int
const_double_rtx_ok_for_fpu (rtx x)
{
  REAL_VALUE_TYPE r;

  if (!fpa_consts_inited)
    init_fpa_table ();

  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  if (REAL_VALUE_MINUS_ZERO (r))
    return 0;
  return 0;
}

/* Return TRUE if rtx X is a valid immediate FPU constant.  */

int
neg_const_double_rtx_ok_for_fpu (rtx x)
{
  REAL_VALUE_TYPE r;

  if (!fpa_consts_inited)
    init_fpa_table ();

  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  r = REAL_VALUE_NEGATE (r);
  if (REAL_VALUE_MINUS_ZERO (r))
    return 0;
  return 0;
}

/* Return TRUE if X references a SYMBOL_REF.  */

int
symbol_mentioned_p (rtx x)
{
  const char *fmt;
  int i;

  if (GET_CODE (x) == SYMBOL_REF)
    return 1;

  /* UNSPEC_TLS entries for a symbol include the SYMBOL_REF, but they
     are constant offsets, not symbols.  */
  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
    return 0;

  fmt = GET_RTX_FORMAT (GET_CODE (x));

  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (symbol_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && symbol_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}

/* Return TRUE if X references a LABEL_REF.  */

int
label_mentioned_p (rtx x)
{
  const char *fmt;
  int i;

  if (GET_CODE (x) == LABEL_REF)
    return 1;

  /* UNSPEC_TLS entries for a symbol include a LABEL_REF for the referencing
     instruction, but they are constant offsets, not symbols.  */
  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
    return 0;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (label_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && label_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}

int
tls_mentioned_p (rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST:
      return tls_mentioned_p (XEXP (x, 0));

    case UNSPEC:
      if (XINT (x, 1) == UNSPEC_TLS)
	return 1;

    default:
      return 0;
    }
}

enum rtx_code
minmax_code (rtx x)
{
  enum rtx_code code = GET_CODE (x);

  if (code == SMAX)
    return GE;
  else if (code == SMIN)
    return LE;
  else if (code == UMIN)
    return LEU;
  else if (code == UMAX)
    return GEU;

  abort ();
}

/* Return 1 if memory locations are adjacent.  */

// huangping: modify
int
adjacent_mem_locations (rtx a, rtx b)
{
  if ((GET_CODE (XEXP (a, 0)) == REG
       || (GET_CODE (XEXP (a, 0)) == PLUS
	   && GET_CODE (XEXP (XEXP (a, 0), 1)) == CONST_INT))
      && (GET_CODE (XEXP (b, 0)) == REG
	  || (GET_CODE (XEXP (b, 0)) == PLUS
	      && GET_CODE (XEXP (XEXP (b, 0), 1)) == CONST_INT)))
    {
      int val0 = 0, val1 = 0;
      int reg0, reg1;

      if (GET_CODE (XEXP (a, 0)) == PLUS)
	{
	  reg0 = REGNO (XEXP (XEXP (a, 0), 0));
	  val0 = INTVAL (XEXP (XEXP (a, 0), 1));
	}
      else
	reg0 = REGNO (XEXP (a, 0));

      if (GET_CODE (XEXP (b, 0)) == PLUS)
	{
	  reg1 = REGNO (XEXP (XEXP (b, 0), 0));
	  val1 = INTVAL (XEXP (XEXP (b, 0), 1));
	}
      else
	reg1 = REGNO (XEXP (b, 0));

      return (reg0 == reg1) && ((val1 - val0) == 8 || (val0 - val1) == 8);
    }
  return 0;
}



//int
//unicore64_gen_movmemqi (rtx * operands)
//{
//  HOST_WIDE_INT in_words_to_go, out_words_to_go, last_bytes;
//  int i;
//  rtx src, dst;
//  rtx st_src, st_dst, fin_src, fin_dst;
//  rtx part_bytes_reg = NULL;
//  rtx mem;
//  int dst_unchanging_p, dst_in_struct_p, src_unchanging_p, src_in_struct_p;
//  int dst_scalar_p, src_scalar_p;
//
//  if (GET_CODE (operands[2]) != CONST_INT
//      || GET_CODE (operands[3]) != CONST_INT
//      || INTVAL (operands[2]) > 64 || INTVAL (operands[3]) & 3)
//    return 0;
//
//  st_dst = XEXP (operands[0], 0);
//  st_src = XEXP (operands[1], 0);
//
//  dst_unchanging_p = MEM_READONLY_P (operands[0]);
//  dst_in_struct_p = MEM_IN_STRUCT_P (operands[0]);
//  dst_scalar_p = MEM_SCALAR_P (operands[0]);
//  src_unchanging_p = MEM_READONLY_P (operands[1]);
//  src_in_struct_p = MEM_IN_STRUCT_P (operands[1]);
//  src_scalar_p = MEM_SCALAR_P (operands[1]);
//
//  fin_dst = dst = copy_to_mode_reg (SImode, st_dst);
//  fin_src = src = copy_to_mode_reg (SImode, st_src);
//
//  in_words_to_go = NUM_INTS (INTVAL (operands[2]));
//  out_words_to_go = INTVAL (operands[2]) / 4;
//  last_bytes = INTVAL (operands[2]) & 3;
//
//  if (out_words_to_go != in_words_to_go && ((in_words_to_go - 1) & 3) != 0)
//    part_bytes_reg = gen_rtx_REG (SImode, (in_words_to_go - 1) & 3);
//
//  for (i = 0; in_words_to_go >= 2; i += 4)
//    {
//      if (in_words_to_go > 4)
//	emit_insn (unicore64_gen_load_multiple (0, 4, src, TRUE, TRUE,
//						src_unchanging_p,
//						src_in_struct_p,
//						src_scalar_p));
//      else
//	emit_insn (unicore64_gen_load_multiple (0, in_words_to_go, src, TRUE,
//						FALSE, src_unchanging_p,
//						src_in_struct_p,
//						src_scalar_p));
//
//      if (out_words_to_go)
//	{
//	  if (out_words_to_go > 4)
//	    emit_insn (unicore64_gen_store_multiple (0, 4, dst, TRUE, TRUE,
//						     dst_unchanging_p,
//						     dst_in_struct_p,
//						     dst_scalar_p));
//	  else if (out_words_to_go != 1)
//	    emit_insn (unicore64_gen_store_multiple (0, out_words_to_go,
//						     dst, TRUE,
//						     (last_bytes == 0
//						      ? FALSE : TRUE),
//						     dst_unchanging_p,
//						     dst_in_struct_p,
//						     dst_scalar_p));
//	  else
//	    {
//	      mem = gen_rtx_MEM (SImode, dst);
//	      MEM_READONLY_P (mem) = dst_unchanging_p;
//	      MEM_IN_STRUCT_P (mem) = dst_in_struct_p;
//	      MEM_SCALAR_P (mem) = dst_scalar_p;
//	      emit_move_insn (mem, gen_rtx_REG (SImode, 0));
//	      if (last_bytes != 0)
//		emit_insn (gen_addsi3 (dst, dst, GEN_INT (4)));
//	    }
//	}
//
//      in_words_to_go -= in_words_to_go < 4 ? in_words_to_go : 4;
//      out_words_to_go -= out_words_to_go < 4 ? out_words_to_go : 4;
//    }
//
//  /* OUT_WORDS_TO_GO will be zero here if there are byte stores to do.  */
//  if (out_words_to_go)
//    {
//      rtx sreg;
//
//      mem = gen_rtx_MEM (SImode, src);
//      MEM_READONLY_P (mem) = src_unchanging_p;
//      MEM_IN_STRUCT_P (mem) = src_in_struct_p;
//      MEM_SCALAR_P (mem) = src_scalar_p;
//      emit_move_insn (sreg = gen_reg_rtx (SImode), mem);
//      emit_move_insn (fin_src = gen_reg_rtx (SImode), plus_constant (src, 4));
//
//      mem = gen_rtx_MEM (SImode, dst);
//      MEM_READONLY_P (mem) = dst_unchanging_p;
//      MEM_IN_STRUCT_P (mem) = dst_in_struct_p;
//      MEM_SCALAR_P (mem) = dst_scalar_p;
//      emit_move_insn (mem, sreg);
//      emit_move_insn (fin_dst = gen_reg_rtx (SImode), plus_constant (dst, 4));
//      in_words_to_go--;
//
//      if (in_words_to_go)	/* Sanity check */
//	abort ();
//    }
//
//  if (in_words_to_go)
//    {
//      if (in_words_to_go < 0)
//	abort ();
//
//      mem = gen_rtx_MEM (SImode, src);
//      MEM_READONLY_P (mem) = src_unchanging_p;
//      MEM_IN_STRUCT_P (mem) = src_in_struct_p;
//      MEM_SCALAR_P (mem) = src_scalar_p;
//      part_bytes_reg = copy_to_mode_reg (SImode, mem);
//    }
//
//  if (last_bytes && part_bytes_reg == NULL)
//    abort ();
//
//  if (BYTES_BIG_ENDIAN && last_bytes)
//    {
//      rtx tmp = gen_reg_rtx (SImode);
//
//      /* The bytes we want are in the top end of the word.  */
//      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg,
//			      GEN_INT (8 * (4 - last_bytes))));
//      part_bytes_reg = tmp;
//
//      while (last_bytes)
//	{
//	  mem = gen_rtx_MEM (QImode, plus_constant (dst, last_bytes - 1));
//	  MEM_READONLY_P (mem) = dst_unchanging_p;
//	  MEM_IN_STRUCT_P (mem) = dst_in_struct_p;
//	  MEM_SCALAR_P (mem) = dst_scalar_p;
//	  emit_move_insn (mem, gen_lowpart (QImode, part_bytes_reg));
//
//	  if (--last_bytes)
//	    {
//	      tmp = gen_reg_rtx (SImode);
//	      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg, GEN_INT (8)));
//	      part_bytes_reg = tmp;
//	    }
//	}
//
//    }
//  else
//    {
//      if (last_bytes > 1)
//	{
//	  mem = gen_rtx_MEM (HImode, dst);
//	  MEM_READONLY_P (mem) = dst_unchanging_p;
//	  MEM_IN_STRUCT_P (mem) = dst_in_struct_p;
//	  MEM_SCALAR_P (mem) = dst_scalar_p;
//	  emit_move_insn (mem, gen_lowpart (HImode, part_bytes_reg));
//	  last_bytes -= 2;
//	  if (last_bytes)
//	    {
//	      rtx tmp = gen_reg_rtx (SImode);
//
//	      emit_insn (gen_addsi3 (dst, dst, GEN_INT (2)));
//	      emit_insn (gen_lshrsi3 (tmp, part_bytes_reg, GEN_INT (16)));
//	      part_bytes_reg = tmp;
//	    }
//	}
//
//      if (last_bytes)
//	{
//	  mem = gen_rtx_MEM (QImode, dst);
//	  MEM_READONLY_P (mem) = dst_unchanging_p;
//	  MEM_IN_STRUCT_P (mem) = dst_in_struct_p;
//	  MEM_SCALAR_P (mem) = dst_scalar_p;
//	  emit_move_insn (mem, gen_lowpart (QImode, part_bytes_reg));
//	}
//    }
//
//  return 1;
//}

/* star */
static enum machine_mode
select_dominance_cc_mode (rtx x, rtx y, HOST_WIDE_INT cond_or)
{
  enum rtx_code cond1, cond2;
  int swapped = 0;

  /* Currently we will probably get the wrong result if the individual
     comparisons are not simple.  This also ensures that it is safe to
     reverse a comparison if necessary.  */
  if ((unicore64_select_cc_mode
       (cond1 = GET_CODE (x), XEXP (x, 0), XEXP (x, 1)) != CCmode)
      ||
      (unicore64_select_cc_mode
       (cond2 = GET_CODE (y), XEXP (y, 0), XEXP (y, 1)) != CCmode))
    return CCmode;

  /* The if_then_else variant of this tests the second condition if the
     first passes, but is true if the first fails.  Reverse the first
     condition to get a true "inclusive-or" expression.  */
  if (cond_or == 1)
    cond1 = reverse_condition (cond1);

  /* If the comparisons are not equal, and one doesn't dominate the other,
     then we can't do this.  */
  if (cond1 != cond2
      && !comparison_dominates_p (cond1, cond2)
      && (swapped = 1, !comparison_dominates_p (cond2, cond1)))
    return CCmode;

  if (swapped)
    {
      enum rtx_code temp = cond1;
      cond1 = cond2;
      cond2 = temp;
    }

  switch (cond1)
    {
    case EQ:
      if (cond2 == EQ || !cond_or)
	return CC_DEQmode;

      switch (cond2)
	{
	case LE:
	  return CC_DLEmode;
	case LEU:
	  return CC_DLEUmode;
	case GE:
	  return CC_DGEmode;
	case GEU:
	  return CC_DGEUmode;
	default:
	  break;
	}

      break;

    case LT:
      if (cond2 == LT || !cond_or)
	return CC_DLTmode;
      if (cond2 == LE)
	return CC_DLEmode;
      if (cond2 == NE)
	return CC_DNEmode;
      break;

    case GT:
      if (cond2 == GT || !cond_or)
	return CC_DGTmode;
      if (cond2 == GE)
	return CC_DGEmode;
      if (cond2 == NE)
	return CC_DNEmode;
      break;

    case LTU:
      if (cond2 == LTU || !cond_or)
	return CC_DLTUmode;
      if (cond2 == LEU)
	return CC_DLEUmode;
      if (cond2 == NE)
	return CC_DNEmode;
      break;

    case GTU:
      if (cond2 == GTU || !cond_or)
	return CC_DGTUmode;
      if (cond2 == GEU)
	return CC_DGEUmode;
      if (cond2 == NE)
	return CC_DNEmode;
      break;

      /* The remaining cases only occur when both comparisons are the
         same.  */
    case NE:
      return CC_DNEmode;

    case LE:
      return CC_DLEmode;

    case GE:
      return CC_DGEmode;

    case LEU:
      return CC_DLEUmode;

    case GEU:
      return CC_DGEUmode;

    default:
      break;
    }

  abort ();
}

/* star */
enum machine_mode
unicore64_select_cc_mode (enum rtx_code op, rtx x, rtx y)
{
  /* All floating point compares return CCFP if it is an equality
     comparison, and CCFPE otherwise.  */
  if (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT)
    {
      switch (op)
	{
	case EQ:
	case NE:
	case UNORDERED:
	case ORDERED:
	case UNLT:
	case UNLE:
	case UNGT:
	case UNGE:
	case UNEQ:
	case LTGT:
	  return CCFPmode;

	case LT:
	case LE:
	case GT:
	case GE:
	  return CCFPEmode;

	default:
	  abort ();
	}
    }

  /* A compare with a shifted operand.  Because of canonicalization, the
     comparison will have to be swapped when we emit the assembler.  */
  if (GET_MODE (y) == DImode && GET_CODE (y) == REG
      && (GET_CODE (x) == ASHIFT || GET_CODE (x) == ASHIFTRT
	  || GET_CODE (x) == LSHIFTRT || GET_CODE (x) == ROTATE
	  || GET_CODE (x) == ROTATERT))
    return CC_SWPmode;

  /* This is a special case that is used by combine to allow a 
     comparison of a shifted byte load to be split into a zero-extend
     followed by a comparison of the shifted integer (only valid for
     equalities and unsigned inequalities).  */
  if (GET_MODE (x) == DImode
      && GET_CODE (x) == ASHIFT
      && GET_CODE (XEXP (x, 1)) == CONST_INT && INTVAL (XEXP (x, 1)) == 24
      && GET_CODE (XEXP (x, 0)) == SUBREG
      && GET_CODE (SUBREG_REG (XEXP (x, 0))) == MEM
      && GET_MODE (SUBREG_REG (XEXP (x, 0))) == QImode
      && (op == EQ || op == NE
	  || op == GEU || op == GTU || op == LTU || op == LEU)
      && GET_CODE (y) == CONST_INT)
    return CC_Zmode;

  /* A construct for a conditional compare, if the false unicore64 contains
     0, then both conditions must be true, otherwise either condition
     must be true.  Not all conditions are possible, so CCmode is
     returned if it can't be done.  */
  if (GET_CODE (x) == IF_THEN_ELSE
      && (XEXP (x, 2) == const0_rtx
	  || XEXP (x, 2) == const1_rtx)
      && GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
      && GET_RTX_CLASS (GET_CODE (XEXP (x, 1))) == '<')
    return select_dominance_cc_mode (XEXP (x, 0), XEXP (x, 1),
				     INTVAL (XEXP (x, 2)));

  /* Alternate canonicalizations of the above.  These are somewhat cleaner.  */
  if (GET_CODE (x) == AND
      && GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
      && GET_RTX_CLASS (GET_CODE (XEXP (x, 1))) == '<')
    return select_dominance_cc_mode (XEXP (x, 0), XEXP (x, 1), 0);

  if (GET_CODE (x) == IOR
      && GET_RTX_CLASS (GET_CODE (XEXP (x, 0))) == '<'
      && GET_RTX_CLASS (GET_CODE (XEXP (x, 1))) == '<')
    return select_dominance_cc_mode (XEXP (x, 0), XEXP (x, 1), 2);

  /* An operation that sets the condition codes as a side-effect, the
     V flag is not set correctly, so we can only use comparisons where
     this doesn't matter.  (For LT and GE we can use "mi" and "pl"
     instead.  */
  if (GET_MODE (x) == DImode
      && y == const0_rtx
      && (op == EQ || op == NE || op == LT || op == GE)
      && (GET_CODE (x) == PLUS || GET_CODE (x) == MINUS
	  || GET_CODE (x) == AND || GET_CODE (x) == IOR
	  || GET_CODE (x) == XOR || GET_CODE (x) == MULT
	  || GET_CODE (x) == NOT || GET_CODE (x) == NEG
	  || GET_CODE (x) == LSHIFTRT
	  || GET_CODE (x) == ASHIFT || GET_CODE (x) == ASHIFTRT
	  || GET_CODE (x) == ROTATERT || GET_CODE (x) == ZERO_EXTRACT))
    return CC_NOOVmode;

  if (GET_MODE (x) == QImode && (op == EQ || op == NE))
    return CC_Zmode;

  if (GET_MODE (x) == DImode && (op == LTU || op == GEU)
      && GET_CODE (x) == PLUS
      && (rtx_equal_p (XEXP (x, 0), y) || rtx_equal_p (XEXP (x, 1), y)))
    return CC_Cmode;

  return CCmode;
}

/* X and Y are two things to compare using CODE.  Emit the compare insn and
   return the rtx for register 0 in the proper mode.  FP means this is a
   floating point compare: I don't think that it is needed on the unicore64.  */
/* star */
rtx
unicore64_gen_compare_reg (enum rtx_code code, rtx x, rtx y)
{
  enum machine_mode mode = SELECT_CC_MODE (code, x, y);
  rtx cc_reg = gen_rtx_REG (mode, CC_REGNUM);

  emit_insn (gen_rtx_SET (VOIDmode, cc_reg, gen_rtx_COMPARE (mode, x, y)));

  return cc_reg;
}

/* star modify, 20090505 */
void
gen_fp_conditional_branch (rtx operands[], enum rtx_code test_code)
{
  rtx cmp0 = unicore64_compare_op0;
  rtx cmp1 = unicore64_compare_op1;
  rtx reg;
  enum machine_mode mode;
  rtx label1, label2, tmp;

  label1 = gen_rtx_LABEL_REF (VOIDmode, operands[0]);
  label2 = pc_rtx;

  /* star add, 20090505 */
  switch (test_code)
    {
    case NE:
      test_code = EQ;
      tmp = label1; label1 = label2; label2 = tmp;
      break;
    case LTGT:
      test_code = UNEQ;
      tmp = label1; label1 = label2; label2 = tmp;
      break;
    case ORDERED:
      test_code = UNORDERED;
      tmp = label1; label1 = label2; label2 = tmp;
      break;
    default:
      break;
    }
  /* star add end */

  reg = gen_rtx_REG (CCFPEmode, CC_REGNUM);

  emit_insn (gen_rtx_SET (VOIDmode, reg,
			  gen_rtx_fmt_ee (test_code == NE ? EQ : test_code,
					  CCFPEmode, cmp0, cmp1)));
  mode = CCFPEmode;
  cmp0 = reg;
  cmp1 = const0_rtx;

  emit_jump_insn (gen_rtx_SET (VOIDmode, pc_rtx,
			       gen_rtx_IF_THEN_ELSE (VOIDmode,
						     gen_rtx_fmt_ee (test_code
								     ==
								     NE ? EQ :
								     NE, mode,
								     cmp0,
								     cmp1),
						     label1, label2)));
}

/* Handle storing a half-word to memory during reload by synthesising as two
   byte stores.  Take care not to clobber the input values until after we
   have moved them somewhere safe.  This code assumes that if the DImode
   scratch in operands[2] overlaps either the input value or output address
   in some way, then that value must die in this insn (we absolutely need
   two scratch registers for some corner cases).  */

void
unicore64_reload_out_hi (rtx * operands)
{
  rtx ref = operands[0];
  rtx outval = operands[1];
  rtx base, scratch;
  HOST_WIDE_INT offset = 0;

  if (GET_CODE (ref) == SUBREG)
    {
      offset = SUBREG_BYTE (ref);
      ref = SUBREG_REG (ref);
    }

  if (GET_CODE (ref) == REG)
    {
      /* We have a pseudo which has been spilt onto the stack; there
         are two cases here: the first where there is a simple
         stack-slot replacement and a second where the stack-slot is
         out of range, or is used as a subreg.  */
      if (reg_equiv_mem[REGNO (ref)])
	{
	  ref = reg_equiv_mem[REGNO (ref)];
	  base = find_replacement (&XEXP (ref, 0));
	}
      else
	/* The slot is out of range, or was dressed up in a SUBREG.  */
	base = reg_equiv_address[REGNO (ref)];
    }
  else
    base = find_replacement (&XEXP (ref, 0));

  scratch = gen_rtx_REG (DImode, REGNO (operands[2]));

  /* Handle the case where the address is too complex to be offset by 1.  */
  if (GET_CODE (base) == MINUS
      || (GET_CODE (base) == PLUS && GET_CODE (XEXP (base, 1)) != CONST_INT))
    {
      rtx base_plus = gen_rtx_REG (DImode, REGNO (operands[2]) + 1);

      /* Be careful not to destroy OUTVAL.  */
      if (reg_overlap_mentioned_p (base_plus, outval))
	{
	  /* Updating base_plus might destroy outval, see if we can
	     swap the scratch and base_plus.  */
	  if (!reg_overlap_mentioned_p (scratch, outval))
	    {
	      rtx tmp = scratch;
	      scratch = base_plus;
	      base_plus = tmp;
	    }
	  else
	    {
	      rtx scratch_hi = gen_rtx_REG (HImode, REGNO (operands[2]));

	      /* Be conservative and copy OUTVAL into the scratch now,
	         this should only be necessary if outval is a subreg
	         of something larger than a word.  */
	      /* XXX Might this clobber base?  I can't see how it can,
	         since scratch is known to overlap with OUTVAL, and
	         must be wider than a word.  */
	      emit_insn (gen_movhi (scratch_hi, outval));
	      outval = scratch_hi;
	    }
	}

      emit_insn (gen_rtx_SET (VOIDmode, base_plus, base));
      base = base_plus;
    }
  else if (GET_CODE (base) == PLUS)
    {
      /* The addend must be CONST_INT, or we would have dealt with it above.  */
      HOST_WIDE_INT hi, lo;

      offset += INTVAL (XEXP (base, 1));
      base = XEXP (base, 0);

      /* Rework the address into a legal sequence of insns.  */
      /* Valid range for lo is -4095 -> 4095 */
      lo = (offset >= 0 ? (offset & 0xfff) : -((-offset) & 0xfff)); // huangping: FIXME

      /* Corner case, if lo is the max offset then we would be out of range
         once we have added the additional 1 below, so bump the msb into the
         pre-loading insn(s).  */
      if (lo == 4095)
	lo &= 0x7ff;

      hi = ((((offset - lo) & (HOST_WIDE_INT) 0xffffffff)
	     ^ (HOST_WIDE_INT) 0x80000000) - (HOST_WIDE_INT) 0x80000000);

      if (hi + lo != offset)
	abort ();

      if (hi != 0)
	{
	  rtx base_plus = gen_rtx_REG (DImode, REGNO (operands[2]) + 1);

	  /* Be careful not to destroy OUTVAL.  */
	  if (reg_overlap_mentioned_p (base_plus, outval))
	    {
	      /* Updating base_plus might destroy outval, see if we
	         can swap the scratch and base_plus.  */
	      if (!reg_overlap_mentioned_p (scratch, outval))
		{
		  rtx tmp = scratch;
		  scratch = base_plus;
		  base_plus = tmp;
		}
	      else
		{
		  rtx scratch_hi = gen_rtx_REG (HImode, REGNO (operands[2]));

		  /* Be conservative and copy outval into scratch now,
		     this should only be necessary if outval is a
		     subreg of something larger than a word.  */
		  /* XXX Might this clobber base?  I can't see how it
		     can, since scratch is known to overlap with
		     outval.  */
		  emit_insn (gen_movhi (scratch_hi, outval));
		  outval = scratch_hi;
		}
	    }

	  /* Get the base address; addsi3 knows how to handle constants
	     that require more than one insn.  */
	  emit_insn (gen_addsi3 (base_plus, base, GEN_INT (hi)));
	  base = base_plus;
	  offset = lo;
	}
    }

  if (BYTES_BIG_ENDIAN)
    {
      emit_insn (gen_movqi (gen_rtx_MEM (QImode,
					 plus_constant (base, offset + 1)),
			    gen_lowpart (QImode, outval)));
      emit_insn (gen_lshrsi3 (scratch,
			      gen_rtx_SUBREG (SImode, outval, 0),
			      GEN_INT (8)));
      emit_insn (gen_movqi
		 (gen_rtx_MEM (QImode, plus_constant (base, offset)),
		  gen_lowpart (QImode, scratch)));
    }
  else
    {
      emit_insn (gen_movqi
		 (gen_rtx_MEM (QImode, plus_constant (base, offset)),
		  gen_lowpart (QImode, outval)));
      emit_insn (gen_lshrsi3
		 (scratch, gen_rtx_SUBREG (SImode, outval, 0), GEN_INT (8)));
      emit_insn (gen_movqi
		 (gen_rtx_MEM (QImode, plus_constant (base, offset + 1)),
		  gen_lowpart (QImode, scratch)));
    }
}

/* Print a symbolic form of X to the debug file, F.  */

static void
unicore64_print_value (FILE * f, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
      fprintf (f, HOST_WIDE_INT_PRINT_HEX, INTVAL (x));
      return;

    case CONST_DOUBLE:
      fprintf (f, "<0x%lx,0x%lx>", (long) XWINT (x, 2), (long) XWINT (x, 3));
      return;

    case CONST_STRING:
      fprintf (f, "\"%s\"", XSTR (x, 0));
      return;

    case SYMBOL_REF:
      fprintf (f, "`%s'", XSTR (x, 0));
      return;

    case LABEL_REF:
      fprintf (f, "L%d", INSN_UID (XEXP (x, 0)));
      return;

    case CONST:
      unicore64_print_value (f, XEXP (x, 0));
      return;

    case PLUS:
      unicore64_print_value (f, XEXP (x, 0));
      fprintf (f, "+");
      unicore64_print_value (f, XEXP (x, 1));
      return;

    case PC:
      fprintf (f, "pc");
      return;

    default:
      fprintf (f, "????");
      return;
    }
}

/* Routines for manipulation of the constant pool.  */

/* Unicore64 instructions cannot load a large constant directly into a
   register; they have to come from a pc relative load.  The constant
   must therefore be placed in the addressable range of the pc
   relative load.  Depending on the precise pc relative load
   instruction the range is somewhere between 256 bytes and 4k.  This
   means that we often have to dump a constant inside a function, and
   generate code to branch around it.

   It is important to minimize this, since the branches will slow
   things down and make the code larger.

   Normally we can hide the table after an existing unconditional
   branch so that there is no interruption of the flow, but in the
   worst case the code looks like this:

	ldr	rn, L1
	...
	b	L2
	align
	L1:	.long value
	L2:
	...

	ldr	rn, L3
	...
	b	L4
	align
	L3:	.long value
	L4:
	...

   We fix this by performing a scan after scheduling, which notices
   which instructions need to have their operands fetched from the
   constant table and builds the table.

   The algorithm starts by building a table of all the constants that
   need fixing up and all the natural barriers in the function (places
   where a constant table can be dropped without breaking the flow).
   For each fixup we note how far the pc-relative replacement will be
   able to reach and the offset of the instruction into the function.

   Having built the table we then group the fixes together to form
   tables that are as large as possible (subject to addressing
   constraints) and emit each table of constants after the last
   barrier that is within range of all the instructions in the group.
   If a group does not contain a barrier, then we forcibly create one
   by inserting a jump instruction into the flow.  Once the table has
   been inserted, the insns are then modified to reference the
   relevant entry in the pool.

   Possible enhancements to the algorithm (not implemented) are:

   1) For some processors and object formats, there may be benefit in
   aligning the pools to the start of cache lines; this alignment
   would need to be taken into account when calculating addressability
   of a pool.  */

/* These typedefs are located at the start of this file, so that
   they can be used in the prototypes there.  This comment is to
   remind readers of that fact so that the following structures
   can be understood more easily.

     typedef struct minipool_node    Mnode;
     typedef struct minipool_fixup   Mfix;  */

struct minipool_node
{
  /* Doubly linked chain of entries.  */
  Mnode *next;
  Mnode *prev;
  /* The maximum offset into the code that this entry can be placed.  While
     pushing fixes for forward references, all entries are sorted in order
     of increasing max_address.  */
  HOST_WIDE_INT max_address;
  /* Similarly for an entry inserted for a backwards ref.  */
  HOST_WIDE_INT min_address;
  /* The number of fixes referencing this entry.  This can become zero
     if we "unpush" an entry.  In this case we ignore the entry when we
     come to emit the code.  */
  int refcount;
  /* The offset from the start of the minipool.  */
  HOST_WIDE_INT offset;
  /* The value in table.  */
  rtx value;
  /* The mode of value.  */
  enum machine_mode mode;
  int fix_size;
};

struct minipool_fixup
{
  Mfix *next;
  rtx insn;
  HOST_WIDE_INT address;
  rtx *loc;
  enum machine_mode mode;
  int fix_size;
  rtx value;
  Mnode *minipool;
  HOST_WIDE_INT forwards;
  HOST_WIDE_INT backwards;
};

/* Fixes less than a word need padding out to a word boundary.  */
// huangping modify:FIXME
#define MINIPOOL_FIX_SIZE(mode) \
  (GET_MODE_SIZE ((mode)) >= 8 ? GET_MODE_SIZE ((mode)) : 8)

static Mnode *minipool_vector_head;
static Mnode *minipool_vector_tail;
static rtx minipool_vector_label;

/* The linked list of all minipool fixes required for this function.  */
Mfix *minipool_fix_head;
Mfix *minipool_fix_tail;
/* The fix entry for the current minipool, once it has been placed.  */
Mfix *minipool_barrier;

/* Determines if INSN is the start of a jump table.  Returns the end
   of the TABLE or NULL_RTX.  */

static rtx
is_jump_table (rtx insn)
{
  rtx table;

  if (GET_CODE (insn) == JUMP_INSN
      && JUMP_LABEL (insn) != NULL
      && ((table = next_real_insn (JUMP_LABEL (insn)))
	  == next_real_insn (insn))
      && table != NULL
      && GET_CODE (table) == JUMP_INSN
      && (GET_CODE (PATTERN (table)) == ADDR_VEC
	  || GET_CODE (PATTERN (table)) == ADDR_DIFF_VEC))
    return table;

  return NULL_RTX;
}

#ifndef JUMP_TABLES_IN_TEXT_SECTION
#define JUMP_TABLES_IN_TEXT_SECTION 0
#endif

static HOST_WIDE_INT
get_jump_table_size (rtx insn)
{
  /* ADDR_VECs only take room if read-only data does into the text
     section.  */
  if (JUMP_TABLES_IN_TEXT_SECTION || readonly_data_section == text_section)
    {
      rtx body = PATTERN (insn);
      int elt = GET_CODE (body) == ADDR_DIFF_VEC ? 1 : 0;

      return GET_MODE_SIZE (GET_MODE (body)) * XVECLEN (body, elt);
    }

  return 0;
}

/* Move a minipool fix MP from its current location to before MAX_MP.
   If MAX_MP is NULL, then MP doesn't need moving, but the addressing
   contrains may need updating.  */

static Mnode *
move_minipool_fix_forward_ref (Mnode * mp,
			       Mnode * max_mp, HOST_WIDE_INT max_address)
{
  /* This should never be true and the code below assumes these are
     different.  */
  if (mp == max_mp)
    abort ();

  if (max_mp == NULL)
    {
      if (max_address < mp->max_address)
	mp->max_address = max_address;
    }
  else
    {
      if (max_address > max_mp->max_address - mp->fix_size)
	mp->max_address = max_mp->max_address - mp->fix_size;
      else
	mp->max_address = max_address;

      /* Unlink MP from its current position.  Since max_mp is non-null,
         mp->prev must be non-null.  */
      mp->prev->next = mp->next;
      if (mp->next != NULL)
	mp->next->prev = mp->prev;
      else
	minipool_vector_tail = mp->prev;

      /* Re-insert it before MAX_MP.  */
      mp->next = max_mp;
      mp->prev = max_mp->prev;
      max_mp->prev = mp;

      if (mp->prev != NULL)
	mp->prev->next = mp;
      else
	minipool_vector_head = mp;
    }

  /* Save the new entry.  */
  max_mp = mp;

  /* Scan over the preceding entries and adjust their addresses as
     required.  */
  while (mp->prev != NULL
	 && mp->prev->max_address > mp->max_address - mp->prev->fix_size)
    {
      mp->prev->max_address = mp->max_address - mp->prev->fix_size;
      mp = mp->prev;
    }

  return max_mp;
}

/* Add a constant to the minipool for a forward reference.  Returns the
   node added or NULL if the constant will not fit in this pool.  */

static Mnode *
add_minipool_forward_ref (Mfix * fix)
{
  /* If set, max_mp is the first pool_entry that has a lower
     constraint than the one we are trying to add.  */
  Mnode *max_mp = NULL;
  HOST_WIDE_INT max_address = fix->address + fix->forwards;
  Mnode *mp;

  /* If this fix's address is greater than the address of the first
     entry, then we can't put the fix in this pool.  We subtract the
     size of the current fix to ensure that if the table is fully
     packed we still have enough room to insert this value by suffling
     the other fixes forwards.  */
  if (minipool_vector_head &&
      fix->address >= minipool_vector_head->max_address - fix->fix_size)
    return NULL;

  /* Scan the pool to see if a constant with the same value has
     already been added.  While we are doing this, also note the
     location where we must insert the constant if it doesn't already
     exist.  */
  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      if (GET_CODE (fix->value) == GET_CODE (mp->value)
	  && fix->mode == mp->mode
	  && (GET_CODE (fix->value) != CODE_LABEL
	      || (CODE_LABEL_NUMBER (fix->value)
		  == CODE_LABEL_NUMBER (mp->value)))
	  && rtx_equal_p (fix->value, mp->value))
	{
	  /* More than one fix references this entry.  */
	  mp->refcount++;
	  return move_minipool_fix_forward_ref (mp, max_mp, max_address);
	}

      /* Note the insertion point if necessary.  */
      if (max_mp == NULL && mp->max_address > max_address)
	max_mp = mp;
    }

  /* The value is not currently in the minipool, so we need to create
     a new entry for it.  If MAX_MP is NULL, the entry will be put on
     the end of the list since the placement is less constrained than
     any existing entry.  Otherwise, we insert the new fix before
     MAX_MP and, if neceesary, adjust the constraints on the other
     entries.  */
  mp = xmalloc (sizeof (*mp));
  mp->fix_size = fix->fix_size;
  mp->mode = fix->mode;
  mp->value = fix->value;
  mp->refcount = 1;
  /* Not yet required for a backwards ref.  */
  mp->min_address = -65536;

  if (max_mp == NULL)
    {
      mp->max_address = max_address;
      mp->next = NULL;
      mp->prev = minipool_vector_tail;

      if (mp->prev == NULL)
	{
	  minipool_vector_head = mp;
	  minipool_vector_label = gen_label_rtx ();
	}
      else
	mp->prev->next = mp;

      minipool_vector_tail = mp;
    }
  else
    {
      if (max_address > max_mp->max_address - mp->fix_size)
	mp->max_address = max_mp->max_address - mp->fix_size;
      else
	mp->max_address = max_address;

      mp->next = max_mp;
      mp->prev = max_mp->prev;
      max_mp->prev = mp;
      if (mp->prev != NULL)
	mp->prev->next = mp;
      else
	minipool_vector_head = mp;
    }

  /* Save the new entry.  */
  max_mp = mp;

  /* Scan over the preceding entries and adjust their addresses as
     required.  */
  while (mp->prev != NULL
	 && mp->prev->max_address > mp->max_address - mp->prev->fix_size)
    {
      mp->prev->max_address = mp->max_address - mp->prev->fix_size;
      mp = mp->prev;
    }

  return max_mp;
}

static Mnode *
move_minipool_fix_backward_ref (Mnode * mp,
				Mnode * min_mp, HOST_WIDE_INT min_address)
{
  HOST_WIDE_INT offset;

  /* This should never be true, and the code below assumes these are
     different.  */
  if (mp == min_mp)
    abort ();

  if (min_mp == NULL)
    {
      if (min_address > mp->min_address)
	mp->min_address = min_address;
    }
  else
    {
      /* We will adjust this below if it is too loose.  */
      mp->min_address = min_address;

      /* Unlink MP from its current position.  Since min_mp is non-null,
         mp->next must be non-null.  */
      mp->next->prev = mp->prev;
      if (mp->prev != NULL)
	mp->prev->next = mp->next;
      else
	minipool_vector_head = mp->next;

      /* Reinsert it after MIN_MP.  */
      mp->prev = min_mp;
      mp->next = min_mp->next;
      min_mp->next = mp;
      if (mp->next != NULL)
	mp->next->prev = mp;
      else
	minipool_vector_tail = mp;
    }

  min_mp = mp;

  offset = 0;
  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      mp->offset = offset;
      if (mp->refcount > 0)
	offset += mp->fix_size;

      if (mp->next && mp->next->min_address < mp->min_address + mp->fix_size)
	mp->next->min_address = mp->min_address + mp->fix_size;
    }

  return min_mp;
}

/* Add a constant to the minipool for a backward reference.  Returns the
   node added or NULL if the constant will not fit in this pool.  

   Note that the code for insertion for a backwards reference can be
   somewhat confusing because the calculated offsets for each fix do
   not take into account the size of the pool (which is still under
   construction.  */

static Mnode *
add_minipool_backward_ref (Mfix * fix)
{
  /* If set, min_mp is the last pool_entry that has a lower constraint
     than the one we are trying to add.  */
  Mnode *min_mp = NULL;
  /* This can be negative, since it is only a constraint.  */
  HOST_WIDE_INT min_address = fix->address - fix->backwards;
  Mnode *mp;

  /* If we can't reach the current pool from this insn, or if we can't
     insert this entry at the end of the pool without pushing other
     fixes out of range, then we don't try.  This ensures that we
     can't fail later on.  */
  if (min_address >= minipool_barrier->address
      || (minipool_vector_tail->min_address + fix->fix_size
	  >= minipool_barrier->address))
    return NULL;

  /* Scan the pool to see if a constant with the same value has
     already been added.  While we are doing this, also note the
     location where we must insert the constant if it doesn't already
     exist.  */
  for (mp = minipool_vector_tail; mp != NULL; mp = mp->prev)
    {
      if (GET_CODE (fix->value) == GET_CODE (mp->value)
	  && fix->mode == mp->mode
	  && (GET_CODE (fix->value) != CODE_LABEL
	      || (CODE_LABEL_NUMBER (fix->value)
		  == CODE_LABEL_NUMBER (mp->value)))
	  && rtx_equal_p (fix->value, mp->value)
	  /* Check that there is enough slack to move this entry to the
	     end of the table (this is conservative).  */
	  && (mp->max_address
	      > (minipool_barrier->address
		 + minipool_vector_tail->offset
		 + minipool_vector_tail->fix_size)))
	{
	  mp->refcount++;
	  return move_minipool_fix_backward_ref (mp, min_mp, min_address);
	}

      if (min_mp != NULL)
	mp->min_address += fix->fix_size;
      else
	{
	  /* Note the insertion point if necessary.  */
	  if (mp->min_address < min_address)
	    min_mp = mp;
	  else if (mp->max_address
		   < minipool_barrier->address + mp->offset + fix->fix_size)
	    {
	      /* Inserting before this entry would push the fix beyond
	         its maximum address (which can happen if we have
	         re-located a forwards fix); force the new fix to come
	         after it.  */
	      min_mp = mp;
	      min_address = mp->min_address + fix->fix_size;
	    }
	}
    }

  /* We need to create a new entry.  */
  mp = xmalloc (sizeof (*mp));
  mp->fix_size = fix->fix_size;
  mp->mode = fix->mode;
  mp->value = fix->value;
  mp->refcount = 1;
  mp->max_address = minipool_barrier->address + 65536;

  mp->min_address = min_address;

  if (min_mp == NULL)
    {
      mp->prev = NULL;
      mp->next = minipool_vector_head;

      if (mp->next == NULL)
	{
	  minipool_vector_tail = mp;
	  minipool_vector_label = gen_label_rtx ();
	}
      else
	mp->next->prev = mp;

      minipool_vector_head = mp;
    }
  else
    {
      mp->next = min_mp->next;
      mp->prev = min_mp;
      min_mp->next = mp;

      if (mp->next != NULL)
	mp->next->prev = mp;
      else
	minipool_vector_tail = mp;
    }

  /* Save the new entry.  */
  min_mp = mp;

  if (mp->prev)
    mp = mp->prev;
  else
    mp->offset = 0;

  /* Scan over the following entries and adjust their offsets.  */
  while (mp->next != NULL)
    {
      if (mp->next->min_address < mp->min_address + mp->fix_size)
	mp->next->min_address = mp->min_address + mp->fix_size;

      if (mp->refcount)
	mp->next->offset = mp->offset + mp->fix_size;
      else
	mp->next->offset = mp->offset;

      mp = mp->next;
    }

  return min_mp;
}

static void
assign_minipool_offsets (Mfix * barrier)
{
  HOST_WIDE_INT offset = 0;
  Mnode *mp;

  minipool_barrier = barrier;

  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      mp->offset = offset;

      if (mp->refcount > 0)
	offset += mp->fix_size;
    }
}

/* Output the literal table */
static void
dump_minipool (rtx scan)
{
  Mnode *mp;
  Mnode *nmp;

  if (dump_file)
    fprintf (dump_file,
	     ";; Emitting minipool after insn %u; address %ld\n",
	     INSN_UID (scan), (unsigned long) minipool_barrier->address);

  scan = emit_label_after (gen_label_rtx (), scan);
  scan = emit_insn_after (gen_align_8 (), scan);
  scan = emit_label_after (minipool_vector_label, scan);

  for (mp = minipool_vector_head; mp != NULL; mp = nmp)
    {
      if (mp->refcount > 0)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file,
		       ";;  Offset %u, min %ld, max %ld ",
		       (unsigned) mp->offset, (unsigned long) mp->min_address,
		       (unsigned long) mp->max_address);
	      unicore64_print_value (dump_file, mp->value);
	      fputc ('\n', dump_file);
	    }

	  switch (mp->fix_size)
	    {
#ifdef HAVE_consttable_1
	    case 1:
	      scan = emit_insn_after (gen_consttable_1 (mp->value), scan);
	      break;

#endif
#ifdef HAVE_consttable_2
	    case 2:
	      scan = emit_insn_after (gen_consttable_2 (mp->value), scan);
	      break;

#endif
#ifdef HAVE_consttable_4
	    case 4:
	      scan = emit_insn_after (gen_consttable_4 (mp->value), scan);
	      break;

#endif
#ifdef HAVE_consttable_8
	    case 8:
	      scan = emit_insn_after (gen_consttable_8 (mp->value), scan);
	      break;

#endif
	    default:
	      abort ();
	      break;
	    }
	}

      nmp = mp->next;
      free (mp);
    }

  minipool_vector_head = minipool_vector_tail = NULL;
  scan = emit_insn_after (gen_consttable_end (), scan);
  scan = emit_barrier_after (scan);
}

/* Return the cost of forcibly inserting a barrier after INSN.  */

static int
unicore64_barrier_cost (rtx insn)
{
  /* Basing the location of the pool on the loop depth is preferable,
     but at the moment, the basic block information seems to be
     corrupt by this stage of the compilation.  */
  int base_cost = 50;
  rtx next = next_nonnote_insn (insn);

  if (next != NULL && GET_CODE (next) == CODE_LABEL)
    base_cost -= 20;

  switch (GET_CODE (insn))
    {
    case CODE_LABEL:
      /* It will always be better to place the table before the label, rather
         than after it.  */
      return 50;

    case INSN:
    case CALL_INSN:
      return base_cost;

    case JUMP_INSN:
      return base_cost - 10;

    default:
      return base_cost + 10;
    }
}

/* Find the best place in the insn stream in the range
   (FIX->address,MAX_ADDRESS) to forcibly insert a minipool barrier.
   Create the barrier by inserting a jump and add a new fix entry for
   it.  */

static Mfix *
create_fix_barrier (Mfix * fix, HOST_WIDE_INT max_address)
{
  HOST_WIDE_INT count = 0;
  rtx barrier;
  rtx from = fix->insn;
  rtx selected = from;
  int selected_cost;
  HOST_WIDE_INT selected_address;
  Mfix *new_fix;
  HOST_WIDE_INT max_count = max_address - fix->address;
  rtx label = gen_label_rtx ();

  selected_cost = unicore64_barrier_cost (from);
  selected_address = fix->address;

  while (from && count < max_count)
    {
      rtx tmp;
      int new_cost;

      /* This code shouldn't have been called if there was a natural barrier
         within range.  */
      if (GET_CODE (from) == BARRIER)
	abort ();

      /* Count the length of this insn.  */
      count += get_attr_length (from);

      /* If there is a jump table, add its length.  */
      tmp = is_jump_table (from);
      if (tmp != NULL)
	{
	  count += get_jump_table_size (tmp);

	  /* Jump tables aren't in a basic block, so base the cost on
	     the dispatch insn.  If we select this location, we will
	     still put the pool after the table.  */
	  new_cost = unicore64_barrier_cost (from);

	  if (count < max_count && new_cost <= selected_cost)
	    {
	      selected = tmp;
	      selected_cost = new_cost;
	      selected_address = fix->address + count;
	    }

	  /* Continue after the dispatch table.  */
	  from = NEXT_INSN (tmp);
	  continue;
	}

      new_cost = unicore64_barrier_cost (from);

      if (count < max_count && new_cost <= selected_cost)
	{
	  selected = from;
	  selected_cost = new_cost;
	  selected_address = fix->address + count;
	}

      from = NEXT_INSN (from);
    }

  /* Create a new JUMP_INSN that branches around a barrier.  */
  from = emit_jump_insn_after (gen_jump (label), selected);
  JUMP_LABEL (from) = label;
  barrier = emit_barrier_after (from);
  emit_label_after (label, barrier);

  /* Create a minipool barrier entry for the new barrier.  */
  new_fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (*new_fix));
  new_fix->insn = barrier;
  new_fix->address = selected_address;
  new_fix->next = fix->next;
  fix->next = new_fix;

  return new_fix;
}

/* Record that there is a natural barrier in the insn stream at
   ADDRESS.  */
static void
push_minipool_barrier (rtx insn, HOST_WIDE_INT address)
{
  Mfix *fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (*fix));

  fix->insn = insn;
  fix->address = address;

  fix->next = NULL;
  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}

/* Record INSN, which will need fixing up to load a value from the
   minipool.  ADDRESS is the offset of the insn since the start of the
   function; LOC is a pointer to the part of the insn which requires
   fixing; VALUE is the constant that must be loaded, which is of type
   MODE.  */
static void
push_minipool_fix (rtx insn,
		   HOST_WIDE_INT address,
		   rtx * loc, enum machine_mode mode, rtx value)
{
  Mfix *fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (*fix));

  fix->insn = insn;
  fix->address = address;
  fix->loc = loc;
  fix->mode = mode;
  fix->fix_size = MINIPOOL_FIX_SIZE (mode);
  fix->value = value;
  fix->forwards = get_attr_pool_range (insn);
  fix->backwards = get_attr_neg_pool_range (insn);
  fix->minipool = NULL;

  /* If an insn doesn't have a range defined for it, then it isn't
     expecting to be reworked by this code.  Better to abort now than
     to generate duff assembly code.  */
  if (fix->forwards == 0 && fix->backwards == 0)
    abort ();

  if (dump_file)
    {
      fprintf (dump_file,
	       ";; %smode fixup for i%d; addr %lu, range (%ld,%ld): ",
	       GET_MODE_NAME (mode),
	       INSN_UID (insn), (unsigned long) address,
	       -1 * (long) fix->backwards, (long) fix->forwards);
      unicore64_print_value (dump_file, fix->value);
      fprintf (dump_file, "\n");
    }

  /* Add it to the chain of fixes.  */
  fix->next = NULL;

  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}

/* Scan INSN and note any of its operands that need fixing.  */

static void
note_invalid_constants (rtx insn, HOST_WIDE_INT address)
{
  int opno;

  extract_insn (insn);

  if (!constrain_operands (1))
    fatal_insn_not_found (insn);

  /* Fill in recog_op_alt with information about the constraints of this
     insn.  */
  preprocess_constraints ();

  for (opno = 0; opno < recog_data.n_operands; opno++)
    {
      /* Things we need to fix can only occur in inputs.  */
      if (recog_data.operand_type[opno] != OP_IN)
	continue;

      /* If this alternative is a memory reference, then any mention
         of constants in this alternative is really to fool reload
         into allowing us to accept one there.  We need to fix them up
         now so that we output the right code.  */
      if (recog_op_alt[opno][which_alternative].memory_ok)
	{
	  rtx op = recog_data.operand[opno];

	  if (CONSTANT_P (op))
	    push_minipool_fix (insn, address, recog_data.operand_loc[opno],
			       recog_data.operand_mode[opno], op);
	  else if (GET_CODE (op) == MEM
		   && GET_CODE (XEXP (op, 0)) == SYMBOL_REF
		   && CONSTANT_POOL_ADDRESS_P (XEXP (op, 0)))
	    push_minipool_fix (insn, address, recog_data.operand_loc[opno],
			       recog_data.operand_mode[opno],
			       get_pool_constant (XEXP (op, 0)));
	}
    }
}

static void
unicore64_reorg (void)
{
  rtx insn;
  HOST_WIDE_INT address = 0;
  Mfix *fix;

  minipool_fix_head = minipool_fix_tail = NULL;

  /* The first insn must always be a note, or the code below won't
     scan it properly.  */
  insn = get_insns ();
  if (GET_CODE (insn) != NOTE)
    abort ();

  /* Scan all the insns and record the operands that will need fixing.  */
  for (insn = next_nonnote_insn (insn); insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) == BARRIER)
	push_minipool_barrier (insn, address);
      else if (GET_CODE (insn) == INSN || GET_CODE (insn) == CALL_INSN
	       || GET_CODE (insn) == JUMP_INSN)
	{
	  rtx table;

	  note_invalid_constants (insn, address);
	  address += get_attr_length (insn);

	  /* If the insn is a vector jump, add the size of the table
	     and skip the table.  */
	  if ((table = is_jump_table (insn)) != NULL)
	    {
	      address += get_jump_table_size (table);
	      insn = table;
	    }
	}
    }

  fix = minipool_fix_head;

  /* Now scan the fixups and perform the required changes.  */
  while (fix)
    {
      Mfix *ftmp;
      Mfix *fdel;
      Mfix *last_added_fix;
      Mfix *last_barrier = NULL;
      Mfix *this_fix;

      /* Skip any further barriers before the next fix.  */
      while (fix && GET_CODE (fix->insn) == BARRIER)
	fix = fix->next;

      /* No more fixes.  */
      if (fix == NULL)
	break;

      last_added_fix = NULL;

      for (ftmp = fix; ftmp; ftmp = ftmp->next)
	{
	  if (GET_CODE (ftmp->insn) == BARRIER)
	    {
	      if (ftmp->address >= minipool_vector_head->max_address)
		break;

	      last_barrier = ftmp;
	    }
	  else if ((ftmp->minipool = add_minipool_forward_ref (ftmp)) == NULL)
	    break;

	  last_added_fix = ftmp;	/* Keep track of the last fix added.  */
	}

      /* If we found a barrier, drop back to that; any fixes that we
         could have reached but come after the barrier will now go in
         the next mini-pool.  */
      if (last_barrier != NULL)
	{
	  /* Reduce the refcount for those fixes that won't go into this 
	     pool after all.  */
	  for (fdel = last_barrier->next;
	       fdel && fdel != ftmp; fdel = fdel->next)
	    {
	      fdel->minipool->refcount--;
	      fdel->minipool = NULL;
	    }

	  ftmp = last_barrier;
	}
      else
	{
	  /* ftmp is first fix that we can't fit into this pool and
	     there no natural barriers that we could use.  Insert a
	     new barrier in the code somewhere between the previous
	     fix and this one, and arrange to jump around it.  */
	  HOST_WIDE_INT max_address;

	  /* The last item on the list of fixes must be a barrier, so
	     we can never run off the end of the list of fixes without
	     last_barrier being set.  */
	  if (ftmp == NULL)
	    abort ();

	  max_address = minipool_vector_head->max_address;
	  /* Check that there isn't another fix that is in range that
	     we couldn't fit into this pool because the pool was
	     already too large: we need to put the pool before such an
	     instruction.  */
	  if (ftmp->address < max_address)
	    max_address = ftmp->address;

	  last_barrier = create_fix_barrier (last_added_fix, max_address);
	}

      assign_minipool_offsets (last_barrier);

      while (ftmp)
	{
	  if (GET_CODE (ftmp->insn) != BARRIER
	      && ((ftmp->minipool = add_minipool_backward_ref (ftmp))
		  == NULL))
	    break;

	  ftmp = ftmp->next;
	}

      /* Scan over the fixes we have identified for this pool, fixing them
         up and adding the constants to the pool itself.  */
      for (this_fix = fix; this_fix && ftmp != this_fix;
	   this_fix = this_fix->next)
	if (GET_CODE (this_fix->insn) != BARRIER)
	  {
	    rtx addr = plus_constant (gen_rtx_LABEL_REF (VOIDmode,
							 minipool_vector_label),
				      this_fix->minipool->offset);
	    *this_fix->loc = gen_rtx_MEM (this_fix->mode, addr);
	  }

      dump_minipool (last_barrier->insn);
      fix = ftmp;
    }

  /* From now on we must synthesize any constants that we can't handle
     directly.  This can happen if the RTL gets split during final
     instruction generation.  */
  after_unicore64_reorg = 1;

  /* Free the minipool memory.  */
  obstack_free (&minipool_obstack, minipool_startobj);
}
/* Routines to output assembly language.  */

/* If the rtx is the correct value then return the string of the number.
   In this way we can ensure that valid double constants are generated even
   when cross compiling.  */

const char *
fp_immediate_constant (rtx x)
{
  REAL_VALUE_TYPE r;
  int i;

  if (!fpa_consts_inited)
    init_fpa_table ();

  REAL_VALUE_FROM_CONST_DOUBLE (r, x);
  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (r, values_fpa[i]))
      return strings_fpa[i];

  abort ();
}

/* As for fp_immediate_constant, but value is passed directly, not in rtx.  */

static const char *
fp_const_from_val (REAL_VALUE_TYPE * r)
{
  int i;

  if (!fpa_consts_inited)
    init_fpa_table ();

  for (i = 0; i < 8; i++)
    if (REAL_VALUES_EQUAL (*r, values_fpa[i]))
      return strings_fpa[i];

  abort ();
}

/* Output the operands of a LDM/STM instruction to STREAM.
   MASK is the UNICORE64 register set mask of which only bits 0-15 are important.
   REG is the base register, either the frame pointer or the stack pointer,
   INSTR is the possibly suffixed load or store instruction.  */
// huangping: FIXME
static void
print_multi_reg (FILE * stream, 
		 const char *instr, 
		 const char *suffix,
		 int reg,
		 int mask)
{
  int i;
  int offset;
  int mark = 0;
  int regnum = 0;
  int is_first = 1;
  int base_in_list = 0;
  int is_sub = 0;
  char mov_inst[20] = {};
  char inst_no_wb[5] = {};
  for (i = 0; i <= LAST_UNICORE64_REGNUM; i++)
    if (mask & (1 << i))
      {
	if (reg == i)
	  base_in_list = 1;
	regnum ++;
      }
  strncpy (inst_no_wb, instr, 3);
  
  if (*suffix == '-')
    offset = 8 * regnum; 
  else if (*suffix == '+')
    offset = 8;
  else if (*(suffix + 1) == '-')
    offset = 8 * regnum - 8;
  else if (*(suffix + 1) == '+')
    offset = 0;

  if (*suffix == '-' || *(suffix + 1) == '-')
    is_sub = 1;

  if (!mask)
    return;
  for (i = 0; i <= LAST_UNICORE64_REGNUM; i++)
    if (mask & (1 << i))
      {
	if (base_in_list && (i == reg))
	  {
	    sprintf (mov_inst, "\tdmov\tip, %s\n", reg_names[reg]);
	    fprintf (stream, mov_inst);
	    reg = IP_REGNUM;
	  }
	fputc ('\t', stream);
	fprintf (stream, inst_no_wb);
	fprintf (stream, "\t");
	
	// huangping: Load LR before SP to fix the interupt. 2012-11-22 
	if (i == SP_REGNUM && (mask & (1 << PC_REGNUM)))
	  {
	    asm_fprintf (stream, "%r", LR_REGNUM);
	    fprintf (stream , " , [");
	    asm_fprintf (stream, "%r", reg);
	    if (is_sub)
	      {
		fprintf (stream, "-], #%d", offset-8);
	      }
	    else
	      {
		fprintf (stream, "+], #%d", offset+8);
	      }

	  }
	else if (i == PC_REGNUM && (mask & (1 << SP_REGNUM)))
	  {
	    asm_fprintf (stream, "%r", SP_REGNUM);
	    fprintf (stream , " , [");
	    asm_fprintf (stream, "%r", reg);
	    if (is_sub)
	      {
		fprintf (stream, "-], #%d", offset);
		offset -=16;
	      }
	    else
	      {
		fprintf (stream, "+], #%d", offset);
		offset += 16;
	      }

	  }
	else 
	  {
	    if (i == PC_REGNUM)
	      asm_fprintf (stream, "%r", LR_REGNUM);
	    else 
	      asm_fprintf (stream, "%r", i);
	    fprintf (stream , " , [");
	    asm_fprintf (stream, "%r", reg);

	    if (is_sub)
	      {
		fprintf (stream, "-], #%d", offset);
		offset -=8;
	      }
	    else
	      {
		fprintf (stream, "+], #%d", offset);
		offset += 8;
	      }
	  }
#if 0
	if (instr[4] == 'w')
	  {
	    if (is_sub)
	      {
		if (is_first)
		  fprintf (stream, "-], #%d", offset);
		else
		  fprintf (stream, "+], #4");
		is_first = FALSE;
	      }
	    else
	      {
		fprintf (stream, suffix);
		fprintf (stream, ", #%d", 4);
	      }
	  }
	else
	  {
	    if (is_sub)
	      {
		fprintf (stream, "-], #%d", offset);
		offset -= 4;
	      }
	    else
	      {
		fprintf (stream, "+], #%d", offset);
		offset += 4;
	      }
	  }
#endif 
	fputc ('\n', stream);
	if (i == PC_REGNUM)
	  {
	    mark = 1;
	  }
      }
  if (instr[4] == 'w')
    {
      if (is_sub)
	sprintf (mov_inst, "\tdsub\t%s, %s, #%d\n", reg_names[reg], reg_names[reg], 8*regnum);
      else
	sprintf (mov_inst, "\tdadd\t%s, %s, #%d\n", reg_names[reg], reg_names[reg], 8*regnum);
      fprintf (stream, mov_inst);
    }
  if (mark)
    {
      sprintf (mov_inst, "\treturn\n");
      fprintf (stream, mov_inst);
    }
}

/* Output a 'call' insn.  */
/* gaoyi:
 */
const char *
output_call (rtx * operands)
{
  /* Handle calls to lr using ip (which may be clobbered in subr anyway).  */

  if (REGNO (operands[0]) == LR_REGNUM)
    {
      operands[0] = gen_rtx_REG (DImode, IP_REGNUM);
      output_asm_insn ("dmov\t%0, %|lr", operands);
    }
  /* star UniCore-3 */
//  output_asm_insn ("add\t%|lr, %|pc, #4", operands);
//  output_asm_insn ("jump\t%0", operands);
  output_asm_insn ("call.R\t%0", operands);
  return "";
}

static int
eliminate_lr2ip (rtx * x)
{
  int something_changed = 0;
  rtx x0 = *x;
  int code = GET_CODE (x0);
  int i, j;
  const char *fmt;

  switch (code)
    {
    case REG:
      if (REGNO (x0) == LR_REGNUM)
	{
	  *x = gen_rtx_REG (DImode, IP_REGNUM);
	  return 1;
	}
      return 0;
    default:
      /* Scan through the sub-elements and change any references there.  */
      fmt = GET_RTX_FORMAT (code);

      for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
	if (fmt[i] == 'e')
	  something_changed |= eliminate_lr2ip (&XEXP (x0, i));
	else if (fmt[i] == 'E')
	  for (j = 0; j < XVECLEN (x0, i); j++)
	    something_changed |= eliminate_lr2ip (&XVECEXP (x0, i, j));

      return something_changed;
    }
}

/* Output a 'call' insn that is a reference in memory.  */
/* gaoyi:
 */
const char *
output_call_mem (rtx * operands)
{
  operands[0] = copy_rtx (operands[0]);	/* Be ultra careful.  */
  /* Handle calls using lr by using ip (which may be clobbered in subr anyway).  */
  if (eliminate_lr2ip (&operands[0]))
    output_asm_insn ("dmov\t%|ip, %|lr", operands);

  if (TARGET_INTERWORK)
    {
      output_asm_insn ("ldd%T0\t%|ip, %0", operands);
      /* star UniCore-3 */
//      output_asm_insn ("add\t%|lr, %|pc, #4", operands);
//      output_asm_insn ("jump\t%|ip", operands);
      output_asm_insn ("call.R\t%|ip", operands);
    }
  else
    {
      /* star UniCore-3 */
//      output_asm_insn ("add\t%|lr, %|pc, #8", operands);
//      output_asm_insn ("ldw%T0\t%|ip, %0", operands);
//      output_asm_insn ("jump\t%|ip", operands);
      output_asm_insn ("ldd%T0\t%|ip, %0", operands);
      output_asm_insn ("call.R\t%|ip", operands);
    }

  return "";
}


#if 0 //star, 20090727 
/* Output a move from unicore64 registers to an fpu registers.
   OPERANDS[0] is an fpu register.
   OPERANDS[1] is the first registers of an unicore64 register pair.  */

const char *
output_mov_long_double_fpu_from_unicore64 (rtx * operands)
{
  int unicore64_reg0 = REGNO (operands[1]);
  rtx ops[3];

  if (unicore64_reg0 == IP_REGNUM)
    abort ();

  ops[0] = gen_rtx_REG (SImode, unicore64_reg0);
  ops[1] = gen_rtx_REG (SImode, 1 + unicore64_reg0);
  ops[2] = gen_rtx_REG (SImode, 2 + unicore64_reg0);

  output_asm_insn ("stm%?fd\t%|sp!, {%0, %1, %2}", ops);
  output_asm_insn ("ldf%?e\t%0, [%|sp], #12", operands);

  return "";
}

/* Output a move from an fpu register to unicore64 registers.
   OPERANDS[0] is the first registers of an unicore64 register pair.
   OPERANDS[1] is an fpu register.  */

const char *
output_mov_long_double_unicore64_from_fpu (rtx * operands)
{
  int unicore64_reg0 = REGNO (operands[0]);
  rtx ops[3];

  if (unicore64_reg0 == IP_REGNUM)
    abort ();

  ops[0] = gen_rtx_REG (SImode, unicore64_reg0);
  ops[1] = gen_rtx_REG (SImode, 1 + unicore64_reg0);
  ops[2] = gen_rtx_REG (SImode, 2 + unicore64_reg0);

  output_asm_insn ("stf%?e\t%1, [%|sp, #-12]!", operands);
  output_asm_insn ("ldm%?fd\t%|sp!, {%0, %1, %2}", ops);
  return "";
}
#endif // star, 20090727

/* gaoyi, star
 * functisn for print odd fp-register names.
 */
void
get_next_reg_name (char *buff, int i)
{
  int regnum;
  regnum = i - FIRST_UNICORE64_FP_REGNUM;
  regnum = regnum * 2 + 1;
  buff[0] = 'f';
  if (regnum / 10 == 0)
    {
      buff[1] = regnum % 10 + 48;
      buff[2] = 0;
    }
  else
    {
      buff[1] = regnum / 10 + 48;
      buff[2] = regnum % 10 + 48;
      buff[3] = 0;
    }
}

static void
get_fp_reg_name (char *buff, int regnum)
{
  buff[0] = 'f';
  if (regnum / 10 == 0)
    {
      buff[1] = regnum % 10 + 48;
      buff[2] = 0;
    }
  else
    {
      buff[1] = regnum / 10 + 48;
      buff[2] = regnum % 10 + 48;
      buff[3] = 0;
    }
}
#if 0
/* Output a move from unicore64 registers to unicore64 registers of a long double
   OPERANDS[0] is the destination.
   OPERANDS[1] is the source.  */
/* gaoyi,star
 */
// huangping: FIXME
const char *
output_mov_long_double_unicore64_from_unicore64 (rtx * operands)
{
  /* We have to be careful here because the two might overlap.  */
  int dest_start = REGNO (operands[0]);
  int src_start = REGNO (operands[1]);
  rtx ops[2];
  int i;

  if (dest_start < src_start)
    {
      for (i = 0; i < 3; i++)
	{
	  ops[0] = gen_rtx_REG (SImode, dest_start + i);
	  ops[1] = gen_rtx_REG (SImode, src_start + i);
	  output_asm_insn ("mov\t%0, %1", ops);
	}
    }
  else
    {
      for (i = 2; i >= 0; i--)
	{
	  ops[0] = gen_rtx_REG (SImode, dest_start + i);
	  ops[1] = gen_rtx_REG (SImode, src_start + i);
	  output_asm_insn ("mov\t%0, %1", ops);
	}
    }

  return "";
}
#endif

/* Output a move from unicore64 registers to an fpu registers.
   OPERANDS[0] is an fpu register.
   OPERANDS[1] is the unicore64 register.  */

const char *
output_mov_double_fpu_from_unicore64 (rtx * operands)
{
  int unicore64_reg0 = REGNO (operands[1]);
  rtx ops[2];

  ops[0] = gen_rtx_REG (DImode, unicore64_reg0);
  ops[1] = operands[0];
  output_asm_insn ("mtf\t%0, %1", ops);
  return "";
}

/* Output a move from an fpu register to unicore64 registers.
   OPERANDS[0] is the unicore64 register.
   OPERANDS[1] is an fpu register.  */

const char *
output_mov_double_unicore64_from_fpu (rtx * operands)
{
  output_asm_insn ("mff\t%0, %1", operands);
  return "";
}
/* star */
char *
output_move_double_fpu (rtx * operands)
{
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);
  rtx otherops[3];
  int regn0, regn1, offset;
  char buf[200];
  char fgname[4];
  if (code0 == REG)
    {

      if (code1 == MEM)
	{
	  regn0 = REGNO (operands[0]);
	  regn1 = GET_CODE (XEXP (operands[1], 0)) == REG 
	    ? REGNO (XEXP(operands[1], 0)) 
	    : REGNO (XEXP(XEXP(operands[1], 0), 0));
	  switch (GET_CODE (XEXP (operands[1], 0)))
	    {
	    case REG:
	      sprintf (buf, "ldwf\t%s%s, [%sr%d+], #%d", REGISTER_PREFIX, reg_names[regn0],
		      REGISTER_PREFIX, regn1, 0);
	      output_asm_insn (buf, operands);
	      break;

	    case PRE_INC:
	      abort ();		/* Should never happen now */
	      break;

	    case PRE_DEC:
	      sprintf (buf, "ldwf.w\t%s%s, [%sr%d-], #%d", REGISTER_PREFIX, reg_names[regn0],
		      REGISTER_PREFIX, regn1, 8);
	      output_asm_insn (buf, operands);
	      break;

	    case POST_INC:
	      sprintf (buf, "ldwf.w\t%s%s, [%sr%d]+, #%d", REGISTER_PREFIX, reg_names[regn0],
		      REGISTER_PREFIX, regn1, 8);
	      output_asm_insn (buf, operands);
	      break;

	    case POST_DEC:
	      abort ();		/* Should never happen now */
	      break;

	    case LABEL_REF:
	      output_asm_insn ("ldwf\t%0,%W1", operands);
	      break;

	    case CONST:
	    default:
	      output_asm_insn ("ldwf\t%0,%W1", operands);
	      break;
	    }

	}
      else
	abort ();
    }
  else if (code0 == MEM && code1 == REG)
    {
      if (REGNO (operands[1]) == IP_REGNUM)
	abort ();

      regn0 = REGNO (operands[1]);
      regn1 = GET_CODE (XEXP (operands[0], 0)) == REG 
	? REGNO (XEXP(operands[0], 0)) 
	: REGNO (XEXP(XEXP(operands[0], 0), 0));
      switch (GET_CODE (XEXP (operands[0], 0)))
	{
	case REG:
	  sprintf (buf, "sdwf\t%s%s, [%sr%d+], #%d", REGISTER_PREFIX, reg_names[regn0],
		   REGISTER_PREFIX, regn1, 0);
	  output_asm_insn (buf, operands);
	  break;

	case PRE_INC:
	  abort ();		/* Should never happen now */
	  break;

	case PRE_DEC:
	  sprintf (buf, "sdwf.w\t%s%s, [%sr%d-], #%d", REGISTER_PREFIX, reg_names[regn0],
		   REGISTER_PREFIX, regn1, 8);
	  output_asm_insn (buf, operands);
	  break;

	case POST_INC:
	  sprintf (buf, "sdwf.w\t%s%s, [%sr%d]+, #%d", REGISTER_PREFIX, reg_names[regn0],
		   REGISTER_PREFIX, regn1, 8);
	  output_asm_insn (buf, operands);
	  break;

	case POST_DEC:
	  abort ();		/* Should never happen now */
	  break;

	default:
	  output_asm_insn ("sdwf\t%1, %W0", operands);
	}
    }
  else
    abort ();			/* Constraints should prevent this */
  return "";

}

const char *
output_ldm (rtx * operands)
{
  int regno_0, regno_1;
  int no_regs;
  char buf[200];
  int i;
  
  regno_0 = REGNO (operands[0]);
  regno_1 = GET_CODE (XEXP (operands[1], 0)) == REG 
    ? REGNO (XEXP(operands[1], 0)) 
    : REGNO (XEXP(XEXP(operands[1], 0), 0));
  no_regs = NUM_REGS (GET_MODE (operands[0]));
  for (i = 0; i< no_regs; i++)
    {
      if (regno_0 == PC_REGNUM)
	{
	  sprintf (buf, "ldd\t%sr%d, [%sr%d+], #%d", REGISTER_PREFIX, LR_REGNUM,
		   REGISTER_PREFIX, regno_1, 8 * i);
	  output_asm_insn (buf, operands);
	  sprintf (buf, "return\t");
	  output_asm_insn (buf, operands);
	}
      else
	{
	  sprintf (buf, "ldd\t%sr%d, [%sr%d+], #%d", REGISTER_PREFIX, regno_0,
		   REGISTER_PREFIX, regno_1, 8 * i);
	  output_asm_insn (buf, operands);
	}
      regno_0 ++;
    }
	return "";
}


const char *
output_stm (rtx * operands)
{
  int regno_0, regno_1;
  int no_regs;
  char buf[200];
  int i;
  
  regno_0 = REGNO (operands[1]);
  regno_1 = GET_CODE (XEXP (operands[0], 0)) == REG 
    ? REGNO (XEXP(operands[0], 0)) 
    : REGNO (XEXP(XEXP(operands[0], 0), 0));
  no_regs = NUM_REGS (GET_MODE (operands[1]));
  for (i = 0; i< no_regs; i++)
    {
	  sprintf (buf, "std\t%sr%d, [%sr%d+], #%d", REGISTER_PREFIX, regno_0 + i,
		   REGISTER_PREFIX, regno_1, 8 * i);
	  output_asm_insn (buf, operands);
    }
	return "";
}

/* Output a move between double words.
   It must be REG<-REG, REG<-CONST_DOUBLE, REG<-CONST_INT, REG<-MEM
   or MEM<-REG and all MEMs must be offsettable addresses.  */
/* gaoyi,star
 */
// huangping: FIXME
const char *
output_move_double (rtx * operands)
{
  enum rtx_code code0 = GET_CODE (operands[0]);
  enum rtx_code code1 = GET_CODE (operands[1]);
  rtx otherops[3];
  int regn0, regn1, offset;
  char buf[200];
  if (code0 == REG)
    {
      int reg0 = REGNO (operands[0]);

      if (code1 == REG)
	{
	  int reg1 = REGNO (operands[1]);
	  output_asm_insn ("dmov\t%0, %1", operands);
	}
      else if (code1 == CONST_DOUBLE)  // huangping: FIXME
	{
	  output_mov_immediate (operands);
	}
      else if (code1 == CONST_INT)
	{
	  /*FIXME  star */
	  output_mov_immediate (operands);
	}
      else if (code1 == MEM)
	{
	  regn0 = REGNO (operands[0]);
	  regn1 = GET_CODE (XEXP (operands[1], 0)) == REG 
	    ? REGNO (XEXP(operands[1], 0)) 
	    : REGNO (XEXP(XEXP(operands[1], 0), 0));
	  switch (GET_CODE (XEXP (operands[1], 0)))
	    {
	    case REG:
	      output_asm_insn ("ldd\t%0, [%m1]", operands);
	      break;

	    case PRE_INC:
	      abort ();		/* Should never happen now.  */
	      break;

	    case PRE_DEC:
		output_asm_insn ("ldd.w\t%0, [%m1-], #8", operands);
	      break;

	    case POST_INC:
		output_asm_insn ("ldd.w\t%0, [%m1]+, #8", operands);
	      break;

	    case POST_DEC:
	      abort ();		/* Should never happen now.  */
	      break;

	    case LABEL_REF:
	    case CONST:
	      output_asm_insn ("adr\t%0, %1", operands);
	      output_asm_insn ("ldd\t%0, [%0+], #0", operands);
	      break;

	    default:
	      if (unicore64_add_operand (XEXP (XEXP (operands[1], 0), 1),
					 GET_MODE (XEXP
						   (XEXP (operands[1], 0),
						    1))))
		{
		  otherops[0] = operands[0];
		  otherops[1] = XEXP (XEXP (operands[1], 0), 0);
		  otherops[2] = XEXP (XEXP (operands[1], 0), 1);

		  if (GET_CODE (XEXP (operands[1], 0)) == PLUS)
		    {
		      if (GET_CODE (otherops[2]) == CONST_INT)
			{
			  switch (INTVAL (otherops[2]))
			    {
			      /*FIXME-64 : star */
			    case -8:
			      output_asm_insn ("ldd\t%0, [%1-], #8", otherops);
			      return "";
			    case -4:
			      output_asm_insn ("ldd\t%0, [%1-], #4", otherops);
			      return "";
			    case 4:
			      output_asm_insn ("ldd\t%0, [%1+], #4", otherops);
			      return "";
			    }

			  if (!
			      (const_ok_for_unicore64(INTVAL (otherops[2]))))
			    output_asm_insn ("dsub\t%0, %1, #%n2", otherops);
			  else
			    output_asm_insn ("dadd\t%0, %1, %2", otherops);
			}
		      else
			output_asm_insn ("dadd\t%0, %1, %2", otherops);
		    }
		  else
		    output_asm_insn ("dsub\t%0, %1, %2", otherops);

		  return "ldd\t%0, [%0+]";
		}
	      else
		{
		  if ((GET_CODE (XEXP (operands[1], 0)) == PLUS))
		    {
		      output_asm_insn ("ldd%T1\t%0, %1", operands);
		    }
		}
	    }
	}
      else
	abort ();		/* Constraints should prevent this.  */
    }
  else if (code0 == MEM && code1 == REG)
    {
      switch (GET_CODE (XEXP (operands[0], 0)))
	{
	  
	case REG:
	  output_asm_insn ("std\t%1, [%m0]", operands);
	  break;

	case PRE_INC:
	  abort ();		/* Should never happen now.  */
	  break;

	case PRE_DEC:
	  output_asm_insn ("std.w\t%1, [%m0-], #8", operands);
	  break;

	case POST_INC:
	  output_asm_insn ("std.w\t%1, [%m0]+, #8", operands);
	  break;

	case POST_DEC:
	  abort ();		/* Should never happen now.  */
	  break;

	case PLUS:
	  if (GET_CODE (XEXP (XEXP (operands[0], 0), 1)) == CONST_INT)
	    {
	      switch (INTVAL (XEXP (XEXP (operands[0], 0), 1)))
		{
		case -8:
		  output_asm_insn ("std\t%1, [%m0-], #8", operands);
		  return "";

		case -4:
		  output_asm_insn ("std\t%1, [%m0-], #4", operands);
		  return "";

		case 4:
		  output_asm_insn ("std\t%1, [%m0+], #4", operands);
		  return "";
		}
	    }
	  /* Fall through */

	default:
	  if ((GET_CODE (XEXP (operands[0], 0)) == PLUS))
	    {
	      output_asm_insn ("std%T0\t%1, %0", operands);
	    }

	}
    }
  else
    /* Constraints should prevent this.  */
    abort ();

  return "";
}

/* Output an arbitrary MOV reg, #n.
   OPERANDS[0] is a register.  OPERANDS[1] is a const_int.  */

const char *
output_mov_immediate (rtx * operands)
{
  HOST_WIDE_INT n = INTVAL (operands[1]);

  /* Try to use one MOV.  */
  if (const_ok_for_unicore64 (n))
    output_asm_insn ("dmov\t%0, %1", operands);

  /* Try to use one MVN.  */
  else if (const_ok_for_unicore64 (~n))
    {
      operands[1] = GEN_INT (~n);
      output_asm_insn ("dnot\t%0, %1", operands);
    }
  else
    {
      /* star : UniCore-3 */
      emit_insn (gen_rtx_SET (VOIDmode, operands[0], operands[1]));
    }

  return "";
}

/* Output an ADD r, s, #n where n may be too big for one instruction.
   If adding zero to one register, output nothing.  */

const char *
output_add_immediate (rtx * operands)
{
  HOST_WIDE_INT n = INTVAL (operands[2]);

  if (n != 0 || REGNO (operands[0]) != REGNO (operands[1]))
    {
      /* star modify for UniCore-3. FIXME-UniCore3*/
      if (const_ok_for_unicore64 (n))
	output_asm_insn ("dadd\t%0, %1, %2", operands);
      else if (const_ok_for_unicore64 (-n))
	{
	  operands[2] = GEN_INT (-n);
	  output_asm_insn ("dsub\t%0, %1, %2", operands);
	}
      else
	{
	  /* star modify for UniCore-3. huangping: FIXME */
	    {
	      int H = (n & (0x7ff << 22)) >> 22; 
	      int M = (n & (0x7ff << 11)) >> 11; 
	      int L = n & 0x7ff; 
	      int init_ip = 0;

	      if (H)
		{
		  int shift = M ? 11 : 22;
		  init_ip = 1;
		  fprintf (asm_out_file, "\tdmov\tip, #%d\t@output_multi_immediate\n", H);
		  fprintf (asm_out_file, "\tdlsl\tip, ip, #%d\n", shift);
		}
	      if (M)
		{
		  if (init_ip)
		    fprintf (asm_out_file, "\tdadd\tip, ip, #%d\n", M);
		  else
		    fprintf (asm_out_file, "\tdmov\tip, #%d\t@output_multi_immediate\n", M);
		  init_ip = 1;
		  fprintf (asm_out_file, "\tdlsl\tip, ip, #11\n");
		}
	      if (L)
		{
		  if (init_ip)
		    fprintf (asm_out_file, "\tdadd\tip, ip, #%d\n", L);
		  else
		    fprintf (asm_out_file, "\tdmov\tip, #%d\t@output_multi_immediate\n", L);
		}

	      operands[2] = gen_rtx_REG (SImode, IP_REGNUM);
	      output_asm_insn ("dadd\t%0, %1, %2", operands);
	    }
	}
    }

  return "";
}

/* Return the appropriate UNICORE64 instruction for the operation code.
   The returned result should not be overwritten.  OP is the rtx of the
   operation.  SHIFT_FIRST_ARG is TRUE if the first argument of the operator
   was shifted.  */

const char *
arithmetic_instr (rtx op, int shift_first_arg)
{
  switch (GET_CODE (op))
    {
    case PLUS:
      return "dadd";

    case MINUS:
      return shift_first_arg ? "drsub" : "dsub";

    case IOR:
      return "dor";

    case XOR:
      return "dxor";

    case AND:
      return "dand";

    default:
      abort ();
    }
}

/* Output a .ascii pseudo-op, keeping track of lengths.  This is because
   /bin/as is horribly restrictive.  */
#define MAX_ASCII_LEN 51

void
output_ascii_pseudo_op (FILE * stream, const unsigned char *p, int len)
{
  int i;
  int len_so_far = 0;

  fputs ("\t.ascii\t\"", stream);

  for (i = 0; i < len; i++)
    {
      int c = p[i];

      if (len_so_far >= MAX_ASCII_LEN)
	{
	  fputs ("\"\n\t.ascii\t\"", stream);
	  len_so_far = 0;
	}

      if (ISPRINT (c))
	{
	  if (c == '\\' || c == '\"')
	    {
	      putc ('\\', stream);
	      len_so_far++;
	    }
	  putc (c, stream);
	  len_so_far++;
	}
      else
	{
	  fprintf (stream, "\\%03o", c);
	  len_so_far += 4;
	}
    }

  fputs ("\"\n", stream);
}

/* Compute the register sabe mask for registers 0 through 12
   inclusive.  This code is used by both unicore64_compute_save_reg_mask
   and unicore64_compute_initial_elimination_offset.  */
/* gaoyi: 
 * in UNICORE64 save reg0-reg28
 * assume FIQ only use reg0-reg7,ISR use reg0-reg28
 * sar:
 * add exception handler return
 */
static unsigned long
unicore64_compute_save_reg0_reg28_mask (void)
{
  unsigned long func_type = unicore64_current_func_type ();
  unsigned int save_reg_mask = 0;
  unsigned int reg;

  if (IS_INTERRUPT (func_type))
    {
      unsigned int max_reg;
      /* Interrupt functions must not corrupt any registers,
         even call clobbered ones.  If this is a leaf function
         we can just examine the registers used by the RTL, but
         otherwise we have to assume that whatever function is
         called might clobber anything, and so we have to save
         all the call-clobbered registers as well.  */
      if (UNICORE64_FUNC_TYPE (func_type) == UNICORE64_FT_FIQ)
	/* FIQ handlers have registers r8 - r12 banked, so
	   we only need to check r0 - r7, Normal ISRs only
	   bank r14 and r15, so we must check up to r12.
	   r13 is the stack pointer which is always preserved,
	   so we do not need to consider it here.  */
	max_reg = 7;
      else
	max_reg = 28;

      for (reg = 0; reg <= max_reg; reg++)
	if (df_regs_ever_live_p (reg)
	    || (!current_function_is_leaf && call_used_regs[reg]))
	  save_reg_mask |= (1 << reg);
      /* Also save the pic base register if necessary.  */
      if (flag_pic
	  && !TARGET_SINGLE_PIC_BASE
	  && unicore64_pic_register != INVALID_REGNUM
	  && crtl->uses_pic_offset_table)
	save_reg_mask |= 1 << PIC_OFFSET_TABLE_REGNUM;
    }

  else
    {
      /* In the normal case we only need to save those registers
         which are call saved and which are used by this function.  */
      for (reg = 0; reg <= 26; reg++)
	if (df_regs_ever_live_p (reg) && !call_used_regs[reg])
	  save_reg_mask |= (1 << reg);

      /* Handle the frame pointer as a special case.  */
      if (!frame_pointer_needed
	  && df_regs_ever_live_p (HARD_FRAME_POINTER_REGNUM)
	  && !call_used_regs[HARD_FRAME_POINTER_REGNUM])
	save_reg_mask |= 1 << HARD_FRAME_POINTER_REGNUM;

      /* If we aren't loading the PIC register,
         don't stack it even though it may be live.  */
/* star modify for sl save */
      if (flag_pic
	  && !TARGET_SINGLE_PIC_BASE
	  && unicore64_pic_register != INVALID_REGNUM
	  && (df_regs_ever_live_p (PIC_OFFSET_TABLE_REGNUM)
	      || crtl->uses_pic_offset_table))
	save_reg_mask |= 1 << PIC_OFFSET_TABLE_REGNUM;
    }

  /* Save registers so the exception handler can modify them.  */
  if (crtl->calls_eh_return)
    {
      unsigned int i;

      for (i = 0;; i++)
	{
	  reg = EH_RETURN_DATA_REGNO (i);
	  if (reg == INVALID_REGNUM)
	    break;
	  save_reg_mask |= 1 << reg;
	}
    }

  return save_reg_mask;
}

/* Compute a bit mask of which registers need to be
   saved on the stack for the current function.  */

static unsigned long
unicore64_compute_save_reg_mask (void)
{
  unsigned int save_reg_mask = 0;
  unsigned long func_type = unicore64_current_func_type ();

  if (IS_NAKED (func_type))
    /* This should never really happen.  */
    return 0;

  /* If we are creating a stack frame, then we must save the frame pointer,
     IP (which will hold the old stack pointer), LR and the PC.  */
  if (frame_pointer_needed)
    save_reg_mask |=
      (1 << UNICORE64_HARD_FRAME_POINTER_REGNUM)
      | (1 << IP_REGNUM) | (1 << LR_REGNUM) | (1 << PC_REGNUM);

  /* Volatile functions do not return, so there
     is no need to save any other registers.  */
  if (IS_VOLATILE (func_type))
    return save_reg_mask;

  save_reg_mask |= unicore64_compute_save_reg0_reg28_mask ();

  /* Decide if we need to save the link register.
     Interrupt routines have their own banked link register,
     so they never need to save it.
     Otherwise if we do not use the link register we do not need to save
     it.  If we are pushing other registers onto the stack however, we
     can save an instruction in the epilogue by pushing the link register
     now and then popping it back into the PC.  This incurs extra memory
     accesses though, so we only do it when optimising for size, and only
     if we know that we will not need a fancy return sequence.  */
  if (df_regs_ever_live_p (LR_REGNUM))
    save_reg_mask |= 1 << LR_REGNUM;

  if (cfun->machine->lr_save_eliminated)
    save_reg_mask &= ~(1 << LR_REGNUM);

  return save_reg_mask;
}

/* Generate a function exit sequence.  If REALLY_RETURN is true, then do
   everything bar the final return instruction.  */
/* gaoyi:
 * delete all conditonal insn
 */
const char *
output_return_instruction (rtx operand,
			   int really_return, int reverse ATTRIBUTE_UNUSED)
{
  char instr[200];
  char suffix[5];
  char instr_bak[200];
  int offset_bak;
  int change_base;
  int reg;
  unsigned long live_regs_mask;
  unsigned long func_type;

  func_type = unicore64_current_func_type ();

  if (IS_NAKED (func_type))
    return "";

  if (IS_VOLATILE (func_type) && TARGET_ABORT_NORETURN)
    {
      /* If this function was declared non-returning, and we have found a tail 
         call, then we have to trust that the called function won't return.  */
      if (really_return)
	{
	  rtx ops[2];

	  /* Otherwise, trap an attempted return by aborting.  */
	  ops[0] = operand;
	  ops[1] = gen_rtx_SYMBOL_REF (Pmode, NEED_PLT_RELOC ? "abort(PLT)"
				       : "abort");
	  assemble_external_libcall (ops[1]);
	  output_asm_insn ("call\t%a1", ops);
	}

      return "";
    }

  if (cfun->calls_alloca && !really_return)
    abort ();

  /* Construct the conditional part of the instruction(s) to be emitted.  */
  /* gaoyi delete
     sprintf (conditional, "%%?%%%c0", reverse ? 'D' : 'd');
   */
  return_used_this_function = 1;

  live_regs_mask = unicore64_compute_save_reg_mask ();

  if (live_regs_mask)
    {
      const char *return_reg;

      /* If we do not have any special requirements for function exit 
         (eg interworking, or ISR) then we can load the return address 
         directly into the PC.  Otherwise we must load it into LR.  */
      if (really_return && !TARGET_INTERWORK)
	return_reg = reg_names[PC_REGNUM];
      else
	return_reg = reg_names[LR_REGNUM];

      if ((live_regs_mask & (1 << IP_REGNUM)) == (1 << IP_REGNUM))
	/* There are two possible reasons for the IP register being saved.
	   Either a stack frame was created, in which case IP contains the
	   old stack pointer, or an ISR routine corrupted it.  If this in an
	   ISR routine then just restore IP, otherwise restore IP into SP.  */
	if (!IS_INTERRUPT (func_type))
	  {
	    live_regs_mask &= ~(1 << IP_REGNUM);
	    live_regs_mask |= (1 << SP_REGNUM);
	  }

      /* On some UNICORE64 architectures it is faster to use LDR rather than
         LDM to load a single register.  On other architectures, the
         cost is the same.  In 26 bit mode, or for exception handlers,
         we have to use LDM to load the PC so that the CPSR is also
         restored.  */
      for (reg = 0; reg <= LAST_UNICORE64_REGNUM; reg++)
	{
	  if (live_regs_mask == (unsigned int) (1 << reg))
	    break;
	}
      if (reg <= LAST_UNICORE64_REGNUM
	  && (reg != LR_REGNUM
	      || !really_return || !IS_INTERRUPT (func_type)))
	{
	  if (((reg == LR_REGNUM) && (really_return && !TARGET_INTERWORK)) || (reg == PC_REGNUM))
	    {
	      sprintf (instr, "ldd\t%%|%s, [%%|sp+], #0", 
		       reg_names[LR_REGNUM]);
	      output_asm_insn (instr, &operand);
	      sprintf (instr, "dadd\t%%|sp, %%|sp, #8");
	      output_asm_insn (instr, &operand);
	      sprintf (instr, "return");
	      output_asm_insn (instr, &operand);
	    }
	  else
	    {
	      sprintf (instr, "ldd\t%%|%s, [%%|sp+], #0", 
		       (reg == LR_REGNUM)? return_reg : reg_names[reg]);
	      output_asm_insn (instr, &operand);
	      sprintf (instr, "dadd\t%%|sp, %%|sp, #8");
	      output_asm_insn (instr, &operand);

	    }
	}
      else
	{
	  char *p;
	  int first = 1;
	  int  ld_type;

	  /* Generate the load multiple instruction to restore the registers.  */
	  /* gaoyi  */
	  if (frame_pointer_needed)
	    {
	      if (live_regs_mask & 0xFFFF)
		{
		  int reg_count = 0;
		  for (reg = 16; reg <= LR_REGNUM; reg++)
		    if (live_regs_mask & (1 << reg))
		      reg_count++;
		  sprintf (instr, "dsub\t%%|fp, %%|fp, #%d", reg_count * 8);
		  output_asm_insn (instr, &operand);
		  int reg_no = 0;
		  for (reg = 0; reg <= 15; reg++)
		    if (live_regs_mask & (1 << reg))
		      reg_no ++;

		  for (reg = 0; reg <= 15; reg++)
		    if (live_regs_mask & (1 << reg))
		      {
			int l = strlen (reg_names[reg]);
			sprintf (instr, "ldd\t");
			p = instr + strlen (instr);
			memcpy (p, "%|", 2);
			memcpy (p + 2, reg_names[reg], l);
			p += l + 2;
			strcpy (p, ", [%%|fp-], #");
			sprintf (instr + strlen(instr), "%d", reg_no * 8);
			output_asm_insn (instr, &operand);
			reg_no --; // huangping: FIXME
		      }
		  //sprintf (instr, "ldmfd\t%%|fp, (");
		  ld_type = 1;

		}
	      else
		{
		  //sprintf (instr, "ldmea\t%%|fp, (");
		  ld_type = 2;
		}
	    }
	  else
	    {
	      if (live_regs_mask & 0xFFFF)
		{
		  int k = 0;
		  for (reg = 0; reg <= 15; reg++)
		    if (live_regs_mask & (1 << reg))
		      {
			sprintf (instr, "ldd.w\t");
			p = instr + strlen (instr);
			int l = strlen (reg_names[reg]);
			memcpy (p, "%|", 2);
			memcpy (p + 2, reg_names[reg], l);
			p += l + 2;
			sprintf (p , ", [%%|sp]+, #%d ", k*8);
			output_asm_insn (instr, &operand);
			k++;
		      }
		  sprintf (p , "dadd\tsp, sp, #%d ", k*8);
		}

	      //sprintf (instr, "ldmfd\t%%|sp!, (");
	      ld_type = 3;
	    }
	  first = 1;
	  int count = 0;
	  int reg_no = 0;
	  for (reg = 16; reg <= SP_REGNUM; reg++)
	    if(live_regs_mask & (1<<reg))
	      reg_no ++;
	  if (live_regs_mask & (1 << LR_REGNUM))
	    reg_no ++;
	  change_base = 0;
	  for (reg = 16; reg < SP_REGNUM; reg++)
	    if (live_regs_mask & (1 << reg))
	      {
		switch (ld_type)
		  {
		  case 1:
		    sprintf (instr, "ldd\t");
		    sprintf (suffix, "+], #%d", 8 * count);
		    break;
		  case 2:
		    sprintf (instr, "ldd\t");
		    sprintf (suffix, "-], #%d", 8 * (reg_no - count));
		    break;
		  case 3:
		    sprintf (instr, "ldd.w\t");
		    sprintf (suffix, "]+, #8");
		    break;
		  }
		int l = strlen (reg_names[reg]);
		p = instr + strlen (instr);
		
		memcpy (p, "%|", 2);
		memcpy (p + 2, reg_names[reg], l);
		p += l + 2;
		memcpy (p, ", ", 2);
		p += 2;
		if (frame_pointer_needed)
		  {
		    if (!change_base)
		      sprintf (p, " [fp%s", suffix);
		    else
		      sprintf (p, " [ip%s", suffix);
		    if (reg == FP_REGNUM)
		      {
			output_asm_insn ("dmov\tip, fp", &operand);
			change_base = 1;
		      }
		  }
		else
		  {
		    if (!change_base)
		      sprintf (p, " [sp%s", suffix);
		    else
		      sprintf (p, " [ip%s", suffix);
		    if (reg == SP_REGNUM)
		      {
			output_asm_insn ("dmov\tip, sp", &operand);
			change_base = 1;
		      }
		  }
		output_asm_insn (instr, &operand);
		count ++;
	      }

	  if (live_regs_mask & (1 << LR_REGNUM))
	    {
	      switch (ld_type)
		{
		  case 1:
		    sprintf (instr, "ldd\t");
		    sprintf (suffix, "+], #%d", 8 * count);
		    break;
		  case 2:
		    sprintf (instr, "ldd\t");
		    sprintf (suffix, "-], #%d", 8);
		    break;
		  case 3:
		    sprintf (instr, "ldd.w\t");
		    sprintf (suffix, "]+, #8");
		    break;
		}
	      int l = strlen (return_reg);
	      p = instr + strlen (instr);
	      memcpy (p, "%|", 2);
	      if (strcmp (return_reg, reg_names[PC_REGNUM]) == 0)
		{
		  memcpy (p + 2, reg_names[LR_REGNUM], l);
		}
	      else
		  memcpy (p + 2, return_reg, l);
	      p += l + 2;
	      memcpy (p, ", ", 2);
	      p += 2;
	      if (frame_pointer_needed)
		{
		  if (!change_base)
		    sprintf (p, " [fp%s", suffix);
		  else
		    sprintf (p, " [ip%s", suffix);
		}
	      else
		{
		  if (!change_base)
		    sprintf (p, " [sp%s", suffix);
		  else
		    sprintf (p, " [ip%s", suffix);
		}
	      output_asm_insn (instr, &operand);
	    }

	  if (live_regs_mask & (1 << SP_REGNUM))
	    {
		switch (ld_type)
		  {
		  case 1:
		    sprintf (instr, "ldd\t");
		    sprintf (suffix, "+], #%d", 8 * count);
		    break;
		  case 2:
		    sprintf (instr, "ldd\t");
		    sprintf (suffix, "-], #%d", 16);
		    break;
		  case 3:
		    sprintf (instr, "ldd.w\t");
		    sprintf (suffix, "]+, #16");
		    break;
		  }
		int l = strlen (reg_names[SP_REGNUM]);
		p = instr + strlen (instr);
		
		memcpy (p, "%|", 2);
		memcpy (p + 2, reg_names[SP_REGNUM], l);
		p += l + 2;
		memcpy (p, ", ", 2);
		p += 2;
		if (frame_pointer_needed)
		  {
		    if (!change_base)
		      sprintf (p, " [fp%s", suffix);
		    else
		      sprintf (p, " [ip%s", suffix);
		  }
		else
		  {
		    if (!change_base)
		      sprintf (p, " [sp%s", suffix);
		    else
		      sprintf (p, " [ip%s", suffix);
		    if (reg == SP_REGNUM)
		      {
			output_asm_insn ("dmov\tip, sp", &operand);
			change_base = 1;
		      }
		  }
		output_asm_insn (instr, &operand);

	    }

	  if (strcmp (return_reg, reg_names[PC_REGNUM]) == 0)
	    {
	      output_asm_insn ("return", &operand);
	    }
	}


      /* See if we need to generate an extra instruction to
         perform the actual function return.  */
      if (really_return
	  && func_type != UNICORE64_FT_INTERWORKED
	  && (live_regs_mask & (1 << LR_REGNUM)) != 0)
	{
	  /* The return has already been handled
	     by loading the LR into the PC.  */
	  really_return = 0;
	}
    }

  if (really_return)
    {
      switch ((int) UNICORE64_FUNC_TYPE (func_type))
	{
	case UNICORE64_FT_ISR:
	case UNICORE64_FT_FIQ:
	  sprintf (instr, "dsub.a\t%%|pc, %%|lr, #8");
	  break;

	case UNICORE64_FT_INTERWORKED:
	  sprintf (instr, "return");
	  break;

	case UNICORE64_FT_EXCEPTION:
	  sprintf (instr, "dcmpsub.a\t%%|lr, #0\n", LR_REGNUM);//cmc 20110422
	  output_asm_insn (instr, &operand);
	  sprintf (instr, "dmov\t%%|pc, %%|lr");
	  break;

	default:
	  /* UNICORE64 implementations always provide BX, so interworking
	     is the default unless lr is in use.  */
	  /*star
	  if ((insn_flags & FL_ARCH1) != 0)
	    sprintf (instr, "jump\t%%|lr");
	  else
	    sprintf (instr, "mov\t%%|pc, %%|lr");
	  */
	    sprintf (instr, "return");
	    break;
	}

      output_asm_insn (instr, &operand);
    }

  return "";
}

/* Write the function name into the code section, directly preceding
   the function prologue.

   Code will be output similar to this:
     t0
	 .ascii "unicore64_poke_function_name", 0
	 .align
     t1
	 .word 0xff000000 + (t1 - t0)
     unicore64_poke_function_name
	 mov     ip, sp
	 stm.w   (fp, ip, lr, pc), [sp]+ 
	 sub     fp, ip, #4

   When performing a stack backtrace, code can inspect the value
   of 'pc' stored at 'fp' + 0.  If the trace function then looks
   at location pc - 12 and the top 8 bits are set, then we know
   that there is a function name embedded immediately preceding this
   location and has length ((pc[-3]) & 0xff000000).

   We assume that pc is declared as a pointer to an unsigned long.

   It is of no benefit to output the function name if we are assembling
   a leaf function.  These function types will not contain a stack
   backtrace structure, therefore it is not possible to determine the
   function name.  */

void
unicore64_poke_function_name (FILE * stream, const char *name)
{
  unsigned long alignlength;
  unsigned long length;
  rtx x;

  length = strlen (name) + 1;
  alignlength = ROUND_UP_WORD (length);

  ASM_OUTPUT_ASCII (stream, name, length);
  ASM_OUTPUT_ALIGN (stream, 2);
  x = GEN_INT ((unsigned HOST_WIDE_INT) 0xff000000 + alignlength);
  assemble_aligned_integer (UNITS_PER_WORD, x);
}

/* Place some comments into the assembler stream
   describing the current function.  */
/* gaoyi:
 */
static void
unicore64_output_function_prologue (FILE * f, HOST_WIDE_INT frame_size)
{
  unsigned long func_type;

  if (!TARGET_UNICORE64)
    {
      return;
    }

  /* Sanity check.  */
  if (unicore64_ccfsm_state || unicore64_target_insn)
    abort ();

  func_type = unicore64_current_func_type ();

  switch ((int) UNICORE64_FUNC_TYPE (func_type))
    {
    default:
    case UNICORE64_FT_NORMAL:
      break;
    case UNICORE64_FT_INTERWORKED:
      asm_fprintf (f, "\t%@ Function supports interworking.\n");
      break;
    case UNICORE64_FT_EXCEPTION_HANDLER:
      asm_fprintf (f, "\t%@ C++ Exception Handler.\n");
      break;
    case UNICORE64_FT_ISR:
      asm_fprintf (f, "\t%@ Interrupt Service Routine.\n");
      break;
    case UNICORE64_FT_FIQ:
      asm_fprintf (f, "\t%@ Fast Interrupt Service Routine.\n");
      break;
    case UNICORE64_FT_EXCEPTION:
      asm_fprintf (f, "\t%@ UNICORE64 Exception Handler.\n");
      break;
    }

  if (IS_NAKED (func_type))
    asm_fprintf (f,
		 "\t%@ Naked Function: prologue and epilogue provided by programmer.\n");

  if (IS_VOLATILE (func_type))
    asm_fprintf (f, "\t%@ Volatile: function does not return.\n");

  if (IS_NESTED (func_type))
    asm_fprintf (f,
		 "\t%@ Nested: function declared inside another function.\n");

  asm_fprintf (f, "\t%@ args = %d, pretend = %d, frame = %ld\n",
	       crtl->args.size,
	       crtl->args.pretend_args_size, frame_size);

  asm_fprintf (f, "\t%@ frame_needed = %d, uses_anonymous_args = %d\n",
	       frame_pointer_needed, cfun->machine->uses_anonymous_args);

  if (cfun->machine->lr_save_eliminated)
    asm_fprintf (f, "\t%@ link register save eliminated.\n");

  return_used_this_function = 0;
}

// huangping: FIXME
const char *
unicore64_output_epilogue (int really_return)
{
  int reg;
  unsigned long saved_regs_mask;
  unsigned long func_type;
  /* Floats_offset is the offset from the "virtual" frame. */
  int floats_offset = 0;
  rtx operands[3];
  int frame_size = get_frame_size ();
  FILE *f = asm_out_file;
  char buffer[4];
  rtx eh_ofs = cfun->machine->eh_epilogue_sp_ofs;
  char fp_regname[4];
  /* If we have already generated the return instruction
     then it is futile to generate anything else.  */
  if (use_return_insn (FALSE) && return_used_this_function)
    return "";

  func_type = unicore64_current_func_type ();

  if (IS_NAKED (func_type))
    /* Naked functions don't have epilogues.  */
    return "";

  if (IS_VOLATILE (func_type) && TARGET_ABORT_NORETURN)
    {
      rtx op;

      /* A volatile function should never return.  Call abort.  */
      op =
	gen_rtx_SYMBOL_REF (Pmode, NEED_PLT_RELOC ? "abort(PLT)" : "abort");
      assemble_external_libcall (op);
      output_asm_insn ("call\t%a0", &op);

      return "";
    }

  if (UNICORE64_FUNC_TYPE (func_type) == UNICORE64_FT_EXCEPTION_HANDLER
      && !really_return)
    /* If we are throwing an exception, then we really must
       be doing a return,  so we can't tail-call.  */
    abort ();

  saved_regs_mask = unicore64_compute_save_reg_mask ();

  /* XXX We should adjust floats_offset for any anonymous args, and then
     re-adjust vfp_offset below to compensate.  */

  /* Compute how far away the floats will be.  */
  for (reg = 0; reg <= LAST_UNICORE64_REGNUM; reg++)
    if (saved_regs_mask & (1 << reg))
      floats_offset += 8;

  if (frame_pointer_needed)
    {
      int vfp_offset = 8;

      if (unicore64_fpu_arch == FP_SOFT2)
	{
	  for (reg = LAST_UNICORE64_FP_REGNUM;
	       reg >= FIRST_UNICORE64_FP_REGNUM; reg--)
	    if (df_regs_ever_live_p (reg) && !call_used_regs[reg])
	      {
		floats_offset += 8; // huangping: FIXME
		asm_fprintf (f, "\tldwf\t%r, [%r, #-%d]\n",
			     reg, FP_REGNUM, floats_offset - vfp_offset);
	      }
	}
      else
	{
	  int start_reg = LAST_UNICORE64_FP_REGNUM;
	  
	  int k = 0;
	  for (reg = LAST_UNICORE64_FP_REGNUM;
	       reg >= FIRST_UNICORE64_FP_REGNUM; reg--)
	    {
	      if (df_regs_ever_live_p (reg) && !call_used_regs[reg])
		{
		  floats_offset += 8;

		  /* We can't unstack more than four registers at once.  */
		  if (start_reg - reg == 3 || reg % 4 == 0)
		    {
		      get_next_reg_name (buffer, start_reg);
		      fprintf (f, "\tdadd fp, fp, #-%d\n",
			       floats_offset - vfp_offset);
		      int i, end;
		      end = 2 * (start_reg - FIRST_UNICORE64_FP_REGNUM) + 1;
		      k = 0;
		      //for (i = (reg - FIRST_UNICORE64_FP_REGNUM) * 2; i <= end; i++, k++)
		      /* Rain: 20110815 modified: i++ ----> i += 2 */
		      for (i = (reg - FIRST_UNICORE64_FP_REGNUM) * 2; i <= end; i += 2, k++)
			{
			  get_fp_reg_name (fp_regname, i);
			  fprintf (f, "\tldwf %s%s, [%sfp+], #%d\n", REGISTER_PREFIX, fp_regname, REGISTER_PREFIX, k * 8);
			}

		      fprintf (f, "\tdsub fp, fp, #-%d\n",
			       floats_offset - vfp_offset);
		      start_reg = reg - 1;
		    }
		}
	      else
		{
		  if (reg != start_reg)
		    {
		      get_next_reg_name (buffer, start_reg);
		      fprintf (f, "\tdadd fp, fp, #-%d\n",
			       floats_offset - vfp_offset);
		      int i, end;
		      end = 2 * (start_reg - FIRST_UNICORE64_FP_REGNUM) + 1;
		      k = 0;
		      //for (i = (reg + 1 - FIRST_UNICORE64_FP_REGNUM) * 2; i <= end; i++, k++)
		      /* Rain: 20110815 modified: i++ ----> i += 2 */
		      for (i = (reg + 1 - FIRST_UNICORE64_FP_REGNUM) * 2; i <= end; i += 2, k++)
			{
			  get_fp_reg_name (fp_regname, i);
			  fprintf (f, "\tldwf %s%s, [%sfp+], #%d\n", REGISTER_PREFIX, fp_regname, REGISTER_PREFIX, k * 8);
			}
		      fprintf (f, "\tdsub fp, fp, #-%d\n",
			       floats_offset - vfp_offset);
		      start_reg = reg - 1;
		    }


		  
		  start_reg = reg - 1;
		}
	      
	    }

	  /* Just in case the last register checked also needs unstacking.  */
	  if (reg != start_reg)
	    {
	      get_next_reg_name (buffer, start_reg);
	      fprintf (f, "\tdadd fp, fp, #-%d\n", floats_offset - vfp_offset);
	      int i, end;
	      end = 2 * (start_reg - FIRST_UNICORE64_FP_REGNUM) + 1;
	      k = 0;
	     // for (i = (reg + 1 - FIRST_UNICORE64_FP_REGNUM) * 2; i <= end; i++, k++)
	      /* Rain: 20110815 modified: i++ ----> i += 2 */
	      for (i = (reg + 1 - FIRST_UNICORE64_FP_REGNUM) * 2; i <= end; i += 2, k++)
		{
		  get_fp_reg_name (fp_regname, i);
		  fprintf (f, "\tldwf %s%s, [%sfp+], #%d\n", REGISTER_PREFIX, fp_regname, REGISTER_PREFIX, k * 8);
		}
	      fprintf (f, "\tdsub fp, fp, #-%d\n", floats_offset - vfp_offset);
	      start_reg = reg - 1;
	    }
	}

      /* saved_regs_mask should contain the IP, which at the time of stack
         frame generation actually contains the old stack pointer.  So a
         quick way to unwind the stack is just pop the IP register directly
         into the stack pointer.  */
      if ((saved_regs_mask & (1 << IP_REGNUM)) == 0)
	abort ();
      saved_regs_mask &= ~(1 << IP_REGNUM);
      saved_regs_mask |= (1 << SP_REGNUM);

      /* There are two registers left in saved_regs_mask - LR and PC.  We
         only need to restore the LR register (the return address), but to
         save time we can load it directly into the PC, unless we need a
         special function exit sequence, or we are not really returning.  */
      if (really_return
	  && UNICORE64_FUNC_TYPE (func_type) == UNICORE64_FT_NORMAL)
	/* Delete the LR from the register mask, so that the LR on
	   the stack is loaded into the PC in the register mask.  */
	saved_regs_mask &= ~(1 << LR_REGNUM);
      else
	saved_regs_mask &= ~(1 << PC_REGNUM);
      /* gaoyi */
      if ((saved_regs_mask & 0xFFFF) != 0)
	{
	  int int_offset = 0;
	  int i;
	  for (i = 16; i < 32; i++)
	    {
	      if ((saved_regs_mask & (1 << i)) != 0)
		int_offset += 8;
	    }
	  fprintf (f, "\tdsub\t%sfp, %sfp, #%d\n", REGISTER_PREFIX,
		   REGISTER_PREFIX, int_offset);
	  print_multi_reg (f, "ldd", "-]", FP_REGNUM,
			   saved_regs_mask & 0xFFFF);
	  print_multi_reg (f, "ldd.w", "]+", FP_REGNUM,
			   saved_regs_mask & 0xFFFF0000);
	}
      else
	print_multi_reg (f, "ldd", "-]", FP_REGNUM, saved_regs_mask);

      if (IS_INTERRUPT (func_type))
	/* Interrupt handlers will have pushed the
	   IP onto the stack, so restore it now.  */
	print_multi_reg (f, "ldd.w", "]+", SP_REGNUM, 1 << IP_REGNUM);
    }
  else
    {
      /* Restore stack pointer if necessary.  */
      if (frame_size + crtl->outgoing_args_size != 0)
	{
	  operands[0] = operands[1] = stack_pointer_rtx;
	  operands[2] = GEN_INT (frame_size
				 + crtl->outgoing_args_size);
	  output_add_immediate (operands);
	}

      if (unicore64_fpu_arch == FP_SOFT2)
	{
	  for (reg = FIRST_UNICORE64_FP_REGNUM;
	       /* Rain: 20110815 modified: reg++ ----> reg += 2 */
	       //reg <= LAST_UNICORE64_FP_REGNUM; reg++)
	       reg <= LAST_UNICORE64_FP_REGNUM; reg += 2)
	    if (df_regs_ever_live_p (reg) && !call_used_regs[reg])
	      asm_fprintf (f, "\tldwf\t%r, [%r], #8\n", reg, SP_REGNUM);
	}
      else
	{
	  int start_reg = FIRST_UNICORE64_FP_REGNUM;

	  for (reg = FIRST_UNICORE64_FP_REGNUM;
	       reg <= LAST_UNICORE64_FP_REGNUM; reg++)
	    {
	      if (df_regs_ever_live_p (reg) && !call_used_regs[reg])
		{
		  if (start_reg - reg == 3 || reg % 4 == 3)
		    {
		      get_next_reg_name (buffer, reg);
		      int i, end;
		      end = 2 * (reg - FIRST_UNICORE64_FP_REGNUM) + 1;
		      //for (i = (start_reg  - FIRST_UNICORE64_FP_REGNUM) * 2; i <= end; i++)
		      for (i = (start_reg  - FIRST_UNICORE64_FP_REGNUM) * 2; i <= end; i += 2)
			{
			  get_fp_reg_name (fp_regname, i);
			  fprintf (f, "\tldwf.w %s%s, [%ssp]+, #%d\n", REGISTER_PREFIX, fp_regname, REGISTER_PREFIX, 8);
			}
		      start_reg = reg + 1;
		    }
		}
	      else
		{
		  if (reg != start_reg)
		    {
		      get_next_reg_name (buffer, reg - 1);
		      int i, end;
		      end = 2 * ((reg - 1) - FIRST_UNICORE64_FP_REGNUM) + 1;
		      //for (i = (start_reg  - FIRST_UNICORE64_FP_REGNUM) * 2; i <= end; i++)
		      /* Rain: 20110815 modified: i++ ----> i += 2 */
		      for (i = (start_reg  - FIRST_UNICORE64_FP_REGNUM) * 2; i <= end; i+=2)
			{
			  get_fp_reg_name (fp_regname, i);
			  fprintf (f, "\tldwf.w %s%s, [%ssp]+, #%d\n", REGISTER_PREFIX, fp_regname, REGISTER_PREFIX, 8);
			}
		    }
		  start_reg = reg + 1;
		}
	    }

	  /* Just in case the last register checked also needs unstacking.  */
	  if (reg != start_reg)
	    {
	      get_next_reg_name (buffer, reg - 1);
	      int i, end;
	      end = 2 * ((reg - 1) - FIRST_UNICORE64_FP_REGNUM) + 1;
	      //for (i = (start_reg  - FIRST_UNICORE64_FP_REGNUM) * 2; i <= end; i++)
		      /* Rain: 20110815 modified: i++ ----> i += 2 */
	      for (i = (start_reg  - FIRST_UNICORE64_FP_REGNUM) * 2; i <= end; i+=2)
		{
		  get_fp_reg_name (fp_regname, i);
		  fprintf (f, "\tldwf.w %s%s, [%ssp]+, #%d\n", REGISTER_PREFIX, fp_regname, REGISTER_PREFIX, 8);
		}
	    }
	}

      /* If we can, restore the LR into the PC.  */
      if (UNICORE64_FUNC_TYPE (func_type) == UNICORE64_FT_NORMAL
	  && really_return
	  && crtl->args.pretend_args_size == 0
	  && saved_regs_mask & (1 << LR_REGNUM))
	{
	  saved_regs_mask &= ~(1 << LR_REGNUM);
	  saved_regs_mask |= (1 << PC_REGNUM);
	}

      /* Load the registers off the stack.  If we only have one register
         to load use the LDR instruction - it is faster.  */
      if (saved_regs_mask == (1 << LR_REGNUM))
	{
	  /* The exception handler ignores the LR, so we do
	     not really need to load it off the stack.  */
	  if (eh_ofs)
	    asm_fprintf (f, "\tdadd\t%r, %r, #4\n", SP_REGNUM, SP_REGNUM);
	  else
	    asm_fprintf (f, "\tldd.w\t%r, [%r], #8\n", LR_REGNUM, SP_REGNUM);
	}
      else if (saved_regs_mask)
	{
	  /* gaoyi */
	  if ((saved_regs_mask & 0xFFFF) != 0)
	    {
	      print_multi_reg (f, "ldw.w", "]+", SP_REGNUM,
			       saved_regs_mask & 0xFFFF);
	      print_multi_reg (f, "ldw.w", "]+", SP_REGNUM,
			       saved_regs_mask & 0xFFFF0000);
	    }
	  else
	    print_multi_reg (f, "ldd.w", "]+", SP_REGNUM, saved_regs_mask);
	}
      if (crtl->args.pretend_args_size)
	{
	  /* Unwind the pre-pushed regs.  */
	  operands[0] = operands[1] = stack_pointer_rtx;
	  operands[2] = GEN_INT (crtl->args.pretend_args_size);
	  output_add_immediate (operands);
	}
    }

#if 0
  if (UNICORE64_FUNC_TYPE (func_type) == UNICORE64_FT_EXCEPTION_HANDLER)
    /* Adjust the stack to remove the exception handler stuff.  */
    asm_fprintf (f, "\tadd\t%r, %r, %r\n", SP_REGNUM, SP_REGNUM,
		 REGNO (eh_ofs));
#endif

  if (!really_return
      || (UNICORE64_FUNC_TYPE (func_type) == UNICORE64_FT_NORMAL
	  && crtl->args.pretend_args_size == 0
	  && saved_regs_mask & (1 << PC_REGNUM)))
    return "";

  /* Generate the return instruction.  */
  switch ((int) UNICORE64_FUNC_TYPE (func_type))
    {
    case UNICORE64_FT_EXCEPTION_HANDLER:
      /* Even in 26-bit mode we do a mov (rather than a movs)
         because we don't have the PSR bits set in the address.  */
      asm_fprintf (f, "\tdmov\t%r, %r\n", PC_REGNUM, EXCEPTION_LR_REGNUM);
      break;

    case UNICORE64_FT_ISR:
    case UNICORE64_FT_FIQ:
      asm_fprintf (f, "\tdsub.a\t%r, %r, #8\n", PC_REGNUM, LR_REGNUM);
      break;

    case UNICORE64_FT_EXCEPTION:
      asm_fprintf (f, "\tdcmpsub.a\t%r, #0\n", LR_REGNUM);//cmc 20110422
      asm_fprintf (f, "\tdmov\t%r, %r\n", PC_REGNUM, LR_REGNUM);
      break;

    case UNICORE64_FT_INTERWORKED:
      asm_fprintf (f, "\treturn\n");
      break;

    default:
      if (frame_pointer_needed)
	/* If we used the frame pointer then the return adddress
	   will have been loaded off the stack directly into the
	   PC, so there is no need to issue a MOV instruction
	   here.  */
	;
      else if (crtl->args.pretend_args_size == 0
	       && (saved_regs_mask & (1 << LR_REGNUM)))
	/* Similarly we may have been able to load LR into the PC
	   even if we did not create a stack frame.  */
	;
      else
	asm_fprintf (f, "\treturn\n");
      break;
    }

  return "";
}

static void
unicore64_output_function_epilogue (FILE * file ATTRIBUTE_UNUSED,
				    HOST_WIDE_INT frame_size)
{
  if (use_return_insn (FALSE)
      && return_used_this_function
      && (frame_size + crtl->outgoing_args_size) != 0
      && !frame_pointer_needed)
    abort ();

  /* Reset the UNICORE64-specific per-function variables.  */
  after_unicore64_reorg = 0;
}

/* Generate and emit an insn that we will recognize as a push_multi.
   Unfortunately, since this insn does not reflect very well the actual
   semantics of the operation, we need to annotate the insn for the benefit
   of DWARF2 frame unwind information.  */

static rtx
emit_multi_reg_push (int mask)
{
  int num_regs = 0;
  int num_dwarf_regs;
  int i, j;
  rtx par;
  rtx dwarf;
  int dwarf_par_index;
  rtx tmp, reg;

  for (i = 0; i <= LAST_UNICORE64_REGNUM; i++)
    if (mask & (1 << i))
      num_regs++;
  /* gaoyi */
  if (num_regs == 0 || num_regs > FIRST_UNICORE64_FP_REGNUM - 1)
    abort ();

  /* We don't record the PC in the dwarf frame information.  */
  num_dwarf_regs = num_regs;
  if (mask & (1 << PC_REGNUM))
    num_dwarf_regs--;

  /* For the body of the insn we are going to generate an UNSPEC in
     parallel with several USEs.  This allows the insn to be recognised
     by the push_multi pattern in the unicore64.md file.  The insn looks
     something like this:

     (parallel [ 
     (set (mem:BLK (pre_dec:BLK (reg:SI sp)))
     (unspec:BLK [(reg:SI r4)] UNSPEC_PUSH_MULT))
     (use (reg:SI 11 fp))
     (use (reg:SI 12 ip))
     (use (reg:SI 14 lr))
     (use (reg:SI 15 pc))
     ])

     For the frame note however, we try to be more explicit and actually
     show each register being stored into the stack frame, plus a (single)
     decrement of the stack pointer.  We do it this way in order to be
     friendly to the stack unwinding code, which only wants to see a single
     stack decrement per instruction.  The RTL we generate for the note looks
     something like this:

     (sequence [ 
     (set (reg:SI sp) (plus:SI (reg:SI sp) (const_int -20)))
     (set (mem:SI (reg:SI sp)) (reg:SI r4))
     (set (mem:SI (plus:SI (reg:SI sp) (const_int 4))) (reg:SI fp))
     (set (mem:SI (plus:SI (reg:SI sp) (const_int 8))) (reg:SI ip))
     (set (mem:SI (plus:SI (reg:SI sp) (const_int 12))) (reg:SI lr))
     ])

     This sequence is used both by the code to support stack unwinding for
     exceptions handlers and the code to generate dwarf2 frame debugging.  */

  par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (num_regs));
  dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (num_dwarf_regs + 1));
  RTX_FRAME_RELATED_P (dwarf) = 1;
  dwarf_par_index = 1;

  for (i = 0; i <= LAST_UNICORE64_REGNUM; i++)
    {
      if (mask & (1 << i))
	{
	  reg = gen_rtx_REG (DImode, i);

	  XVECEXP (par, 0, 0)
	    = gen_rtx_SET (VOIDmode,
			   gen_rtx_MEM (BLKmode,
					gen_rtx_PRE_DEC (BLKmode,
							 stack_pointer_rtx)),
			   gen_rtx_UNSPEC (BLKmode,
					   gen_rtvec (1, reg),
					   UNSPEC_PUSH_MULT));

	  if (i != PC_REGNUM)
	    {
	      tmp = gen_rtx_SET (VOIDmode,
				 gen_rtx_MEM (DImode, stack_pointer_rtx),
				 reg);
	      RTX_FRAME_RELATED_P (tmp) = 1;
	      XVECEXP (dwarf, 0, dwarf_par_index) = tmp;
	      dwarf_par_index++;
	    }

	  break;
	}
    }

  for (j = 1, i++; j < num_regs; i++)
    {
      if (mask & (1 << i))
	{
	  reg = gen_rtx_REG (DImode, i);

	  XVECEXP (par, 0, j) = gen_rtx_USE (VOIDmode, reg);

	  if (i != PC_REGNUM)
	    {
	      tmp = gen_rtx_SET (VOIDmode,
				 gen_rtx_MEM (DImode,
					      plus_constant
					      (stack_pointer_rtx, 8 * j)),
				 reg);
	      RTX_FRAME_RELATED_P (tmp) = 1;
	      XVECEXP (dwarf, 0, dwarf_par_index++) = tmp;
	    }

	  j++;
	}
    }

  par = emit_insn (par);

  tmp = gen_rtx_SET (DImode,
		     stack_pointer_rtx,
		     gen_rtx_PLUS (DImode,
				   stack_pointer_rtx,
				   GEN_INT (-8 * num_regs)));
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (dwarf, 0, 0) = tmp;

  REG_NOTES (par) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR, dwarf,
				       REG_NOTES (par));
  return par;
}

static rtx
emit_sfm (int base_reg, int count)
{
  rtx par;
  rtx dwarf;
  rtx tmp, reg;
  int i;

  par = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));
  dwarf = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));
  RTX_FRAME_RELATED_P (dwarf) = 1;

  reg = gen_rtx_REG (DFmode, base_reg++);

  XVECEXP (par, 0, 0)
    = gen_rtx_SET (VOIDmode,
		   gen_rtx_MEM (BLKmode,
				gen_rtx_PRE_DEC (BLKmode, stack_pointer_rtx)),
		   gen_rtx_UNSPEC (BLKmode,
				   gen_rtvec (1, reg), UNSPEC_PUSH_MULT));
  tmp
    = gen_rtx_SET (VOIDmode,
		   gen_rtx_MEM (DFmode,
				gen_rtx_PRE_DEC (BLKmode, stack_pointer_rtx)),
		   reg);
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (dwarf, 0, count - 1) = tmp;

  for (i = 1; i < count; i++)
    {
      reg = gen_rtx_REG (DFmode, base_reg++);
      XVECEXP (par, 0, i) = gen_rtx_USE (VOIDmode, reg);

      tmp = gen_rtx_SET (VOIDmode,
			 gen_rtx_MEM (DFmode,
				      gen_rtx_PRE_DEC (BLKmode,
						       stack_pointer_rtx)),
			 reg);
      RTX_FRAME_RELATED_P (tmp) = 1;
      XVECEXP (dwarf, 0, count - i - 1) = tmp;
    }

  par = emit_insn (par);
  REG_NOTES (par) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR, dwarf,
				       REG_NOTES (par));
  return par;
}

/* Compute the distance from register FROM to register TO.
   These can be the arg pointer (50), the soft frame pointer (49),
   the stack pointer (29) or the hard frame pointer (27).
   Typical stack layout looks like this:

       old stack pointer -> |    |
                             ----
                            |    | \
                            |    |   saved arguments for
                            |    |   vararg functions
			    |    | /
                              --
   hard FP & arg pointer -> |    | \
                            |    |   stack
                            |    |   frame
                            |    | /
                              --
                            |    | \
                            |    |   call saved
                            |    |   registers
      soft frame pointer -> |    | /
                              --
                            |    | \
                            |    |   local
                            |    |   variables
                            |    | /
                              --
                            |    | \
                            |    |   outgoing
                            |    |   arguments
   current stack pointer -> |    | /
                              --

  For a given funciton some or all of these stack compomnents
  may not be needed, giving rise to the possibility of
  eliminating some of the registers.

  The values returned by this function must reflect the behaviour
  of unicore64_expand_prologue() and unicore64_compute_save_reg_mask().

  The sign of the number returned reflects the direction of stack
  growth, so the values are positive for all eliminations except
  from the soft frame pointer to the hard frame pointer.  */

// huangping: FIXME 2012-02-09 unsigned int ==> HOST_WIDE_INT
HOST_WIDE_INT
unicore64_compute_initial_elimination_offset (unsigned int from,
					      unsigned int to)
{
  unsigned HOST_WIDE_INT local_vars = (get_frame_size () + 7) & ~7;
  unsigned HOST_WIDE_INT outgoing_args = crtl->outgoing_args_size;
  unsigned HOST_WIDE_INT stack_frame;
  unsigned HOST_WIDE_INT call_saved_registers;
  unsigned HOST_WIDE_INT func_type;

  func_type = unicore64_current_func_type ();

  /* Volatile functions never return, so there is
     no need to save call saved registers.  */
  call_saved_registers = 0;
  if (!IS_VOLATILE (func_type))
    {
      unsigned HOST_WIDE_INT reg_mask;
      unsigned HOST_WIDE_INT reg;

      /* Make sure that we compute which registers will be saved
         on the stack using the same algorithm that is used by
         unicore64_compute_save_reg_mask().  */
      reg_mask = unicore64_compute_save_reg0_reg28_mask ();

      /* Now count the number of bits set in save_reg_mask.
         For each set bit we need 8 bytes of stack space.  */
      while (reg_mask)
	{
	  call_saved_registers += 8;
	  reg_mask = reg_mask & ~(reg_mask & -reg_mask);
	}
      if (df_regs_ever_live_p (LR_REGNUM)
	  /* If a stack frame is going to be created, the LR will
	     be saved as part of that, so we do not need to allow
	     for it here.  */
	  && !frame_pointer_needed)
	call_saved_registers += 8;
      /* If the hard floating point registers are going to be
         used then they must be saved on the stack as well.
         Each register occupies 8 bytes of stack space.  */
      for (reg = FIRST_UNICORE64_FP_REGNUM; reg <= LAST_UNICORE64_FP_REGNUM;
	   reg++)
	if (df_regs_ever_live_p (reg) && !call_used_regs[reg])
	  call_saved_registers += 8;
    }

  /* The stack frame contains 4 registers - the old frame pointer,
     the old stack pointer, the return address and PC of the start
     of the function.  */
  stack_frame = frame_pointer_needed ? 32 : 0;

  /* OK, now we have enough information to compute the distances.
     There must be an entry in these switch tables for each pair
     of registers in ELIMINABLE_REGS, even if some of the entries
     seem to be redundant or useless.  */
  switch (from)
    {
    case ARG_POINTER_REGNUM:
      switch (to)
	{
//	case UNICORE16_HARD_FRAME_POINTER_REGNUM:
//	  return 0;

	case FRAME_POINTER_REGNUM:
	  /* This is the reverse of the soft frame pointer
	     to hard frame pointer elimination below.  */
	  if (call_saved_registers == 0 && stack_frame == 0)
	    return 0;
	  return (call_saved_registers + stack_frame - 8);

	case UNICORE64_HARD_FRAME_POINTER_REGNUM:
	  /* If there is no stack frame then the hard
	     frame pointer and the arg pointer coincide.  */
	  if (stack_frame == 0 && call_saved_registers != 0)
	    return 0;
	  /* FIXME:  Not sure about this.  Maybe we should always return 0 ?  */
	  return (frame_pointer_needed
		  && cfun->static_chain_decl
		  && !cfun->machine->uses_anonymous_args) ? 8 : 0;

	case STACK_POINTER_REGNUM:
	  /* If nothing has been pushed on the stack at all
	     then this will return -8.  This *is* correct!  */
	  return call_saved_registers + stack_frame + local_vars +
	    outgoing_args - 8;

	default:
	  abort ();
	}
      break;

    case FRAME_POINTER_REGNUM:
      switch (to)
	{
//	case UNICORE16_HARD_FRAME_POINTER_REGNUM:
//	  return 0;

	case UNICORE64_HARD_FRAME_POINTER_REGNUM:
	  /* The hard frame pointer points to the top entry in the
	     stack frame.  The soft frame pointer to the bottom entry
	     in the stack frame.  If there is no stack frame at all,
	     then they are identical.  */
	  if (call_saved_registers == 0 && stack_frame == 0)
	    return 0;
	  return -(call_saved_registers + stack_frame - 8ULL);

	case STACK_POINTER_REGNUM:
	  return local_vars + outgoing_args;

	default:
	  abort ();
	}
      break;

    default:
      /* You cannot eliminate from the stack pointer.
         In theory you could eliminate from the hard frame
         pointer to the stack pointer, but this will never
         happen, since if a stack frame is not needed the
         hard frame pointer will never be used.  */
      abort ();
    }
}

/* Generate the prologue instructions for entry into an UNICORE64 function.  */
/* star modify for unicore64 */

void
unicore64_expand_prologue (void)
{
  HOST_WIDE_INT reg;
  rtx amount;
  rtx insn;
  rtx ip_rtx;
  unsigned long live_regs_mask;
  unsigned long func_type;
  HOST_WIDE_INT fp_offset = 0;
  HOST_WIDE_INT saved_pretend_args = 0;
  unsigned HOST_WIDE_INT args_to_push;

  func_type = unicore64_current_func_type ();

  /* Naked functions don't have prologues.  */
  if (IS_NAKED (func_type))
    return;

  /* Make a copy of c_f_p_a_s as we may need to modify it locally.  */
  args_to_push = crtl->args.pretend_args_size;

  /* Compute which register we will have to save onto the stack.  */
  live_regs_mask = unicore64_compute_save_reg_mask ();

  ip_rtx = gen_rtx_REG (DImode, IP_REGNUM);

  if (frame_pointer_needed)
    {
      if (IS_INTERRUPT (func_type))
	{
	  /* Interrupt functions must not corrupt any registers.
	     Creating a frame pointer however, corrupts the IP
	     register, so we must push it first.  */
	  insn = emit_multi_reg_push (1 << IP_REGNUM);

	  /* Do not set RTX_FRAME_RELATED_P on this insn.
	     The dwarf stack unwinding code only wants to see one
	     stack decrement per function, and this is not it.  If
	     this instruction is labeled as being part of the frame
	     creation sequence then dwarf2out_frame_debug_expr will
	     abort when it encounters the assignment of IP to FP
	     later on, since the use of SP here establishes SP as
	     the CFA register and not IP.

	     Anyway this instruction is not really part of the stack
	     frame creation although it is part of the prologue.  */
	}
      //star : Modify this for UniCore-3.
      else if (IS_NESTED (func_type))
	{
	  /* The Static chain register is the same as the IP register
	     used as a scratch register during stack frame creation.
	     To get around this need to find somewhere to store IP
	     whilst the frame is being created.  We try the following
	     places in order:

	     1. The last argument register.
	     2. A slot on the stack above the frame.  (This only
	     works if the function is not a varargs function).
	     3. Register r3, after pushing the argument registers
	     onto the stack.

	     Note - we only need to tell the dwarf2 backend about the SP
	     adjustment in the second variant; the static chain register
	     doesn't need to be unwound, as it doesn't contain a value
	     inherited from the caller.  */

	  if (df_regs_ever_live_p (TRAMPOLINE_REG) == 0)
	    {
	      insn = gen_rtx_REG (DImode, TRAMPOLINE_REG);
	      insn = gen_rtx_SET (DImode, insn, ip_rtx);
	      insn = emit_insn (insn);
	    }
	  else if (args_to_push == 0)
	    {
	      rtx dwarf;
	      insn = gen_rtx_PRE_DEC (DImode, stack_pointer_rtx);
	      insn = gen_rtx_MEM (DImode, insn);
	      insn = gen_rtx_SET (VOIDmode, insn, ip_rtx);
	      insn = emit_insn (insn);

	      fp_offset = 8;

	      /* Just tell the dwarf backend that we adjusted SP.  */
	      dwarf = gen_rtx_SET (VOIDmode, stack_pointer_rtx,
				   gen_rtx_PLUS (DImode, stack_pointer_rtx,
						 GEN_INT (-fp_offset)));
	      RTX_FRAME_RELATED_P (insn) = 1;
	      REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
						    dwarf, REG_NOTES (insn));
	    }
	  else
	    {
	      /* Store the args on the stack.  */
	      if (cfun->machine->uses_anonymous_args)
		insn = emit_multi_reg_push
		  ((0xf0 >> (args_to_push / 8)) & 0xf); // huangping: FIXME args_to_push is offset speculated below
	      else
		insn = emit_insn
		  (gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx,
			       GEN_INT (-args_to_push)));

	      RTX_FRAME_RELATED_P (insn) = 1;

	      saved_pretend_args = 1;
	      fp_offset = args_to_push;
	      args_to_push = 0;

	      /* Now reuse r3 to preserve IP.  */
	      insn = gen_rtx_REG (DImode, TRAMPOLINE_REG);
	      insn = gen_rtx_SET (DImode, insn, ip_rtx);
	      (void) emit_insn (insn);
	    }
	}

      if (fp_offset)
	{
	  insn =
	    gen_rtx_PLUS (DImode, stack_pointer_rtx, GEN_INT (fp_offset));
	  insn = gen_rtx_SET (DImode, ip_rtx, insn);
	}
      else
	insn = gen_movdi (ip_rtx, stack_pointer_rtx);

      insn = emit_insn (insn);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (args_to_push)
    {
      /* Push the argument registers, or reserve space for them.  */
      if (cfun->machine->uses_anonymous_args)
	insn = emit_multi_reg_push ((0xf0 >> (args_to_push / 8)) & 0xf);
      else
	insn = emit_insn
	  (gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx,
		       GEN_INT (-args_to_push)));
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  /* If this is an interrupt service routine, and the link register is
     going to be pushed, subtracting four now will mean that the
     function return can be done with a single instruction.  */
  if ((func_type == UNICORE64_FT_ISR || func_type == UNICORE64_FT_FIQ)
      && (live_regs_mask & (1 << LR_REGNUM)) != 0)
    {
      emit_insn (gen_rtx_SET (DImode,
			      gen_rtx_REG (DImode, LR_REGNUM),
			      gen_rtx_PLUS (DImode,
					    gen_rtx_REG (DImode, LR_REGNUM),
					    GEN_INT (-8))));
    }

  if (live_regs_mask)
    {
      insn = emit_multi_reg_push (live_regs_mask);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (!IS_VOLATILE (func_type))
    {
      /* Save any floating point call-saved registers used by this function.  */
      if (unicore64_fpu_arch == FP_SOFT2)
	{
	  for (reg = LAST_UNICORE64_FP_REGNUM;
	       reg >= FIRST_UNICORE64_FP_REGNUM; reg--)
	    if (df_regs_ever_live_p (reg) && !call_used_regs[reg])
	      {
		insn = gen_rtx_PRE_DEC (XFmode, stack_pointer_rtx);
		insn = gen_rtx_MEM (XFmode, insn);
		insn = emit_insn (gen_rtx_SET (VOIDmode, insn,
					       gen_rtx_REG (XFmode, reg)));
		RTX_FRAME_RELATED_P (insn) = 1;
	      }
	}
      else
	{
	  int start_reg = LAST_UNICORE64_FP_REGNUM;

	  for (reg = LAST_UNICORE64_FP_REGNUM;
	       reg >= FIRST_UNICORE64_FP_REGNUM; reg--)
	    {
	      if (df_regs_ever_live_p (reg) && !call_used_regs[reg])
		{
		  if (start_reg - reg == 3 || reg % 4 == 0)
		    {
		      insn = emit_sfm (reg, start_reg - reg + 1);
		      RTX_FRAME_RELATED_P (insn) = 1;
		      start_reg = reg - 1;
		    }
		}
	      else
		{
		  if (start_reg != reg)
		    {
		      insn = emit_sfm (reg + 1, start_reg - reg);
		      RTX_FRAME_RELATED_P (insn) = 1;
		    }
		  start_reg = reg - 1;
		}
	    }

	  if (start_reg != reg)
	    {
	      insn = emit_sfm (reg + 1, start_reg - reg);
	      RTX_FRAME_RELATED_P (insn) = 1;
	    }
	}
    }

  if (frame_pointer_needed)
    {
      /* Create the new frame pointer.  */
      insn = GEN_INT (-(8 + args_to_push + fp_offset));
      insn = emit_insn (gen_adddi3 (hard_frame_pointer_rtx, ip_rtx, insn));
      RTX_FRAME_RELATED_P (insn) = 1;

      if (IS_NESTED (func_type))
	{
	  /* Recover the static chain register.  */
	  if (df_regs_ever_live_p (TRAMPOLINE_REG) == 0 || saved_pretend_args)
	    insn = gen_rtx_REG (DImode, TRAMPOLINE_REG);
	  else			/* if (crtl->args.pretend_args_size == 0) */
	    {
	      insn =
		gen_rtx_PLUS (DImode, hard_frame_pointer_rtx, GEN_INT (8));
	      insn = gen_rtx_MEM (DImode, insn);
	    }

	  emit_insn (gen_rtx_SET (DImode, ip_rtx, insn));
	  /* Add a USE to stop propagate_one_insn() from barfing.  */
	  emit_insn (gen_prologue_use (ip_rtx));
	}
    }

  amount = GEN_INT (-(get_frame_size ()
		      + crtl->outgoing_args_size));

  if (amount != const0_rtx)
    {
      /* This add can produce multiple insns for a large constant, so we
         need to get tricky.  */
      rtx last = get_last_insn ();

      /* star : modify for UniCore-3. FIXME-UniCore3 */
      HOST_WIDE_INT con = get_frame_size () + crtl->outgoing_args_size;
      if (const_ok_for_unicore64 (con) || const_ok_for_unicore64 (-con))
	insn = emit_insn (gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx,
				      amount));
      else
	{
	  emit_insn (gen_rtx_SET (VOIDmode, ip_rtx, amount));
	  insn = emit_insn (gen_adddi3 (stack_pointer_rtx, stack_pointer_rtx,
					ip_rtx));
	}

      do
	{
	  last = last ? NEXT_INSN (last) : get_insns ();
//	  RTX_FRAME_RELATED_P (last) = 1;
	}
      while (last != insn);

      /* If the frame pointer is needed, emit a special barrier that
         will prevent the scheduler from moving stores to the frame
         before the stack adjustment.  */
      if (frame_pointer_needed)
	{
	  rtx unspec = gen_rtx_UNSPEC (DImode,
				       gen_rtvec (2, stack_pointer_rtx,
						  hard_frame_pointer_rtx),
				       UNSPEC_PRLG_STK);

	  insn = emit_insn (gen_rtx_CLOBBER (VOIDmode,
					     gen_rtx_MEM (BLKmode, unspec)));
	}
    }

/* star */
  if (flag_pic && unicore64_pic_register != INVALID_REGNUM)
    unicore64_finalize_pic (0);

  /* If we are profiling, make sure no instructions are scheduled before
     the call to mcount.  Similarly if the user has requested no
     scheduling in the prolog.  */
  if (crtl->profile || !TARGET_SCHED_PROLOG
      || flag_non_call_exceptions)
    emit_insn (gen_blockage ());

  /* If the link register is being kept alive, with the return address in it,
     then make sure that it does not get reused by the ce2 pass.  */
  if ((live_regs_mask & (1 << LR_REGNUM)) == 0)
    {
      emit_insn (gen_prologue_use (gen_rtx_REG (DImode, LR_REGNUM)));
      cfun->machine->lr_save_eliminated = 1;
    }
}


/* If CODE is 'd', then the X is a condition operand and the instruction
   should only be executed if the condition is true.
   if CODE is 'D', then the X is a condition operand and the instruction
   should only be executed if the condition is false: however, if the mode
   of the comparison is CCFPEmode, then always execute the instruction -- we
   do this because in these circumstances !GE does not necessarily imply LT;
   in these cases the instruction pattern will take care to make sure that
   an instruction containing %d will follow, thereby undoing the effects of
   doing this instruction unconditionally.
   If CODE is 'N' then X is a floating point operand that must be negated
   before output.
   If CODE is 'B' then output a bitwise inverted value of X (a const int).
   If X is a REG and CODE is `M', output a ldm/stm style multi-reg.  */

void
unicore64_print_operand (FILE * stream, rtx x, int code)
{
  char fgname[4];
  switch (code)
    {
    case '@':
      fputs (ASM_COMMENT_START, stream);
      return;

    case '_':
      fputs (user_label_prefix, stream);
      return;

    case '|':
      fputs (REGISTER_PREFIX, stream);
      return;

    case '?':
      return;

    case 'N':
      {
	REAL_VALUE_TYPE r;
	REAL_VALUE_FROM_CONST_DOUBLE (r, x);
	r = REAL_VALUE_NEGATE (r);
	fprintf (stream, "%s", fp_const_from_val (&r));
      }
      return;

    case 'B':
      if (GET_CODE (x) == CONST_INT)
	{
	  HOST_WIDE_INT val;
	  val = UNICORE64_SIGN_EXTEND (~INTVAL (x));
	  fprintf (stream, HOST_WIDE_INT_PRINT_DEC, val);
	}
      else
	{
	  putc ('~', stream);
	  output_addr_const (stream, x);
	}
      return;

    case 'i':
      fprintf (stream, "%s", arithmetic_instr (x, 1));
      return;

    case 'I':
      fprintf (stream, "%s", arithmetic_instr (x, 0));
      return;

      /* An explanation of the 'Q', 'R' and 'H' register operands:

         In a pair of registers containing a DI or DF value the 'Q'
         operand returns the register number of the register containing
         the least signficant part of the value.  The 'R' operand returns
         the register number of the register containing the most
         significant part of the value.

         The 'H' operand returns the higher of the two register numbers.
         On a run where WORDS_BIG_ENDIAN is true the 'H' operand is the
         same as the 'Q' operand, since the most signficant part of the
         value is held in the lower number register.  The reverse is true
         on systems where WORDS_BIG_ENDIAN is false.

         The purpose of these operands is to distinguish between cases
         where the endian-ness of the values is important (for example
         when they are added together), and cases where the endian-ness
         is irrelevant, but the order of register operations is important.
         For example when loading a value from memory into a register
         pair, the endian-ness does not matter.  Provided that the value
         from the lower memory address is put into the lower numbered
         register, and the value from the higher address is put into the
         higher numbered register, the load will work regardless of whether
         the value being loaded is big-wordian or little-wordian.  The
         order of the two register loads can matter however, if the address
         of the memory location is actually held in one of the registers
         being overwritten by the load.  */
    case 'Q':
      if (REGNO (x) > LAST_UNICORE64_REGNUM)
	abort ();
      asm_fprintf (stream, "%r", REGNO (x) + (WORDS_BIG_ENDIAN ? 1 : 0));
      return;

    case 'R':
      if (REGNO (x) > LAST_UNICORE64_REGNUM)
	abort ();
      asm_fprintf (stream, "%r", REGNO (x) + (WORDS_BIG_ENDIAN ? 0 : 1));
      return;

    case 'H':
      if (REGNO (x) > LAST_UNICORE64_REGNUM)
	abort ();
      asm_fprintf (stream, "%r", REGNO (x) + 1);
      return;

    case 'm':
      asm_fprintf (stream, "%r",
		   GET_CODE (XEXP (x, 0)) == REG
		   ? REGNO (XEXP (x, 0)) : REGNO (XEXP (XEXP (x, 0), 0)));
      return;

    case 'M':
      asm_fprintf (stream, "(%r-%r)",
		   REGNO (x), REGNO (x) + NUM_REGS (GET_MODE (x)) - 1);
      return;

    case 'd':
      if (!x)
	return;

      if (TARGET_UNICORE64)
	fputs (unicore64_condition_codes[get_unicore64_condition_code (x)],
	       stream);
      else
	;
      return;

    case 'D':
      if (!x)
	return;

      if (TARGET_UNICORE64)
	fputs (unicore64_condition_codes[UNICORE64_INVERSE_CONDITION_CODE
					 (get_unicore64_condition_code (x))],
	       stream);
      else
	;
      return;

// star, 20090803, add for write back.
    case 'T':
      if (!x) return;
      
      if (unicore64_opd_need_write (x))
	fputs (".w",stream);
      return ;

    case 'F':
      get_next_reg_name (fgname, REGNO (x));
      fprintf (stream, "(%s%s-%s%s)", REGISTER_PREFIX, reg_names[REGNO (x)],
	       REGISTER_PREFIX, fgname);
      return;

    case 'O':
      get_next_reg_name (fgname, REGNO (x));
      fprintf (stream, "%s%s", REGISTER_PREFIX, fgname);
      return;

    case 'W':
      if (GET_CODE (x) == MEM)
	{
	  output_memory_reference_mode = GET_MODE (x);
	  is_lwf_swf = 1;
	  output_address (XEXP (x, 0));
	  is_lwf_swf = 0;
	}
      else
	abort ();
      return;
    default:
      if (x == 0)
	abort ();

      if (GET_CODE (x) == REG)
	asm_fprintf (stream, "%r", REGNO (x));
      else if (GET_CODE (x) == MEM)
	{
	  output_memory_reference_mode = GET_MODE (x);
	  output_address (XEXP (x, 0));
	}
      else if (GET_CODE (x) == CONST_DOUBLE)
	fprintf (stream, "#%s", fp_immediate_constant (x));
      else if (GET_CODE (x) == NEG)
	abort ();		/* This should never happen now.  */
      else
	{
	  fputc ('#', stream);
	  output_addr_const (stream, x);
	}
    }
}

#ifndef AOF_ASSEMBLER
/* Target hook for assembling integer objects.  The UNICORE64 version needs to
   handle word-sized values specially.  */

static bool
unicore64_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
  if (size == UNITS_PER_WORD && aligned_p)
    {
// huangping FIXME 2011-08-04
//      fputs ("\t.dword\t", asm_out_file);
//      output_addr_const (asm_out_file, x);

      // huangping: FIXME 2012-01-06
//      if (GET_CODE (x) == CONST_DOUBLE && GET_MODE (x) == VOIDmode && CONST_DOUBLE_LOW (x) < 0 && CONST_DOUBLE_HIGH (x) === 0)
//	{
//	  CONST_DOUBLE_HIGH (x) == 0xffffffff;
//	}
 
      /* Mark symbols as position independent.  We only do this in the
         .text segment, not in the .data segment. */
      if (NEED_GOT_RELOC && flag_pic && making_const_table &&
	  (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF))
	{
	  fputs ("\t.dword\t", asm_out_file);
	  output_addr_const (asm_out_file, x);

	  if (GET_CODE (x) == SYMBOL_REF
	      && (CONSTANT_POOL_ADDRESS_P (x)
		  || ENCODED_SHORT_CALL_ATTR_P (XSTR (x, 0))))
	    fputs ("(GOTOFF)", asm_out_file);
	  else if (GET_CODE (x) == LABEL_REF)
	    fputs ("(GOTOFF)", asm_out_file);
	  /* hidden symbol: GOTOFF, Paul, 20090615 */
	  else if ((GET_CODE (x) == SYMBOL_REF)
		   && SYMBOL_REF_DECL (x) && DECL_P (SYMBOL_REF_DECL (x))
		   && (DECL_VISIBILITY (SYMBOL_REF_DECL (x)) == VISIBILITY_HIDDEN)
		   && ! DECL_EXTERNAL (SYMBOL_REF_DECL (x)))
	    fputs ("(GOTOFF)", asm_out_file);
	  else
	    fputs ("(GOT)", asm_out_file);
	  fputc ('\n', asm_out_file);
	  return true;
	}

      fputs ("\t.dword\t", asm_out_file);
      output_addr_const (asm_out_file, x);
      fputc ('\n', asm_out_file);
      return true;
    }

  if (size == UNITS_PER_WORD/2 && aligned_p)
    {
      fputs ("\t.word\t", asm_out_file);
      output_addr_const (asm_out_file, x);

      /* Mark symbols as position independent.  We only do this in the
         .text segment, not in the .data segment. */
      if (NEED_GOT_RELOC && flag_pic && making_const_table &&
	  (GET_CODE (x) == SYMBOL_REF || GET_CODE (x) == LABEL_REF))
	{
	  if (GET_CODE (x) == SYMBOL_REF
	      && (CONSTANT_POOL_ADDRESS_P (x)
		  || ENCODED_SHORT_CALL_ATTR_P (XSTR (x, 0))))
	    fputs ("(GOTOFF)", asm_out_file);
	  else if (GET_CODE (x) == LABEL_REF)
	    fputs ("(GOTOFF)", asm_out_file);
	  /* hidden symbol: GOTOFF, Paul, 20090615 */
	  else if ((GET_CODE (x) == SYMBOL_REF)
		   && SYMBOL_REF_DECL (x) && DECL_P (SYMBOL_REF_DECL (x))
		   && (DECL_VISIBILITY (SYMBOL_REF_DECL (x)) == VISIBILITY_HIDDEN)
		   && ! DECL_EXTERNAL (SYMBOL_REF_DECL (x)))
	    fputs ("(GOTOFF)", asm_out_file);
	  else
	    fputs ("(GOT)", asm_out_file);
	}
      fputc ('\n', asm_out_file);
      return true;
    }

  return default_assemble_integer (x, size, aligned_p);
}
#endif

/* A finite state machine takes care of noticing whether or not instructions
   can be conditionally executed, and thus decrease execution time and code
   size by deleting branch instructions.  The fsm is controlled by
   final_prescan_insn, and controls the actions of ASM_OUTPUT_OPCODE.  */

/* The state of the fsm controlling condition codes are:
   0: normal, do nothing special
   1: make ASM_OUTPUT_OPCODE not output this instruction
   2: make ASM_OUTPUT_OPCODE not output this instruction
   3: make instructions conditional
   4: make instructions conditional

   State transitions (state->state by whom under condition):
   0 -> 1 final_prescan_insn if the `target' is a label
   0 -> 2 final_prescan_insn if the `target' is an unconditional branch
   1 -> 3 ASM_OUTPUT_OPCODE after not having output the conditional branch
   2 -> 4 ASM_OUTPUT_OPCODE after not having output the conditional branch
   3 -> 0 ASM_OUTPUT_INTERNAL_LABEL if the `target' label is reached
          (the target label has CODE_LABEL_NUMBER equal to unicore64_target_label).
   4 -> 0 final_prescan_insn if the `target' unconditional branch is reached
          (the target insn is unicore64_target_insn).

   If the jump clobbers the conditions then we use states 2 and 4.

   A similar thing can be done with conditional return insns.

   XXX In case the `target' is an unconditional branch, this conditionalising
   of the instructions always reduces code size, but not always execution
   time.  But then, I want to reduce the code size to somewhere near what
   /bin/cc produces.  */

/* Returns the index of the UNICORE64 condition code string in
   `unicore64_condition_codes'.  COMPARISON should be an rtx like
   `(eq (...) (...))'.  */

static enum unicore64_cond_code
get_unicore64_condition_code (rtx comparison)
{
  enum machine_mode mode = GET_MODE (XEXP (comparison, 0));
  int code;
  enum rtx_code comp_code = GET_CODE (comparison);

  if (GET_MODE_CLASS (mode) != MODE_CC)
    mode = SELECT_CC_MODE (comp_code, XEXP (comparison, 0),
			   XEXP (comparison, 1));

  switch (mode)
    {
    case CC_DNEmode:
      code = UNICORE64_NE;
      goto dominance;
    case CC_DEQmode:
      code = UNICORE64_EQ;
      goto dominance;
    case CC_DGEmode:
      code = UNICORE64_GE;
      goto dominance;
    case CC_DGTmode:
      code = UNICORE64_GT;
      goto dominance;
    case CC_DLEmode:
      code = UNICORE64_LE;
      goto dominance;
    case CC_DLTmode:
      code = UNICORE64_LT;
      goto dominance;
    case CC_DGEUmode:
      code = UNICORE64_CS;
      goto dominance;
    case CC_DGTUmode:
      code = UNICORE64_HI;
      goto dominance;
    case CC_DLEUmode:
      code = UNICORE64_LS;
      goto dominance;
    case CC_DLTUmode:
      code = UNICORE64_CC;

    dominance:
      if (comp_code != EQ && comp_code != NE)
	abort ();

      if (comp_code == EQ)
	return UNICORE64_INVERSE_CONDITION_CODE (code);
      return code;

    case CC_NOOVmode:
      switch (comp_code)
	{
	case NE:
	  return UNICORE64_NE;
	case EQ:
	  return UNICORE64_EQ;
	case GE:
	  return UNICORE64_PL;
	case LT:
	  return UNICORE64_MI;
	default:
	  abort ();
	}

    case CC_Zmode:
      switch (comp_code)
	{
	case NE:
	  return UNICORE64_NE;
	case EQ:
	  return UNICORE64_EQ;
	default:
	  abort ();
	}

    case CCFPEmode:
    case CCFPmode:
      /* These encodings assume that AC=1 in the FPA system control
         byte.  This allows us to handle all cases except UNEQ and
         LTGT.  */
      switch (comp_code)
	{
	case GE:
	  return UNICORE64_GE;
	case GT:
	  return UNICORE64_GT;
	case LE:
	  return UNICORE64_LS;
	case LT:
	  return UNICORE64_MI;
	case NE:
	  return UNICORE64_NE;
	case EQ:
	  return UNICORE64_EQ;
	case ORDERED:
	  return UNICORE64_VC;
	case UNORDERED:
	  return UNICORE64_VS;
	case UNLT:
	  return UNICORE64_LT;
	case UNLE:
	  return UNICORE64_LE;
	case UNGT:
	  return UNICORE64_HI;
	case UNGE:
	  return UNICORE64_PL;
	  /* UNEQ and LTGT do not have a representation.  */
	case UNEQ:		/* Fall through.  */
	case LTGT:		/* Fall through.  */
	default:
	  abort ();
	}

    case CC_SWPmode:
      switch (comp_code)
	{
	case NE:
	  return UNICORE64_NE;
	case EQ:
	  return UNICORE64_EQ;
	case GE:
	  return UNICORE64_LE;
	case GT:
	  return UNICORE64_LT;
	case LE:
	  return UNICORE64_GE;
	case LT:
	  return UNICORE64_GT;
	case GEU:
	  return UNICORE64_LS;
	case GTU:
	  return UNICORE64_CC;
	case LEU:
	  return UNICORE64_CS;
	case LTU:
	  return UNICORE64_HI;
	default:
	  abort ();
	}

    case CC_Cmode:
      switch (comp_code)
	{
	case LTU:
	  return UNICORE64_CS;
	case GEU:
	  return UNICORE64_CC;
	default:
	  abort ();
	}

    case CCmode:
      switch (comp_code)
	{
	case NE:
	  return UNICORE64_NE;
	case EQ:
	  return UNICORE64_EQ;
	case GE:
	  return UNICORE64_GE;
	case GT:
	  return UNICORE64_GT;
	case LE:
	  return UNICORE64_LE;
	case LT:
	  return UNICORE64_LT;
	case GEU:
	  return UNICORE64_CS;
	case GTU:
	  return UNICORE64_HI;
	case LEU:
	  return UNICORE64_LS;
	case LTU:
	  return UNICORE64_CC;
	default:
	  abort ();
	}

    default:
      abort ();
    }

  abort ();
}


void
unicore64_final_prescan_insn (rtx insn)
{
  /* BODY will hold the body of INSN.  */
  rtx body = PATTERN (insn);

  /* This will be 1 if trying to repeat the trick, and things need to be
     reversed if it appears to fail.  */
  int reverse = 0;

  /* JUMP_CLOBBERS will be one implies that the conditions if a branch is
     taken are clobbered, even if the rtl suggests otherwise.  It also
     means that we have to grub around within the jump expression to find
     out what the conditions are when the jump isn't taken.  */
  int jump_clobbers = 0;

  /* If we start with a return insn, we only succeed if we find another one.  */
  int seeking_return = 0;

  /* START_INSN will hold the insn from where we start looking.  This is the
     first insn after the following code_label if REVERSE is true.  */
  rtx start_insn = insn;

  /* If in state 4, check if the target branch is reached, in order to
     change back to state 0.  */
  if (unicore64_ccfsm_state == 4)
    {
      if (insn == unicore64_target_insn)
	{
	  unicore64_target_insn = NULL;
	  unicore64_ccfsm_state = 0;
	}
      return;
    }

  /* If in state 3, it is possible to repeat the trick, if this insn is an
     unconditional branch to a label, and immediately following this branch
     is the previous target label which is only used once, and the label this
     branch jumps to is not too far off.  */
  if (unicore64_ccfsm_state == 3)
    {
      if (simplejump_p (insn))
	{
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    {
	      /* XXX Isn't this always a barrier?  */
	      start_insn = next_nonnote_insn (start_insn);
	    }
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == unicore64_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    reverse = TRUE;
	  else
	    return;
	}
      else if (GET_CODE (body) == RETURN)
	{
	  start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == BARRIER)
	    start_insn = next_nonnote_insn (start_insn);
	  if (GET_CODE (start_insn) == CODE_LABEL
	      && CODE_LABEL_NUMBER (start_insn) == unicore64_target_label
	      && LABEL_NUSES (start_insn) == 1)
	    {
	      reverse = TRUE;
	      seeking_return = 1;
	    }
	  else
	    return;
	}
      else
	return;
    }

  if (unicore64_ccfsm_state != 0 && !reverse)
    abort ();
  if (GET_CODE (insn) != JUMP_INSN)
    return;

  /* This jump might be paralleled with a clobber of the condition codes 
     the jump should always come first */
  if (GET_CODE (body) == PARALLEL && XVECLEN (body, 0) > 0)
    body = XVECEXP (body, 0, 0);


  if (reverse
      || (GET_CODE (body) == SET && GET_CODE (SET_DEST (body)) == PC
	  && GET_CODE (SET_SRC (body)) == IF_THEN_ELSE))
    {
      int insns_skipped;
      int fail = FALSE, succeed = FALSE;
      /* Flag which part of the IF_THEN_ELSE is the LABEL_REF.  */
      int then_not_else = TRUE;
      rtx this_insn = start_insn, label = 0;

      /* If the jump cannot be done with one instruction, we cannot 
         conditionally execute the instruction in the inverse case.  */
      if (get_attr_conds (insn) == CONDS_JUMP_CLOB)
	{
	  jump_clobbers = 1;
	  return;
	}

      /* Register the insn jumped to.  */
      if (reverse)
	{
	  if (!seeking_return)
	    label = XEXP (SET_SRC (body), 0);
	}
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == LABEL_REF)
	label = XEXP (XEXP (SET_SRC (body), 1), 0);
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == LABEL_REF)
	{
	  label = XEXP (XEXP (SET_SRC (body), 2), 0);
	  then_not_else = FALSE;
	}
      else if (GET_CODE (XEXP (SET_SRC (body), 1)) == RETURN)
	seeking_return = 1;
      else if (GET_CODE (XEXP (SET_SRC (body), 2)) == RETURN)
	{
	  seeking_return = 1;
	  then_not_else = FALSE;
	}
      else
	abort ();

      /* See how many insns this branch skips, and what kind of insns.  If all
         insns are okay, and the label or unconditional branch to the same
         label is not too far away, succeed.  */
      for (insns_skipped = 0;
	   !fail && !succeed && insns_skipped++ < max_insns_skipped;)
	{
	  rtx scanbody;

	  this_insn = next_nonnote_insn (this_insn);
	  if (!this_insn)
	    break;

	  switch (GET_CODE (this_insn))
	    {
	    case CODE_LABEL:
	      /* Succeed if it is the target label, otherwise fail since
	         control falls in from somewhere else.  */
	      if (this_insn == label)
		{
		  if (jump_clobbers)
		    {
		      unicore64_ccfsm_state = 2;
		      this_insn = next_nonnote_insn (this_insn);
		    }
		  else
		    unicore64_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

	    case BARRIER:
	      /* Succeed if the following insn is the target label.
	         Otherwise fail.  
	         If return insns are used then the last insn in a function 
	         will be a barrier.  */
	      this_insn = next_nonnote_insn (this_insn);
	      if (this_insn && this_insn == label)
		{
		  if (jump_clobbers)
		    {
		      unicore64_ccfsm_state = 2;
		      this_insn = next_nonnote_insn (this_insn);
		    }
		  else
		    unicore64_ccfsm_state = 1;
		  succeed = TRUE;
		}
	      else
		fail = TRUE;
	      break;

/* gaoyi:
 * this case can't be predicated
 */

/*	    case CALL_INSN:*/


	    case JUMP_INSN:
	      /* If this is an unconditional branch to the same label, succeed.
	         If it is to another label, do nothing.  If it is conditional,
	         fail.  */
	      /* XXX Probably, the tests for SET and the PC are unnecessary.  */

	      scanbody = PATTERN (this_insn);
	      if (GET_CODE (scanbody) == SET
		  && GET_CODE (SET_DEST (scanbody)) == PC)
		{
		  if (GET_CODE (SET_SRC (scanbody)) == LABEL_REF
		      && XEXP (SET_SRC (scanbody), 0) == label && !reverse)
		    {
		      unicore64_ccfsm_state = 2;
		      succeed = TRUE;
		    }
		  else if (GET_CODE (SET_SRC (scanbody)) == IF_THEN_ELSE)
		    fail = TRUE;
		}
/* gaoyi : always fail :)*/
	      fail = TRUE;	/* Unrecognized jump (eg epilogue).  */

	      break;

	    default:
/* gaoyi: default fail :) */
	      fail = TRUE;
	      break;
	    }
	}
      if (succeed)
	{
	  if ((!seeking_return) && (unicore64_ccfsm_state == 1 || reverse))
	    unicore64_target_label = CODE_LABEL_NUMBER (label);
	  else if (seeking_return || unicore64_ccfsm_state == 2)
	    {
	      while (this_insn && GET_CODE (PATTERN (this_insn)) == USE)
		{
		  this_insn = next_nonnote_insn (this_insn);
		  if (this_insn && (GET_CODE (this_insn) == BARRIER
				    || GET_CODE (this_insn) == CODE_LABEL))
		    abort ();
		}
	      if (!this_insn)
		{
		  /* Oh, dear! we ran off the end.. give up */
		  recog (PATTERN (insn), insn, NULL);
		  unicore64_ccfsm_state = 0;
		  unicore64_target_insn = NULL;
		  return;
		}
	      unicore64_target_insn = this_insn;
	    }
	  else
	    abort ();
	  if (jump_clobbers)
	    {
	      if (reverse)
		abort ();
	      unicore64_current_cc =
		get_unicore64_condition_code (XEXP
					      (XEXP
					       (XEXP (SET_SRC (body), 0), 0),
					       1));
	      if (GET_CODE (XEXP (XEXP (SET_SRC (body), 0), 0)) == AND)
		unicore64_current_cc =
		  UNICORE64_INVERSE_CONDITION_CODE (unicore64_current_cc);
	      if (GET_CODE (XEXP (SET_SRC (body), 0)) == NE)
		unicore64_current_cc =
		  UNICORE64_INVERSE_CONDITION_CODE (unicore64_current_cc);
	    }
	  else
	    {
	      /* If REVERSE is true, UNICORE64_CURRENT_CC needs to be inverted from
	         what it was.  */
	      if (!reverse)
		unicore64_current_cc =
		  get_unicore64_condition_code (XEXP (SET_SRC (body), 0));
	    }

	  if (reverse || then_not_else)
	    unicore64_current_cc =
	      UNICORE64_INVERSE_CONDITION_CODE (unicore64_current_cc);
	}

      /* Restore recog_data (getting the attributes of other insns can
         destroy this array, but final.c assumes that it remains intact
         across this call; since the insn has been recognized already we
         call recog direct).  */
      recog (PATTERN (insn), insn, NULL);
    }
}

/* Returns true if REGNO is a valid register
   for holding a quantity of type MODE.  */

int
unicore64_hard_regno_mode_ok (unsigned int regno, enum machine_mode mode)
{
  if (GET_MODE_CLASS (mode) == MODE_CC)
    return regno == CC_REGNUM;

  if (regno <= LAST_UNICORE64_REGNUM)
    /* We allow any value to be stored in the general regisetrs.  */
    return 1;

  if (regno == FRAME_POINTER_REGNUM || regno == ARG_POINTER_REGNUM)
    /* We only allow integers in the fake hard registers.  */
    return GET_MODE_CLASS (mode) == MODE_INT;

  /* The only registers left are the FPU registers
     which we only allow to hold FP values.  */
  return TARGET_HARD_FLOAT && GET_MODE_CLASS (mode) == MODE_FLOAT
    && regno >= FIRST_UNICORE64_FP_REGNUM
    && regno <= LAST_UNICORE64_FP_REGNUM && mode != XFmode;
}

/* star */
int
unicore64_regno_class (int regno)
{
  if (regno <= LAST_UNICORE64_REGNUM
      || regno == FRAME_POINTER_REGNUM || regno == ARG_POINTER_REGNUM)
    return GENERAL_REGS;

  if (regno == CC_REGNUM)
    return NO_REGS;

  return FPU_REGS;
}

/* Handle a special case when computing the offset
   of an argument from the frame pointer.  */

int
unicore64_debugger_arg_offset (int value, rtx addr)
{
  rtx insn;

  /* We are only interested if dbxout_parms() failed to compute the offset.  */
  if (value != 0)
    return 0;

  /* We can only cope with the case where the address is held in a register.  */
  if (GET_CODE (addr) != REG)
    return 0;

  /* If we are using the frame pointer to point at the argument, then
     an offset of 0 is correct.  */
  if (REGNO (addr) == (unsigned) HARD_FRAME_POINTER_REGNUM)
    return 0;

  /* If we are using the stack pointer to point at the
     argument, then an offset of 0 is correct.  */
  if ((TARGET_UNICORE16 || !frame_pointer_needed)
      && REGNO (addr) == SP_REGNUM)
    return 0;

  /* Oh dear.  The argument is pointed to by a register rather
     than being held in a register, or being stored at a known
     offset from the frame pointer.  Since GDB only understands
     those two kinds of argument we must translate the address
     held in the register into an offset from the frame pointer.
     We do this by searching through the insns for the function
     looking to see where this register gets its value.  If the
     register is initialised from the frame pointer plus an offset
     then we are in luck and we can continue, otherwise we give up.

     This code is exercised by producing debugging information
     for a function with arguments like this:

     double func (double a, double b, int c, double d) {return d;}

     Without this code the stab for parameter 'd' will be set to
     an offset of 0 from the frame pointer, rather than 8.  */

  /* The if() statement says:

     If the insn is a normal instruction
     and if the insn is setting the value in a register
     and if the register being set is the register holding the address of the argument
     and if the address is computing by an addition
     that involves adding to a register
     which is the frame pointer
     a constant integer

     then... */

  for (insn = get_insns (); insn; insn = NEXT_INSN (insn))
    {
      if (GET_CODE (insn) == INSN
	  && GET_CODE (PATTERN (insn)) == SET
	  && REGNO (XEXP (PATTERN (insn), 0)) == REGNO (addr)
	  && GET_CODE (XEXP (PATTERN (insn), 1)) == PLUS
	  && GET_CODE (XEXP (XEXP (PATTERN (insn), 1), 0)) == REG
	  && REGNO (XEXP (XEXP (PATTERN (insn), 1), 0)) ==
	  (unsigned) HARD_FRAME_POINTER_REGNUM
	  && GET_CODE (XEXP (XEXP (PATTERN (insn), 1), 1)) == CONST_INT)
	{
	  value = INTVAL (XEXP (XEXP (PATTERN (insn), 1), 1));

	  break;
	}
    }

  if (value == 0)
    {
      debug_rtx (addr);
      warning (0, "unable to compute real location of stacked parameter");
      value = 8;		/* XXX magic hack */
    }

  return value;
}

#define def_builtin(NAME, TYPE, CODE) \
  builtin_function ((NAME), (TYPE), (CODE), BUILT_IN_MD, NULL)


static void
unicore64_init_tls_builtins (void)
{
  tree ftype, decl;

  ftype = build_function_type (ptr_type_node, void_list_node);
  decl = add_builtin_function ("__builtin_thread_pointer", ftype,
			       UNICORE64_BUILTIN_THREAD_POINTER, BUILT_IN_MD,
			       NULL, NULL_TREE);
  TREE_NOTHROW (decl) = 1;
  TREE_READONLY (decl) = 1;
}

void
unicore64_init_builtins (void)
{
  tree endlink = void_list_node;
  tree int_endlink = tree_cons (NULL_TREE, integer_type_node, endlink);
  tree pchar_type_node = build_pointer_type (char_type_node);

  tree int_ftype_int, void_ftype_pchar;

  // star , 20091104
  unicore64_init_tls_builtins ();

  /* void func (void *) */
  void_ftype_pchar
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, pchar_type_node, endlink));

  /* int func (int) */
  int_ftype_int = build_function_type (integer_type_node, int_endlink);

}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

rtx
unicore64_expand_builtin (tree exp,
			  rtx target,
			  rtx subtarget ATTRIBUTE_UNUSED,
			  enum machine_mode mode ATTRIBUTE_UNUSED,
			  int ignore ATTRIBUTE_UNUSED)
{
  enum insn_code icode;
  //tree fndecl = TREE_OPERAND (TREE_OPERAND (exp, 0), 0);
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  tree arglist = TREE_OPERAND (exp, 1);
  tree arg0;
  rtx op0, pat;
  enum machine_mode tmode, mode0;
  int fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
/* star delete it, 20091109 */
#if 0
    case UNICORE64_BUILTIN_CLZ:
      icode = CODE_FOR_clz;
      arg0 = TREE_VALUE (arglist);
      op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;

      if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);
      if (target == 0
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0);
      if (!pat)
	return 0;
      emit_insn (pat);
      return target;
#endif

    case UNICORE64_BUILTIN_THREAD_POINTER:
      return unicore64_load_tp (target);

    default:
      break;
    }


  /* @@@ Should really do something sensible here.  */
  return NULL_RTX;
}

/* Recursively search through all of the blocks in a function
   checking to see if any of the variables created in that
   function match the RTX called 'orig'.  If they do then
   replace them with the RTX called 'new'.  */

static void
replace_symbols_in_block (tree block, rtx orig, rtx new)
{
  for (; block; block = BLOCK_CHAIN (block))
    {
      tree sym;

      if (!TREE_USED (block))
	continue;

      for (sym = BLOCK_VARS (block); sym; sym = TREE_CHAIN (sym))
	{
	  if ((DECL_NAME (sym) == 0 && TREE_CODE (sym) != TYPE_DECL)
	      || DECL_IGNORED_P (sym)
	      || TREE_CODE (sym) != VAR_DECL
	      || DECL_EXTERNAL (sym) || !rtx_equal_p (DECL_RTL (sym), orig))
	    continue;

	  SET_DECL_RTL (sym, new);
	}

      replace_symbols_in_block (BLOCK_SUBBLOCKS (block), orig, new);
    }
}

/* Return the number (counting from 0) of
   the least significant set bit in MASK.  */

#ifdef __GNUC__
inline
#endif
  static int
number_of_first_bit_set (int mask)
{
  int bit;

  for (bit = 0; (mask & (1 << bit)) == 0; ++bit)
    continue;

  return bit;
}

static struct machine_function *
unicore64_init_machine_status (void)
{
  struct machine_function *machine;
  machine =
    (machine_function *) ggc_alloc_cleared (sizeof (machine_function));

#if UNICORE64_FT_UNKNOWN != 0
  machine->func_type = UNICORE64_FT_UNKNOWN;
#endif
  return machine;
}

/* Return an RTX indicating where the return address to the
   calling function can be found.  */

rtx
unicore64_return_addr (int count,rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return NULL_RTX;

  return get_hard_reg_initial_val (Pmode, LR_REGNUM);
}

/* Do anything needed before RTL is emitted for each function.  */

void
unicore64_init_expanders (void)
{
  /* Arrange to initialize and mark the machine per-function status.  */
  init_machine_status = unicore64_init_machine_status;
}

/* Return the length of a function name prefix
    that starts with the character 'c'.  */
/* star */

static int
unicore64_get_strip_length (char c)
{
  switch (c)
    {
      UNICORE64_NAME_ENCODING_LENGTHS default:return 0;
    }
}

/* Return a pointer to a function's name with any
   and all prefix encodings stripped from it.  */

const char *
unicore64_strip_name_encoding (const char *name)
{
  int skip;

  while ((skip = unicore64_get_strip_length (*name)))
    name += skip;

  return name;
}

#ifdef OBJECT_FORMAT_ELF
/* Switch to an arbitrary section NAME with attributes as specified
   by FLAGS.  ALIGN specifies any known alignment requirements for
   the section; 0 if the default should be used.

   Differs from the default elf version only in the prefix character
   used before the section type.  */

static void
unicore64_elf_asm_named_section (const char *name, unsigned int flags)
{
  char flagchars[8], *f = flagchars;
  const char *type;

  if (!(flags & SECTION_DEBUG))
    *f++ = 'a';
  if (flags & SECTION_WRITE)
    *f++ = 'w';
  if (flags & SECTION_CODE)
    *f++ = 'x';
  if (flags & SECTION_SMALL)
    *f++ = 's';
  if (flags & SECTION_MERGE)
    *f++ = 'M';
  if (flags & SECTION_STRINGS)
    *f++ = 'S';
  *f = '\0';

  if (flags & SECTION_BSS)
    type = "nobits";
  else
    type = "progbits";

  if (flags & SECTION_ENTSIZE)
    fprintf (asm_out_file, "\t.section\t%s,\"%s\",%%%s,%d\n",
	     name, flagchars, type, flags & SECTION_ENTSIZE);
  else
    fprintf (asm_out_file, "\t.section\t%s,\"%s\",%%%s\n",
	     name, flagchars, type);
}
#endif

// huangping: FIXME 2011-09-01
static bool
unicore64_split_complex_arg (const_tree type)
{
  return false;
}

/* star */
static int
unicore64_arg_partial_bytes (CUMULATIVE_ARGS * pcum,
			     enum machine_mode mode,
			     tree type, bool named ATTRIBUTE_UNUSED)
{
  int nregs = pcum->nregs;

  if (NUM_ARG_REGS > nregs && (NUM_ARG_REGS < nregs + NUM_REGS2 (mode, type)))
    return (NUM_ARG_REGS - nregs) * UNITS_PER_WORD;

  return 0;
}

/* Worker function for TARGET_SETUP_INCOMING_VARARGS.

   On the UNICORE64, PRETEND_SIZE is set in order to have the prologue push the last
   named arg and all anonymous args onto the stack.
   XXX I know the prologue shouldn't be pushing registers, but it is faster
   that way.  */

static void
unicore64_setup_incoming_varargs (CUMULATIVE_ARGS * cum,
				  enum machine_mode mode ATTRIBUTE_UNUSED,
				  tree type ATTRIBUTE_UNUSED,
				  int *pretend_size,
				  int second_time ATTRIBUTE_UNUSED)
{
  cfun->machine->uses_anonymous_args = 1;
  if (cum->nregs < NUM_ARG_REGS)
    *pretend_size = (NUM_ARG_REGS - cum->nregs) * UNITS_PER_WORD;
}

/* Symbols in the text segment can be accessed without indirecting via the
   constant pool; it may take an extra binary operation, but this is still
   faster than indirecting via memory.  Don't do this when not optimizing,
   since we won't be calculating al of the offsets necessary to do this
   simplification.  */

static void
unicore64_encode_section_info (tree decl, rtx rtl, int first)
{
  if (optimize > 0 && TREE_CONSTANT (decl))
    SYMBOL_REF_FLAG (XEXP (rtl, 0)) = 1;

  /* If we are referencing a function that is weak then encode a long call
     flag in the function name, otherwise if the function is static or
     or known to be defined in this file then encode a short call flag.  */
  if (first && TREE_CODE_CLASS (TREE_CODE (decl)) == 'd')
    {
      if (TREE_CODE (decl) == FUNCTION_DECL && DECL_WEAK (decl))
	unicore64_encode_call_attribute (decl, LONG_CALL_FLAG_CHAR);
      else if (!TREE_PUBLIC (decl))
	unicore64_encode_call_attribute (decl, SHORT_CALL_FLAG_CHAR);
    }

  default_encode_section_info (decl, rtl, first);
}

/* Output code to add DELTA to the first argument, and then jump
   to FUNCTION.  Used for C++ multiple inheritance.  */
static void
unicore64_output_mi_thunk (FILE * file,
			   tree thunk ATTRIBUTE_UNUSED,
			   HOST_WIDE_INT delta,
			   HOST_WIDE_INT vcall_offset ATTRIBUTE_UNUSED,
			   tree function)
{
  int mi_delta = delta;
  const char *const mi_op = mi_delta < 0 ? "dsub" : "dadd";
  int shift = 22;
  int this_regno =
    (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function) ? 1 : 0);
  if (mi_delta < 0)
    mi_delta = -mi_delta;
  if (mi_delta != 0)
    {
      asm_fprintf (file, "\tdmov\tip, #0\n");
      if (mi_delta & (0x3ff << shift))
	{
	  asm_fprintf (file, "\t%s\tip, ip, #%d\n",
		       mi_op,(( mi_delta & (0x3ff << shift)) >> shift ));
	  if (mi_delta & (0x7ff << 11))
	    asm_fprintf (file, "\tdlsl\tip, ip, #%d\n",11);
	  else
	    asm_fprintf (file, "\tdlsl\tip, ip, #%d\n",22);
	}
      shift -= 11;
      if (mi_delta & (0x7ff << shift))
	{
	  asm_fprintf (file, "\t%s\tip, ip, #%d\n",
		       mi_op,(( mi_delta & (0x7ff << shift)) >> shift ));
	  asm_fprintf (file, "\tdlsl\tip, ip, #%d\n",11);
	}
      shift -= 11;
      if (mi_delta & (0x7ff << shift))
	asm_fprintf (file, "\t%s\tip, ip, #%d\n",
		     mi_op, ( mi_delta & 0x7ff) );
      asm_fprintf (file, "\tdadd\t%r, %r, ip\n",
		        this_regno, this_regno);

    }
  fputs ("\tb\t", file);
  assemble_name (file, XSTR (XEXP (DECL_RTL (function), 0), 0));
  if (NEED_PLT_RELOC)
    fputs ("(PLT)", file);
  fputc ('\n', file);
}

/* Determine the machine mode for untyped call return. 
   unicore64 don't support INT mode larger than DImode
   or FLOAT mode lager than DFmode*/
/* star add */
static enum machine_mode
unicore64_apply_result_mode (unsigned regno)
{
  return regno > 31 ? DFmode : DImode;
}

/* Determine the amount of memory needed to store the possible return
   registers of an untyped call.  */
/* star add */
int
unicore64_apply_result_size (void)
{
  int size = 8;

  if (TARGET_UNICORE64)
    {
      if (FP_HARD)
	size += 8;
    }

  return size;
}


int
unicore64_addr_need_write (rtx X)
{
    if (GET_CODE (X) == PRE_INC || GET_CODE (X) == POST_INC
	|| GET_CODE (X) == PRE_DEC || GET_CODE (X) == POST_DEC)
      {
	if (GET_CODE (XEXP (X, 0)) != REG)
	  abort ();
	return 1;
      }
    return 0;
}

int
unicore64_opd_need_write (rtx X)
{
  if (!X)
    return 0;

  if (GET_CODE (X) == MEM 
      && unicore64_addr_need_write (XEXP (X, 0)))
    return 1;
  return 0;
}


/* Test for various thread-local symbols.  */

/* Return TRUE if X is a thread-local symbol.  */

bool
unicore64_tls_symbol_p (rtx x)
{
  if (! TARGET_HAVE_TLS)
    return false;

  if (GET_CODE (x) != SYMBOL_REF)
    return false;

  return SYMBOL_REF_TLS_MODEL (x) != 0;
}

/* Helper for unicore64_tls_referenced_p.  */

static int
unicore64_tls_operand_p_1 (rtx *x, void *data ATTRIBUTE_UNUSED)
{
  if (GET_CODE (*x) == SYMBOL_REF)
    return SYMBOL_REF_TLS_MODEL (*x) != 0;

  /* Don't recurse into UNSPEC_TLS looking for TLS symbols; these are
     TLS offsets, not real symbol references.  */
  if (GET_CODE (*x) == UNSPEC
      && XINT (*x, 1) == UNSPEC_TLS)
    return -1;

  return 0;
}

/* Return TRUE if X contains any TLS symbol references.  */

bool
unicore64_tls_referenced_p (rtx x)
{
  if (! TARGET_HAVE_TLS)
    return false;

  return for_each_rtx (&x, unicore64_tls_operand_p_1, NULL);
}

/* Implement TARGET_CANNOT_FORCE_CONST_MEM.  */

bool
unicore64_cannot_force_const_mem (rtx x)
{
  rtx base, offset;

  /*
  if (ARM_OFFSETS_MUST_BE_WITHIN_SECTIONS_P)
    {
      split_const (x, &base, &offset);
      if (GET_CODE (base) == SYMBOL_REF
	  && !offset_within_block_p (base, INTVAL (offset)))
	return true;
    }
    */
  return unicore64_tls_referenced_p (x);
}

static rtx
get_tls_get_addr (void)
{
  if (!tls_get_addr_libfunc)
    tls_get_addr_libfunc = init_one_libfunc ("__tls_get_addr");
  return tls_get_addr_libfunc;
}

static rtx
unicore64_load_tp (rtx target)
{
  if (TARGET_HARD_TP)
    {
      /* We use r16 as the tp base register. */ 
      rtx tp = gen_rtx_REG (DImode, 16);
      if (!target)
	return tp;
      else
	{
	  emit_move_insn (target, tp);
	  return target;
	}
    }
  else
    {
      /* Always returned in r0.  Immediately copy the result into a pseudo,
	 otherwise other uses of r0 (e.g. setting up function arguments) may
	 clobber the value.  */
      if (!target)
	target = gen_reg_rtx (DImode);
      emit_insn (gen_load_tp_soft ());
      emit_move_insn (target, gen_rtx_REG (DImode, 0));
      return target;
    }
  return NULL;
}

static rtx
load_tls_operand (rtx x, rtx reg)
{
  rtx tmp;

  if (reg == NULL_RTX)
    reg = gen_reg_rtx (DImode);

  tmp = gen_rtx_CONST (DImode, x);

  emit_move_insn (reg, tmp);

  return reg;
}

static rtx
unicore64_call_tls_get_addr (rtx x, rtx reg, rtx *valuep, int reloc)
{
  rtx insns, label, labelno, sum;

  start_sequence ();

  labelno = GEN_INT (pic_labelno++);
  label = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, labelno), UNSPEC_PIC_LABEL);
  label = gen_rtx_CONST (VOIDmode, label);
  sum = gen_rtx_UNSPEC (Pmode,
			gen_rtvec (4, x, GEN_INT (reloc), label,
				   GEN_INT (TARGET_UNICORE64 ? 0 : 2)), 
			UNSPEC_TLS); // huangping: FIXME
  reg = load_tls_operand (sum, reg);

  if (TARGET_UNICORE64)
    emit_insn (gen_pic_add_dot_plus_four (reg, reg, labelno));

  *valuep = emit_library_call_value (get_tls_get_addr (), NULL_RTX, LCT_PURE, /* LCT_CONST?  */
				     Pmode, 1, reg, Pmode);

  insns = get_insns ();
  end_sequence ();

  return insns;
}

rtx
legitimize_tls_address (rtx x, rtx reg)
{
  rtx dest, tp, label, labelno, sum, insns, ret, eqv, addend;
  unsigned int model = SYMBOL_REF_TLS_MODEL (x);

  switch (model)
    {
    case TLS_MODEL_GLOBAL_DYNAMIC:
      insns = unicore64_call_tls_get_addr (x, reg, &ret, TLS_GD32);
      dest = gen_reg_rtx (Pmode);
      emit_libcall_block (insns, dest, ret, x);
      return dest;

    case TLS_MODEL_LOCAL_DYNAMIC:
      insns = unicore64_call_tls_get_addr (x, reg, &ret, TLS_LDM32);

      /* Attach a unique REG_EQUIV, to allow the RTL optimizers to
	 share the LDM result with other LD model accesses.  */
      eqv = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, const1_rtx),
			    UNSPEC_TLS);
      dest = gen_reg_rtx (Pmode);
      emit_libcall_block (insns, dest, ret, eqv);

      /* Load the addend.  */
      addend = gen_rtx_UNSPEC (Pmode, gen_rtvec (2, x, GEN_INT (TLS_LDO32)),
			       UNSPEC_TLS);
      addend = force_reg (DImode, gen_rtx_CONST (DImode, addend));
      return gen_rtx_PLUS (Pmode, dest, addend);

    case TLS_MODEL_INITIAL_EXEC:
      labelno = GEN_INT (pic_labelno++);
      label = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, labelno), UNSPEC_PIC_LABEL);
      label = gen_rtx_CONST (VOIDmode, label);
      sum = gen_rtx_UNSPEC (Pmode,
			    gen_rtvec (4, x, GEN_INT (TLS_IE32), label,
				       GEN_INT (TARGET_UNICORE64 ? 0 : 2)),
			    UNSPEC_TLS);
      reg = load_tls_operand (sum, reg);

      if (TARGET_UNICORE64)
	emit_insn (gen_tls_load_dot_plus_four (reg, reg, labelno));

      tp = unicore64_load_tp (NULL_RTX);

      return gen_rtx_PLUS (Pmode, tp, reg);

    case TLS_MODEL_LOCAL_EXEC:
      tp = unicore64_load_tp (NULL_RTX);

      reg = gen_rtx_UNSPEC (Pmode,
			    gen_rtvec (2, x, GEN_INT (TLS_LE32)),
			    UNSPEC_TLS);
      reg = force_reg (DImode, gen_rtx_CONST (DImode, reg));

      return gen_rtx_PLUS (Pmode, tp, reg);

    default:
      abort ();
    }
}

static bool
unicore64_emit_tls_decoration (FILE *fp, rtx x)
{
  enum tls_reloc reloc;
  rtx val;

  val = XVECEXP (x, 0, 0);
  reloc = INTVAL (XVECEXP (x, 0, 1));

  output_addr_const (fp, val);

  switch (reloc)
    {
    case TLS_GD32:
      fputs ("(tlsgd)", fp);
      break;
    case TLS_LDM32:
      fputs ("(tlsldm)", fp);
      break;
    case TLS_LDO32:
      fputs ("(tlsldo)", fp);
      break;
    case TLS_IE32:
      fputs ("(gottpoff)", fp);
      break;
    case TLS_LE32:
      fputs ("(tpoff)", fp);
      break;
    default:
      gcc_unreachable ();
    }

  switch (reloc)
    {
    case TLS_GD32:
    case TLS_LDM32:
    case TLS_IE32:
      fputs (" + (. - ", fp);
      output_addr_const (fp, XVECEXP (x, 0, 2));
      fputs (" - ", fp);
      output_addr_const (fp, XVECEXP (x, 0, 3));
      fputc (')', fp);
      break;
    default:
      break;
    }

  return TRUE;
}

/* UNICORE64 implementation of TARGET_ASM_OUTPUT_DWARF_DTPREL.  */

static void
unicore64_output_dwarf_dtprel (FILE *file, int size, rtx x)
{
  gcc_assert (size == 8); 
  fputs ("\t.dword\t", file);
  output_addr_const (file, x);
  fputs ("(tlsldo)", file);
}

bool
unicore64_output_addr_const_extra (FILE *fp, rtx x)
{
  if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_TLS)
    return unicore64_emit_tls_decoration (fp, x);
  else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_PIC_LABEL)
    {
      char label[256];
      int labelno = INTVAL (XVECEXP (x, 0, 0));

      ASM_GENERATE_INTERNAL_LABEL (label, "LPIC", labelno);
      assemble_name_raw (fp, label);

      return TRUE;
    }
  else if (GET_CODE (x) == UNSPEC && XINT (x, 1) == UNSPEC_GOTSYM_OFF)
    {
      assemble_name (fp, "_GLOBAL_OFFSET_TABLE_");
      if (GOT_PCREL)
	fputs ("+.", fp);
      fputs ("-(", fp);
      output_addr_const (fp, XVECEXP (x, 0, 0));
      fputc (')', fp);
      return TRUE;
    }
  return FALSE;
}

/* Must not copy a SET whose source operand is PC-relative.  */

static bool
unicore64_cannot_copy_insn_p (rtx insn)
{
  rtx pat = PATTERN (insn);

  if (GET_CODE (pat) == SET)
    {
      rtx rhs = SET_SRC (pat);

      if (GET_CODE (rhs) == UNSPEC
	  && XINT (rhs, 1) == UNSPEC_PIC_BASE)
	return TRUE;

      if (GET_CODE (rhs) == MEM
	  && GET_CODE (XEXP (rhs, 0)) == UNSPEC
	  && XINT (XEXP (rhs, 0), 1) == UNSPEC_PIC_BASE)
	return TRUE;
    }

  return FALSE;
}
/* Return TRUE if this rtx is the difference of a symbol and a label,
   and will reduce to a PC-relative relocation in the object file.
   Expressions like this can be left alone when generating PIC, rather
   than forced through the GOT.  */
int
pcrel_constant_p (rtx x)
{
  if (GET_CODE (x) == MINUS)
    return symbol_mentioned_p (XEXP (x, 0)) && label_mentioned_p (XEXP (x, 1));

  return FALSE;
}
static int 
unicore64_issue_rate (void)
{
  return 2;
}


#include "gt-unicore64.h"

