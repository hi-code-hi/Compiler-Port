/* Prototypes for exported functions defined in unicore64.c . 
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2007
   Free Software Foundation, Inc.
   Contributed by Gaoyi and  Star (tanmingxing@mprc.pku.edu.cn)

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_UNICORE64_PROTOS_H
#define GCC_UNICORE64_PROTOS_H
extern void get_next_reg_name (char *, int);
extern char *output_move_double_fpu (rtx *);

extern void rdata_section (void);
extern void unicore64_override_options (void);
extern int use_return_insn (int);

extern int unicore64_regno_class (int);
extern void unicore64_finalize_pic (int);
extern int unicore64_volatile_func (void);
extern const char *unicore64_output_epilogue (int);
extern void unicore64_expand_prologue (void);
/* Used in unicore64.md, but defined in output.c.  */
extern void assemble_align (int);
extern const char *unicore64_strip_name_encoding (const char *);
extern unsigned long unicore64_current_func_type (void);
// huangping: FIXME 2012-02-09 unsigned int ==> HOST_WIDE_INT
extern HOST_WIDE_INT unicore64_compute_initial_elimination_offset (unsigned
								  int,
								  unsigned
								  int);
extern int unicore64_apply_result_size (void);

#ifdef TREE_CODE
extern int unicore64_return_in_memory (tree);
extern void unicore64_encode_call_attribute (tree, int);
#endif
#ifdef RTX_CODE
extern void gen_fp_conditional_branch (rtx *, RTX_CODE);
extern int unicore64_hard_regno_mode_ok (unsigned int, enum machine_mode);
extern int const_ok_for_unicore64 (HOST_WIDE_INT);
extern int unicore64_split_constant (RTX_CODE, enum machine_mode, rtx,
				     HOST_WIDE_INT, rtx, rtx, int);
extern RTX_CODE unicore64_canonicalize_comparison (RTX_CODE, rtx *);
extern int legitimate_pic_operand_p (rtx);
extern rtx legitimize_pic_address (rtx, enum machine_mode, rtx);
extern int const_double_rtx_ok_for_fpu (rtx);
extern int neg_const_double_rtx_ok_for_fpu (rtx);

/* Predicates.  */
extern int bad_signed_byte_operand (rtx, enum machine_mode);
extern int power_of_two_operand (rtx, enum machine_mode);
extern int nonimmediate_di_operand (rtx, enum machine_mode);
extern int di_operand (rtx, enum machine_mode);
extern int nonimmediate_soft_df_operand (rtx, enum machine_mode);
extern int soft_df_operand (rtx, enum machine_mode);
extern int index_operand (rtx, enum machine_mode);
extern int const_shift_operand (rtx, enum machine_mode);
extern int unicore64_comparison_operator (rtx, enum machine_mode);
extern int shiftable_operator (rtx, enum machine_mode);
extern int shift_operator (rtx, enum machine_mode);
extern int equality_operator (rtx, enum machine_mode);
extern int minmax_operator (rtx, enum machine_mode);
extern int cc_register (rtx, enum machine_mode);
extern int dominant_cc_register (rtx, enum machine_mode);
extern int logical_binary_operator (rtx, enum machine_mode);
extern int multi_register_push (rtx, enum machine_mode);
extern int load_multiple_operation (rtx, enum machine_mode);
extern int store_multiple_operation (rtx, enum machine_mode);

extern int symbol_mentioned_p (rtx);
extern int label_mentioned_p (rtx);
extern RTX_CODE minmax_code (rtx);
extern int adjacent_mem_locations (rtx, rtx);
extern int load_multiple_sequence (rtx *, int, int *, int *, HOST_WIDE_INT *);
extern const char *emit_ldm_seq (rtx *, int);
extern int store_multiple_sequence (rtx *, int, int *, int *,
				    HOST_WIDE_INT *);
extern const char *emit_stm_seq (rtx *, int);
extern rtx unicore64_gen_load_multiple (int, int, rtx, int, int, int,
					int, int);
extern rtx unicore64_gen_store_multiple (int, int, rtx, int, int, int,
					 int, int);
extern int unicore64_gen_movstrqi (rtx *);
extern rtx unicore64_gen_rotated_half_load (rtx);
extern enum machine_mode unicore64_select_cc_mode (RTX_CODE, rtx, rtx);
extern rtx unicore64_gen_compare_reg (RTX_CODE, rtx, rtx);
extern void unicore64_reload_in_hi (rtx *);
extern void unicore64_reload_out_hi (rtx *);
extern const char *fp_immediate_constant (rtx);
extern const char *output_call (rtx *);
extern const char *output_call_mem (rtx *);
extern const char *output_mov_long_double_fpu_from_unicore64 (rtx *);
extern const char *output_mov_long_double_unicore64_from_fpu (rtx *);
extern const char *output_mov_long_double_unicore64_from_unicore64 (rtx *);
extern const char *output_mov_double_fpu_from_unicore64 (rtx *);
extern const char *output_mov_double_unicore64_from_fpu (rtx *);
extern const char *output_move_double (rtx *);
extern const char *output_mov_immediate (rtx *);
extern const char *output_add_immediate (rtx *);
extern const char *arithmetic_instr (rtx, int);
extern void output_ascii_pseudo_op (FILE *, const unsigned char *, int);
extern const char *output_return_instruction (rtx, int, int);
extern void unicore64_poke_function_name (FILE *, const char *);
extern void unicore64_print_operand (FILE *, rtx, int);
extern void unicore64_print_operand_address (FILE *, rtx);
extern void unicore64_final_prescan_insn (rtx);
extern int unicore64_go_if_legitimate_address (enum machine_mode, rtx);
extern int unicore64_debugger_arg_offset (int, rtx);
extern int unicore64_is_longcall_p (rtx, int, int);

#if defined TREE_CODE
extern rtx unicore64_function_arg (CUMULATIVE_ARGS *,
				   enum machine_mode, tree, int);
extern void unicore64_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx,
					    tree);
#endif

#if defined AOF_ASSEMBLER
extern rtx aof_pic_entry (rtx);
extern void aof_dump_pic_table (FILE *);
extern char *aof_text_section (void);
extern char *aof_data_section (void);
extern void aof_add_import (const char *);
extern void aof_delete_import (const char *);
extern void aof_dump_imports (FILE *);
extern void zero_init_section (void);
extern void common_section (void);
#endif /* AOF_ASSEMBLER */

#endif /* RTX_CODE */

/* Unicore16 functions.  */
extern void unicore64_init_expanders (void);
//extern int unicore16_far_jump_used_p (int);
//extern const char *unicore16_unexpanded_epilogue (void);
//extern void unicore16_expand_prologue (void);
//extern void unicore16_expand_epilogue (void);
#ifdef TREE_CODE
extern int is_called_in_UNICORE64_mode (tree);
#endif
//extern int unicore16_shiftable_const (unsigned HOST_WIDE_INT);
#ifdef RTX_CODE
//extern void unicore16_final_prescan_insn (rtx);
//extern const char *unicore16_load_double_from_address (rtx *);
//extern const char *unicore16_output_move_mem_multiple (int, rtx *);
//extern void unicore16_expand_movstrqi (rtx *);
//extern int unicore16_cmp_operand (rtx, enum machine_mode);
//extern rtx *unicore16_legitimize_pic_address (rtx, enum machine_mode, rtx);
//extern int unicore16_go_if_legitimate_address (enum machine_mode, rtx);
extern rtx unicore64_return_addr (int, rtx);
//extern void unicore16_reload_out_hi (rtx *);
//extern void unicore16_reload_in_hi (rtx *);
#endif

/* Defined in pe.c.  */
extern int unicore64_dllexport_name_p (const char *);
extern int unicore64_dllimport_name_p (const char *);

#ifdef TREE_CODE
extern void unicore64_pe_unique_section (tree, int);
extern void unicore64_pe_encode_section_info (tree);
extern int unicore64_dllexport_p (tree);
extern int unicore64_dllimport_p (tree);
extern void unicore64_mark_dllexport (tree);
extern void unicore64_mark_dllimport (tree);
#endif

extern void unicore64_init_builtins (void);
#if defined (TREE_CODE) && defined (RTX_CODE)
extern rtx unicore64_expand_builtin (tree, rtx, rtx, enum machine_mode, int);
#endif

#ifdef GCC_C_PRAGMA_H		/* included from code that cares about pragmas */
extern void unicore64_pr_long_calls (cpp_reader *);
extern void unicore64_pr_no_long_calls (cpp_reader *);
extern void unicore64_pr_long_calls_off (cpp_reader *);
#endif

/* star add 20090803 */
extern int unicore64_addr_need_write (rtx);
extern int unicore64_opd_need_write (rtx);

/* star add for tls, 20091118 */
extern bool unicore64_tls_symbol_p (rtx x);
extern rtx legitimize_tls_address (rtx, rtx);
extern bool unicore64_tls_referenced_p (rtx);
extern bool unicore64_cannot_force_const_mem (rtx);

extern int tls_mentioned_p (rtx);
extern bool unicore64_output_addr_const_extra (FILE *, rtx);
extern int pcrel_constant_p (rtx x);

#endif /* ! GCC_UNICORE64_PROTOS_H */
