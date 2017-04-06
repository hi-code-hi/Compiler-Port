;;- Machine description for UNICORE64 for GNU compiler
;;  Copyright 1991, 1993, 1994, 1995, 1996, 1996, 1997, 1998, 1999, 2000,
;;  2001, 2002, 2003, 2004, 2005, 2006, 2007  Free Software Foundation, Inc.
;;  Contributed by Gaoyi and Star (tanmingxing@mprc.pku.edu.cn)

;; This file is part of GCC.

;; GCC is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; GCC is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GCC; see the file COPYING3.  If not see
;; <http://www.gnu.org/licenses/>.

;;- See file "rtl.def" for documentation on define_insn, match_*, et. al.

;;---------------------- Constants---------------------------------

;; Register numbers
(define_constants
  [(R32_REGNUM	    32)		; First FP register
   (IP_REGNUM	    28)		; Scratch register
   (SP_REGNUM	    29)		; Stack pointer
   (LR_REGNUM       30)		; Return address register
   (PC_REGNUM	    31)		; Program counter
   (CC_REGNUM       48)		; Condition code pseudo register
   (LAST_UNICORE64_REGNUM 31)
  ]
)

;; UNSPEC Usage:

(define_constants
  [
   (UNSPEC_PUSH_MULT 2)	; `push multiple' operation:
			;   operand 0 is the first register,
			;   subsequent registers are in parallel (use ...)
			;   expressions.
   (UNSPEC_PIC_SYM   3) ; A symbol that has been treated properly for pic
			;   usage, that is, we will add the pic_register
			;   value to it before trying to dereference it.
   (UNSPEC_PIC_BASE  4)	; Add PC and all but the last operand together,
			;   The last operand is the number of a PIC_LABEL
			;   that points at the containing instruction.
   (UNSPEC_PRLG_STK  5) ; A special barrier that prevents frame accesses 
			;   being scheduled before the stack adjustment insn.
   (UNSPEC_CLZ	     6) ; `clz' instruction, count leading zeros (SImode):
			;   operand 0 is the result,
			;   operand 1 is the parameter.
   (UNSPEC_PROLOGUE_USE 7) ; As USE insns are not meaningful after reload,
   			; this unspec is used to prevent the deletion of
   			; instructions setting registers for EH handling
   			; and stack frame generation.  Operand 0 is the
   			; register to "use".
   (UNSPEC_TLS      20) ; A symbol that has been treated properly for TLS usage.
   (UNSPEC_PIC_LABEL 21) ; A label used for PIC access that does not appear in the
                         ; instruction stream.
   (UNSPEC_GOTSYM_OFF 24) ; The offset of the start of the the GOT from a
			  ; a given symbolic address.
  ]
)

;; UNSPEC_VOLATILE Usage:

(define_constants
  [(VUNSPEC_BLOCKAGE 0) ; `blockage' insn to prevent scheduling across an
			;   insn in the code.
   (VUNSPEC_EPILOGUE 1) ; `epilogue' insn, used to represent any part of the
			;   instruction epilogue sequence that isn't expanded
			;   into normal RTL.  Used for both normal and sibcall
			;   epilogues.
   (VUNSPEC_ALIGN    2) ; `align' insn.  Used at the head of a minipool table 
			;   for inlined constants.
   (VUNSPEC_POOL_END 3) ; `end-of-table'.  Used to mark the end of a minipool
			;   table.
   (VUNSPEC_POOL_1   4) ; `pool-entry(1)'.  An entry in the constant pool for
			;   an 8-bit object.
   (VUNSPEC_POOL_2   5) ; `pool-entry(2)'.  An entry in the constant pool for
			;   a 16-bit object.
   (VUNSPEC_POOL_4   6) ; `pool-entry(4)'.  An entry in the constant pool for
			;   a 32-bit object.
   (VUNSPEC_POOL_8   7) ; `pool-entry(8)'.  An entry in the constant pool for
			;   a 64-bit object.
  ]
)

;;--------------------- Attributes---------------------------------

;; Operand number of an input operand that is shifted.  Zero if the
;; given instruction does not shift one of its input operands.
(define_attr "shift" "" (const_int 0))

; Floating Point Unit.  We only have one FPU or none.
 (define_attr "fpu" "no,yes" (const (symbol_ref "TARGET_HARD_FLOAT")))

; LENGTH of an instruction (in bytes)
(define_attr "length" "" (const_int 4))

; POOL_RANGE is how far away from a constant pool entry that this insn
; can be placed.  If the distance is zero, then this insn will never
; reference the pool.
; NEG_POOL_RANGE is nonzero for insns that can reference a constant pool entry
; before its address. Three kinds of instructions can refer to constant pool:
;  (1) adr  reg, OFF1 (9 bits -> 512==412)
;  (2) ldb  reg, [OFF1] (11 bits -> 2048==1948)
;  (3) ldw  reg, [OFF1] (11 bits -> 2048==1948)
;  (4) lwf  reg, [OFF1] (9 bits, 2 lshift -> 2048==1948)
(define_attr "pool_range" "" (const_int 0))
(define_attr "neg_pool_range" "" (const_int 0))

; An assembler sequence may clobber the condition codes without us knowing.
; If such an insn references the pool, then we have no way of knowing how,
; so use the most conservative value for pool_range.
(define_asm_attributes
 [(set_attr "conds" "clob")
  (set_attr "length" "4")
  (set_attr "pool_range" "412")])

; TYPE attribute is used to descripte pipeline.
; normal	any data instruction that doesn't hit memory or fp regs
; mult		a multiply instruction
; block		blockage insn, this blocks all functional units
; fdivd		DFmode floating-point division
; fdivs		SFmode floating-point division
; fmuld		DFmode floating-point multiply
; fmuls		SFmode floating-point multiply
; farithd	DFmode floating-point arithmetic
; fariths	SFmode floating-point arithmetic
; f_load	a floating point load from memory
; f_store	a floating point store to memory
; sf_f_2_r	SFmode float-point register transfer
; sf_r_2_f	the reverse of sf_f_2_r
; df_f_2_r	DFmode float-point register transfer
;		complex implementation for this function. 
; df_r_2_f	the reverse of df_f_2_r
; call		a subroutine call
; branch	a branch instruction
; load		any load from memory
; store1	store 1 word to memory from unicore64 registers
; store2	store 2 words
; store3	store 3 words
; store4	store 4 words
;

(define_attr "type"
	"normal, mult, mla, mlal, block, udiv, div, fdivs, fdivd, fmuld, fmuls, farithd,\
        fariths, f_load, f_store, sf_f_2_r, sf_r_2_f, df_f_2_r, df_r_2_f, \
	call, load, store1, store2, store3, store4, branch, modify_asr" 
	(const_string "normal"))

; condition codes: this one is used by final_prescan_insn to speed up
; conditionalizing instructions.  It saves having to scan the rtl to see if
; it uses or alters the condition codes.
; 
; USE means that the condition codes are used by the insn in the process of
;   outputting code, this means (at present) that we can't use the insn in
;   inlined branches
;
; SET means that the purpose of the insn is to set the condition codes in a
;   well defined manner.
;
; CLOB means that the condition codes are altered in an undefined manner, if
;   they are altered at all
;
; JUMP_CLOB is used when the condition cannot be represented by a single
;   instruction (UNEQ and LTGT).  These cannot be predicated.
;
; NOCOND means that the condition codes are neither altered nor affect the
;   output of this insn
(define_attr "conds" "use,set,clob,jump_clob,nocond"
	(if_then_else (eq_attr "type" "call")
	  (const_string "clob")
	 (const_string "nocond")))

; Predicable means that the insn can be conditionally executed based on
; an automatically added predicate (additional patterns are generated by 
; gen...).  We default to 'no' because no Unicore16 patterns match this rule
; and not all UNICORE64 patterns do.
(define_attr "predicable" "no,yes" (const_string "no"))

; WRITE_CONFLICT implies that a read following an unrelated write is likely
(define_attr "write_conflict" "no,yes"
  (if_then_else (eq_attr "type"
		 "block,f_load,f_store,sf_f_2_r,sf_r_2_f,call,load")
		(const_string "yes")
		(const_string "no")))

; Classify the insns into those that take one cycle and those that take more
; than one on the main cpu execution unit.
(define_attr "core_cycles" "single,multi"
  (if_then_else (eq_attr "type"
		 "normal, block, f_load, f_store, call, sf_f_2_r, sf_r_2_f, df_f_2_r, df_r_2_f")
		(const_string "single")
	        (const_string "multi")))

;; FAR_JUMP is "yes" if a BL instruction is used to generate a branch to a
;; distant label.  Only applicable to Unicore16 code.
(define_attr "far_jump" "yes,no" (const_string "no"))

;; Pipeline descriptions
;; True if the generic scheduling description should be used.
;; always define generic_sched ture for unicore64
(define_attr "generic_sched" "yes,no"
  (const (const_string "yes")))
;;------------------------- Predicate ------------------------------

(define_predicate "s_register_operand"
  (match_code "reg,subreg")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) != NO_REGS));
})

;; Any hard register.
(define_predicate "unicore64_hard_register_operand"
  (match_code "reg")
{
  return REGNO (op) < FIRST_PSEUDO_REGISTER;
})

;; Any core register, or any pseudo.  */ 
(define_predicate "unicore64_general_register_operand"
  (match_code "reg,subreg")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return (GET_CODE (op) == REG
	  && (REGNO (op) <= LAST_UNICORE64_REGNUM
	      || REGNO (op) >= FIRST_PSEUDO_REGISTER));
})

(define_predicate "f_register_operand"
  (match_code "reg,subreg")
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  /* We don't consider registers whose class is NO_REGS
     to be a register operand.  */
  return (GET_CODE (op) == REG
	  && (REGNO (op) >= FIRST_PSEUDO_REGISTER
	      || REGNO_REG_CLASS (REGNO (op)) == FPU_REGS));
})

;; Reg, subreg(reg) or const_int.
(define_predicate "reg_or_int_operand"
  (ior (match_code "const_int")
       (match_operand 0 "s_register_operand")))

(define_predicate "unicore64_immediate_operand"
  (and (match_code "const_int")
       (match_test "const_ok_for_unicore64 (INTVAL (op))")))

(define_predicate "unicore64_neg_immediate_operand"
  (and (match_code "const_int")
       (match_test "const_ok_for_unicore64 (-INTVAL (op))")))

(define_predicate "unicore64_not_immediate_operand"
  (and (match_code "const_int")
       (match_test "const_ok_for_unicore64 (~INTVAL (op))")))

;; Something valid on the RHS of an UNICORE data-processing instruction
(define_predicate "unicore64_rhs_operand"
  (ior (match_operand 0 "s_register_operand")
       (match_operand 0 "unicore64_immediate_operand")))

(define_predicate "unicore64_rhsm_operand"
  (ior (match_operand 0 "unicore64_rhs_operand")
       (match_operand 0 "memory_operand")))

(define_predicate "unicore64_add_operand"
  (ior (match_operand 0 "unicore64_rhs_operand")
       (match_operand 0 "unicore64_neg_immediate_operand")))

(define_predicate "unicore64_and_operand"
  (ior (match_operand 0 "unicore64_rhs_operand")
       (match_operand 0 "unicore64_not_immediate_operand")))

(define_predicate "unicore64_addimm_operand"
  (ior (match_operand 0 "unicore64_immediate_operand")
       (match_operand 0 "unicore64_neg_immediate_operand")))

(define_predicate "unicore64_not_operand"
  (ior (match_operand 0 "unicore64_rhs_operand")
       (match_operand 0 "unicore64_not_immediate_operand")))

;; True if the operand is a memory reference which contains an
;; offsettable address.
(define_predicate "offsettable_memory_operand"
  (and (match_code "mem")
       (match_test
        "offsettable_address_p (reload_completed | reload_in_progress,
				mode, XEXP (op, 0))")))

(define_predicate "bad_signed_byte_operand"
  (match_code "mem")
{
  op = XEXP (op, 0);

  /* A sum of anything more complex than reg + reg or reg + const is bad.  */
  if ((GET_CODE (op) == PLUS || GET_CODE (op) == MINUS)
      && (!s_register_operand (XEXP (op, 0), VOIDmode)
	  || (!s_register_operand (XEXP (op, 1), VOIDmode)
	      && GET_CODE (XEXP (op, 1)) != CONST_INT)))
    return 1;

  /* Big constants are also bad.  */
  if (GET_CODE (op) == PLUS && GET_CODE (XEXP (op, 1)) == CONST_INT
      && (INTVAL (XEXP (op, 1)) > 0x7ff
	  || -INTVAL (XEXP (op, 1)) > 0x7ff))
    return 1;

  /* Everything else is good, or can will automatically be made so.  */
  return 0;
})

;; True if the operand is a memory reference which is, or can be made,
;; word aligned by adjusting the offset.
(define_predicate "alignable_memory_operand"
  (match_code "mem")
{
  rtx reg;

  op = XEXP (op, 0);

  return ((GET_CODE (reg = op) == REG
	   || (GET_CODE (op) == SUBREG
	       && GET_CODE (reg = SUBREG_REG (op)) == REG)
	   || (GET_CODE (op) == PLUS
	       && GET_CODE (XEXP (op, 1)) == CONST_INT
	       && (GET_CODE (reg = XEXP (op, 0)) == REG
		   || (GET_CODE (XEXP (op, 0)) == SUBREG
		       && GET_CODE (reg = SUBREG_REG (XEXP (op, 0))) == REG))))
	  && REGNO_POINTER_ALIGN (REGNO (reg)) >= 32);
})

(define_predicate "unicore64_reload_memory_operand"
  (and (match_code "reg,subreg")
       (match_test "(!CONSTANT_P (op)
		     && (true_regnum(op) == -1
			 || (GET_CODE (op) == REG
			     && REGNO (op) >= FIRST_PSEUDO_REGISTER)))")))

;; True for valid operands for the rhs of an floating point insns.
;;   Allows regs or certain consts on FPA, just regs for everything else.
(define_predicate "unicore64_float_rhs_operand"
  (ior (match_operand 0 "s_register_operand")
       (and (match_code "const_double")
	    (match_test "const_double_rtx_ok_for_fpu (op)"))))

(define_predicate "unicore64_float_add_operand"
  (ior (match_operand 0 "unicore64_float_rhs_operand")
       (and (match_code "const_double")
	    (match_test "neg_const_double_rtx_ok_for_fpu (op)"))))

;; True for valid index operands.
(define_predicate "index_operand"
  (ior (match_operand 0 "s_register_operand")
       (and (match_operand 0 "immediate_operand")
	    (match_test "(GET_CODE (op) != CONST_INT
			  || (INTVAL (op) < 4096 && INTVAL (op) > -4096))"))))

;;;;; True for operators that can be combined with a shift in UNICORE state.
(define_special_predicate "shift_operator"
  (and (ior (and (match_code "mult")
		      (match_test "power_of_two_operand (XEXP (op, 1), mode)"))
	    (match_code "ashift,ashiftrt,lshiftrt"))
       (match_test "mode == GET_MODE (op)")))

;; True for EQ & NE
(define_special_predicate "equality_operator"
  (match_code "eq,ne"))

;; True for comparisons other than LTGT or UNEQ.
(define_special_predicate "unicore64_comparison_operator"
  (match_code "eq,ne,le,lt,ge,gt,geu,gtu,leu,ltu,unordered,ordered,uneq,ltgt,unlt,unle,ungt,unge"))

(define_special_predicate "minmax_operator"
  (and (match_code "smin,smax,umin,umax")
       (match_test "mode == GET_MODE (op)")))

(define_special_predicate "unicore64_ccfpe_operator"
  (and (match_code "eq,ne,le,lt,ge,gt,ordered,unordered,uneq,ltgt,unlt,unle,ungt,unge")
       (match_test "GET_MODE (op) == CCFPEmode")))

(define_special_predicate "cc_register"
  (and (match_code "reg")
       (and (match_test "REGNO (op) == CC_REGNUM")
	    (ior (match_test "mode == GET_MODE (op)")
		 (match_test "mode == VOIDmode && GET_MODE_CLASS (GET_MODE (op)) == MODE_CC")))))

(define_special_predicate "dominant_cc_register"
  (match_code "reg")
{
  if (mode == VOIDmode)
    {
      mode = GET_MODE (op);
      
      if (GET_MODE_CLASS (mode) != MODE_CC)
	return false;
    }

  return (cc_register (op, mode)
	  && (mode == CC_DNEmode
	     || mode == CC_DEQmode
	     || mode == CC_DLEmode
	     || mode == CC_DLTmode
	     || mode == CC_DGEmode
	     || mode == CC_DGTmode
	     || mode == CC_DLEUmode
	     || mode == CC_DLTUmode
	     || mode == CC_DGEUmode
	     || mode == CC_DGTUmode));
})

(define_predicate "power_of_two_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT value = INTVAL (op);

  return value != 0 && (value & (value - 1)) == 0;
})

(define_predicate "nonimmediate_di_operand"
  (match_code "reg,subreg,mem")
{
   if (s_register_operand (op, mode))
     return true;

   if (GET_CODE (op) == SUBREG)
     op = SUBREG_REG (op);

   return GET_CODE (op) == MEM && memory_address_p (DImode, XEXP (op, 0));
})

(define_predicate "di_operand"
  (ior (match_code "const_int,const_double")
       (and (match_code "reg,subreg,mem")
	    (match_operand 0 "nonimmediate_di_operand"))))

(define_predicate "nonimmediate_soft_df_operand"
  (match_code "reg,subreg,mem")
{
  if (s_register_operand (op, mode))
    return true;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);

  return GET_CODE (op) == MEM && memory_address_p (DFmode, XEXP (op, 0));
})

(define_predicate "soft_df_operand"
  (ior (match_code "const_double")
       (and (match_code "reg,subreg,mem")
	    (match_operand 0 "nonimmediate_soft_df_operand"))))

(define_predicate "const_shift_operand"
  (and (match_code "const_int")
       (ior (match_operand 0 "power_of_two_operand")
	    (match_test "((unsigned HOST_WIDE_INT) INTVAL (op)) < 32"))))


(define_special_predicate "load_multiple_operation"
  (match_code "parallel")
{
  HOST_WIDE_INT count = XVECLEN (op, 0);
  int dest_regno;
  rtx src_addr;
  HOST_WIDE_INT i = 1, base = 0;
  rtx elt;

  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET)
    return false;

  /* Check to see if this might be a write-back.  */
  if (GET_CODE (SET_SRC (elt = XVECEXP (op, 0, 0))) == PLUS)
    {
      i++;
      base = 1;

      /* Now check it more carefully.  */
      if (GET_CODE (SET_DEST (elt)) != REG
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != REG
          || GET_CODE (XEXP (SET_SRC (elt), 1)) != CONST_INT
          || INTVAL (XEXP (SET_SRC (elt), 1)) != (count - 1) * 4)
        return false;
    }

  /* Perform a quick check so we don't blow up below.  */
  if (count <= i
      || GET_CODE (XVECEXP (op, 0, i - 1)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, i - 1))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, i - 1))) != MEM)
    return false;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, i - 1)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, i - 1)), 0);

  for (; i < count; i++)
    {
      elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
          || GET_CODE (SET_DEST (elt)) != REG
          || GET_MODE (SET_DEST (elt)) != SImode
          || REGNO (SET_DEST (elt)) != (unsigned int)(dest_regno + i - base)
          || GET_CODE (SET_SRC (elt)) != MEM
          || GET_MODE (SET_SRC (elt)) != SImode
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != PLUS
          || !rtx_equal_p (XEXP (XEXP (SET_SRC (elt), 0), 0), src_addr)
          || GET_CODE (XEXP (XEXP (SET_SRC (elt), 0), 1)) != CONST_INT
          || INTVAL (XEXP (XEXP (SET_SRC (elt), 0), 1)) != (i - base) * 4)
        return false;
    }

  return true;
})

(define_special_predicate "store_multiple_operation"
  (match_code "parallel")
{
  HOST_WIDE_INT count = XVECLEN (op, 0);
  int src_regno;
  rtx dest_addr;
  HOST_WIDE_INT i = 1, base = 0;
  rtx elt;

  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, 0)) != SET)
    return false;

  /* Check to see if this might be a write-back.  */
  if (GET_CODE (SET_SRC (elt = XVECEXP (op, 0, 0))) == PLUS)
    {
      i++;
      base = 1;

      /* Now check it more carefully.  */
      if (GET_CODE (SET_DEST (elt)) != REG
          || GET_CODE (XEXP (SET_SRC (elt), 0)) != REG
          || GET_CODE (XEXP (SET_SRC (elt), 1)) != CONST_INT
          || INTVAL (XEXP (SET_SRC (elt), 1)) != (count - 1) * 4)
        return false;
    }

  /* Perform a quick check so we don't blow up below.  */
  if (count <= i
      || GET_CODE (XVECEXP (op, 0, i - 1)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, i - 1))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, i - 1))) != REG)
    return false;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, i - 1)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, i - 1)), 0);

  for (; i < count; i++)
    {
      elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
          || GET_CODE (SET_SRC (elt)) != REG
          || GET_MODE (SET_SRC (elt)) != SImode
          || REGNO (SET_SRC (elt)) != (unsigned int)(src_regno + i - base)
          || GET_CODE (SET_DEST (elt)) != MEM
          || GET_MODE (SET_DEST (elt)) != SImode
          || GET_CODE (XEXP (SET_DEST (elt), 0)) != PLUS
          || !rtx_equal_p (XEXP (XEXP (SET_DEST (elt), 0), 0), dest_addr)
          || GET_CODE (XEXP (XEXP (SET_DEST (elt), 0), 1)) != CONST_INT
          || INTVAL (XEXP (XEXP (SET_DEST (elt), 0), 1)) != (i - base) * 4)
        return false;
    }

  return true;
})

(define_special_predicate "multi_register_push"
  (match_code "parallel")
{
  if ((GET_CODE (XVECEXP (op, 0, 0)) != SET)
      || (GET_CODE (SET_SRC (XVECEXP (op, 0, 0))) != UNSPEC)
      || (XINT (SET_SRC (XVECEXP (op, 0, 0)), 1) != UNSPEC_PUSH_MULT))
    return false;

  return true;
})

(include "unicore64-generic.md")
;;-------------------------------------------------------------------
;; Special patterns for dealing with the constant pool

(define_insn "consttable_end"
  [(unspec_volatile [(const_int 0)] VUNSPEC_POOL_END)]
  "TARGET_EITHER"
  "*
  making_const_table = FALSE;
  return \"\";
  "
)

(define_insn "consttable_4"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_4)]
  "TARGET_EITHER"
  "*
  {
    making_const_table = TRUE;
    switch (GET_MODE_CLASS (GET_MODE (operands[0])))
      {
      case MODE_FLOAT:
      {
        REAL_VALUE_TYPE r;
        REAL_VALUE_FROM_CONST_DOUBLE (r, operands[0]);
        assemble_real (r, GET_MODE (operands[0]), BITS_PER_WORD);
        break;
      }
      default:
        assemble_integer (operands[0], 4, BITS_PER_WORD, 1);
        break;
      }
    return \"\";
  }"
  [(set_attr "length" "4")]
)

(define_insn "consttable_8"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_8)]
  "TARGET_EITHER"
  "*
  {
    making_const_table = TRUE;
    switch (GET_MODE_CLASS (GET_MODE (operands[0])))
      {
       case MODE_FLOAT:
        {
          REAL_VALUE_TYPE r;
          REAL_VALUE_FROM_CONST_DOUBLE (r, operands[0]);
          assemble_real (r, GET_MODE (operands[0]), BITS_PER_WORD);
	  if (GET_MODE(operands[0]) == SFmode)
	  	assemble_integer (gen_rtx_CONST_INT (VOIDmode, 0), 4, 64, 1);
          break;
        }
      default:
        assemble_integer (operands[0], 8, BITS_PER_WORD, 1);
        break;
      }
    return \"\";
  }"
  [(set_attr "length" "8")]
)

(define_insn "align_8"
  [(unspec_volatile [(const_int 0)] VUNSPEC_ALIGN)]
  "TARGET_EITHER"
  "*
  assemble_align (64);
  return \"\";
  "
)

(define_insn "clz"
  [(set (match_operand:SI             0 "s_register_operand" "=r")
        (clz:SI (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64 && unicore64_arch1"
  "cntlz\\t%0, %1")

;; Misc insns
(define_insn "nop"
  [(const_int 0)]
  "TARGET_EITHER"
  "*
  if (TARGET_UNICORE64)
    return \"dmov\\t%|r0, %|r0\\t%@ nop\";
  return  \"dmov\\tr8, r8\";
  "
  [(set (attr "length") (const_int 4))]
)
;;-----------------------------level 1---------------------------------

;;<epilogue>
(define_expand "epilogue"
  [(unspec_volatile [(return)] VUNSPEC_EPILOGUE)]
  "TARGET_EITHER"
  "
  if (USE_RETURN_INSN (FALSE))
    {
      emit_jump_insn (gen_return ());
      DONE;
    }
  emit_jump_insn (gen_rtx_UNSPEC_VOLATILE (VOIDmode,
	gen_rtvec (1,
		gen_rtx_RETURN (VOIDmode)),
	VUNSPEC_EPILOGUE));
  DONE;
  "
)

(define_insn "sibcall_epilogue"
  [(unspec_volatile [(const_int 0)] VUNSPEC_EPILOGUE)]
  "TARGET_UNICORE64"
  "*
  output_asm_insn (\"%@ Sibcall epilogue\", operands);
  if (USE_RETURN_INSN (FALSE))
    return output_return_instruction (NULL, FALSE, FALSE);
  return unicore64_output_epilogue (FALSE);
  "
  [(set_attr "length" "44")
   (set_attr "type" "block")]
)

(define_insn "*epilogue_insns"
  [(unspec_volatile [(return)] VUNSPEC_EPILOGUE)]
  "TARGET_EITHER"
  "*
  if (TARGET_UNICORE64)
    return unicore64_output_epilogue (TRUE);
  else /* TARGET_UNICORE16 */
    ;
  "
  ; Length is absolute worst case
  [(set_attr "length" "44")
   (set_attr "type" "block")]
)
(define_expand "eh_epilogue"
  [(use (match_operand:DI 0 "register_operand" "r"))
   (use (match_operand:DI 1 "register_operand" "r"))
   (use (match_operand:DI 2 "register_operand" "r"))]
  "TARGET_EITHER"
  "
  {
    cfun->machine->eh_epilogue_sp_ofs = operands[1];
    if (GET_CODE (operands[2]) != REG || REGNO (operands[2]) != 2)
      {
	rtx ra = gen_rtx_REG (Pmode, 2);

	emit_move_insn (ra, operands[2]);
	operands[2] = ra;
      }
    /* This is a hack -- we may have crystalized the function type too
       early.  */
    cfun->machine->func_type = 0;
  }"
)

;;<return>
;; Often the return insn will be the same as loading from memory, so set attr
(define_insn "return"
  [(return)]
  "TARGET_UNICORE64 && USE_RETURN_INSN (FALSE)"
  "*
  {
    if (unicore64_ccfsm_state == 2)
      {
        unicore64_ccfsm_state += 2;
        return \"\";
      }
    return output_return_instruction (NULL, TRUE, FALSE);
  }"
  [(set_attr "length" "64")
  (set_attr "type" "load")]
)

;;<jump>

(define_expand "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "TARGET_EITHER"
  ""
)
(define_insn "*unicore64_jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  "TARGET_UNICORE64"
  "*
  {
    if (unicore64_ccfsm_state == 1 || unicore64_ccfsm_state == 2)
      {
        unicore64_ccfsm_state += 2;
        return \"\";
      }
    return \"b\\t%l0\";
  }
  "
  [(set_attr "type" "branch")]
)
(define_expand "indirect_jump"
  [(set (pc)
	(match_operand:DI 0 "s_register_operand" ""))]
  "TARGET_EITHER"
  ""
)
(define_insn "*unicore64_indirect_jump"
  [(set (pc)
	(match_operand:DI 0 "s_register_operand" "r"))]
  "TARGET_UNICORE64"
  "jump\\t%0\\t%@ indirect register jump"
  [(set_attr "predicable" "yes")]
)

;; Although not supported by the define_expand above,
;; cse/combine may generate this form.
(define_insn "*load_indirect_jump"
  [(set (pc)
	(match_operand:DI 0 "memory_operand" "m"))]
  "TARGET_UNICORE64"
  "ldd%T0\\t%|ip, %0\\t\;\n\t jump %|ip %@indirect memory jump"
  [(set_attr "type" "load")
   (set_attr "pool_range" "1448")
   (set_attr "neg_pool_range" "1448")]
)

(define_expand "beq"
  [(set (pc)
	(if_then_else (eq (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (EQ, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, EQ);
  		DONE;
 	}
   }"
)

(define_expand "bne"
  [(set (pc)
	(if_then_else (ne (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (NE, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, NE);
  		DONE;
 	}
   }"
)

(define_expand "bgt"
  [(set (pc)
	(if_then_else (gt (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (GT, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, GT);
  		DONE;
 	}
   }"
)

(define_expand "ble"
  [(set (pc)
	(if_then_else (le (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (LE, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, LE);
  		DONE;
 	}
   }"
)

(define_expand "bge"
  [(set (pc)
	(if_then_else (ge (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (GE, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, GE);
  		DONE;
 	}
   }"
)

(define_expand "blt"
  [(set (pc)
	(if_then_else (lt (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (LT, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, LT);
  		DONE;
 	}
   }"
)

(define_expand "bgtu"
  [(set (pc)
	(if_then_else (gtu (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64"
  "operands[1] = unicore64_gen_compare_reg (GTU, unicore64_compare_op0, unicore64_compare_op1);"
)

(define_expand "bleu"
  [(set (pc)
	(if_then_else (leu (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64"
  "operands[1] = unicore64_gen_compare_reg (LEU, unicore64_compare_op0, unicore64_compare_op1);"
)

(define_expand "bgeu"
  [(set (pc)
	(if_then_else (geu (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64"
  "operands[1] = unicore64_gen_compare_reg (GEU, unicore64_compare_op0, unicore64_compare_op1);"
)

(define_expand "bltu"
  [(set (pc)
	(if_then_else (ltu (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64"
  "operands[1] = unicore64_gen_compare_reg (LTU, unicore64_compare_op0, unicore64_compare_op1);"
)

;;<prologue>
(define_expand "prologue"
  [(clobber (const_int 0))]
  "TARGET_EITHER"
  "if (TARGET_UNICORE64)
     unicore64_expand_prologue ();
   else
     ;
     /* unicore16_expand_prologue (); */
  DONE;
  "
)
(define_insn "prologue_use"
  [(unspec:DI [(match_operand:DI 0 "register_operand" "")] UNSPEC_PROLOGUE_USE)]
  ""
  "%@ %0 needed for prologue"
)

;;<mov>
(define_expand "movsi"
  [(set (match_operand:SI 0 "general_operand" "")
        (match_operand:SI 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_UNICORE64)
    {
      if (GET_CODE (operands[0]) == SUBREG)
      {
      	rtx inner = XEXP (operands[0], 0);
	int offset = XINT (operands[0], 1);
	if (offset == 0 && GET_MODE (inner) == DImode)
	{
	  rtx op0, op1;
	  rtx src = copy_to_reg (inner); 
	  rtx shift_offset = gen_rtx_CONST_INT (VOIDmode, 32);
	  emit_insn (gen_zero_extendsidi2 (src, operands[1]));
	  emit_insn (gen_lshrdi3 (inner, inner, shift_offset));
	  emit_insn (gen_ashldi3 (inner, inner, shift_offset));
	  emit_insn (gen_iordi3 (inner, inner, src));
	  DONE;
	}

      }

    /* Everything except mem = const or mem = mem can be done easily */
      if (GET_CODE (operands[0]) == MEM)
        operands[1] = force_reg (SImode, operands[1]);
      if (unicore64_general_register_operand (operands[0], SImode)
          && GET_CODE (operands[1]) == CONST_INT
          && !(const_ok_for_unicore64 (INTVAL (operands[1]))
               || const_ok_for_unicore64 (~INTVAL (operands[1]))))
        {
	
           unicore64_split_constant (SET, SImode, NULL_RTX,
	                       INTVAL (operands[1]), operands[0], NULL_RTX,
			       optimize && can_create_pseudo_p ());
          DONE;
        }
    }
  else /* TARGET_UNICORE16.... */
    FAIL;
    
  /* Recognize the case where operand[1] is a reference to thread-local
     data and load its address to a register.  */
  if (unicore64_tls_referenced_p (operands[1]))
    {
      rtx tmp = operands[1];
      rtx addend = NULL;

      if (GET_CODE (tmp) == CONST && GET_CODE (XEXP (tmp, 0)) == PLUS)
        {
          addend = XEXP (XEXP (tmp, 0), 1);
          tmp = XEXP (XEXP (tmp, 0), 0);
        }

      gcc_assert (GET_CODE (tmp) == SYMBOL_REF);
      gcc_assert (SYMBOL_REF_TLS_MODEL (tmp) != 0);

      tmp = legitimize_tls_address (tmp,
				    !can_create_pseudo_p () ? operands[0] : 0);
      if (addend)
        {
          tmp = gen_rtx_PLUS (SImode, tmp, addend);
          tmp = force_operand (tmp, operands[0]);
        }
      operands[1] = tmp;
    }
    else if (flag_pic
      && (CONSTANT_P (operands[1])
	 || symbol_mentioned_p (operands[1])
	 || label_mentioned_p (operands[1])))
      operands[1] = legitimize_pic_address (operands[1], SImode,
					    (!can_create_pseudo_p ()
					     ? operands[0]
					     : 0));
    
  "
)

(define_insn "*unicore64_movsi_insn"
  [(set (match_operand:SI 0 "nonimmediate_operand" "=r,r,r, m")
	(match_operand:SI 1 "general_operand"      "rI,K,mi,r"))]
  "TARGET_UNICORE64
   && (register_operand (operands[0], SImode)
       || register_operand (operands[1], SImode))"
  "@
   mov\\t%0, %1
   not\\t%0, #%B1
   ldw%T1\\t%0, %1
   stw%T0\\t%1, %0"
  [(set_attr "type" "*,*,load,store1")
   (set_attr "pool_range" "*,*,1448,*")
   (set_attr "neg_pool_range" "*,*,1448,*")]
)


;; If copying one reg to another we can set the condition codes according to
;; its value.  Such a move is common after a return from subroutine and the
;; result is being tested against zero.
(define_insn "*movsi_compare0"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 1 "s_register_operand" "0,r")
		    (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(match_dup 1))]
  "TARGET_UNICORE64"
  "@
   cmpsub.a\\t%0, #0
   sub.a\\t%0, %1, #0"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)
(define_expand "movhi"
  [(set (match_operand:HI 0 "general_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_UNICORE64)
    {
      if (can_create_pseudo_p ())
        {
          if (GET_CODE (operands[0]) == MEM)
	    {
	      if (unicore64_arch1)
	        {
	          emit_insn (gen_storehi_single_op (operands[0], operands[1]));
	          DONE;
	        }
	      if (GET_CODE (operands[1]) == CONST_INT)
	        emit_insn (gen_storeinthi (operands[0], operands[1]));
	      else
	        {
	          if (GET_CODE (operands[1]) == MEM)
		    operands[1] = force_reg (HImode, operands[1]);
	          if (BYTES_BIG_ENDIAN)
		    emit_insn (gen_storehi_bigend (operands[1], operands[0]));
	          else
		   emit_insn (gen_storehi (operands[1], operands[0]));
	        }
	      DONE;
	    }
          /* Sign extend a constant, and keep it in an SImode reg.  */
          else if (GET_CODE (operands[1]) == CONST_INT)
	    {
	      rtx reg = gen_reg_rtx (SImode);
	      HOST_WIDE_INT val = INTVAL (operands[1]) & 0xffff;

	      /* If the constant is already valid, leave it alone.  */
	      if (!const_ok_for_unicore64 (val))
	        {
	          /* If setting all the top bits will make the constant 
		     loadable in a single instruction, then set them.  
		     Otherwise, sign extend the number.  */

	          if (const_ok_for_unicore64 (~(val | ~0xffff)))
		    val |= ~0xffff;
	          else if (val & 0x8000)
		    val |= ~0xffff;
	        }

	      emit_insn (gen_movsi (reg, GEN_INT (val)));
	      operands[1] = gen_lowpart (HImode, reg);
	    }
          else if (!unicore64_arch1)
	    {
	     /* Note: We do not have to worry about TARGET_MMU_TRAPS
	        for v4 and up architectures because LDRH instructions will
	        be used to access the HI values, and these cannot generate
	        unaligned word access faults in the MMU.  */
	      if (GET_CODE (operands[1]) == MEM)
	        {
	          if (TARGET_MMU_TRAPS)
		    {
		      rtx base;
		      rtx offset = const0_rtx;
		      rtx reg = gen_reg_rtx (SImode);

		      if ((GET_CODE (base = XEXP (operands[1], 0)) == REG
		           || (GET_CODE (base) == PLUS
			       && (GET_CODE (offset = XEXP (base, 1))
				   == CONST_INT)
                               && ((INTVAL(offset) & 1) != 1)
			       && GET_CODE (base = XEXP (base, 0)) == REG))
		          && REGNO_POINTER_ALIGN (REGNO (base)) >= 32)
		        {
		          HOST_WIDE_INT new_offset = INTVAL (offset) & ~3;
		          rtx new;

		          new = gen_rtx_MEM (SImode,
				   	     plus_constant (base, new_offset));
	                  MEM_COPY_ATTRIBUTES (new, operands[1]);
		          emit_insn (gen_movsi (reg, new));
		          if (((INTVAL (offset) & 2) != 0)
			      ^ (BYTES_BIG_ENDIAN ? 1 : 0))
			    {
			      rtx reg2 = gen_reg_rtx (SImode);

			      emit_insn (gen_lshrsi3 (reg2, reg,
					 GEN_INT (16)));
			      reg = reg2;
			    }
		        }
		      else
		        emit_insn (gen_movhi_bytes (reg, operands[1]));

		      operands[1] = gen_lowpart (HImode, reg);
		    }
	          else if (BYTES_BIG_ENDIAN)
		    {
		      rtx base;
		      rtx offset = const0_rtx;

		      if ((GET_CODE (base = XEXP (operands[1], 0)) == REG
		           || (GET_CODE (base) == PLUS
			      && (GET_CODE (offset = XEXP (base, 1))
				  == CONST_INT)
			      && GET_CODE (base = XEXP (base, 0)) == REG))
		          && REGNO_POINTER_ALIGN (REGNO (base)) >= 32)
		        {
		          rtx reg = gen_reg_rtx (SImode);
		          rtx new;

		          if ((INTVAL (offset) & 2) == 2)
			    {
			      HOST_WIDE_INT new_offset = INTVAL (offset) ^ 2;
			      new = gen_rtx_MEM (SImode,
				  	         plus_constant (base,
								new_offset));
                              MEM_COPY_ATTRIBUTES (new, operands[1]);
			      emit_insn (gen_movsi (reg, new));
			    }
		          else
			    {
			      new = gen_rtx_MEM (SImode,
						 XEXP (operands[1], 0));
	                      MEM_COPY_ATTRIBUTES (new, operands[1]);
			      emit_insn (gen_rotated_loadsi (reg, new));
			    }

		          operands[1] = gen_lowpart (HImode, reg);
		        }
		      else
		        {
		          emit_insn (gen_movhi_bigend (operands[0],
						       operands[1]));
		          DONE;
		        }
		    }
	       }
	   }
        }
      /* Handle loading a large integer during reload */
      else if (GET_CODE (operands[1]) == CONST_INT
	       && !const_ok_for_unicore64 (INTVAL (operands[1]))
	       && !const_ok_for_unicore64 (~INTVAL (operands[1])))
        {
          /* Writing a constant to memory needs a scratch, which should
	     be handled with SECONDARY_RELOADs.  */
          if (GET_CODE (operands[0]) != REG)
	    abort ();

          operands[0] = gen_rtx_SUBREG (DImode, operands[0], 0);
          emit_insn (gen_movdi (operands[0], operands[1]));
          DONE;
       }
    }
  else /* TARGET_UNICORE16 */
    FAIL;
  "
)

(define_expand "movhi_bytes"
  [(set (match_dup 2) (zero_extend:SI (match_operand:HI 1 "" "")))
   (set (match_dup 3)
	(zero_extend:SI (match_dup 6)))
   (set (match_operand:SI 0 "" "")
	 (ior:SI (ashift:SI (match_dup 4) (const_int 8)) (match_dup 5)))]
  "TARGET_UNICORE64"
  "
  {
    rtx mem1, mem2;
    rtx addr = copy_to_mode_reg (SImode, XEXP (operands[1], 0));

    mem1 = gen_rtx_MEM (QImode, addr);
    MEM_COPY_ATTRIBUTES (mem1, operands[1]);
    mem2 = gen_rtx_MEM (QImode, plus_constant (addr, 1));
    MEM_COPY_ATTRIBUTES (mem2, operands[1]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[1] = mem1;
    operands[2] = gen_reg_rtx (SImode);
    operands[3] = gen_reg_rtx (SImode);
    operands[6] = mem2;

    if (BYTES_BIG_ENDIAN)
      {
	operands[4] = operands[2];
	operands[5] = operands[3];
      }
    else
      {
	operands[4] = operands[3];
	operands[5] = operands[2];
      }
  }"
)

(define_expand "movhi_bigend"
  [(set (match_dup 2)
	(rotate:SI (subreg:SI (match_operand:HI 1 "memory_operand" "") 0)
		   (const_int 16)))
   (set (match_dup 3)
	(ashiftrt:SI (match_dup 2) (const_int 16)))
   (set (match_operand:HI 0 "s_register_operand" "")
	(subreg:HI (match_dup 3) 0))]
  "TARGET_UNICORE64"
  "
  operands[2] = gen_reg_rtx (SImode);
  operands[3] = gen_reg_rtx (SImode);
  "
)

;; Pattern to recognise insn generated default case above
(define_insn "*movhi_insn_arch4"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,m,r")    
	(match_operand:HI 1 "general_operand"      "rI,K,r,m"))]
  "TARGET_UNICORE64
   && unicore64_arch1
   && (GET_CODE (operands[1]) != CONST_INT
       || const_ok_for_unicore64 (INTVAL (operands[1]))
       || const_ok_for_unicore64 (~INTVAL (operands[1])))"
  "@
   mov\\t%0, %1\\t%@ movhi
   not\\t%0, #%B1\\t%@ movhi
   sth%T0\\t%1, %0\\t%@ movhi
   ldh%T1\\t%0, %1\\t%@ movhi"
  [(set_attr "type" "*,*,store1,load")
   (set_attr "pool_range" "*,*,*,1448")
   (set_attr "neg_pool_range" "*,*,*,1448")]
)
(define_insn "*movhi_insn_littleend"
  [(set (match_operand:HI 0 "s_register_operand" "=r,r,r")
	(match_operand:HI 1 "general_operand"  "rI,K,m"))]
  "TARGET_UNICORE64
   && !unicore64_arch1
   && !BYTES_BIG_ENDIAN
   && !TARGET_MMU_TRAPS
   && (GET_CODE (operands[1]) != CONST_INT
       || const_ok_for_unicore64 (INTVAL (operands[1]))
       || const_ok_for_unicore64 (~INTVAL (operands[1])))"
  "@
   mov\\t%0, %1\\t%@ movhi
   not\\t%0, #%B1\\t%@ movhi
   ldw%T1\\t%0, %1\\t%@ movhi"
  [(set_attr "type" "*,*,load")
   (set_attr "pool_range" "1448")
   (set_attr "neg_pool_range" "1448")]
)
(define_insn "*movhi_insn_bigend"
  [(set (match_operand:HI 0 "s_register_operand" "=r,r,r")
	(match_operand:HI 1 "general_operand"    "rI,K,m"))]
  "TARGET_UNICORE64
   && !unicore64_arch1
   && BYTES_BIG_ENDIAN
   && !TARGET_MMU_TRAPS
   && (GET_CODE (operands[1]) != CONST_INT
       || const_ok_for_unicore64 (INTVAL (operands[1]))
       || const_ok_for_unicore64 (~INTVAL (operands[1])))"
  "@
   mov\\t%0, %1\\t%@ movhi
   not\\t%0, #%B1\\t%@ movhi
   ldw%T1\\t%0, %1\\t%@ movhi_bigend\;asr\\t%0, %0, #16"
  [(set_attr "type" "*,*,load")
   (set_attr "length" "4,4,8")
   (set_attr "pool_range" "*,*,1448")
   (set_attr "neg_pool_range" "*,*,1448")]
)
(define_expand "call"
  [(parallel [(call (match_operand 0 "memory_operand" "")
	            (match_operand 1 "general_operand" ""))
	      (use (match_operand 2 "" ""))
	      (clobber (reg:DI LR_REGNUM))])]
  "TARGET_EITHER"
  "
  {
    rtx callee;
    if (operands[2] == NULL_RTX)
      operands[2] = const0_rtx;
    callee  = XEXP (operands[0], 0);
    if ((GET_CODE (callee) == SYMBOL_REF
	 && unicore64_is_longcall_p (operands[0], INTVAL (operands[2]), 0))
	|| (GET_CODE (callee) != SYMBOL_REF
	    && GET_CODE (callee) != REG))
      XEXP (operands[0], 0) = force_reg (Pmode, callee);
  }"
)
(define_expand "call_value"
  [(parallel [(set (match_operand       0 "" "")
	           (call (match_operand 1 "memory_operand" "")
		         (match_operand 2 "general_operand" "")))
	      (use (match_operand 3 "" ""))
	      (clobber (reg:DI LR_REGNUM))])]
  "TARGET_EITHER"
  "
  {
    rtx callee = XEXP (operands[1], 0);
    if (operands[3] == 0)
      operands[3] = const0_rtx;
    if (GET_CODE (callee) != REG
	&& unicore64_is_longcall_p (operands[1], INTVAL (operands[3]), 0))
      XEXP (operands[1], 0) = force_reg (Pmode, callee);
  }"
)
(define_insn "*loadhi_si_bigend"
  [(set (match_operand:SI                       0 "s_register_operand" "=r")
	(rotate:SI (subreg:SI (match_operand:HI 1 "memory_operand"      "m") 0)
		   (const_int 16)))]
  "TARGET_UNICORE64
   && BYTES_BIG_ENDIAN
   && !TARGET_MMU_TRAPS"
  "ldw%T1\\t%0, %1\\t%@ movhi_bigend"
  [(set_attr "type" "load")
   (set_attr "pool_range" "1448")
   (set_attr "neg_pool_range" "1448")]
)
(define_insn "*movhi_bytes"
  [(set (match_operand:HI 0 "s_register_operand" "=r,r")
	(match_operand:HI 1 "unicore64_rhs_operand"  "rI,K"))]
  "TARGET_UNICORE64 && TARGET_MMU_TRAPS"
  "@
   mov\\t%0, %1\\t%@ movhi
   not\\t%0, #%B1\\t%@ movhi"
)

;; We use a DImode scratch because we may occasionally need an additional
;; temporary if the address isn't offsettable -- push_reload doesn't seem
;; to take any notice of the "o" constraints on reload_memory_operand operand.
(define_expand "reload_outhi"
  [(parallel [(match_operand:HI 0 "unicore64_reload_memory_operand" "=o")
	      (match_operand:HI 1 "s_register_operand"        "r")
	      (match_operand:DI 2 "s_register_operand"        "=&l")])]
  "TARGET_EITHER"
  "if (TARGET_UNICORE64)
     unicore64_reload_out_hi (operands);
   else
     ;
   /* unicore16_reload_out_hi (operands); */
  DONE;
  "
)

(define_expand "movqi"
  [(set (match_operand:QI 0 "general_operand" "")
        (match_operand:QI 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_UNICORE64)
    {
      /* Everything except mem = const or mem = mem can be done easily */

      if (can_create_pseudo_p ())
        {
          if (GET_CODE (operands[1]) == CONST_INT)
	    {
	      rtx reg = gen_reg_rtx (SImode);

	      emit_insn (gen_movsi (reg, operands[1]));
	      operands[1] = gen_lowpart (QImode, reg);
	    }
         if (GET_CODE (operands[0]) == MEM)
	   operands[1] = force_reg (QImode, operands[1]);
       }
    }
  else /* TARGET_UNICORE16 */
    FAIL;
  "
)

(define_insn "*unicore64_movqi_insn"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,r,m")
	(match_operand:QI 1 "general_operand" "rI,K,m,r"))]
  "TARGET_UNICORE64
   && (   register_operand (operands[0], QImode)
       || register_operand (operands[1], QImode))"
  "@
   mov\\t%0, %1
   not\\t%0, #%B1
   ldb%T1\\t%0, %1
   stb%T0\\t%1, %0"
  [(set_attr "type" "*,*,load,store1")]
)

(define_expand "movsf"
  [(set (match_operand:SF 0 "general_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_UNICORE64)
    {
      if (GET_CODE (operands[0]) == MEM)
        operands[1] = force_reg (SFmode, operands[1]);
    }
  else /* TARGET_UNICORE16 */
    FAIL;
  "
)


(define_insn "*unicore64_movsf_hard_insn"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=f,f,m,f,r,r,r, m")
	(match_operand:SF 1 "general_operand" "f,mE,f,r,f,r,mE,r"))]
  "TARGET_UNICORE64
   && TARGET_HARD_FLOAT
   && (GET_CODE (operands[0]) != MEM
       || register_operand (operands[1], SFmode))"
  "@
   fmov.s\\t%0, %1
   lwf%T1\\t%0, %W1
   swf%T0\\t%1, %W0
   mtf\\t%1, %0
   mff\\t%0, %1
   mov\\t%0, %1
   ldw%T1\\t%0, %1\\t%@ float
   stw%T0\\t%1, %0\\t%@ float"
  [(set_attr "type"
	 "fariths,f_load,f_store,sf_r_2_f,sf_f_2_r,*,load,store1")
   (set_attr "pool_range" "*,1448,*,*,*,*,1448,*")
   (set_attr "neg_pool_range" "*,1448,*,*,*,*,1448,*")]
)

;; Exactly the same as above, except that all `f' cases are deleted.
;; This is necessary to prevent reload from ever trying to use a `f' reg
;; when -msoft-float.
(define_insn "*unicore64_movsf_soft_insn"
  [(set (match_operand:SF 0 "nonimmediate_operand" "=r,r,m")
	(match_operand:SF 1 "general_operand"  "r,mE,r"))]
  "TARGET_UNICORE64
   && TARGET_SOFT_FLOAT
   && (GET_CODE (operands[0]) != MEM
       || register_operand (operands[1], SFmode))"
  "@
   mov\\t%0, %1
   ldw%T1\\t%0, %1\\t%@ float
   stw%T0\\t%1, %0\\t%@ float"
  [(set_attr "length" "4,4,4")
   (set_attr "type" "*,load,store1")
   (set_attr "pool_range" "*,1448,*")
   (set_attr "neg_pool_range" "*,1448,*")]
)

;;;This should have alternatives for constants.
(define_expand "movdf"
  [(set (match_operand:DF 0 "general_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_UNICORE64)
    {
      if (GET_CODE (operands[0]) == MEM)
        operands[1] = force_reg (DFmode, operands[1]);
    }
  else /* TARGET_UNICORE16 */
    FAIL;
  "
)

;; Reloading a df mode value stored in integer regs to memory can require a
;; scratch reg.
(define_expand "reload_outdf"
  [(match_operand:DF 0 "unicore64_reload_memory_operand" "=o")
   (match_operand:DF 1 "s_register_operand" "r")
   (match_operand:DI 2 "s_register_operand" "=&r")]
  "TARGET_UNICORE64"
  "
  {
    enum rtx_code code = GET_CODE (XEXP (operands[0], 0));

    if (code == REG)
      operands[2] = XEXP (operands[0], 0);
    else if (code == POST_INC || code == PRE_DEC)
      {
	operands[0] = gen_rtx_SUBREG (DImode, operands[0], 0);
	operands[1] = gen_rtx_SUBREG (DImode, operands[1], 0);
	emit_insn (gen_movdi (operands[0], operands[1]));
      }
    else if (code == PRE_INC)
      {
	rtx reg = XEXP (XEXP (operands[0], 0), 0);

	emit_insn (gen_adddi3 (reg, reg, GEN_INT (8)));
	operands[2] = reg;
      }
    else if (code == POST_DEC)
      operands[2] = XEXP (XEXP (operands[0], 0), 0);
    else
      emit_insn (gen_adddi3 (operands[2], XEXP (XEXP (operands[0], 0), 0),
			     XEXP (XEXP (operands[0], 0), 1)));

    emit_insn (gen_rtx_SET (VOIDmode, gen_rtx_MEM (DFmode, operands[2]),
			    operands[1]));

    if (code == POST_DEC)
      emit_insn (gen_adddi3 (operands[2], operands[2], GEN_INT (-8)));

    DONE;
  }"
)

(define_insn "*movdf_hard_insn"
  [(set (match_operand:DF 0 "nonimmediate_operand"
						"=r,Q,r,m,r,f,f,m,!f,!r")
	(match_operand:DF 1 "general_operand"
						"Q,r,r,r,mF,f,mF,f,r,f"))]
  "TARGET_UNICORE64
   && TARGET_HARD_FLOAT
   && (GET_CODE (operands[0]) != MEM
       || register_operand (operands[1], DFmode))"
  "*
  {
  switch (which_alternative)
    {
    default:
    case 0: return output_ldm(operands);
    case 1: return output_stm(operands);
    case 2: case 3: case 4: return output_move_double (operands);
    case 5: return \"fmov.d\\t%0, %1\";
    case 6: case 7: return output_move_double_fpu (operands);
    case 8: return output_mov_double_fpu_from_unicore64 (operands);
    case 9: return output_mov_double_unicore64_from_fpu (operands);
    }
  }
  "
  [(set_attr "length" "32,32,12,12,12,4,12,12,8,8")
   (set_attr "type"
    "load,store2,*,store2,load,fariths,f_load,f_store,sf_r_2_f,sf_f_2_r")
   (set_attr "pool_range" "*,*,*,*,1020,*,1048,*,*,*")
   (set_attr "neg_pool_range" "*,*,*,*,1008,*,1048,*,*,*")]
)

;; Software floating point version.  This is essentially the same as movdi.
;; Do not use `f' as a constraint to prevent reload from ever trying to use
;; an `f' reg.

(define_insn "*movdf_soft_insn"
  [(set (match_operand:DF 0 "nonimmediate_soft_df_operand" "=r,r,m")
	(match_operand:DF 1 "soft_df_operand" "r,mF,r"))]
  "TARGET_UNICORE64 && TARGET_SOFT_FLOAT
  "
  "* return output_move_double (operands);"
  [(set_attr "length" "8,8,8")
   (set_attr "type" "*,load,store2")
   (set_attr "pool_range" "1020")
   (set_attr "neg_pool_range" "1008")]
)
(define_expand "movdi"
  [(set (match_operand:DI 0 "general_operand" "")
        (match_operand:DI 1 "general_operand" ""))]
  "TARGET_EITHER"
  "
  if (TARGET_UNICORE64)
    {
    /* Everything except mem = const or mem = mem can be done easily */
      if (GET_CODE (operands[0]) == MEM)
        operands[1] = force_reg (DImode, operands[1]);
      if (unicore64_general_register_operand (operands[0], DImode)
          && GET_CODE (operands[1]) == CONST_INT
          && !(const_ok_for_unicore64 (INTVAL (operands[1]))
               || const_ok_for_unicore64 (~INTVAL (operands[1]))))
        {
	
           unicore64_split_constant (SET, DImode, NULL_RTX,
	                       INTVAL (operands[1]), operands[0], NULL_RTX,
			       optimize && can_create_pseudo_p ());
          DONE;
        }
    }
  else /* TARGET_UNICORE16.... */
    FAIL;
    
  /* Recognize the case where operand[1] is a reference to thread-local
     data and load its address to a register.  */
  if (unicore64_tls_referenced_p (operands[1]))
    {
      rtx tmp = operands[1];
      rtx addend = NULL;

      if (GET_CODE (tmp) == CONST && GET_CODE (XEXP (tmp, 0)) == PLUS)
        {
          addend = XEXP (XEXP (tmp, 0), 1);
          tmp = XEXP (XEXP (tmp, 0), 0);
        }

      gcc_assert (GET_CODE (tmp) == SYMBOL_REF);
      gcc_assert (SYMBOL_REF_TLS_MODEL (tmp) != 0);

      tmp = legitimize_tls_address (tmp,
				    !can_create_pseudo_p () ? operands[0] : 0);
      if (addend)
        {
          tmp = gen_rtx_PLUS (DImode, tmp, addend);
          tmp = force_operand (tmp, operands[0]);
        }
      operands[1] = tmp;
    }
    else if (flag_pic
      && (CONSTANT_P (operands[1])
	 || symbol_mentioned_p (operands[1])
	 || label_mentioned_p (operands[1])))
      operands[1] = legitimize_pic_address (operands[1], DImode,
					    (!can_create_pseudo_p ()
					     ? operands[0]
					     : 0));
    
  "
)

(define_insn "*unicore64_movdi_insn"
  [(set (match_operand:DI 0 "nonimmediate_operand" "=r,r,r, m")
	(match_operand:DI 1 "general_operand"      "rI,K,mi,r"))]
  "TARGET_UNICORE64
   && (register_operand (operands[0], DImode)
       || register_operand (operands[1], DImode))"
  "@
   dmov\\t%0, %1
   dnot\\t%0, #%B1
   ldd%T1\\t%0, %1
   std%T0\\t%1, %0"
  [(set_attr "type" "*,*,load,store1")
   (set_attr "pool_range" "*,*,1448,*")
   (set_attr "neg_pool_range" "*,*,1448,*")]
)

;; If copying one reg to another we can set the condition codes according to
;; its value.  Such a move is common after a return from subroutine and the
;; result is being tested against zero.
(define_insn "*movdi_compare0"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:DI 1 "s_register_operand" "0,r")
		    (const_int 0)))
   (set (match_operand:DI 0 "s_register_operand" "=r,r")
	(match_dup 1))]
  "TARGET_UNICORE64"
  "@
   dcmpsub.a\\t%0, #0
   dsub.a\\t%0, %1, #0"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

;; Comparison and test insns

(define_expand "cmpdi"
  [(match_operand:DI 0 "s_register_operand" "")
   (match_operand:DI 1 "unicore64_add_operand" "")]
  "TARGET_UNICORE64"
  "{
    unicore64_compare_op0 = operands[0];
    unicore64_compare_op1 = operands[1];
    unicore64_compare_type = CMP_INT;
    DONE;
  }"
)

(define_insn "*unicore64_cmpdi_insn"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:DI 0 "s_register_operand" "r,r")
		    (match_operand:DI 1 "unicore64_add_operand"    "rI,L")))]
  "TARGET_UNICORE64"
  "@
   dcmpsub.a\\t%0, %1
   dcmpadd.a\\t%0, #%n1"
  [(set_attr "conds" "set")
  (set_attr "type" "modify_asr")]
)

(define_insn "*dmov_scc"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(match_operator:DI 1 "unicore64_comparison_operator"
	 [(match_operand 2 "cc_register" "") (const_int 0)]))]
  "TARGET_UNICORE64"
  "dcmov%D1\\t%0, #0\;\n\tdcmov%d1\\t%0, #1"
  [(set_attr "conds" "use")
   (set_attr "length" "8")]
)
(define_insn "*dmov_negscc"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(neg:DI (match_operator:DI 1 "unicore64_comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_UNICORE64"
  "dcmov%D1\\t%0, #0\;\n\tdcnot%d1\\t%0, #0"
  [(set_attr "conds" "use")
   (set_attr "length" "8")]
)

(define_insn "*dmov_notscc"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(not:DI (match_operator:DI 1 "unicore64_comparison_operator"
		 [(match_operand 2 "cc_register" "") (const_int 0)])))]
  "TARGET_UNICORE64"
  "dcmov%D1\\t%0, #0\;\n\tdcnot%d1\\t%0, #1"
  [(set_attr "conds" "use")
   (set_attr "length" "8")]
)

;; Conditional move insns
(define_expand "movdicc"
  [(set (match_operand:DI 0 "s_register_operand" "")
	(if_then_else:DI (match_operand 1 "unicore64_comparison_operator" "")
			 (match_operand:DI 2 "unicore64_not_operand" "")
			 (match_operand:DI 3 "unicore64_not_operand" "")))]
  "TARGET_UNICORE64"
  "
  {
    enum rtx_code code = GET_CODE (operands[1]);
    rtx ccreg;

    if (code == UNEQ || code == LTGT)
      FAIL;

    ccreg = unicore64_gen_compare_reg (code, unicore64_compare_op0, unicore64_compare_op1);
    operands[1] = gen_rtx_fmt_ee (code, VOIDmode, ccreg, const0_rtx);
  }"
)

(define_insn "*movdicc_insn"
  [(set (match_operand:DI 0 "s_register_operand" "=r,r,r,r,r,r,r,r")
	(if_then_else:DI
	 (match_operator 3 "unicore64_comparison_operator"
	  [(match_operand 4 "cc_register" "") (const_int 0)])
	 (match_operand:DI 1 "unicore64_not_operand" "0,0,rI,K,rI,rI,K,K")
	 (match_operand:DI 2 "unicore64_not_operand" "rI,K,0,0,rI,K,rI,K")))]
  "TARGET_UNICORE64"
  "@
   dcmov%D3\\t%0, %2
   dcnot%D3\\t%0, #%B2
   dcmov%d3\\t%0, %1
   dcnot%d3\\t%0, #%B1
   dcmov%d3\\t%0, %1\;dcmov%D3\\t%0, %2
   dcmov%d3\\t%0, %1\;dcnot%D3\\t%0, #%B2
   dcnot%d3\\t%0, #%B1\;dcmov%D3\\t%0, %2
   dcnot%d3\\t%0, #%B1\;dcnot%D3\\t%0, #%B2"
  [(set_attr "length" "4,4,4,4,8,8,8,8")
   (set_attr "conds" "use")]
)
;;<store>

;; Subroutine to store a half word from a register into memory.
;; Operand 0 is the source register (HImode)
;; Operand 1 is the destination address in a register (SImode)

;; In both this routine and the next, we must be careful not to spill
;; a memory address of reg+large_const into a separate PLUS insn, since this
;; can generate unrecognizable rtl.

(define_expand "storehi"
  [;; store the low byte
   (set (match_operand 1 "" "") (match_dup 3))
   ;; extract the high byte
   (set (match_dup 2)
	(ashiftrt:SI (match_operand 0 "" "") (const_int 8)))
   ;; store the high byte
   (set (match_dup 4) (subreg:QI (match_dup 2) 0))]	;explicit subreg safe
  "TARGET_UNICORE64"
  "
  {
    rtx op1 = operands[1];
    rtx addr = XEXP (op1, 0);
    enum rtx_code code = GET_CODE (addr);

    if ((code == PLUS && GET_CODE (XEXP (addr, 1)) != CONST_INT)
	|| code == MINUS)
      op1 = replace_equiv_address (operands[1], force_reg (SImode, addr));

    operands[4] = adjust_address (op1, QImode, 1);
    operands[1] = adjust_address (operands[1], QImode, 0);
    operands[3] = gen_lowpart (QImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[2] = gen_reg_rtx (SImode); 
  }"
)

(define_expand "storehi_bigend"
  [(set (match_dup 4) (match_dup 3))
   (set (match_dup 2)
	(ashiftrt:SI (match_operand 0 "" "") (const_int 8)))
   (set (match_operand 1 "" "")	(subreg:QI (match_dup 2) 3))]
  "TARGET_UNICORE64"
  "
  {
    rtx op1 = operands[1];
    rtx addr = XEXP (op1, 0);
    enum rtx_code code = GET_CODE (addr);

    if ((code == PLUS && GET_CODE (XEXP (addr, 1)) != CONST_INT)
	|| code == MINUS)
      op1 = replace_equiv_address (op1, force_reg (SImode, addr));

    operands[4] = adjust_address (op1, QImode, 1);
    operands[1] = adjust_address (operands[1], QImode, 0);
    operands[3] = gen_lowpart (QImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[2] = gen_reg_rtx (SImode);
  }"
)

;; Subroutine to store a half word integer constant into memory.
(define_expand "storeinthi"
  [(set (match_operand 0 "" "")
	(subreg:QI (match_operand 1 "" "") 0))
   (set (match_dup 3) (match_dup 2))]
  "TARGET_UNICORE64"
  "
  {
    HOST_WIDE_INT value = INTVAL (operands[1]);
    rtx addr = XEXP (operands[0], 0);
    rtx op0 = operands[0];
    enum rtx_code code = GET_CODE (addr);

    if ((code == PLUS && GET_CODE (XEXP (addr, 1)) != CONST_INT)
	|| code == MINUS)
      op0 = replace_equiv_address (op0, force_reg (SImode, addr));

    operands[1] = gen_reg_rtx (SImode);
    if (BYTES_BIG_ENDIAN)
      {
	emit_insn (gen_movsi (operands[1], GEN_INT ((value >> 8) & 255)));
	if ((value & 255) == ((value >> 8) & 255))
	  operands[2] = operands[1];
	else
	  {
	    operands[2] = gen_reg_rtx (SImode);
	    emit_insn (gen_movsi (operands[2], GEN_INT (value & 255)));
	  }
      }
    else
      {
	emit_insn (gen_movsi (operands[1], GEN_INT (value & 255)));
	if ((value & 255) == ((value >> 8) & 255))
	  operands[2] = operands[1];
	else
	  {
	    operands[2] = gen_reg_rtx (SImode);
	    emit_insn (gen_movsi (operands[2], GEN_INT ((value >> 8) & 255)));
	  }
      }

    operands[3] = adjust_address (op0, QImode, 1);
    operands[0] = adjust_address (operands[0], QImode, 0);
    operands[2] = gen_lowpart (QImode, operands[2]);
  }"
)

(define_expand "storehi_single_op"
  [(set (match_operand:HI 0 "memory_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  "TARGET_UNICORE64 && unicore64_arch1"
  "
  if (!s_register_operand (operands[1], HImode))
    operands[1] = copy_to_mode_reg (HImode, operands[1]);
  "
)
;;<load>
;; TLS support
(define_insn "load_tp_hard"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(const_int 0)] UNSPEC_TLS))]
  "TARGET_HARD_TP"
  "dmov\\%0, r16\\t@ load_tp_hard"
  [(set_attr "predicable" "yes")]
)

;; Doesn't clobber R1-R3.  Must use r0 for the first operand.
(define_insn "load_tp_soft"
  [(set (reg:DI 0) (unspec:DI [(const_int 0)] UNSPEC_TLS))
   (clobber (reg:DI LR_REGNUM))
   (clobber (reg:DI IP_REGNUM))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_SOFT_TP"
  "call\\t__aeabi_read_tp\\t@ load_tp_soft"
  [(set_attr "conds" "clob")
  (set_attr "type" "modify_asr")]
)

;; When generating pic, we need to load the symbol offset into a register.
;; So that the optimizer does not confuse this with a normal symbol load
;; we use an unspec.  The offset will be loaded from a constant pool entry,
;; since that is the only type of relocation we can use.

;; The rather odd constraints on the following are to force reload to leave
;; the insn alone, and to force the minipool generation pass to then move
;; the GOT symbol to memory.
(define_insn "pic_load_addr_unicore64"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "" "mX")] UNSPEC_PIC_SYM))]
  "TARGET_UNICORE64 && flag_pic"
  "ldd%T1\\t%0, %1"
  [(set_attr "type" "load")
   (set (attr "pool_range")     (const_int 1448))
   (set (attr "neg_pool_range") (const_int 1448))]
)

;; This variant is used for AOF assembly, since it needs to mention the
;; pic register in the rtl.
(define_expand "pic_load_addr_based"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand 1 "" "") (match_dup 2)] UNSPEC_PIC_SYM))]
  "TARGET_UNICORE64 && flag_pic"
  "operands[2] = pic_offset_table_rtx;"
)
(define_insn "*pic_load_addr_based_insn"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(unspec:DI [(match_operand 1 "" "")
		    (match_operand 2 "s_register_operand" "r")]
		   UNSPEC_PIC_SYM))]
  "TARGET_EITHER && flag_pic && operands[2] == pic_offset_table_rtx"
  "*
#ifdef AOF_ASSEMBLER
  operands[1] = aof_pic_entry (operands[1]);
#endif
  if (unicore64_addr_need_write (operands[1]))
    output_asm_insn (\"ldd.w\\t%0, %a1\", operands);
  else 
    output_asm_insn (\"ldd\\t%0, %a1\", operands);
  return \"\";
  "
  [(set_attr "type" "load")
   (set (attr "pool_range") (const_int 1448))
   (set (attr "neg_pool_range")	(const_int 1448))]
)

(define_insn "pic_add_dot_plus_four"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:DI 1 "register_operand" "r")
		    (const_int 0)
		    (match_operand 2 "" "")]
		   UNSPEC_PIC_BASE))]
  "TARGET_UNICORE64"
  "*
    (*targetm.asm_out.internal_label) (asm_out_file, \"LPIC\",
				       INTVAL (operands[2]));
    return \"dadd%?\\t%0, %|pc, %1\";
  "
  [(set_attr "predicable" "yes")]
)

(define_insn "tls_load_dot_plus_four"
  [(set (match_operand:DI 0 "register_operand" "+r")
	(mem:DI (unspec:DI [(match_operand:DI 1 "register_operand" "r")
			    (const_int 0)
			    (match_operand 2 "" "")]
			   UNSPEC_PIC_BASE)))]
  "TARGET_UNICORE64"
  "*
    (*targetm.asm_out.internal_label) (asm_out_file, \"LPIC\",
				       INTVAL (operands[2]));
    return \"ldd%?\\t%0, [%|pc+], %1\t\t@ tls_load_dot_plus_four\";
  "
  [(set_attr "predicable" "yes")]
)


(define_expand "builtin_setjmp_receiver"
  [(label_ref (match_operand 0 "" ""))]
  "flag_pic"
  "
{
  unicore64_finalize_pic (0);
  DONE;
}")

;<push registers>
;; Push multiple registers to the stack.  Registers are in parallel (use ...)
;; expressions.  For simplicity, the first register is also in the unspec
;; part.
(define_insn "*push_multi"
  [(match_parallel 2 "multi_register_push"
    [(set (match_operand:BLK 0 "memory_operand" "=m")
	  (unspec:BLK [(match_operand:DI 1 "s_register_operand" "r")]
		      UNSPEC_PUSH_MULT))])]
  "TARGET_UNICORE64"
  "*
  {
    int num_saves = XVECLEN (operands[2], 0);
     
    /* For the StrongUNICORE64 at least it is faster to
       use STR to store only a single register.  */
    if (num_saves == 1)
      output_asm_insn (\"dsub\\t%m0, %m0, #8\;std\\t%1, [%m0+], #0\", operands);
    else
      {
	int i;
	int k = 0;
	char pattern1[300]=\"\";
	char pattern2[300]=\"\";
	char buff[50]=\"\";
	int tag = num_saves - 1;
	for (i = num_saves - 1; i >= 1; i--)
	  {
	    if (!(REGNO (operands[1]) <= 15 && REGNO (XEXP (XVECEXP (operands[2], 0 , i), 0))> 15))
	      break;
	  }
	tag = i;
	if (REGNO (operands[1]) <= 15 && REGNO (XEXP (XVECEXP (operands[2], 0 , num_saves-1), 0))> 15)
	  {
	    k = 1;

	    strcpy (pattern2, \"dsub\\t%m0, %m0, #\");
	    sprintf (buff, \"%d;\", 8 * (XVECLEN(operands[2], 0) - tag));
	    strcat (pattern2, buff);
	    output_asm_insn (pattern2, operands);

	    for (i = XVECLEN(operands[2], 0) - 1; i > tag ; i--)
	      {
		char *p;
		strcpy (pattern2, \"std\\t%|\");
		strcat (pattern2, reg_names[REGNO(XEXP (XVECEXP (operands[2], 0, i),0))]);
		strcat (pattern2, \", [%m0+], #\");
		sprintf (buff, \"%d;\", 8 * (XVECLEN(operands[2], 0) - tag - k));
		strcat (pattern2, buff);
		output_asm_insn (pattern2, operands);
		k++;
	      }
	    strcpy (pattern2, \"std\\t%|\");
	    strcat (pattern2, reg_names[REGNO (XEXP (XVECEXP (operands[2], 0, i), 0))]);
	    strcat (pattern2, \", [%m0+], #\");
	    sprintf (buff, \"%d;\", 8 * 0);
	    strcat (pattern2, buff);
	    output_asm_insn (pattern2, operands);

	  }
	k = 1;

	strcpy (pattern1, \"dsub\\t%m0, %m0, #\");
	sprintf (buff, \"%d;\", 8 * (tag+1));
	strcat (pattern1, buff);
	output_asm_insn (pattern1, operands);

	for (i = tag; i >= 1; i--)
	  {
	    strcpy (pattern1, \"std\\t%|\");
	    strcat (pattern1,
		    reg_names[REGNO (XEXP (XVECEXP (operands[2], 0, i), 0))]);
	    strcat (pattern1, \", [%m0+], #\");
	    sprintf (buff, \"%d;\", 8 * (tag-k+1));
	    strcat (pattern1, buff);
	    output_asm_insn (pattern1, operands);
	    k++; 
	  }
	strcpy (pattern1, \"std\\t%1, [%m0+], #\");
	sprintf (buff, \"%d;\", 8 * 0);
	strcat (pattern1, buff);
	output_asm_insn (pattern1, operands);
      }

    return \"\";
  }"
  [(set_attr "type" "store4")]
)

;; Similarly for the floating point registers
(define_insn "*push_fp_multi"
  [(match_parallel 2 "multi_register_push"
    [(set (match_operand:BLK 0 "memory_operand" "=m")
	  (unspec:BLK [(match_operand:DF 1 "f_register_operand" "f")]
		      UNSPEC_PUSH_MULT))])]
  "TARGET_UNICORE64"
  "*
  {
  char pattern[200];
  char next_reg_name[4];
  int i;
  int reg_num;
  char buf[4];
  reg_num = XVECLEN (operands[2], 0);
  strcpy (pattern, \"sdwf\\t%1, [%m0-], #\");
  sprintf (buf, \"%d\", 8 * reg_num);
  strcat (pattern, buf);
  output_asm_insn (pattern, operands);

  for (i = 1; i < XVECLEN (operands[2], 0); i++)
    {
      strcpy (pattern, \"sdwf\\t%|\");
      strcat (pattern, reg_names[REGNO (XEXP (XVECEXP (operands[2], 0, i),
                                              0))]);
      strcat (pattern, \", [%m0-], #\");
      sprintf (buf, \"%d\", 8 * (reg_num - i));
      strcat (pattern, buf);
      output_asm_insn (pattern, operands);
    }
  strcpy (pattern, \"dsub %m0, %m0, #\");
  sprintf (buf, \"%d\", 8 * reg_num);
  strcat (pattern, buf);
  output_asm_insn (pattern, operands);
  return \"\";
  }"
  [(set_attr "type" "f_store")]
)


;;<addsi3>
(define_expand "addsi3"
  [(set (match_operand:SI          0 "s_register_operand" "")
	(plus:SI (match_operand:SI 1 "s_register_operand" "")
		 (match_operand:SI 2 "unicore64_add_operand" "")))]
  "TARGET_EITHER"
  "
  if (TARGET_UNICORE64 && GET_CODE (operands[2]) == CONST_INT)
    {

      unicore64_split_constant (PLUS, SImode, NULL_RTX, 
			  INTVAL (operands[2]), operands[0],
			  operands[1],
			  can_create_pseudo_p ());
      DONE;
    }
  "
)

(define_insn "*unicore64_addsi3"
  [(set (match_operand:SI          0 "s_register_operand" "=r,r")
	(plus:SI (match_operand:SI 1 "s_register_operand" "%r,r")
		 (match_operand:SI 2 "unicore64_add_operand" "rI,L")))]
  "TARGET_UNICORE64"
  "@
   add\\t%0, %1, %2
   sub\\t%0, %1, #%n2"
  [(set_attr "length" "4,4")]
)

(define_insn "*addsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:SI (match_operand:SI 1 "s_register_operand" "r, r")
		  (match_operand:SI 2 "unicore64_add_operand"    "rI,L"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_UNICORE64"
  "@
   add.a\\t%0, %1, %2
   sub.a\\t%0, %1, #%n2"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

(define_insn "*addsi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:SI (match_operand:SI 0 "s_register_operand" "r, r")
		  (match_operand:SI 1 "unicore64_add_operand"    "rI,L"))
	 (const_int 0)))]
  "TARGET_UNICORE64"
  "@
   cmpadd.a\\t%0, %1
   cmpsub.a\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

;; These patterns are the same ones as the two regular addsi3_compare0
;; patterns, except we write them slightly different - the combiner
;; tends to generate them this way.
(define_insn "*addsi3_compare0_for_combiner"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	 (match_operand:SI 1 "s_register_operand" "r,r")
	 (neg:SI (match_operand:SI 2 "unicore64_add_operand" "rI,L"))))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(plus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_UNICORE64"
  "@
   add.a\\t%0, %1, %2
   sub.a\\t%0, %1, #%n2"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

(define_insn "*addsi3_compare0_scratch_for_combiner"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	 (match_operand:SI 0 "s_register_operand" "r,r")
	 (neg:SI (match_operand:SI 1 "unicore64_add_operand" "rI,L"))))]
  "TARGET_UNICORE64"
  "@
   cmpadd.a\\t%0, %1
   cmpsub.a\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)


(define_insn "*addsi3_carryin"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))
		 (plus:SI (match_operand:SI 1 "s_register_operand" "r")
			  (match_operand:SI 2 "unicore64_rhs_operand" "rI"))))]
  "TARGET_UNICORE64"
  "addc\\t%0, %1, %2"
  [(set_attr "conds" "use")]
)

(define_insn "*addsi3_carryin_alt1"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (plus:SI (match_operand:SI 1 "s_register_operand" "r")
			  (match_operand:SI 2 "unicore64_rhs_operand" "rI"))
		 (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "TARGET_UNICORE64"
  "addc\\t%0, %1, %2"
  [(set_attr "conds" "use")]
)
(define_insn "*addsi3_carryin_alt2"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (plus:SI (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))
			  (match_operand:SI 1 "s_register_operand" "r"))
		 (match_operand:SI 2 "unicore64_rhs_operand" "rI")))]
  "TARGET_UNICORE64"
  "addc\\t%0, %1, %2"
  [(set_attr "conds" "use")]
)
(define_insn "*addsi3_carryin_alt3"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(plus:SI (plus:SI (ltu:SI (reg:CC_C CC_REGNUM) (const_int 0))
			  (match_operand:SI 2 "unicore64_rhs_operand" "rI"))
		 (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "addc\\t%0, %1, %2"
  [(set_attr "conds" "use")]
)

(define_insn "addsf3"
  [(set (match_operand:SF          0 "s_register_operand" "=f")
	(plus:SF (match_operand:SF 1 "s_register_operand" "f")
		 (match_operand:SF 2 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fadd.s\\t%0, %1, %2"
  [(set_attr "type" "farithd")]
)

(define_insn "adddf3"
  [(set (match_operand:DF          0 "s_register_operand" "=f")
	(plus:DF (match_operand:DF 1 "s_register_operand" "f")
		 (match_operand:DF 2 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fadd.d\\t%0, %1, %2"
  [(set_attr "type" "farithd")]
)

(define_insn "*unicore64_addsf3"
  [(set (pc)
        (if_then_else (match_operator 1 "unicore64_comparison_operator"
                       [(reg:CCFPE CC_REGNUM)
                          (const_int 0)])
                      (label_ref(match_operand 0 "" ""))
                      (pc)))]
  "TARGET_HARD_FLOAT"
  "*
{
  enum rtx_code cmp_code;
  cmp_code=GET_CODE(operands[1]);
  if(cmp_code==NE)
        return \"bea\\t%l0\";
  else if (cmp_code==EQ)
        return \"bub\\t%l0\";
  else abort();
}"
[(set_attr "conds" "use")]
)

(define_insn "*unicore64_adddf3"
  [(set (pc)
	(if_then_else (match_operator 1 "unicore64_comparison_operator"
		       [(match_operand 2 "cc_register" "") (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64"
  "*
  if (unicore64_ccfsm_state == 1 || unicore64_ccfsm_state == 2)
    {
      unicore64_ccfsm_state += 2;
      return \"\";
    }
  return \"b%d1\\t%l0\";
  "
  [(set_attr "conds" "use")
   (set_attr "type" "branch")]
)
;;;;;;;;;;;;; DADD ;;;;;;;;;;;;;;;
(define_expand "adddi3"
  [(set (match_operand:DI          0 "s_register_operand" "")
	(plus:DI (match_operand:DI 1 "s_register_operand" "")
		 (match_operand:DI 2 "unicore64_add_operand" "")))]
  "TARGET_UNICORE64"
  "
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      unicore64_split_constant (PLUS, DImode, NULL_RTX, 
			  INTVAL (operands[2]), operands[0],
			  operands[1],
			  can_create_pseudo_p ());
      DONE;
    }
  "
)

(define_insn "*unicore64_adddi3"
  [(set (match_operand:DI          0 "s_register_operand" "=r,r")
	(plus:DI (match_operand:DI 1 "s_register_operand" "%r,r")
		 (match_operand:DI 2 "unicore64_add_operand" "rI,L")))]
  "TARGET_UNICORE64"
  "@
   dadd\\t%0, %1, %2
   dsub\\t%0, %1, #%n2"
  [(set_attr "length" "4,4")]
)

(define_insn "*adddi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:DI (match_operand:DI 1 "s_register_operand" "r, r")
		  (match_operand:DI 2 "unicore64_add_operand"    "rI,L"))
	 (const_int 0)))
   (set (match_operand:DI 0 "s_register_operand" "=r,r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_UNICORE64"
  "@
   dadd.a\\t%0, %1, %2
   dsub.a\\t%0, %1, #%n2"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

(define_insn "*adddi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (plus:DI (match_operand:DI 0 "s_register_operand" "r, r")
		  (match_operand:DI 1 "unicore64_add_operand"    "rI,L"))
	 (const_int 0)))]
  "TARGET_UNICORE64"
  "@
   dcmpadd.a\\t%0, %1
   dcmpsub.a\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

(define_insn "*adddi3_compare0_for_combiner"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	 (match_operand:DI 1 "s_register_operand" "r,r")
	 (neg:DI (match_operand:DI 2 "unicore64_add_operand" "rI,L"))))
   (set (match_operand:DI 0 "s_register_operand" "=r,r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_UNICORE64"
  "@
   dadd.a\\t%0, %1, %2
   dsub.a\\t%0, %1, #%n2"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

(define_insn "*adddi3_compare0_scratch_for_combiner"
  [(set (reg:CC CC_REGNUM)
	(compare:CC
	 (match_operand:DI 0 "s_register_operand" "r,r")
	 (neg:DI (match_operand:DI 1 "unicore64_add_operand" "rI,L"))))]
  "TARGET_UNICORE64"
  "@
   dcmpadd.a\\t%0, %1
   dcmpsub.a\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

(define_insn "*adddi3_compare_op1"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	 (plus:DI (match_operand:DI 1 "s_register_operand" "r,r")
		  (match_operand:DI 2 "unicore64_add_operand" "rI,L"))
	 (match_dup 1)))
   (set (match_operand:DI 0 "s_register_operand" "=r,r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_UNICORE64"
  "@
   dadd.a\\t%0, %1, %2
   dsub.a\\t%0, %1, #%n2"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

(define_insn "*adddi3_compare_op2"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	 (plus:DI (match_operand:DI 1 "s_register_operand" "r,r")
		  (match_operand:DI 2 "unicore64_add_operand" "rI,L"))
	 (match_dup 2)))
   (set (match_operand:DI 0 "s_register_operand" "=r,r")
	(plus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_UNICORE64"
  "@
   dadd.a\\t%0, %1, %2
   dsub.a\\t%0, %1, #%n2"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

(define_insn "*compare_adddi2_op0"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	 (plus:DI (match_operand:DI 0 "s_register_operand" "r,r")
		  (match_operand:DI 1 "unicore64_add_operand" "rI,L"))
	 (match_dup 0)))]
  "TARGET_UNICORE64"
  "@
   dcmpadd.a\\t%0, %1
   dcmpsub.a\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

(define_insn "*compare_adddi2_op1"
  [(set (reg:CC_C CC_REGNUM)
	(compare:CC_C
	 (plus:DI (match_operand:DI 0 "s_register_operand" "r,r")
		  (match_operand:DI 1 "unicore64_add_operand" "rI,L"))
	 (match_dup 1)))]
  "TARGET_UNICORE64"
  "@
   dcmpadd.a\\t%0, %1
   dcmpsub.a\\t%0, #%n1"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

(define_insn "*adddi3_carryin"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(plus:DI (ltu:DI (reg:CC_C CC_REGNUM) (const_int 0))
		 (plus:DI (match_operand:DI 1 "s_register_operand" "r")
			  (match_operand:DI 2 "unicore64_rhs_operand" "rI"))))]
  "TARGET_UNICORE64"
  "daddc\\t%0, %1, %2"
  [(set_attr "conds" "use")]
)

(define_insn "*adddi3_carryin_alt1"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(plus:DI (plus:DI (match_operand:DI 1 "s_register_operand" "r")
			  (match_operand:DI 2 "unicore64_rhs_operand" "rI"))
		 (ltu:DI (reg:CC_C CC_REGNUM) (const_int 0))))]
  "TARGET_UNICORE64"
  "daddc\\t%0, %1, %2"
  [(set_attr "conds" "use")]
)

(define_insn "*adddi3_carryin_alt2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(plus:DI (plus:DI (ltu:DI (reg:CC_C CC_REGNUM) (const_int 0))
			  (match_operand:DI 1 "s_register_operand" "r"))
		 (match_operand:DI 2 "unicore64_rhs_operand" "rI")))]
  "TARGET_UNICORE64"
  "daddc\\t%0, %1, %2"
  [(set_attr "conds" "use")]
)

(define_insn "*adddi3_carryin_alt3"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(plus:DI (plus:DI (ltu:DI (reg:CC_C CC_REGNUM) (const_int 0))
			  (match_operand:DI 2 "unicore64_rhs_operand" "rI"))
		 (match_operand:DI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "daddc\\t%0, %1, %2"
  [(set_attr "conds" "use")]
)

;;<subsi3>
(define_expand "subsi3"
  [(set (match_operand:SI           0 "s_register_operand" "")
	(minus:SI (match_operand:SI 1 "unicore64_add_operand" "")
		  (match_operand:SI 2 "s_register_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      if (TARGET_UNICORE64)
        {
          unicore64_split_constant (MINUS, SImode, NULL_RTX,
	                      INTVAL (operands[1]), operands[0],
	  		      operands[2],  can_create_pseudo_p ());
          DONE;
	}
      else /* TARGET_UNICORE16 */
        FAIL;
    }
  "
)
(define_insn "*unicore64_subsi3_insn"
  [(set (match_operand:SI           0 "s_register_operand" "=r")
	(minus:SI (match_operand:SI 1 "unicore64_add_operand" "rI")
		  (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "rsub\\t%0, %2, %1"
  [(set_attr "length" "4")]
)

(define_insn "*subsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (minus:SI (match_operand:SI 1 "unicore64_rhs_operand" "r,I")
		   (match_operand:SI 2 "unicore64_rhs_operand" "rI,r"))
	 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=r,r")
	(minus:SI (match_dup 1) (match_dup 2)))]
  "TARGET_UNICORE64"
  "@
   sub.a\\t%0, %1, %2
   rsub.a\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)

(define_insn "subsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(minus:SF (match_operand:SF 1 "s_register_operand" "f")
		  (match_operand:SF 2 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fsub.s\\t%0, %1, %2"
  [(set_attr "type" "farithd")]
)

(define_insn "subdf3"
  [(set (match_operand:DF           0 "s_register_operand" "=f")
	(minus:DF (match_operand:DF 1 "s_register_operand" "f")
		  (match_operand:DF 2 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fsub.d\\t%0, %1, %2"
  [(set_attr "type" "farithd")]
)
(define_insn "*subsf_c"
  [(call (mem:DI (match_operand:DI 0 "s_register_operand" "r"))
         (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_UNICORE64"
  "*
  return output_call (operands);
  "
  ;; length is worst case, normally it is only two
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)

(define_insn "*subsf_m"
  [(call (mem:DI (match_operand:DI 0 "memory_operand" "m"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_UNICORE64"
  "*
  return output_call_mem (operands);
  "
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)


(define_insn "*subdf_r"
  [(set (match_operand 0 "" "=r,f")
        (call (mem:DI (match_operand:DI 1 "s_register_operand" "r,r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_UNICORE64"
  "*
  return output_call (&operands[1]);
  "
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)

(define_insn "*subdf_m"
  [(set (match_operand 0 "" "=r,f")
	(call (mem:DI (match_operand:DI 1 "memory_operand" "m,m"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_UNICORE64 && (!CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))"
  "*
  return output_call_mem (&operands[1]);
  "
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)

(define_insn "*subsf_s"
  [(call (mem:DI (match_operand:DI 0 "" "X"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_UNICORE64
   && (GET_CODE (operands[0]) == SYMBOL_REF)
   && !unicore64_is_longcall_p (operands[0], INTVAL (operands[2]), 1)"
  "*
  {
    return NEED_PLT_RELOC ? \"call\\t%a0(PLT)\" : \"call\\t%a0\";
  }"
  [(set_attr "type" "call")]
)
(define_insn "*subdf_s"
  [(set (match_operand 0 "s_register_operand" "=r,f")
	(call (mem:DI (match_operand:DI 1 "" "X,X"))
	      (match_operand:DI 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_UNICORE64
   && (GET_CODE (operands[1]) == SYMBOL_REF)
   && !unicore64_is_longcall_p (operands[1], INTVAL (operands[3]), 1)"
  "*
  {
    return NEED_PLT_RELOC ? \"call\\t%a1(PLT)\" : \"call\\t%a1\";
  }"
  [(set_attr "type" "call")]
)

;<subdi>
(define_expand "subdi3"
  [(set (match_operand:DI           0 "s_register_operand" "")
	(minus:DI (match_operand:DI 1 "unicore64_add_operand" "")
		  (match_operand:DI 2 "s_register_operand" "")))]
  "TARGET_UNICORE64"
  "
  if (GET_CODE (operands[1]) == CONST_INT)
    {
      unicore64_split_constant (MINUS, DImode, NULL_RTX,
				INTVAL (operands[1]), operands[0],
				operands[2],  can_create_pseudo_p ());
      DONE;
    }
  "
)

(define_insn "*unicore64_subdi3_insn"
  [(set (match_operand:DI           0 "s_register_operand" "=r")
	(minus:DI (match_operand:DI 1 "unicore64_add_operand" "rI")
		  (match_operand:DI 2 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "drsub\\t%0, %2, %1"
  [(set_attr "length" "4")]
)

(define_insn "*subdi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV
	 (minus:DI (match_operand:DI 1 "unicore64_rhs_operand" "r,I")
		   (match_operand:DI 2 "unicore64_rhs_operand" "rI,r"))
	 (const_int 0)))
   (set (match_operand:DI 0 "s_register_operand" "=r,r")
	(minus:DI (match_dup 1) (match_dup 2)))]
  "TARGET_UNICORE64"
  "@
   dsub.a\\t%0, %1, %2
   drsub.a\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "modify_asr")]
)
;<using in movhi> 
(define_expand "anddi3"
  [(set (match_operand:DI         0 "s_register_operand" "")
	(and:DI (match_operand:DI 1 "s_register_operand" "")
		(match_operand:DI 2 "unicore64_and_operand" "")))]
  "TARGET_EITHER"
  "
  if (TARGET_UNICORE64)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
        {
          unicore64_split_constant (AND, DImode, NULL_RTX,
	                      INTVAL (operands[2]), operands[0],
			      operands[1], can_create_pseudo_p ());
          DONE;
        }
    }
  else /* TARGET_UNICORE16 */
    {
      FAIL;
    }
  "
)

(define_insn "*unicore64_anddi3_insn"
  [(set (match_operand:DI         0 "s_register_operand" "=r,r")
	(and:DI (match_operand:DI 1 "s_register_operand" "r,r")
		(match_operand:DI 2 "unicore64_and_operand" "rI,K")))]
  "TARGET_UNICORE64"
  "@
   dand\\t%0, %1, %2
   dandn\\t%0, %1, #%B2"
  [(set_attr "length" "4,4")]
)

; constants for op 2 will never be given to these patterns.
(define_insn "anddi_notdi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
	(and:DI (not:DI (match_operand:DI 1 "s_register_operand" "r"))
		(match_operand:DI 2 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "dandn\\t%0, %2, %1"
  [(set_attr "length" "4")]
)
(define_insn "*anddi_notzesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
	(and:DI (not:DI (zero_extend:DI
			 (match_operand:SI 2 "s_register_operand" "r")))
		(match_operand:DI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "dlsl\\t%2, %2, #32\;dlsr\\t%2, %2, #32\;\tdandn\\t%0, %1, %2"
  [(set_attr "length" "8")]
)
(define_insn "*anddi_notsesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
	(and:DI (not:DI (sign_extend:DI
			 (match_operand:SI 2 "s_register_operand" "r")))
		(match_operand:DI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "dlsl\\t%2, %2, #32;\n\tdasr\\t%2, %2, #32;\tdandn\\t%0, %1, %2"
  [(set_attr "length" "12")]
)
(define_insn "unicore64_iordi3"
  [(set (match_operand:DI         0 "s_register_operand" "=&r")
	(ior:DI (match_operand:DI 1 "s_register_operand"  "%r")
		(match_operand:DI 2 "s_register_operand"   "r")))]
  "TARGET_UNICORE64"
  "dor\\t%0, %1, %2"
  [(set_attr "length" "4")]
)
(define_insn "*iordi_zesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
	(ior:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r"))
		(match_operand:DI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "dlsl\\t%2, %2, #32\;dlsr\\t%2, %2, #32\;\tdor\\t%0, %1, %2"
  [(set_attr "length" "8")]
)
(define_insn "*iordi_sesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(ior:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r,r"))
		(match_operand:DI 1 "s_register_operand" "?r,0")))]
  "TARGET_UNICORE64"
  "dlsl\\t%2, %2, #32;\n\tdasr\\t%2, %2, #32;\n\tdor\\t%0, %1, %2"
  [(set_attr "length" "12")]
)

(define_expand "iordi3"
  [(set (match_operand:DI         0 "s_register_operand" "")
	(ior:DI (match_operand:DI 1 "s_register_operand" "")
		(match_operand:DI 2 "unicore64_add_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (TARGET_UNICORE64)
        {
          unicore64_split_constant (IOR, DImode, NULL_RTX,
	                      INTVAL (operands[2]), operands[0], operands[1],
			       can_create_pseudo_p ());
          DONE;
	}
      else /* TARGET_UNICORE16 */
        FAIL;
    }
  "
)
(define_insn "*unicore64_iordi3"
  [(set (match_operand:DI         0 "s_register_operand" "=r")
	(ior:DI (match_operand:DI 1 "s_register_operand" "r")
		(match_operand:DI 2 "unicore64_add_operand" "rI")))]
  "TARGET_UNICORE64"
  "dor\\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)
(define_insn "xordi3"
  [(set (match_operand:DI         0 "s_register_operand" "=&r")
	(xor:DI (match_operand:DI 1 "s_register_operand"  "%r")
		(match_operand:DI 2 "s_register_operand"   "r")))]
  "TARGET_UNICORE64"
  "dxor\\t%0, %1, %2"
  [(set_attr "length" "4")]
)
(define_insn "*xordi_zesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
	(xor:DI (zero_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r"))
		(match_operand:DI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "dlsl\\t%2, %2, #32\;dlsr\\t%2, %2, #32\;\tdxor\\t%0, %1, %2"
  [(set_attr "length" "8")]
)
(define_insn "*xordi_sesidi_di"
  [(set (match_operand:DI 0 "s_register_operand" "=&r")
	(xor:DI (sign_extend:DI
		 (match_operand:SI 2 "s_register_operand" "r"))
		(match_operand:DI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "dlsl\\t%2, %2, #32;\n\tdasr\\t%2, %2, #32;\n\tdxor\\t%0, %1, %2"
  [(set_attr "length" "12")]
)

(define_insn "*unicore64_xordi3"
  [(set (match_operand:DI         0 "s_register_operand" "=r")
	(xor:DI (match_operand:DI 1 "s_register_operand" "r")
		(match_operand:DI 2 "unicore64_rhs_operand" "rI")))]
  "TARGET_UNICORE64"
  "dxor\\t%0, %1, %2"
)
(define_expand "ashldi3"
  [(set (match_operand:DI            0 "s_register_operand" "")
	(ashift:DI (match_operand:DI 1 "s_register_operand" "")
		   (match_operand:DI 2 "unicore64_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 63)
    {
      emit_insn (gen_movdi (operands[0], const0_rtx));
      DONE;
    }
  "
)

(define_expand "ashrdi3"
  [(set (match_operand:DI              0 "s_register_operand" "")
	(ashiftrt:DI (match_operand:DI 1 "s_register_operand" "")
		     (match_operand:DI 2 "unicore64_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 63)
    operands[2] = GEN_INT (63);
  "
)


(define_expand "lshrdi3"
  [(set (match_operand:DI              0 "s_register_operand" "")
	(lshiftrt:DI (match_operand:DI 1 "s_register_operand" "")
		     (match_operand:DI 2 "unicore64_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 63)
    {
      emit_insn (gen_movdi (operands[0], const0_rtx));
      DONE;
    }
  "
)

(define_insn "*unicore64_shiftdi3"
  [(set (match_operand:DI   0 "s_register_operand" "=r")
	(match_operator:DI  3 "shift_operator"
	 [(match_operand:DI 1 "s_register_operand"  "r")
	  (match_operand:DI 2 "reg_or_int_operand" "rM")]))]
  "TARGET_UNICORE64"
  "*
  {
    switch (GET_CODE (operands[3]))
      {
	case ASHIFT:
	  return \"dlsl\\t%0, %1, %2\";

	case ASHIFTRT:
	  return \"dasr\\t%0, %1, %2\";

	case LSHIFTRT:
	  return \"dlsr\\t%0, %1, %2\";
      }
  }
  "
  [(set_attr "shift" "1")]
)
;; Unary arithmetic insns

(define_expand "negdi2"
 [(parallel
   [(set (match_operand:DI          0 "s_register_operand" "")
	  (neg:DI (match_operand:DI 1 "s_register_operand" "")))
    (clobber (reg:CC CC_REGNUM))])]
  "TARGET_EITHER"
  "
  "
)

(define_insn "*unicore64_negdi2"
  [(set (match_operand:DI         0 "s_register_operand" "=r")
	(neg:DI (match_operand:DI 1 "s_register_operand" "r")))
    (clobber (reg:CC CC_REGNUM))]
  "TARGET_UNICORE64"
  "drsub\\t%0, %1, #0"
)

(define_insn "one_cmpldi2"
  [(set (match_operand:DI         0 "s_register_operand" "=r")
	(not:DI (match_operand:DI 1 "s_register_operand"  "r")))]
  "TARGET_UNICORE64"
  "dnot\\t%0, %1"
)


;; Truncation insns

(define_insn "truncdfsf2"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(float_truncate:SF
	 (match_operand:DF 1 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fcvt.s.d\\t%0, %1"
  [(set_attr "type" "fariths")]
)

(define_insn "zero_extendsidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (zero_extend:DI (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "dlsl\\t%0, %1, #32;\n\tdlsr\\t%0, %0, #32"
  [(set_attr "length" "4")]
)
(define_insn "extendsidi2"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
        (sign_extend:DI (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "dlsl\\t%0, %1, #32;\n\tdasr\\t%0, %0, #32"
  [(set_attr "length" "8")
   (set_attr "shift" "1")
  ]
)
(define_expand "lshrsi3"
  [(set (match_operand:SI              0 "s_register_operand" "")
	(lshiftrt:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "unicore64_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    {
      emit_insn (gen_movsi (operands[0], const0_rtx));
      DONE;
    }
  "
)

(define_insn "*unicore64_shiftsi3"
  [(set (match_operand:SI   0 "s_register_operand" "=r")
	(match_operator:SI  3 "shift_operator"
	 [(match_operand:SI 1 "s_register_operand"  "r")
	  (match_operand:SI 2 "reg_or_int_operand" "rM")]))]
  "TARGET_UNICORE64"
  "*
  {
    switch (GET_CODE (operands[3]))
      {
	case ASHIFT:
	  return \"lsl\\t%0, %1, %2\";

	case ASHIFTRT:
	  return \"asr\\t%0, %1, %2\";

	case LSHIFTRT:
	  return \"lsr\\t%0, %1, %2\";
      }
  }
  "
  [(set_attr "shift" "1")]
)
;;--------------------------end level 1---------------------------

;;--------------------------level 2--------------------------------
;;<div>
(define_expand "divsi3"
  [(set (match_operand:SI          0 "s_register_operand" "")
	(div:SI (match_operand:SI 1 "s_register_operand" "")
		 (match_operand:SI 2 "s_register_operand" "")))]
  "TARGET_EITHER"
"   
"
)

;; Use `&' and then `0' to prevent the operands 0 and 1 being the same
(define_insn "*unicore64_divsi3"
  [(set (match_operand:SI          0 "s_register_operand" "=&r")
	(div:SI (match_operand:SI 1 "s_register_operand" "r")
		 (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "divs\\t%0, %1, %2"
)

(define_expand "udivsi3"
  [(set (match_operand:SI          0 "s_register_operand" "")
	(udiv:SI (match_operand:SI 1 "s_register_operand" "")
		 (match_operand:SI 2 "s_register_operand" "")))]
  "TARGET_EITHER"
"   
"
)

;; Use `&' and then `0' to prevent the operands 0 and 1 being the same
(define_insn "*unicore64_udivsi3"
  [(set (match_operand:SI          0 "s_register_operand" "=&r")
	(udiv:SI (match_operand:SI 1 "s_register_operand" "r")
		 (match_operand:SI 2 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "divu\\t%0, %1, %2"
)

;;<modsi3>
(define_expand "modsi3"
  [(set (match_operand:SI          0 "s_register_operand" "")
	(mod:SI (match_operand:SI 1 "s_register_operand" "")
		 (match_operand:SI 2 "s_register_operand" "")))]
  "TARGET_EITHER"
"   
  {
    rtx op0 = gen_reg_rtx (SImode);

    emit_insn (gen_divsi3 (op0, operands[1], operands[2]));
    emit_insn (gen_mulsi3 (op0, op0, operands[2]));
    emit_insn (gen_subsi3 (op0, operands[1], op0));
    emit_insn (gen_movsi (operands[0], op0));

    DONE;
  }
"
)

(define_expand "umodsi3"
  [(set (match_operand:SI          0 "s_register_operand" "")
	(mod:SI (match_operand:SI 1 "s_register_operand" "")
		 (match_operand:SI 2 "s_register_operand" "")))]
  "TARGET_EITHER"
"   
  {
    rtx op0 = gen_reg_rtx (SImode);

    emit_insn (gen_udivsi3 (op0, operands[1], operands[2]));
    emit_insn (gen_mulsi3 (op0, op0, operands[2]));
    emit_insn (gen_subsi3 (op0, operands[1], op0));
    emit_insn (gen_movsi (operands[0], op0));

    DONE;
  }
"
)

(define_insn "divsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(div:SF (match_operand:SF 1 "s_register_operand" "f")
		 (match_operand:SF 2 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fdiv.s\\t%0, %1, %2"
  [(set_attr "type" "fdivs")]
)

(define_insn "divdf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(div:DF (match_operand:DF 1 "s_register_operand" "f")
		 (match_operand:DF 2 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fdiv.d\\t%0, %1, %2"
  [(set_attr "type" "fdivd")]
)
(define_expand "divdi3"
  [(set (match_operand:DI          0 "s_register_operand" "")
	(div:DI (match_operand:DI 1 "s_register_operand" "")
		 (match_operand:DI 2 "s_register_operand" "")))]
  "TARGET_UNICORE64"
"   
"
)

;; Use `&' and then `0' to prevent the operands 0 and 1 being the same
(define_insn "*unicore64_divdi3"
  [(set (match_operand:DI          0 "s_register_operand" "=&r")
	(div:DI (match_operand:DI 1 "s_register_operand" "r")
		 (match_operand:DI 2 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "ddivs\\t%0, %1, %2"
)

(define_expand "udivdi3"
  [(set (match_operand:DI          0 "s_register_operand" "")
	(udiv:DI (match_operand:DI 1 "s_register_operand" "")
		 (match_operand:DI 2 "s_register_operand" "")))]
  "TARGET_UNICORE64"
"   
"
)

;; Use `&' and then `0' to prevent the operands 0 and 1 being the same
(define_insn "*unicore64_udivdi3"
  [(set (match_operand:DI          0 "s_register_operand" "=&r")
	(udiv:DI (match_operand:DI 1 "s_register_operand" "r")
		 (match_operand:DI 2 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "ddivu\\t%0, %1, %2"
)

(define_expand "moddi3"
  [(set (match_operand:DI          0 "s_register_operand" "")
	(mod:DI (match_operand:DI 1 "s_register_operand" "")
		 (match_operand:DI 2 "s_register_operand" "")))]
  "TARGET_UNICORE64"
  "   
    {
      rtx op0 = gen_reg_rtx (DImode);

      emit_insn (gen_divdi3 (op0, operands[1], operands[2]));
      emit_insn (gen_muldi3 (op0, op0, operands[2]));
      emit_insn (gen_subdi3 (op0, operands[1], op0));
      emit_insn (gen_movdi (operands[0], op0));

      DONE;
    }
  "
)

(define_expand "umoddi3"
  [(set (match_operand:DI          0 "s_register_operand" "")
	(mod:DI (match_operand:DI 1 "s_register_operand" "")
		 (match_operand:DI 2 "s_register_operand" "")))]
  "TARGET_UNICORE64"
"   
  {
    rtx op0 = gen_reg_rtx (DImode);

    emit_insn (gen_udivdi3 (op0, operands[1], operands[2]));
    emit_insn (gen_muldi3 (op0, op0, operands[2]));
    emit_insn (gen_subdi3 (op0, operands[1], op0));
    emit_insn (gen_movdi (operands[0], op0));

    DONE;
  }
"
)
;;<mul>
(define_expand "mulsi3"
  [(set (match_operand:SI          0 "s_register_operand" "")
	(mult:SI (match_operand:SI 2 "s_register_operand" "")
		 (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_EITHER"
"   
"
)

;; Use `&' and then `0' to prevent the operands 0 and 1 being the same
(define_insn "*unicore64_mulsi3"
  [(set (match_operand:SI          0 "s_register_operand" "=&r,&r")
	(mult:SI (match_operand:SI 2 "s_register_operand" "r,r")
		 (match_operand:SI 1 "s_register_operand" "%?r,0")))]
  "TARGET_UNICORE64"
  "mul\\t%0, %2, %1"
  [(set_attr "type" "mult")]
)

(define_insn "*mulsi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 1 "s_register_operand" "%?r,0"))
			 (const_int 0)))
   (set (match_operand:SI 0 "s_register_operand" "=&r,&r")
	(mult:SI (match_dup 2) (match_dup 1)))]
  "TARGET_UNICORE64"
  "mul.a\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "mult")
   (set_attr "type" "modify_asr")]
)
(define_insn "*mulsi3_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (mult:SI
			  (match_operand:SI 2 "s_register_operand" "r,r")
			  (match_operand:SI 1 "s_register_operand" "%?r,0"))
			 (const_int 0)))
   (clobber (match_scratch:SI 0 "=&r,&r"))]
  "TARGET_UNICORE64"
  "mul.a\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "mult")
   (set_attr "type" "modify_asr")]
)

(define_insn "mulsf3"
  [(set (match_operand:SF 0 "s_register_operand" "=f")
	(mult:SF (match_operand:SF 1 "s_register_operand" "f")
		 (match_operand:SF 2 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fmul.s\\t%0, %1, %2"
  [(set_attr "type" "fmuls")]
)

(define_insn "muldf3"
  [(set (match_operand:DF 0 "s_register_operand" "=f")
	(mult:DF (match_operand:DF 1 "s_register_operand" "f")
		 (match_operand:DF 2 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fmul.d\\t%0, %1, %2"
  [(set_attr "type" "fmuld")]
)

(define_expand "muldi3"
  [(set (match_operand:DI          0 "s_register_operand" "")
	(mult:DI (match_operand:DI 2 "s_register_operand" "")
		 (match_operand:DI 1 "s_register_operand" "")))]
  "TARGET_UNICORE64"
"   
"
)

(define_insn "*unicore64_muldi3"
  [(set (match_operand:DI          0 "s_register_operand" "=&r,&r")
	(mult:DI (match_operand:DI 2 "s_register_operand" "r,r")
		 (match_operand:DI 1 "s_register_operand" "%?r,0")))]
  "TARGET_UNICORE64"
  "dmul\\t%0, %2, %1"
  [(set_attr "type" "mult")]
)

(define_insn "*muldi3_compare0"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (mult:DI
			  (match_operand:DI 2 "s_register_operand" "r,r")
			  (match_operand:DI 1 "s_register_operand" "%?r,0"))
			 (const_int 0)))
   (set (match_operand:DI 0 "s_register_operand" "=&r,&r")
	(mult:DI (match_dup 2) (match_dup 1)))]
  "TARGET_UNICORE64"
  "dmul.a\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "mult")
   (set_attr "type" "modify_asr")]
)
(define_insn "*muldi_compare0_scratch"
  [(set (reg:CC_NOOV CC_REGNUM)
	(compare:CC_NOOV (mult:DI
			  (match_operand:DI 2 "s_register_operand" "r,r")
			  (match_operand:DI 1 "s_register_operand" "%?r,0"))
			 (const_int 0)))
   (clobber (match_scratch:DI 0 "=&r,&r"))]
  "TARGET_UNICORE64"
  "dmul.a\\t%0, %2, %1"
  [(set_attr "conds" "set")
   (set_attr "type" "mult")
   (set_attr "type" "modify_asr")]
)


;; Boolean and,ior,xor insns
;;<and>
(define_expand "andsi3"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(and:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "unicore64_and_operand" "")))]
  "TARGET_EITHER"
  "
  if (TARGET_UNICORE64)
    {
      if (GET_CODE (operands[2]) == CONST_INT)
        {
          unicore64_split_constant (AND, SImode, NULL_RTX,
	                      INTVAL (operands[2]), operands[0],
			      operands[1], can_create_pseudo_p ());
          DONE;
        }
    }
  else /* TARGET_UNICORE16 */
    {
      FAIL;
    }
  "
)
(define_insn "*unicore64_andsi3_insn"
  [(set (match_operand:SI         0 "s_register_operand" "=r,r")
	(and:SI (match_operand:SI 1 "s_register_operand" "r,r")
		(match_operand:SI 2 "unicore64_and_operand" "rI,K")))]
  "TARGET_UNICORE64"
  "@
   and\\t%0, %1, %2
   andn\\t%0, %1, #%B2"
  [(set_attr "length" "4,4")]
)

(define_insn "*ne_zeroextractsi"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(ne:SI (zero_extract:SI
		(match_operand:SI 1 "s_register_operand" "r")
		(match_operand:SI 2 "const_int_operand" "n")
		(match_operand:SI 3 "const_int_operand" "n"))
	       (const_int 0)))
	(clobber (reg:CC CC_REGNUM))]
  "TARGET_UNICORE64
   && (INTVAL (operands[3]) >= 0 && INTVAL (operands[3]) < 32
       && INTVAL (operands[2]) > 0 
       && INTVAL (operands[2]) + (INTVAL (operands[3]) & 1) <= 8
       && INTVAL (operands[2]) + INTVAL (operands[3]) <= 11)"
  "*
  operands[2] = GEN_INT (((1 << INTVAL (operands[2])) - 1)
			 << INTVAL (operands[3]));
  output_asm_insn (\"and\t%0, %1, %2\", operands);
  output_asm_insn (\"cmpsub.a\t%0, #0\", operands);
  output_asm_insn (\"beq\\t.+8\", operands);
  return \"mov\\t%0, #1\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "12")
   (set_attr "type" "modify_asr")]
)

;;; ??? This pattern is bogus.  If operand3 has bits outside the range
;;; represented by the bitfield, then this will produce incorrect results.
;;; Somewhere, the value needs to be truncated.  On targets like the m68k,
;;; which have a real bitfield insert instruction, the truncation happens
;;; in the bitfield insert instruction itself.  Since unicore64 does not have a
;;; bitfield insert instruction, we would have to emit code here to truncate
;;; the value before we insert.  This loses some of the advantage of having
;;; this insv pattern, so this pattern needs to be reevalutated.

(define_expand "insv"
  [(set (zero_extract:SI (match_operand:SI 0 "s_register_operand" "")
                         (match_operand:SI 1 "general_operand" "")
                         (match_operand:SI 2 "general_operand" ""))
        (match_operand:SI 3 "nonmemory_operand" ""))]
  "TARGET_UNICORE64"
  "
  {
    int start_bit = INTVAL (operands[2]);
    int width = INTVAL (operands[1]);
    rtx target, subtarget;
    HOST_WIDE_INT mask = (((HOST_WIDE_INT)1) << width) - 1;

    target = operands[0];
    /* Avoid using a subreg as a subtarget, and avoid writing a paradoxical 
       subreg as the final target.  */
    if (GET_CODE (target) == SUBREG)
      {
	subtarget = gen_reg_rtx (SImode);
	if (GET_MODE_SIZE (GET_MODE (SUBREG_REG (target)))
	    < GET_MODE_SIZE (SImode))
	  target = SUBREG_REG (target);
      }
    else
      subtarget = target;    

    if (GET_CODE (operands[3]) == CONST_INT)
      {
	/* Since we are inserting a known constant, we may be able to
	   reduce the number of bits that we have to clear so that
	   the mask becomes simple.  */
	/* ??? This code does not check to see if the new mask is actually
	   simpler.  It may not be.  */
	rtx op1 = gen_reg_rtx (SImode);
	/* ??? Truncate operand3 to fit in the bitfield.  See comment before
	   start of this pattern.  */
	HOST_WIDE_INT op3_value = mask & INTVAL (operands[3]);
	HOST_WIDE_INT mask2 = ((mask & ~op3_value) << start_bit);

	emit_insn (gen_andsi3 (op1, operands[0], GEN_INT ((int)~mask2)));
	emit_insn (gen_iorsi3 (subtarget, op1,
			       GEN_INT (op3_value << start_bit)));
      }
    else if (start_bit == 0
	     && !(const_ok_for_unicore64 (mask)
		  || const_ok_for_unicore64 (~mask)))
      {
	/* A Trick, since we are setting the bottom bits in the word,
	   we can shift operand[3] up, operand[0] down, OR them together
	   and rotate the result back again.  This takes 3 insns, and
	   the third might be mergable into another op.  */
	/* The shift up copes with the possibility that operand[3] is
           wider than the bitfield.  */
	rtx op0 = gen_reg_rtx (SImode);
	rtx op1 = gen_reg_rtx (SImode);
	emit_insn (gen_ashlsi3 (op0, operands[3], GEN_INT (32 - width)));
	emit_insn (gen_lshrsi3 (op0, op0, GEN_INT (32 - width))); 
	emit_insn (gen_lshrsi3 (op1, operands[0], operands[1]));
	emit_insn (gen_ashlsi3 (op1, op1, operands[1])); 
	emit_insn (gen_iorsi3  (subtarget, op1, op0));
      }
    else if ((width + start_bit == 32)
	     && !(const_ok_for_unicore64 (mask)
		  || const_ok_for_unicore64 (~mask)))
      {
	/* Similar trick, but slightly less efficient.  */

	rtx op0 = gen_reg_rtx (SImode);
	rtx op1 = gen_reg_rtx (SImode);

	emit_insn (gen_ashlsi3 (op0, operands[3], GEN_INT (32 - width)));
	emit_insn (gen_ashlsi3 (op1, operands[0], operands[1]));
	emit_insn (gen_lshrsi3 (op1, op1, operands[1]));
	emit_insn (gen_iorsi3 (subtarget, op1, op0));
      }
    else
      {
	rtx op0 = GEN_INT (mask);
	rtx op1 = gen_reg_rtx (SImode);
	rtx op2 = gen_reg_rtx (SImode);

	if (!(const_ok_for_unicore64 (mask) || const_ok_for_unicore64 (~mask)))
	  {
	    rtx tmp = gen_reg_rtx (SImode);

	    emit_insn (gen_movsi (tmp, op0));
	    op0 = tmp;
	  }

	/* Mask out any bits in operand[3] that are not needed.  */
	   emit_insn (gen_andsi3 (op1, operands[3], op0));

	if (GET_CODE (op0) == CONST_INT
	    && (const_ok_for_unicore64 (mask << start_bit)
		|| const_ok_for_unicore64 (~(mask << start_bit))))
	  {
	    op0 = GEN_INT ((int)~(mask << start_bit));
	    emit_insn (gen_andsi3 (op2, operands[0], op0));
	  }
	else
	  {
	    if (GET_CODE (op0) == CONST_INT)
	      {
		rtx tmp = gen_reg_rtx (SImode);

		emit_insn (gen_movsi (tmp, op0));
		op0 = tmp;
	      }

	    if (start_bit != 0)
	      emit_insn (gen_ashlsi3 (op0, op0, operands[2]));
	    
	    emit_insn (gen_andsi_notsi_si (op2, operands[0], op0));
	  }

	if (start_bit != 0)
          emit_insn (gen_ashlsi3 (op1, op1, operands[2]));

	emit_insn (gen_iorsi3 (subtarget, op1, op2));
      }

    if (subtarget != target)
      {
	/* If TARGET is still a SUBREG, then it must be wider than a word,
	   so we must be careful only to set the subword we were asked to.  */
	if (GET_CODE (target) == SUBREG)
	  emit_move_insn (target, subtarget);
	else
	  emit_move_insn (target, gen_lowpart (GET_MODE (target), subtarget));
      }

    DONE;
  }"
)


(define_insn "andsi_notsi_si"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(and:SI (not:SI (match_operand:SI 2 "s_register_operand" "r"))
		(match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "andn\\t%0, %1, %2"
)

;;<ior>
(define_expand "iorsi3"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(ior:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "unicore64_add_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT)
    {
      if (TARGET_UNICORE64)
        {
          unicore64_split_constant (IOR, SImode, NULL_RTX,
	                      INTVAL (operands[2]), operands[0], operands[1],
			       can_create_pseudo_p ());
          DONE;
	}
      else /* TARGET_UNICORE16 */
        FAIL;
    }
  "
)
(define_insn "*unicore64_iorsi3"
  [(set (match_operand:SI         0 "s_register_operand" "=r")
	(ior:SI (match_operand:SI 1 "s_register_operand" "r")
		(match_operand:SI 2 "unicore64_add_operand" "rI")))]
  "TARGET_UNICORE64"
  "or\\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "predicable" "yes")]
)

;;<xor>
(define_expand "xorsi3"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(xor:SI (match_operand:SI 1 "s_register_operand" "")
		(match_operand:SI 2 "unicore64_rhs_operand"  "")))]
  "TARGET_EITHER"
  "if (TARGET_UNICORE16)
     if (GET_CODE (operands[2]) == CONST_INT)
       operands[2] = force_reg (SImode, operands[2]);
  "
)
(define_insn "*unicore64_xorsi3"
  [(set (match_operand:SI         0 "s_register_operand" "=r")
	(xor:SI (match_operand:SI 1 "s_register_operand" "r")
		(match_operand:SI 2 "unicore64_rhs_operand" "rI")))]
  "TARGET_UNICORE64"
  "xor\\t%0, %1, %2"
)

(define_insn "*andsi_iorsi3_notsi"
  [(set (match_operand:SI 0 "s_register_operand" "=&r,&r,&r")
	(and:SI (ior:SI (match_operand:SI 1 "s_register_operand" "r,r,0")
			(match_operand:SI 2 "unicore64_rhs_operand" "rI,0,rI"))
		(not:SI (match_operand:SI 3 "unicore64_rhs_operand" "rI,rI,rI"))))]
  "TARGET_UNICORE64"
  "or\\t%0, %1, %2\;\n\tandn\\t%0, %0, %3"
  [(set_attr "length" "8")]
)

;; Shift and rotation insns

(define_expand "ashlsi3"
  [(set (match_operand:SI            0 "s_register_operand" "")
	(ashift:SI (match_operand:SI 1 "s_register_operand" "")
		   (match_operand:SI 2 "unicore64_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    {
      emit_insn (gen_movsi (operands[0], const0_rtx));
      DONE;
    }
  "
)

(define_expand "ashrsi3"
  [(set (match_operand:SI              0 "s_register_operand" "")
	(ashiftrt:SI (match_operand:SI 1 "s_register_operand" "")
		     (match_operand:SI 2 "unicore64_rhs_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[2]) == CONST_INT
      && ((unsigned HOST_WIDE_INT) INTVAL (operands[2])) > 31)
    operands[2] = GEN_INT (31);
  "
)


(define_expand "negsi2"
  [(set (match_operand:SI         0 "s_register_operand" "")
	(neg:SI (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_EITHER"
  ""
)

(define_insn "*unicore64_negsi2"
  [(set (match_operand:SI         0 "s_register_operand" "=r")
	(neg:SI (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "rsub\\t%0, %1, #0"
)

(define_insn "negsf2"
  [(set (match_operand:SF         0 "s_register_operand" "=f")
	(neg:SF (match_operand:SF 1 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fneg.s\\t%0, %1"
  [(set_attr "type" "fariths")]
)

(define_insn "negdf2"
  [(set (match_operand:DF         0 "s_register_operand" "=f")
	(neg:DF (match_operand:DF 1 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fneg.d\\t%0, %1"
  [(set_attr "type" "fariths")]
)

(define_insn "abssf2"
  [(set (match_operand:SF          0 "s_register_operand" "=f")
	 (abs:SF (match_operand:SF 1 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fabs.s\\t%0, %1"
  [(set_attr "type" "fariths")]
)

(define_insn "absdf2"
  [(set (match_operand:DF         0 "s_register_operand" "=f")
	(abs:DF (match_operand:DF 1 "s_register_operand" "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fabs.d\\t%0, %1"
  [(set_attr "type" "fariths")]
)

(define_insn "*unicore64_one_cmplsi2"
  [(set (match_operand:SI         0 "s_register_operand" "=r")
	(not:SI (match_operand:SI 1 "s_register_operand"  "r")))]
  "TARGET_UNICORE64"
  "not\\t%0, %1"
)

;; Floating conversion insns
(define_insn "floatsisf2"
  [(set (match_operand:SF           0 "s_register_operand" "=f")
	(float:SF (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "dlsl\\t%1, %1, #32\;dlsr\\t%1, %1, #32\;mtf\\t%1, %0\;fcvt.s.w\\t%0, %0"
  [(set_attr "type" "df_r_2_f")
   (set_attr "length" "8")]
)

(define_insn "floatsidf2"
  [(set (match_operand:DF           0 "s_register_operand" "=f")
	(float:DF (match_operand:SI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "dlsl\\t%1, %1, #32\;dlsr\\t%1, %1, #32\;mtf\\t%1, %0\;fcvt.d.w\\t%0, %0"
  [(set_attr "type" "df_r_2_f")
   (set_attr "length" "8")]
)

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI         0 "s_register_operand" "=r")
	(fix:SI (match_operand:SF 1 "s_register_operand" "f")))
   (clobber(match_scratch:SF 2 "=f"))
   (clobber(match_scratch:SI 3 "=r"))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "cff\\t%0\;mov\\t%3, %0\;andn\\t%0, %0, #0x7\;or\\t%0, %0, #0x1\;ctf\\t%0\;fcvt.w.s\\t%2, %1\;cff\\t%0\;andn\\t%0, %0, #0x7\;and\\t%3, %3, #0x7\;or\\t%0, %0, %3\;ctf\\t%0\;mff\\t%0, %2;dlsl\\t%0, %0, #32\;dasr\\t%0, %0, #32"
  [(set_attr "type" "df_f_2_r")
   (set_attr "length" "48")]
)

;;;;; Zero and sign extension instructions.
(define_expand "zero_extendhisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:HI 1 "nonimmediate_operand" "")
		   (const_int 16)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(lshiftrt:SI (match_dup 2) (const_int 16)))]
  "TARGET_EITHER"
  "
  {
    if (TARGET_UNICORE64)
      {
        if (unicore64_arch1 && GET_CODE (operands[1]) == MEM)
          {
           /* Note: We do not have to worry about TARGET_MMU_TRAPS
	      here because the insn below will generate an LDRH instruction
	      rather than an LDR instruction, so we cannot get an unaligned
	      word access.  */
            emit_insn (gen_rtx_SET (VOIDmode, operands[0],
			            gen_rtx_ZERO_EXTEND (SImode,
							 operands[1])));
            DONE;
          }
        if (TARGET_MMU_TRAPS && GET_CODE (operands[1]) == MEM)
          {
            emit_insn (gen_movhi_bytes (operands[0], operands[1]));
            DONE;
          }
        if (!s_register_operand (operands[1], HImode))
          operands[1] = copy_to_mode_reg (HImode, operands[1]);
        operands[1] = gen_lowpart (SImode, operands[1]);
        operands[2] = gen_reg_rtx (SImode);
      }
    else /* TARGET_UNICORE16 */
      FAIL;
  }"
)

(define_insn "*unicore64_zero_extendhisi2"
  [(set (match_operand:SI                 0 "s_register_operand" "=r")
	(zero_extend:SI (match_operand:HI 1 "memory_operand"      "m")))]
  "TARGET_UNICORE64 && unicore64_arch1"
  "ldh%T1\\t%0, %1"
  [(set_attr "type" "load")
   (set_attr "pool_range" "1448")
   (set_attr "neg_pool_range" "1448")]
)

(define_expand "zero_extendqisi2"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "")))]
  "TARGET_EITHER"
  "
  if (GET_CODE (operands[1]) != MEM)
    {
      if (TARGET_UNICORE64)
        {
          emit_insn (gen_andsi3 (operands[0],
				 gen_lowpart (SImode, operands[1]),
			         GEN_INT (255)));
        }
      else /* TARGET_UNICORE16 */
	FAIL;
      DONE;
    }
  "
)

(define_insn "*unicore64_zero_extendqisi2"
  [(set (match_operand:SI                 0 "s_register_operand" "=r")
	(zero_extend:SI (match_operand:QI 1 "memory_operand"      "m")))]
  "TARGET_UNICORE64"
  "ldb%T1\\t%0, %1\\t%@ zero_extendqisi2"
  [(set_attr "type" "load")
   (set_attr "pool_range" "1448")
   (set_attr "neg_pool_range" "1448")]
)


(define_expand "extendhisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:HI 1 "nonimmediate_operand" "")
		   (const_int 16)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 16)))]
  "TARGET_EITHER"
  "
  {
    if (TARGET_UNICORE64 && unicore64_arch1 && GET_CODE (operands[1]) == MEM)
      {
       /* Note: We do not have to worry about TARGET_MMU_TRAPS
	  here because the insn below will generate an LDRH instruction
	  rather than an LDR instruction, so we cannot get an unaligned
	  word access.  */
        emit_insn (gen_rtx_SET (VOIDmode, operands[0],
		   gen_rtx_SIGN_EXTEND (SImode, operands[1])));
        DONE;
      }

    if (TARGET_UNICORE64 && TARGET_MMU_TRAPS && GET_CODE (operands[1]) == MEM)
      {
        emit_insn (gen_extendhisi2_mem (operands[0], operands[1]));
        DONE;
      }
    if (!s_register_operand (operands[1], HImode))
      operands[1] = copy_to_mode_reg (HImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[2] = gen_reg_rtx (SImode);

    if (TARGET_UNICORE16)
      FAIL;

  }"
)

(define_expand "extendhisi2_mem"
  [(set (match_dup 2) (zero_extend:SI (match_operand:HI 1 "" "")))
   (set (match_dup 3)
	(zero_extend:SI (match_dup 7)))
   (set (match_dup 6) (ashift:SI (match_dup 4) (const_int 24)))
   (set (match_operand:SI 0 "" "")
	(ior:SI (ashiftrt:SI (match_dup 6) (const_int 16)) (match_dup 5)))]
  "TARGET_UNICORE64"
  "
  {
    rtx mem1, mem2;
    rtx addr = copy_to_mode_reg (SImode, XEXP (operands[1], 0));

    mem1 = gen_rtx_MEM (QImode, addr);
    MEM_COPY_ATTRIBUTES (mem1, operands[1]);
    mem2 = gen_rtx_MEM (QImode, plus_constant (addr, 1));
    MEM_COPY_ATTRIBUTES (mem2, operands[1]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[1] = mem1;
    operands[2] = gen_reg_rtx (SImode);
    operands[3] = gen_reg_rtx (SImode);
    operands[6] = gen_reg_rtx (SImode);
    operands[7] = mem2;

    if (BYTES_BIG_ENDIAN)
      {
	operands[4] = operands[2];
	operands[5] = operands[3];
      }
    else
      {
	operands[4] = operands[3];
	operands[5] = operands[2];
      }
  }"
)

(define_insn "*unicore64_extendhisi_insn"
  [(set (match_operand:SI                 0 "s_register_operand" "=r")
	(sign_extend:SI (match_operand:HI 1 "memory_operand"      "m")))]
  "TARGET_UNICORE64 && unicore64_arch1"
  "ldsh%T1\\t%0, %1\\t%@ zero_extendhisi2"
  [(set_attr "type" "load")
   (set_attr "pool_range" "1448")
   (set_attr "neg_pool_range" "1448")]
)

(define_expand "extendqihi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "general_operand" "")
		   (const_int 24)))
   (set (match_operand:HI 0 "s_register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  "TARGET_UNICORE64"
  "
  {
    if (unicore64_arch1 && GET_CODE (operands[1]) == MEM)
      {
	emit_insn (gen_rtx_SET (VOIDmode,
				operands[0],
				gen_rtx_SIGN_EXTEND (HImode, operands[1])));
	DONE;
      }
    if (!s_register_operand (operands[1], QImode))
      operands[1] = copy_to_mode_reg (QImode, operands[1]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[2] = gen_reg_rtx (SImode);
  }"
)

; Rather than restricting all byte accesses to memory addresses that ldrsb
; can handle, we fix up the ones that ldrsb can't grok with a split.
(define_insn "*extendqihi_insn"
  [(set (match_operand:HI                 0 "s_register_operand" "=r")
	(sign_extend:HI (match_operand:QI 1 "memory_operand"      "m")))]
  "TARGET_UNICORE64 && unicore64_arch1"
  "*
  /* If the address is invalid, this will split the instruction into two. */
  if (bad_signed_byte_operand (operands[1], QImode))
    return \"#\";
  return \"ldsb%T1\\t%0, %1\"; 
  "
  [(set_attr "type" "load")
   (set_attr "length" "8")
   (set_attr "pool_range" "1448")
   (set_attr "neg_pool_range" "1448")]
)


(define_expand "extendqisi2"
  [(set (match_dup 2)
	(ashift:SI (match_operand:QI 1 "general_operand" "")
		   (const_int 24)))
   (set (match_operand:SI 0 "s_register_operand" "")
	(ashiftrt:SI (match_dup 2)
		     (const_int 24)))]
  "TARGET_EITHER"
  "
  {
    if (TARGET_UNICORE64 && unicore64_arch1 && GET_CODE (operands[1]) == MEM)
      {
        emit_insn (gen_rtx_SET (VOIDmode,
			        operands[0],
			        gen_rtx_SIGN_EXTEND (SImode, operands[1])));
        DONE;
      }
    if (!s_register_operand (operands[1], QImode))
      operands[1] = copy_to_mode_reg (QImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[2] = gen_reg_rtx (SImode);
    
    if (TARGET_UNICORE16)
      FAIL;
  }"
)

; Rather than restricting all byte accesses to memory addresses that ldrsb
; can handle, we fix up the ones that ldrsb can't grok with a split.
(define_insn "*unicore64_extendqisi_insn"
  [(set (match_operand:SI                 0 "s_register_operand" "=r")
	(sign_extend:SI (match_operand:QI 1 "memory_operand"      "m")))]
  "TARGET_UNICORE64 && unicore64_arch1"
  "*
  /* If the address is invalid, this will split the instruction into two. */
  if (bad_signed_byte_operand (operands[1], QImode))
    return \"#\";
  return \"ldsb%T1\\t%0, %1\";
  "
  [(set_attr "type" "load")
   (set_attr "length" "8")
   (set_attr "pool_range" "856")
   (set_attr "neg_pool_range" "812")]
)

(define_insn "extendsfdf2"
  [(set (match_operand:DF                  0 "s_register_operand" "=f")
	(float_extend:DF (match_operand:SF 1 "s_register_operand"  "f")))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "fcvt.d.s\\t%0, %1"
  [(set_attr "type" "fariths")]
)


;; The next two patterns occur when an AND operation is followed by a
;; scc insn sequence 
(define_insn "*sign_extract_onebit"
  [(set (match_operand:SI 0 "s_register_operand" "=r")
	(sign_extract:SI (match_operand:SI 1 "s_register_operand" "r")
			 (const_int 1)
			 (match_operand:SI 2 "const_int_operand" "n")))
	(clobber (reg:CC CC_REGNUM))]
  "TARGET_UNICORE64"
  "*
    operands[2] = GEN_INT (1 << INTVAL (operands[2]));
    output_asm_insn (\"and\\t%0, %1, %2\", operands);
    output_asm_insn (\"cmpsub.a\\t%0, #0\", operands);
    output_asm_insn (\"beq\\t.+8\", operands);
    return \"not\\t%0, #0\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "12")
   (set_attr "type" "modify_asr")]
)
;;-------------------------------level 3------------------------------
;;<call>
(define_insn "*call_reg"
  [(call (mem:DI (match_operand:DI 0 "s_register_operand" "r"))
         (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_UNICORE64"
  "*
  return output_call (operands);
  "
  ;; length is worst case, normally it is only two
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)

(define_insn "*call_mem"
  [(call (mem:DI (match_operand:DI 0 "memory_operand" "m"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_UNICORE64"
  "*
  return output_call_mem (operands);
  "
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)
(define_insn "*call_value_reg"
  [(set (match_operand 0 "" "=r,f")
        (call (mem:DI (match_operand:DI 1 "s_register_operand" "r,r"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_UNICORE64"
  "*
  return output_call (&operands[1]);
  "
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)

(define_insn "*call_value_mem"
  [(set (match_operand 0 "" "=r,f")
	(call (mem:DI (match_operand:DI 1 "memory_operand" "m,m"))
	      (match_operand 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_UNICORE64 && (!CONSTANT_ADDRESS_P (XEXP (operands[1], 0)))"
  "*
  return output_call_mem (&operands[1]);
  "
  [(set_attr "length" "12")
   (set_attr "type" "call")]
)

;; Allow calls to SYMBOL_REFs specially as they are not valid general addresses
;; The 'a' causes the operand to be treated as an address, i.e. no '#' output.
(define_insn "*call_symbol"
  [(call (mem:DI (match_operand:DI 0 "" "X"))
	 (match_operand 1 "" ""))
   (use (match_operand 2 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_UNICORE64
   && (GET_CODE (operands[0]) == SYMBOL_REF)
   && !unicore64_is_longcall_p (operands[0], INTVAL (operands[2]), 1)"
  "*
  {
    return NEED_PLT_RELOC ? \"call\\t%a0(PLT)\" : \"call\\t%a0\";
  }"
  [(set_attr "type" "call")]
)
(define_insn "*call_value_symbol"
  [(set (match_operand 0 "s_register_operand" "=r,f")
	(call (mem:DI (match_operand:DI 1 "" "X,X"))
	      (match_operand:DI 2 "" "")))
   (use (match_operand 3 "" ""))
   (clobber (reg:DI LR_REGNUM))]
  "TARGET_UNICORE64
   && (GET_CODE (operands[1]) == SYMBOL_REF)
   && !unicore64_is_longcall_p (operands[1], INTVAL (operands[3]), 1)"
  "*
  {
    return NEED_PLT_RELOC ? \"call\\t%a1(PLT)\" : \"call\\t%a1\";
  }"
  [(set_attr "type" "call")]
)
;; We may also be able to do sibcalls for Unicore16, but it's much harder...
(define_expand "sibcall"
  [(parallel [(call (match_operand 0 "memory_operand" "")
		    (match_operand 1 "general_operand" ""))
	      (use (match_operand 2 "" ""))
	      (return)])]
  "TARGET_UNICORE64"
  "
  {
    if (operands[2] == NULL_RTX)
      operands[2] = const0_rtx;
  }"
)

(define_expand "sibcall_value"
  [(parallel [(set (match_operand 0 "register_operand" "")
		   (call (match_operand 1 "memory_operand" "")
			 (match_operand 2 "general_operand" "")))
	      (use (match_operand 3 "" ""))
	      (return)])]
  "TARGET_UNICORE64"
  "
  {
    if (operands[3] == NULL_RTX)
      operands[3] = const0_rtx;
  }"
)
(define_insn "*sibcall_insn"
 [(call (mem:DI (match_operand:DI 0 "" "X"))
	(match_operand 1 "" ""))
  (use (match_operand 2 "" ""))
  (return)]
  "TARGET_UNICORE64 && GET_CODE (operands[0]) == SYMBOL_REF"
  "*
  return NEED_PLT_RELOC ? \"b\\t%a0(PLT)\" : \"b\\t%a0\";
  "
  [(set_attr "type" "call")]
)
(define_insn "*sibcall_value_insn"
 [(set (match_operand 0 "s_register_operand" "=r,f")
       (call (mem:DI (match_operand:DI 1 "" "X,X"))
	     (match_operand 2 "" "")))
  (use (match_operand 3 "" ""))
  (return)]
  "TARGET_UNICORE64 && GET_CODE (operands[1]) == SYMBOL_REF"
  "*
  return NEED_PLT_RELOC ? \"b\\t%a1(PLT)\" : \"b\\t%a1\";
  "
  [(set_attr "type" "call")]
)


;; Call subroutine returning any type.
(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  "TARGET_EITHER"
  "
  {
    int i;

    emit_call_insn (GEN_CALL (operands[0], const0_rtx, NULL, const0_rtx));

    for (i = 0; i < XVECLEN (operands[2], 0); i++)
      {
	rtx set = XVECEXP (operands[2], 0, i);

	emit_move_insn (SET_DEST (set), SET_SRC (set));
      }

    /* The optimizer does not know that the call sets the function value
       registers we stored in the result block.  We avoid problems by
       claiming that all hard registers are used and clobbered at this
       point.  */
    emit_insn (gen_blockage ());

    DONE;
  }"
)
;;-------------------------------end level 3------------------------------

;;-------------------------level 4---------------------------------
;;<cmp>
(define_expand "cmpsi"
  [(match_operand:SI 0 "s_register_operand" "")
   (match_operand:SI 1 "unicore64_add_operand" "")]
  "TARGET_UNICORE64"
  "{
    unicore64_compare_op0 = operands[0];
    unicore64_compare_op1 = operands[1];
    unicore64_compare_type = CMP_INT;
    DONE;
  }"
)

(define_expand "cmpsf"
  [(match_operand:SF 0 "s_register_operand" "")
   (match_operand:SF 1 "unicore64_float_rhs_operand" "")]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "
  unicore64_compare_op0 = operands[0];
  unicore64_compare_op1 = operands[1];
  unicore64_compare_type = CMP_FP;
  DONE;
  "
)

(define_expand "cmpdf"
  [(match_operand:DF 0 "s_register_operand" "")
   (match_operand:DF 1 "unicore64_float_rhs_operand" "")]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "
  unicore64_compare_op0 = operands[0];
  unicore64_compare_op1 = operands[1];
  unicore64_compare_type = CMP_FP;
  DONE;
  "
)

(define_insn "*unicore64_cmpsi_insn"
  [(set (reg:CC CC_REGNUM)
	(compare:CC (match_operand:SI 0 "s_register_operand" "r,r")
		    (match_operand:SI 1 "unicore64_add_operand"    "rI,L")))]
  "TARGET_UNICORE64"
  "@
   cmpsub.a\\t%0, %1
   cmpadd.a\\t%0, #%n1"
  [(set_attr "conds" "set")
  (set_attr "type" "modify_asr")]
)

(define_insn "*cmpsf_ccfpe_insn"
  [(set (reg:CCFPE CC_REGNUM)
	(match_operator 2 "unicore64_ccfpe_operator"
		[(match_operand:SF 0 "s_register_operand" "f")
		 (match_operand:SF 1 "s_register_operand" "f")]))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "*
  {
	switch( GET_CODE(operands[2]) )
	{
		case LT: return \"fcmp.olt.s\\t %0, %1\\n\tcff pc\";
		case UNLT: return \"fcmp.ult.s\\t %0, %1\\n\tcff pc\";
		case LE: return \"fcmp.ole.s\\t %0, %1\\n\tcff pc\";
		case UNLE: return \"fcmp.ule.s\\t %0, %1\\n\tcff pc\";
		case GT: return \"fcmp.olt.s\\t %1, %0\\n\tcff pc\";
		case UNGT: return \"fcmp.ult.s\\t %1, %0\\n\tcff pc\";
		case GE: return \"fcmp.ole.s\\t %1, %0\\n\tcff pc\";
		case UNGE: return \"fcmp.ule.s\\t %1, %0\\n\tcff pc\";
		case EQ: return \"fcmp.eq.s\\t %0, %1\\n\tcff pc\";
		case UNEQ: return \"fcmp.ueq.s\\t %0, %1\\n\tcff pc\";
		case UNORDERED: return \"fcmp.un.s\\t %0, %1\\n\tcff pc\";
		default: abort();
	}
  }"
  [(set_attr "conds" "set")
   (set_attr "type" "df_f_2_r")]
)


(define_insn "*cmpdf_ccfpe_insn"
  [(set (reg:CCFPE CC_REGNUM)
        (match_operator 2 "unicore64_ccfpe_operator" 
                [(match_operand:DF 0 "s_register_operand" "f")
                 (match_operand:DF 1 "s_register_operand" "f")]))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "*
  {
        switch( GET_CODE(operands[2]) )
        {
		case LT: return \"fcmp.olt.d\\t %0, %1\\n\tcff pc\";
		case UNLT: return \"fcmp.ult.d\\t %0, %1\\n\tcff pc\";
		case LE: return \"fcmp.ole.d\\t %0, %1\\n\tcff pc\";
		case UNLE: return \"fcmp.ule.d\\t %0, %1\\n\tcff pc\";
		case GT: return \"fcmp.olt.d\\t %1, %0\\n\tcff pc\";
		case UNGT: return \"fcmp.ult.d\\t %1, %0\\n\tcff pc\";
		case GE: return \"fcmp.ole.d\\t %1, %0\\n\tcff pc\";
		case UNGE: return \"fcmp.ule.d\\t %1, %0\\n\tcff pc\";
		case EQ: return \"fcmp.eq.d\\t %0, %1\\n\tcff pc\";
		case UNEQ: return \"fcmp.ueq.d\\t %0, %1\\n\tcff pc\";
		case UNORDERED: return \"fcmp.un.d\\t %0, %1\\n\tcff pc\";
                default: abort();
        }
  }"    
  [(set_attr "conds" "set")
   (set_attr "type" "df_f_2_r")]
)

; This insn allows redundant compares to be removed by cse, nothing should
; ever appear in the output file since (set (reg x) (reg x)) is a no-op that
; is deleted later on. The match_dup will match the mode here, so that
; mode changes of the condition codes aren't lost by this even though we don't
; specify what they are.

(define_insn "*deleted_compare"
  [(set (match_operand 0 "cc_register" "") (match_dup 0))]
  "TARGET_UNICORE64"
  "\\t%@ deleted compare"
  [(set_attr "conds" "set")
   (set_attr "length" "0")]
)


(define_expand "bunordered"
  [(set (pc)
	(if_then_else (unordered (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (UNORDERED, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, UNORDERED);
  		DONE;
 	}
   }"
)

(define_expand "bordered"
  [(set (pc)
	(if_then_else (ordered (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (ORDERED, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, ORDERED);
  		DONE;
 	}
   }"
)

(define_expand "bungt"
  [(set (pc)
	(if_then_else (ungt (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (UNGT, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, UNGT);
  		DONE;
 	}
   }"
)
(define_expand "bunlt"
  [(set (pc)
	(if_then_else (unlt (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (UNLT, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, UNLT);
  		DONE;
 	}
   }"
)

(define_expand "bunge"
  [(set (pc)
	(if_then_else (unge (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (UNGE, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, UNGE);
  		DONE;
 	}
   }"
)

(define_expand "bunle"
  [(set (pc)
	(if_then_else (unle (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (UNLE, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, UNLE);
  		DONE;
 	}
   }"
)

(define_expand "buneq"
  [(set (pc)
	(if_then_else (uneq (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (UNEQ, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, UNEQ);
  		DONE;
 	}
   }"
)

(define_expand "bltgt"
  [(set (pc)
	(if_then_else (ltgt (match_dup 1) (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64 && TARGET_HARD_FLOAT"
  "{
	if (unicore64_compare_type == CMP_INT)
		operands[1] = unicore64_gen_compare_reg (LTGT, unicore64_compare_op0, unicore64_compare_op1);
	else {
		gen_fp_conditional_branch(operands, LTGT);
  		DONE;
 	}
   }"
)


(define_insn "*unicore64_fp_cond_branch"
  [(set (pc)
        (if_then_else (match_operator 1 "unicore64_comparison_operator"
                       [(reg:CCFPE CC_REGNUM)
                          (const_int 0)])
                      (label_ref(match_operand 0 "" ""))
                      (pc)))]
  "TARGET_HARD_FLOAT"
  "*
{
  enum rtx_code cmp_code;
  cmp_code=GET_CODE(operands[1]);
  if(cmp_code==NE)
        return \"bea\\t%l0\";
  else if (cmp_code==EQ)
        return \"bub\\t%l0\";
  else abort();
}"
[(set_attr "conds" "use")]
)

(define_insn "*unicore64_cond_branch"
  [(set (pc)
	(if_then_else (match_operator 1 "unicore64_comparison_operator"
		       [(match_operand 2 "cc_register" "") (const_int 0)])
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  "TARGET_UNICORE64"
  "*
  if (unicore64_ccfsm_state == 1 || unicore64_ccfsm_state == 2)
    {
      unicore64_ccfsm_state += 2;
      return \"\";
    }
  return \"b%d1\\t%l0\";
  "
  [(set_attr "conds" "use")
   (set_attr "type" "branch")]
)
          
(define_insn "*unicore64_fp_cond_branch_reversed"
  [(set (pc)
        (if_then_else (match_operator 1 "unicore64_comparison_operator"
                       [(reg:CCFPE CC_REGNUM)
                          (const_int 0)])
                        (pc)
			(label_ref(match_operand 0 "" ""))))]
  "TARGET_HARD_FLOAT"
  "*
  {
  	enum rtx_code cmp_code;
  	cmp_code=GET_CODE(operands[1]);
  	if(cmp_code==NE)        
        	return \"bub\\t%l0\";
  	else if (cmp_code==EQ)
        	return \"bea\\t%l0\";
  	else abort();
  }"
[(set_attr "conds" "use")]
)


(define_insn "*unicore64_cond_branch_reversed"
  [(set (pc)
	(if_then_else (match_operator 1 "unicore64_comparison_operator"
		       [(match_operand 2 "cc_register" "") (const_int 0)])
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  "TARGET_UNICORE64"
  "*
  if (unicore64_ccfsm_state == 1 || unicore64_ccfsm_state == 2)
    {
      unicore64_ccfsm_state += 2;
      return \"\";
    }
  return \"b%D1\\t%l0\";
  "
  [(set_attr "conds" "use")
   (set_attr "type" "branch")]
)
;;<case>
(define_expand "casesi"
  [(match_operand:SI 0 "s_register_operand" "")	; index to jump on
   (match_operand:SI 1 "const_int_operand" "")	; lower bound
   (match_operand:SI 2 "const_int_operand" "")	; total range
   (match_operand:SI 3 "" "")			; table label
   (match_operand:SI 4 "" "")]			; Out of range label
  "TARGET_UNICORE64"
  "
  {
    rtx reg;
    if (operands[1] != const0_rtx)
      {
	reg = gen_reg_rtx (SImode);

	emit_insn (gen_addsi3 (reg, operands[0],
			       GEN_INT (-INTVAL (operands[1]))));
	operands[0] = reg;
      }

    if (!const_ok_for_unicore64 (INTVAL (operands[2])))
      operands[2] = force_reg (SImode, operands[2]);

    emit_jump_insn (gen_casesi_internal (operands[0], operands[2], operands[3],
					 operands[4]));
    DONE;
  }"
)

;; The USE in this pattern is needed to tell flow analysis that this is
;; a CASESI insn.  It has no other purpose.
(define_insn "casesi_internal"
  [(parallel [(set (pc)
	       (if_then_else
		(leu (match_operand:SI 0 "s_register_operand" "r")
		     (match_operand:SI 1 "unicore64_rhs_operand" "rI"))
		(mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
				 (label_ref (match_operand 2 "" ""))))
		(label_ref (match_operand 3 "" ""))))
	      (clobber (match_scratch:SI 4 "=r"))
	      (clobber (match_scratch:SI 5 "=r"))
	      (clobber (reg:CC CC_REGNUM))
	      (use (label_ref (match_dup 2)))])]
  "TARGET_UNICORE64"
  "*
    if (flag_pic)
      return   \"cmpsub.a\\t%0, %1\;\tbua\\t%l3\;\tlsl\\t%0, %0, #2\;\tadr\\t%4, %l2\;\tdadd\\t%4, %4, %0\;\tlsr\\t%0, %0, #2\;\tjump\\t%4\";
  return   \"cmpsub.a\\t%0, %1\;\tbua\\t%l3\;\tlsl\\t%0, %0, #3\;\tadr\\t%5, %l2\;\tldd\\t%5, [%5+], %0\;\tlsr\\t%0, %0, #3\;\tjump\\t%5\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "24")
   (set_attr "type" "modify_asr")]
)

;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] VUNSPEC_BLOCKAGE)]
  "TARGET_EITHER"
  ""
  [(set_attr "length" "0")
   (set_attr "type" "block")]
)

(define_expand "casedi"
  [(match_operand:DI 0 "s_register_operand" "")	; index to jump on
   (match_operand:DI 1 "const_int_operand" "")	; lower bound
   (match_operand:DI 2 "const_int_operand" "")	; total range
   (match_operand:DI 3 "" "")			; table label
   (match_operand:DI 4 "" "")]			; Out of range label
  "TARGET_UNICORE64"
  "
  {
    rtx reg;
    if (operands[1] != const0_rtx)
      {
	reg = gen_reg_rtx (DImode);

	emit_insn (gen_adddi3 (reg, operands[0],
			       GEN_INT (-INTVAL (operands[1]))));
	operands[0] = reg;
      }

    if (!const_ok_for_unicore64 (INTVAL (operands[2])))
      operands[2] = force_reg (DImode, operands[2]);

    emit_jump_insn (gen_casedi_internal (operands[0], operands[2], operands[3],
					 operands[4]));
    DONE;
  }"
)

;; The USE in this pattern is needed to tell flow analysis that this is
;; a CASEDI insn.  It has no other purpose.
(define_insn "casedi_internal"
  [(parallel [(set (pc)
	       (if_then_else
		(leu (match_operand:DI 0 "s_register_operand" "r")
		     (match_operand:DI 1 "unicore64_rhs_operand" "rI"))
		(mem:DI (plus:DI (mult:DI (match_dup 0) (const_int 4))
				 (label_ref (match_operand 2 "" ""))))
		(label_ref (match_operand 3 "" ""))))
	      (clobber (match_scratch:DI 4 "=r"))
	      (clobber (reg:CC CC_REGNUM))
	      (use (label_ref (match_dup 2)))])]
  "TARGET_UNICORE64"
  "*
    if (flag_pic)
      return \"dcmpsub.a\\t%0, %1\;\tbua\\t%l3\;\tdlsl\\t%0, %0, #2\;\tadr\\t%4, %l2\;\tdadd\\t%4, %4, %0\;\tdlsr\\t%0, %0, #2;\tjump\\t%4\";
    return \"dcmpsub.a\\t%0, %1\;\tbua\\t%l3\;\tdlsl\\t%0, %0, #3\;\tadr\\t%4, %l2\;\tldd\\t%4, [%4+], %0\;\tdlsr\\t%0, %0, #3;\tjump\\t%4\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "24")
   (set_attr "type" "modify_asr")]
)

;;---------------------------end level 4------------------------
;;------------------------------level 5--------------------------
;; Miscellaneous Instructions,

(define_expand "ffssi2"
  [(set (match_operand:SI 0 "s_register_operand" "")
        (ffs:SI (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_UNICORE64"
  "
  {
    rtx t1, t2, t3;

    t1 = gen_reg_rtx (SImode);
    t2 = gen_reg_rtx (SImode);
    t3 = gen_reg_rtx (SImode);

    emit_insn (gen_negsi2 (t1, operands[1]));
    emit_insn (gen_andsi3 (t2, operands[1], t1));
    emit_insn (gen_clz (t3, t2));
    emit_insn (gen_subsi3 (operands[0], GEN_INT (32), t3));
    DONE;
  }"
)

(define_expand "ctzsi2"
  [(set (match_operand:SI 0 "s_register_operand" "")
	(ctz:SI (match_operand:SI 1 "s_register_operand" "")))]
  "TARGET_UNICORE64"
  "
  {
    rtx t1, t2, t3;

    t1 = gen_reg_rtx (SImode);
    t2 = gen_reg_rtx (SImode);
    t3 = gen_reg_rtx (SImode);

    emit_insn (gen_negsi2 (t1, operands[1]));
    emit_insn (gen_andsi3 (t2, operands[1], t1));
    emit_insn (gen_clz (t3, t2));
    emit_insn (gen_subsi3 (operands[0], GEN_INT (31), t3));
    DONE;
  }"
)

(define_insn "*dand_scc"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(and:DI (match_operator:DI 1 "unicore64_comparison_operator"
		 [(match_operand 3 "cc_register" "") (const_int 0)])
		(match_operand:DI 2 "s_register_operand" "r")))]
  "TARGET_UNICORE64"
  "dand\\t%0, %2, #1\;\n\tb%d1\\t.+8\;\n\tdmov\\t%0, #0"
  [(set_attr "conds" "use")
   (set_attr "length" "12")
   (set_attr "type" "branch")]
)
(define_insn "*dior_scc"
  [(set (match_operand:DI 0 "s_register_operand" "=r,r")
	(ior:DI (match_operator:DI 2 "unicore64_comparison_operator"
		 [(match_operand 3 "cc_register" "") (const_int 0)])
		(match_operand:DI 1 "s_register_operand" "0,?r")))]
  "TARGET_UNICORE64"
  "@
   b%D2\\t.+8\;dor\\t%0, %1, #1
   dmov\\t%0, %1\;b%D2\\t.+8\;dor\\t%0, %1, #1"
  [(set_attr "conds" "use")
   (set_attr "length" "8,12")
   (set_attr "type" "branch")]
)
(define_insn "*dcompare_scc"
  [(set (match_operand:DI 0 "s_register_operand" "=r,r")
	(match_operator:DI 1 "unicore64_comparison_operator"
	 [(match_operand:DI 2 "s_register_operand" "r,r")
	  (match_operand:DI 3 "unicore64_add_operand" "rI,L")]))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_UNICORE64"
  "*
    if (GET_CODE (operands[1]) == LT && operands[3] == const0_rtx)
      return \"dlsr\\t%0, %2, #63\";

    if (GET_CODE (operands[1]) == GE && operands[3] == const0_rtx)
      return \"dnot\\t%0, %2\;\n\tdlsr\\t%0, %0, #63\";

    if (GET_CODE (operands[1]) == NE)
      {
        if (which_alternative == 1)
	  return \"dadd.a\\t%0, %2, #%n3\;\n\tbeq\\t.+8\;\n\tdmov\\t%0, #1\";
        return \"dsub.a\\t%0, %2, %3\;\n\tbeq\\t.+8\;\n\tdmov\\t%0, #1\";
      }
    if (which_alternative == 1)
      output_asm_insn (\"dcmpadd.a\\t%2, #%n3\", operands);
    else
      output_asm_insn (\"dcmpsub.a\\t%2, %3\", operands);
    return \"dmov\\t%0, #0\;\n\tb%D1\\t.+8\;\n\tdmov\\t%0, #1\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "12")
   (set_attr "type" "branch")]
)

(define_insn "*dnegscc"
  [(set (match_operand:DI 0 "s_register_operand" "=r")
	(neg:DI (match_operator 3 "unicore64_comparison_operator"
		 [(match_operand:DI 1 "s_register_operand" "r")
		  (match_operand:DI 2 "unicore64_rhs_operand" "rI")])))
   (clobber (reg:CC CC_REGNUM))]
  "TARGET_UNICORE64"
  "*
  if (GET_CODE (operands[3]) == LT && operands[3] == const0_rtx)
    return \"dasr\\t%0, %1, #63\";

  if (GET_CODE (operands[3]) == NE)
    return \"dsub.a\\t%0, %1, %2\;\n\tbeq\\t.+8\;\n\tdnot\\t%0, #0\";

  if (GET_CODE (operands[3]) == GT)
    return \"dsub.a\\t%0, %1, %2\;\n\tbeq\\t.+8\;\n\tdasr\\t%0, %0, #63\;\n\tdnot\\t%0,%0\";

  output_asm_insn (\"dcmpsub.a\\t%1, %2\", operands);
  output_asm_insn (\"dmov\\t%0, #0\", operands);
  output_asm_insn (\"b%D3\\t.+8\", operands);
  return \"dnot\\t%0, #0\";
  "
  [(set_attr "conds" "clob")
   (set_attr "length" "16")
   (set_attr "type" "branch")]
)


;; Miscellaneous Instructions,
(define_insn "dclz"
  [(set (match_operand:DI             0 "s_register_operand" "=r")
        (clz:DI (match_operand:DI 1 "s_register_operand" "r")))]
  "TARGET_UNICORE64 && unicore64_arch1"
  "dcntlz\\t%0, %1")

(define_expand "ffsdi2"
  [(set (match_operand:DI 0 "s_register_operand" "")
        (ffs:DI (match_operand:DI 1 "s_register_operand" "")))]
  "TARGET_UNICORE64"
  "
  {
    rtx t1, t2, t3;

    t1 = gen_reg_rtx (DImode);
    t2 = gen_reg_rtx (DImode);
    t3 = gen_reg_rtx (DImode);

    emit_insn (gen_negdi2 (t1, operands[1]));
    emit_insn (gen_anddi3 (t2, operands[1], t1));
    emit_insn (gen_dclz (t3, t2));
    emit_insn (gen_subdi3 (operands[0], GEN_INT (64), t3));
    DONE;
  }"
)

(define_expand "ctzdi2"
  [(set (match_operand:DI 0 "s_register_operand" "")
	(ctz:DI (match_operand:DI 1 "s_register_operand" "")))]
  "TARGET_UNICORE64"
  "
  {
    rtx t1, t2, t3;

    t1 = gen_reg_rtx (DImode);
    t2 = gen_reg_rtx (DImode);
    t3 = gen_reg_rtx (DImode);

    emit_insn (gen_negdi2 (t1, operands[1]));
    emit_insn (gen_anddi3 (t2, operands[1], t1));
    emit_insn (gen_dclz (t3, t2));
    emit_insn (gen_subdi3 (operands[0], GEN_INT (63), t3));
    DONE;
  }"
)
(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
	(match_operand:DI 1 "const_int_operand" ""))]
  "TARGET_UNICORE64
  && (!(const_ok_for_unicore64 (INTVAL (operands[1]))
        || const_ok_for_unicore64 (~INTVAL (operands[1]))))"
  [(clobber (const_int 0))]
  "
  unicore64_split_constant (SET, DImode, NULL_RTX, 
                      INTVAL (operands[1]), operands[0], NULL_RTX, 0);
  DONE;
  "
)

(define_split
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
	(match_operand:SF 1 "immediate_operand" ""))]
  "TARGET_UNICORE64
   && !TARGET_HARD_FLOAT
   && reload_completed
   && GET_CODE (operands[1]) == CONST_DOUBLE"
  [(set (match_dup 2) (match_dup 3))]
  "
  operands[2] = gen_lowpart (SImode, operands[0]);
  operands[3] = gen_lowpart (SImode, operands[1]);
  if (operands[2] == 0 || operands[3] == 0)
    FAIL;
  "
)
; By splitting (IOR (AND (NOT A) (NOT B)) C) as D = AND (IOR A B) (NOT C), 
; (NOT D) we can sometimes merge the final NOT into one of the following
; insns.

(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(ior:SI (and:SI (not:SI (match_operand:SI 1 "s_register_operand" ""))
			(not:SI (match_operand:SI 2 "unicore64_rhs_operand" "")))
		(match_operand:SI 3 "unicore64_rhs_operand" "")))
   (clobber (match_operand:SI 4 "s_register_operand" ""))]
  "TARGET_UNICORE64"
  [(set (match_dup 4) (and:SI (ior:SI (match_dup 1) (match_dup 2))
			      (not:SI (match_dup 3))))
   (set (match_dup 0) (not:SI (match_dup 4)))]
  ""
)
(define_split
  [(set (match_operand:HI 0 "s_register_operand" "")
	(sign_extend:HI (match_operand:QI 1 "bad_signed_byte_operand" "")))]
  "TARGET_UNICORE64 && unicore64_arch1 && reload_completed"
  [(set (match_dup 3) (match_dup 1))
   (set (match_dup 0) (sign_extend:HI (match_dup 2)))]
  "
  {
    HOST_WIDE_INT offset;

    operands[3] = gen_rtx_REG (DImode, REGNO (operands[0]));
    operands[2] = gen_rtx_MEM (QImode, operands[3]);
    MEM_COPY_ATTRIBUTES (operands[2], operands[1]);
    operands[1] = XEXP (operands[1], 0);
    if (GET_CODE (operands[1]) == PLUS
	&& GET_CODE (XEXP (operands[1], 1)) == CONST_INT
	&& !(const_ok_for_unicore64 (offset = INTVAL (XEXP (operands[1], 1)))
	     || const_ok_for_unicore64 (-offset)))
      {
	HOST_WIDE_INT low = (offset > 0
			     ? (offset & 0x3ff) : -((-offset) & 0x3ff));
	XEXP (operands[2], 0) = plus_constant (operands[3], low);
	operands[1] = plus_constant (XEXP (operands[1], 0), offset - low);
      }
    /* Ensure the sum is in correct canonical form */
    else if (GET_CODE (operands[1]) == PLUS
	     && GET_CODE (XEXP (operands[1], 1)) != CONST_INT
	     && !s_register_operand (XEXP (operands[1], 1), VOIDmode))
      operands[1] = gen_rtx_PLUS (GET_MODE (operands[1]),
					   XEXP (operands[1], 1),
					   XEXP (operands[1], 0));
  }"
)
(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(sign_extend:SI (match_operand:QI 1 "bad_signed_byte_operand" "")))]
  "TARGET_UNICORE64 && unicore64_arch1 && reload_completed"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0) (sign_extend:SI (match_dup 2)))]
  "
  {
    HOST_WIDE_INT offset;

    operands[2] = gen_rtx_MEM (QImode, operands[0]);
    MEM_COPY_ATTRIBUTES (operands[2], operands[1]);
    operands[1] = XEXP (operands[1], 0);
    if (GET_CODE (operands[1]) == PLUS
	&& GET_CODE (XEXP (operands[1], 1)) == CONST_INT
	&& !(const_ok_for_unicore64 (offset = INTVAL (XEXP (operands[1], 1)))
	     || const_ok_for_unicore64 (-offset)))
      {
	HOST_WIDE_INT low = (offset > 0
			     ? (offset & 0xff) : -((-offset) & 0xff));
	XEXP (operands[2], 0) = plus_constant (operands[0], low);
	operands[1] = plus_constant (XEXP (operands[1], 0), offset - low);
      }
    /* Ensure the sum is in correct canonical form */
    else if (GET_CODE (operands[1]) == PLUS
	     && GET_CODE (XEXP (operands[1], 1)) != CONST_INT
	     && !s_register_operand (XEXP (operands[1], 1), VOIDmode))
      operands[1] = gen_rtx_PLUS (GET_MODE (operands[1]),
					   XEXP (operands[1], 1),
					   XEXP (operands[1], 0));
  }"
)
(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(and:SI (ge:SI (match_operand:SI 1 "s_register_operand" "")
		       (const_int 0))
		(neg:SI (match_operator:SI 2 "unicore64_comparison_operator"
			 [(match_operand:SI 3 "s_register_operand" "")
			  (match_operand:SI 4 "unicore64_rhs_operand" "")]))))
   (clobber (match_operand:SI 5 "s_register_operand" ""))]
  "TARGET_UNICORE64"
  [(set (match_dup 5) (not:SI (ashiftrt:SI (match_dup 1) (const_int 31))))
   (set (match_dup 0) (and:SI (match_op_dup 2 [(match_dup 3) (match_dup 4)])
			      (match_dup 5)))]
  ""
)
;; This split can be used because CC_Z mode implies that the following
;; branch will be an equality, or an unsigned inequality, so the sign
;; extension is not needed.

(define_split
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
	 (ashift:SI (subreg:SI (match_operand:QI 0 "memory_operand" "") 0)
		    (const_int 24))
	 (match_operand 1 "const_int_operand" "")))
   (clobber (match_scratch:SI 2 ""))]
  "TARGET_UNICORE64
   && (((unsigned HOST_WIDE_INT) INTVAL (operands[1]))
       == (((unsigned HOST_WIDE_INT) INTVAL (operands[1])) >> 24) << 24)"
  [(set (match_dup 2) (zero_extend:SI (match_dup 0)))
   (set (reg:CC CC_REGNUM) (compare:CC (match_dup 2) (match_dup 1)))]
  "
  operands[1] = GEN_INT (((unsigned long) INTVAL (operands[1])) >> 24);
  "
)
(define_split
  [(set (match_operand:DI 0 "s_register_operand" "")
	(and:DI (ge:DI (match_operand:DI 1 "s_register_operand" "")
		       (const_int 0))
		(neg:DI (match_operator:DI 2 "unicore64_comparison_operator"
			 [(match_operand:DI 3 "s_register_operand" "")
			  (match_operand:DI 4 "unicore64_rhs_operand" "")]))))
   (clobber (match_operand:DI 5 "s_register_operand" ""))]
  "TARGET_UNICORE64"
  [(set (match_dup 5) (not:DI (ashiftrt:DI (match_dup 1) (const_int 63))))
   (set (match_dup 0) (and:DI (match_op_dup 2 [(match_dup 3) (match_dup 4)])
			      (match_dup 5)))]
  ""
)

;; This split can be used because CC_Z mode implies that the following
;; branch will be an equality, or an unsigned inequality, so the sign
;; extension is not needed.

(define_split
  [(set (reg:CC_Z CC_REGNUM)
	(compare:CC_Z
	 (ashift:DI (subreg:DI (match_operand:QI 0 "memory_operand" "") 0)
		    (const_int 24))
	 (match_operand 1 "const_int_operand" "")))
   (clobber (match_scratch:DI 2 ""))]
  "TARGET_UNICORE64
   && (((unsigned HOST_WIDE_INT) INTVAL (operands[1]))
       == (((unsigned HOST_WIDE_INT) INTVAL (operands[1])) >> 24) << 24)"
  [(set (match_dup 2) (zero_extend:DI (match_dup 0)))
   (set (reg:CC CC_REGNUM) (compare:CC (match_dup 2) (match_dup 1)))]
  "
  operands[1] = GEN_INT (((unsigned long) INTVAL (operands[1])) >> 24);
  "
)
(define_split
  [(set (match_operand:SI 0 "s_register_operand" "")
	(zero_extend:SI (subreg:QI (match_operand:SI 1 "" "") 0)))
   (clobber (match_operand:SI 2 "s_register_operand" ""))]
  "TARGET_UNICORE64 && (GET_CODE (operands[1]) != MEM) && ! BYTES_BIG_ENDIAN"
  [(set (match_dup 2) (match_dup 1))
   (set (match_dup 0) (and:SI (match_dup 2) (const_int 255)))]
  ""
)
