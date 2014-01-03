;;   AVR32 machine description file.
;;   Copyright 2003,2004,2005,2006,2007,2008,2009 Atmel Corporation.
;;
;;   This file is part of GCC.
;;
;;   This program is free software; you can redistribute it and/or modify
;;   it under the terms of the GNU General Public License as published by
;;   the Free Software Foundation; either version 2 of the License, or
;;   (at your option) any later version.
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;   GNU General Public License for more details.
;;
;;   You should have received a copy of the GNU General Public License
;;   along with this program; if not, write to the Free Software
;;   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; -*- Mode: Scheme -*-

(define_attr "type" "alu,alu2,alu_sat,mulhh,mulwh,mulww_w,mulww_d,div,machh_w,macww_w,macww_d,branch,call,load,load_rm,store,load2,load4,store2,store4,fmul,fcmps,fcmpd,fcast,fmv,fmvcpu,fldd,fstd,flds,fsts,fstm"
  (const_string "alu"))


(define_attr "cc" "none,set_vncz,set_ncz,set_cz,set_z,set_z_if_not_v2,bld,compare,cmp_cond_insn,clobber,call_set,fpcompare,from_fpcc"
  (const_string "none"))


; NB! Keep this in sync with enum architecture_type in avr32.h
(define_attr "pipeline" "ap,ucr1,ucr2,ucr2nomul,ucr3,ucr3fp"
  (const (symbol_ref "avr32_arch->arch_type")))

; Insn length in bytes
(define_attr "length" ""
  (const_int 4))

; Signal if an insn is predicable and hence can be conditionally executed.
(define_attr "predicable" "no,yes" (const_string "no"))

;; Uses of UNSPEC in this file:
(define_constants
  [(UNSPEC_PUSHM                0)
   (UNSPEC_POPM                 1)
   (UNSPEC_UDIVMODSI4_INTERNAL	2)
   (UNSPEC_DIVMODSI4_INTERNAL   3)
   (UNSPEC_STM                  4)
   (UNSPEC_LDM                  5)
   (UNSPEC_MOVSICC              6)
   (UNSPEC_ADDSICC              7)
   (UNSPEC_COND_MI              8)
   (UNSPEC_COND_PL              9)
   (UNSPEC_PIC_SYM              10)
   (UNSPEC_PIC_BASE             11)
   (UNSPEC_STORE_MULTIPLE       12)
   (UNSPEC_STMFP                13)
   (UNSPEC_FRCPA                14)
   (UNSPEC_REG_TO_CC            15)
   (UNSPEC_FORCE_MINIPOOL       16)
   (UNSPEC_SATS                 17)
   (UNSPEC_SATU                 18)
   (UNSPEC_SATRNDS              19)
   (UNSPEC_SATRNDU              20)
  ])

(define_constants
  [(VUNSPEC_EPILOGUE                  0)
   (VUNSPEC_CACHE                     1)
   (VUNSPEC_MTSR                      2)
   (VUNSPEC_MFSR                      3)
   (VUNSPEC_BLOCKAGE                  4)
   (VUNSPEC_SYNC                      5)
   (VUNSPEC_TLBR                      6)
   (VUNSPEC_TLBW                      7)
   (VUNSPEC_TLBS                      8)
   (VUNSPEC_BREAKPOINT                9)
   (VUNSPEC_MTDR                      10)
   (VUNSPEC_MFDR                      11)
   (VUNSPEC_MVCR                      12)
   (VUNSPEC_MVRC                      13)
   (VUNSPEC_COP                       14)
   (VUNSPEC_ALIGN                     15)
   (VUNSPEC_POOL_START                16)
   (VUNSPEC_POOL_END                  17)
   (VUNSPEC_POOL_4                    18)
   (VUNSPEC_POOL_8                    19)
   (VUNSPEC_POOL_16                   20)
   (VUNSPEC_MUSFR                     21)
   (VUNSPEC_MUSTR                     22)
   (VUNSPEC_SYNC_CMPXCHG              23)
   (VUNSPEC_SYNC_SET_LOCK_AND_LOAD    24)
   (VUNSPEC_SYNC_STORE_IF_LOCK        25)
   (VUNSPEC_EH_RETURN                 26)
   (VUNSPEC_FRS                       27)
   (VUNSPEC_CSRF                      28)
   (VUNSPEC_SSRF                      29)
   (VUNSPEC_SLEEP                     30)
   (VUNSPEC_DELAY_CYCLES              31)
   (VUNSPEC_DELAY_CYCLES_1            32)
   (VUNSPEC_DELAY_CYCLES_2            33)
   (VUNSPEC_NOP		       34)
   (VUNSPEC_NOP3		       35)
   ])

(define_constants
  [
   ;; R7 = 15-7 = 8
   (FP_REGNUM   8)
   ;; Return Register = R12 = 15 - 12 = 3
   (RETVAL_REGNUM   3)
   ;; SP = R13 = 15 - 13 = 2
   (SP_REGNUM   2)
   ;; LR = R14 = 15 - 14 = 1
   (LR_REGNUM   1)
   ;; PC = R15 = 15 - 15 = 0
   (PC_REGNUM   0)
   ;; FPSR = GENERAL_REGS + 1 = 17
   (FPCC_REGNUM 17)
   ])




;;******************************************************************************
;; Macros
;;******************************************************************************

;; Integer Modes for basic alu insns
(define_mode_iterator INTM [SI HI QI])
(define_mode_attr  alu_cc_attr [(SI "set_vncz") (HI "clobber") (QI "clobber")])

;; Move word modes
(define_mode_iterator MOVM [SI V2HI V4QI])

;; For mov/addcc insns
(define_mode_iterator ADDCC [SI HI QI])
(define_mode_iterator MOVCC [SF SI HI QI])
(define_mode_iterator CMP [DI SI HI QI])
(define_mode_attr  store_postfix [(SF ".w") (SI ".w") (HI ".h") (QI ".b")])
(define_mode_attr  load_postfix [(SF ".w") (SI ".w") (HI ".sh") (QI ".ub")])
(define_mode_attr  load_postfix_s [(SI ".w") (HI ".sh") (QI ".sb")])
(define_mode_attr  load_postfix_u [(SI ".w") (HI ".uh") (QI ".ub")])
(define_mode_attr  pred_mem_constraint [(SF "RKu11") (SI "RKu11") (HI "RKu10") (QI "RKu09")])
(define_mode_attr  cmp_constraint [(DI "rKu20") (SI "rKs21") (HI "r") (QI "r")])
(define_mode_attr  cmp_predicate [(DI "register_immediate_operand")
                                  (SI "register_const_int_operand")
                                  (HI "register_operand")
                                  (QI "register_operand")])
(define_mode_attr  cmp_length [(DI "6")
                               (SI "4")
                               (HI "4")
                               (QI "4")])

;; For all conditional insns
(define_code_iterator any_cond [eq ne gt ge lt le gtu geu ltu leu])
(define_code_attr cond [(eq "eq") (ne "ne") (gt "gt") (ge "ge") (lt "lt") (le "le")
                        (gtu "hi") (geu "hs") (ltu "lo") (leu "ls")])
(define_code_attr invcond [(eq "ne") (ne "eq") (gt "le") (ge "lt") (lt "ge") (le "gt")
                           (gtu "ls") (geu "lo") (ltu "hs") (leu "hi")])

;; For logical operations
(define_code_iterator logical [and ior xor])
(define_code_attr logical_insn [(and "and") (ior "or") (xor "eor")])

;; Predicable operations with three register operands 
(define_code_iterator predicable_op3 [and ior xor plus minus])
(define_code_attr predicable_insn3 [(and "and") (ior "or") (xor "eor") (plus "add") (minus "sub")])
(define_code_attr predicable_commutative3 [(and "%") (ior "%") (xor "%") (plus "%") (minus "")])

;; Load the predicates
(include "predicates.md")


;;******************************************************************************
;; Automaton pipeline description for avr32
;;******************************************************************************

(define_automaton "avr32_ap")


(define_cpu_unit "is" "avr32_ap")
(define_cpu_unit "a1,m1,da" "avr32_ap")
(define_cpu_unit "a2,m2,d" "avr32_ap")

;;Alu instructions
(define_insn_reservation "alu_op" 1
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "alu"))
  "is,a1,a2")

(define_insn_reservation "alu2_op" 2
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "alu2"))
  "is,is+a1,a1+a2,a2")

(define_insn_reservation "alu_sat_op" 2
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "alu_sat"))
  "is,a1,a2")


;;Mul instructions
(define_insn_reservation "mulhh_op" 2
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "mulhh,mulwh"))
  "is,m1,m2")

(define_insn_reservation "mulww_w_op" 3
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "mulww_w"))
  "is,m1,m1+m2,m2")

(define_insn_reservation "mulww_d_op" 5
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "mulww_d"))
  "is,m1,m1+m2,m1+m2,m2,m2")

(define_insn_reservation "div_op" 33
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "div"))
  "is,m1,m1*31 + m2*31,m2")

(define_insn_reservation "machh_w_op" 3
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "machh_w"))
  "is*2,m1,m2")


(define_insn_reservation "macww_w_op" 4
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "macww_w"))
  "is*2,m1,m1,m2")


(define_insn_reservation "macww_d_op" 6
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "macww_d"))
  "is*2,m1,m1+m2,m1+m2,m2")

;;Bypasses for Mac instructions, because of accumulator cache.
;;Set latency as low as possible in order to let the compiler let
;;mul -> mac and mac -> mac combinations which use the same
;;accumulator cache be placed close together to avoid any
;;instructions which can ruin the accumulator cache come inbetween.
(define_bypass 4 "machh_w_op" "alu_op,alu2_op,alu_sat_op,load_op" "avr32_mul_waw_bypass")
(define_bypass 5 "macww_w_op" "alu_op,alu2_op,alu_sat_op,load_op" "avr32_mul_waw_bypass")
(define_bypass 7 "macww_d_op" "alu_op,alu2_op,alu_sat_op,load_op" "avr32_mul_waw_bypass")

(define_bypass 3 "mulhh_op" "alu_op,alu2_op,alu_sat_op,load_op" "avr32_mul_waw_bypass")
(define_bypass 4 "mulww_w_op" "alu_op,alu2_op,alu_sat_op,load_op" "avr32_mul_waw_bypass")
(define_bypass 6 "mulww_d_op" "alu_op,alu2_op,alu_sat_op,load_op" "avr32_mul_waw_bypass")


;;Bypasses for all mul/mac instructions followed by an instruction
;;which reads the output AND writes the result to the same register.
;;This will generate an Write After Write hazard which gives an
;;extra cycle before the result is ready.
(define_bypass 0 "machh_w_op" "machh_w_op" "avr32_valid_macmac_bypass")
(define_bypass 0 "macww_w_op" "macww_w_op" "avr32_valid_macmac_bypass")
(define_bypass 0 "macww_d_op" "macww_d_op" "avr32_valid_macmac_bypass")

(define_bypass 0 "mulhh_op" "machh_w_op" "avr32_valid_mulmac_bypass")
(define_bypass 0 "mulww_w_op" "macww_w_op" "avr32_valid_mulmac_bypass")
(define_bypass 0 "mulww_d_op" "macww_d_op" "avr32_valid_mulmac_bypass")

;;Branch and call instructions
;;We assume that all branches and rcalls are predicted correctly :-)
;;while calls use a lot of cycles.
(define_insn_reservation "branch_op" 0
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "branch"))
  "nothing")

(define_insn_reservation "call_op" 10
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "call"))
  "nothing")


;;Load store instructions
(define_insn_reservation "load_op" 2
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "load"))
  "is,da,d")

(define_insn_reservation "load_rm_op" 3
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "load_rm"))
  "is,da,d")


(define_insn_reservation "store_op" 0
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "store"))
  "is,da,d")


(define_insn_reservation "load_double_op" 3
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "load2"))
  "is,da,da+d,d")

(define_insn_reservation "load_quad_op" 4
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "load4"))
  "is,da,da+d,da+d,d")

(define_insn_reservation "store_double_op" 0
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "store2"))
  "is,da,da+d,d")


(define_insn_reservation "store_quad_op" 0
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "store4"))
  "is,da,da+d,da+d,d")

;;For store the operand to write to memory is read in d and
;;the real latency between any instruction and a store is therefore
;;one less than for the instructions which reads the operands in the first
;;excecution stage
(define_bypass 2 "load_double_op" "store_double_op" "avr32_store_bypass")
(define_bypass 3 "load_quad_op" "store_quad_op" "avr32_store_bypass")
(define_bypass 1 "load_op" "store_op" "avr32_store_bypass")
(define_bypass 2 "load_rm_op" "store_op" "avr32_store_bypass")
(define_bypass 1 "alu_sat_op" "store_op" "avr32_store_bypass")
(define_bypass 1 "alu2_op" "store_op" "avr32_store_bypass")
(define_bypass 1 "mulhh_op" "store_op" "avr32_store_bypass")
(define_bypass 2 "mulww_w_op" "store_op" "avr32_store_bypass")
(define_bypass 4 "mulww_d_op" "store_op" "avr32_store_bypass" )
(define_bypass 2 "machh_w_op" "store_op" "avr32_store_bypass")
(define_bypass 3 "macww_w_op" "store_op" "avr32_store_bypass")
(define_bypass 5 "macww_d_op" "store_op" "avr32_store_bypass")


; Bypass for load double operation. If only the first loaded word is needed
; then the latency is 2
(define_bypass 2 "load_double_op"
                 "load_op,load_rm_op,alu_sat_op, alu2_op, alu_op, mulhh_op, mulww_w_op,
                  mulww_d_op, machh_w_op, macww_w_op, macww_d_op"
                 "avr32_valid_load_double_bypass")

; Bypass for load quad operation. If only the first or second loaded word is needed
; we set the latency to 2
(define_bypass 2 "load_quad_op"
                 "load_op,load_rm_op,alu_sat_op, alu2_op, alu_op, mulhh_op, mulww_w_op,
                  mulww_d_op, machh_w_op, macww_w_op, macww_d_op"
                 "avr32_valid_load_quad_bypass")


;;******************************************************************************
;; End of Automaton pipeline description for avr32
;;******************************************************************************

(define_cond_exec
  [(match_operator 0 "avr32_comparison_operator"
    [(match_operand:CMP 1 "register_operand" "r")         
     (match_operand:CMP 2 "<CMP:cmp_predicate>" "<CMP:cmp_constraint>")])]
  "TARGET_V2_INSNS" 
  "%!"
)

(define_cond_exec
  [(match_operator 0 "avr32_comparison_operator"
     [(and:SI (match_operand:SI 1 "register_operand" "r")         
              (match_operand:SI 2 "one_bit_set_operand" "i"))
      (const_int 0)])]
  "TARGET_V2_INSNS" 
  "%!"
  )

;;=============================================================================
;; move
;;-----------------------------------------------------------------------------


;;== char - 8 bits ============================================================
(define_expand "movqi"
  [(set (match_operand:QI 0 "nonimmediate_operand" "")
	(match_operand:QI 1 "general_operand" ""))]
  ""
  {
   if ( can_create_pseudo_p () ){
     if (GET_CODE (operands[1]) == MEM && optimize){
         rtx reg = gen_reg_rtx (SImode);

         emit_insn (gen_zero_extendqisi2 (reg, operands[1]));
         operands[1] = gen_lowpart (QImode, reg);
     }

     /* One of the ops has to be in a register.  */
     if (GET_CODE (operands[0]) == MEM)
       operands[1] = force_reg (QImode, operands[1]);
   }

  })

(define_insn "*movqi_internal"
  [(set (match_operand:QI 0 "nonimmediate_operand" "=r,r,m,r")
	(match_operand:QI 1 "general_operand"  "rKs08,m,r,i"))]
  "register_operand (operands[0], QImode)
   || register_operand (operands[1], QImode)"
  "@
   mov\t%0, %1
   ld.ub\t%0, %1
   st.b\t%0, %1
   mov\t%0, %1"
  [(set_attr "length" "2,4,4,4")
   (set_attr "type" "alu,load_rm,store,alu")])



;;== short - 16 bits ==========================================================
(define_expand "movhi"
  [(set (match_operand:HI 0 "nonimmediate_operand" "")
	(match_operand:HI 1 "general_operand" ""))]
  ""
  {
   if ( can_create_pseudo_p () ){
     if (GET_CODE (operands[1]) == MEM && optimize){
         rtx reg = gen_reg_rtx (SImode);

         emit_insn (gen_extendhisi2 (reg, operands[1]));
         operands[1] = gen_lowpart (HImode, reg);
     }

     /* One of the ops has to be in a register.  */
     if (GET_CODE (operands[0]) == MEM)
       operands[1] = force_reg (HImode, operands[1]);
   }

  })


(define_insn "*movhi_internal"
  [(set (match_operand:HI 0 "nonimmediate_operand" "=r,r,m,r")
	(match_operand:HI 1 "general_operand"  "rKs08,m,r,i"))]
  "register_operand (operands[0], HImode)
   || register_operand (operands[1], HImode)"
  "@
   mov\t%0, %1
   ld.sh\t%0, %1
   st.h\t%0, %1
   mov\t%0, %1"
  [(set_attr "length" "2,4,4,4")
   (set_attr "type" "alu,load_rm,store,alu")])


;;== int - 32 bits ============================================================

(define_expand "movmisalignsi"
  [(set (match_operand:SI 0 "nonimmediate_operand" "")
	(match_operand:SI 1 "nonimmediate_operand" ""))]
  "TARGET_UNALIGNED_WORD"
  {
  }
)

(define_expand "mov<mode>"
  [(set (match_operand:MOVM 0 "avr32_non_rmw_nonimmediate_operand" "")
	(match_operand:MOVM 1 "avr32_non_rmw_general_operand" ""))]
  ""
  {

    /* One of the ops has to be in a register.  */
    if (GET_CODE (operands[0]) == MEM)
      operands[1] = force_reg (<MODE>mode, operands[1]);

    /* Check for out of range immediate constants as these may
       occur during reloading, since it seems like reload does
       not check if the immediate is legitimate. Don't know if
       this is a bug? */
    if ( reload_in_progress
         && avr32_imm_in_const_pool
         && GET_CODE(operands[1]) == CONST_INT
         && !avr32_const_ok_for_constraint_p(INTVAL(operands[1]), 'K', "Ks21") ){
        operands[1] = force_const_mem(SImode, operands[1]);
    }
    /* Check for RMW memory operands. They are not allowed for mov operations
       only the atomic memc/s/t operations */
    if ( !reload_in_progress
         && avr32_rmw_memory_operand (operands[0], <MODE>mode) ){
       operands[0] = copy_rtx (operands[0]);                                                              
       XEXP(operands[0], 0) = force_reg (<MODE>mode, XEXP(operands[0], 0));
    }

    if ( !reload_in_progress
         && avr32_rmw_memory_operand (operands[1], <MODE>mode) ){
       operands[1] = copy_rtx (operands[1]);                                                              
      XEXP(operands[1], 0) = force_reg (<MODE>mode, XEXP(operands[1], 0));
    }
    if ( (flag_pic || TARGET_HAS_ASM_ADDR_PSEUDOS)
         && !avr32_legitimate_pic_operand_p(operands[1]) )
      operands[1] = legitimize_pic_address (operands[1], <MODE>mode,
                                            (can_create_pseudo_p () ? 0: operands[0]));
    else if ( flag_pic && avr32_address_operand(operands[1], GET_MODE(operands[1])) )
      /* If we have an address operand then this function uses the pic register. */
      current_function_uses_pic_offset_table = 1;
  })


(define_insn "mov<mode>_internal"
  [(set (match_operand:MOVM 0 "avr32_non_rmw_nonimmediate_operand" "=r,   r,   r,r,r,Q,r")
	(match_operand:MOVM 1 "avr32_non_rmw_general_operand"      "rKs08,Ks21,J,n,Q,r,W"))]
  "(register_operand (operands[0], <MODE>mode)
    || register_operand (operands[1], <MODE>mode))
    && !avr32_rmw_memory_operand (operands[0], <MODE>mode) 
    && !avr32_rmw_memory_operand (operands[1], <MODE>mode)"
  {
    switch (which_alternative) {
      case 0:
      case 1: return "mov\t%0, %1";
      case 2:
        if ( TARGET_V2_INSNS )
           return "movh\t%0, hi(%1)";
        /* Fallthrough */
      case 3: return "mov\t%0, lo(%1)\;orh\t%0,hi(%1)";
      case 4:
        if ( (REG_P(XEXP(operands[1], 0))
              && REGNO(XEXP(operands[1], 0)) == SP_REGNUM)
             || (GET_CODE(XEXP(operands[1], 0)) == PLUS
                 && REGNO(XEXP(XEXP(operands[1], 0), 0)) == SP_REGNUM
	         && GET_CODE(XEXP(XEXP(operands[1], 0), 1)) == CONST_INT
	         && INTVAL(XEXP(XEXP(operands[1], 0), 1)) % 4 == 0
	         && INTVAL(XEXP(XEXP(operands[1], 0), 1)) <= 0x1FC) )
          return "lddsp\t%0, %1";
	else if ( avr32_const_pool_ref_operand(operands[1], GET_MODE(operands[1])) )
          return "lddpc\t%0, %1";
        else
          return "ld.w\t%0, %1";
      case 5:
        if ( (REG_P(XEXP(operands[0], 0))
              && REGNO(XEXP(operands[0], 0)) == SP_REGNUM)
             || (GET_CODE(XEXP(operands[0], 0)) == PLUS
                 && REGNO(XEXP(XEXP(operands[0], 0), 0)) == SP_REGNUM
	         && GET_CODE(XEXP(XEXP(operands[0], 0), 1)) == CONST_INT
	         && INTVAL(XEXP(XEXP(operands[0], 0), 1)) % 4 == 0
	         && INTVAL(XEXP(XEXP(operands[0], 0), 1)) <= 0x1FC) )
          return "stdsp\t%0, %1";
	else
          return "st.w\t%0, %1";
      case 6:
        if ( TARGET_HAS_ASM_ADDR_PSEUDOS )
          return "lda.w\t%0, %1";
        else
          return "ld.w\t%0, r6[%1@got]";
      default:
	abort();
    }
  }
  
  [(set_attr "length" "2,4,4,8,4,4,8")
   (set_attr "type" "alu,alu,alu,alu2,load,store,load")
   (set_attr "cc" "none,none,set_z_if_not_v2,set_z,none,none,clobber")])


(define_expand "reload_out_rmw_memory_operand"
  [(set (match_operand:SI 2 "register_operand" "=r")
        (match_operand:SI 0 "address_operand" ""))
   (set (mem:SI (match_dup 2))
        (match_operand:SI 1 "register_operand" ""))]
  ""
  {
   operands[0] = XEXP(operands[0], 0);
  }
)

(define_expand "reload_in_rmw_memory_operand"
  [(set (match_operand:SI 2 "register_operand" "=r")
        (match_operand:SI 1 "address_operand" ""))
   (set (match_operand:SI 0 "register_operand" "")
        (mem:SI (match_dup 2)))]
  ""
  {
   operands[1] = XEXP(operands[1], 0);
  }
)


;; These instructions are for loading constants which cannot be loaded
;; directly from the constant pool because the offset is too large
;; high and lo_sum are used even tough for our case it should be
;; low and high sum :-)
(define_insn "mov_symbol_lo"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(high:SI (match_operand:SI 1 "immediate_operand" "i" )))]
  ""
  "mov\t%0, lo(%1)"
  [(set_attr "type" "alu")
   (set_attr "length" "4")]
)

(define_insn "add_symbol_hi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(lo_sum:SI (match_dup 0)
                   (match_operand:SI 1 "immediate_operand" "i" )))]
  ""
  "orh\t%0, hi(%1)"
  [(set_attr "type" "alu")
   (set_attr "length" "4")]
)



;; When generating pic, we need to load the symbol offset into a register.
;; So that the optimizer does not confuse this with a normal symbol load
;; we use an unspec.  The offset will be loaded from a constant pool entry,
;; since that is the only type of relocation we can use.
(define_insn "pic_load_addr"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(match_operand:SI 1 "" "")] UNSPEC_PIC_SYM))]
  "flag_pic && CONSTANT_POOL_ADDRESS_P(XEXP(operands[1], 0))"
  "lddpc\t%0, %1"
  [(set_attr "type" "load")
   (set_attr "length" "4")]
)

(define_insn "pic_compute_got_from_pc"
  [(set (match_operand:SI 0 "register_operand" "+r")
	(unspec:SI [(minus:SI (pc)
                              (match_dup 0))] UNSPEC_PIC_BASE))
   (use (label_ref (match_operand 1 "" "")))]
  "flag_pic"
  {
   (*targetm.asm_out.internal_label) (asm_out_file, "L",
	 		     CODE_LABEL_NUMBER (operands[1]));
   return \"rsub\t%0, pc\";
  }
  [(set_attr "cc" "clobber")
   (set_attr "length" "2")]
)

;;== long long int - 64 bits ==================================================

(define_expand "movdi"
  [(set (match_operand:DI 0 "nonimmediate_operand" "")
	(match_operand:DI 1 "general_operand" ""))]
  ""
  {

    /* One of the ops has to be in a register.  */
    if (GET_CODE (operands[0]) != REG)
      operands[1] = force_reg (DImode, operands[1]);

  })


(define_insn_and_split "*movdi_internal"
  [(set (match_operand:DI 0 "nonimmediate_operand"     "=r,r,   r,   r,r,r,m")
	(match_operand:DI 1 "general_operand"          "r, Ks08,Ks21,G,n,m,r"))]
  "register_operand (operands[0], DImode)
   || register_operand (operands[1], DImode)"
  {
    switch (which_alternative ){
    case 0:
    case 1:
    case 2:
    case 3:
    case 4:
        return "#";
    case 5:
      if ( avr32_const_pool_ref_operand(operands[1], GET_MODE(operands[1])))
        return "ld.d\t%0, pc[%1 - .]";
      else
        return "ld.d\t%0, %1";
    case 6:
      return "st.d\t%0, %1";
    default:
      abort();
    }
  }
;; Lets split all reg->reg or imm->reg transfers into two SImode transfers 
  "reload_completed &&
   (REG_P (operands[0]) &&
   (REG_P (operands[1]) 
    || GET_CODE (operands[1]) == CONST_INT
    || GET_CODE (operands[1]) == CONST_DOUBLE))"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))]
  {
    operands[2] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    if ( REG_P(operands[1]) ){
      operands[3] = gen_highpart(SImode, operands[1]);
      operands[1] = gen_lowpart(SImode, operands[1]);
    } else if ( GET_CODE(operands[1]) == CONST_DOUBLE 
                || GET_CODE(operands[1]) == CONST_INT ){
      rtx split_const[2];
      avr32_split_const_expr (DImode, SImode, operands[1], split_const);
      operands[3] = split_const[1];
      operands[1] = split_const[0];
    } else {
      internal_error("Illegal operand[1] for movdi split!");
    }
  }

  [(set_attr "length" "*,*,*,*,*,4,4")
   (set_attr "type" "*,*,*,*,*,load2,store2")
   (set_attr "cc" "*,*,*,*,*,none,none")])


;;== 128 bits ==================================================
(define_expand "movti"
  [(set (match_operand:TI 0 "nonimmediate_operand" "")
	(match_operand:TI 1 "nonimmediate_operand" ""))]
  "TARGET_ARCH_AP"    
  {     
        
    /* One of the ops has to be in a register.  */
    if (GET_CODE (operands[0]) != REG)
      operands[1] = force_reg (TImode, operands[1]);

    /* We must fix any pre_dec for loads and post_inc stores */
    if ( GET_CODE (operands[0]) == MEM
         && GET_CODE (XEXP(operands[0],0)) == POST_INC ){
       emit_move_insn(gen_rtx_MEM(TImode, XEXP(XEXP(operands[0],0),0)), operands[1]);
       emit_insn(gen_addsi3(XEXP(XEXP(operands[0],0),0), XEXP(XEXP(operands[0],0),0), GEN_INT(GET_MODE_SIZE(TImode))));
       DONE;
    }

    if ( GET_CODE (operands[1]) == MEM
         && GET_CODE (XEXP(operands[1],0)) == PRE_DEC ){
       emit_insn(gen_addsi3(XEXP(XEXP(operands[1],0),0), XEXP(XEXP(operands[1],0),0), GEN_INT(-GET_MODE_SIZE(TImode))));
       emit_move_insn(operands[0], gen_rtx_MEM(TImode, XEXP(XEXP(operands[1],0),0)));
       DONE;
    }
  })


(define_insn_and_split "*movti_internal"
  [(set (match_operand:TI 0 "avr32_movti_dst_operand"  "=r,&r,    r,    <RKu00,r,r")
	(match_operand:TI 1 "avr32_movti_src_operand"  " r,RKu00>,RKu00,r,     n,T"))]
  "(register_operand (operands[0], TImode)
    || register_operand (operands[1], TImode))"
  {
    switch (which_alternative ){
    case 0:
    case 2:
    case 4:
        return "#";
    case 1:
        return "ldm\t%p1, %0";
    case 3:
        return "stm\t%p0, %1";
    case 5:
        return "ld.d\t%U0, pc[%1 - .]\;ld.d\t%B0, pc[%1 - . + 8]";
    }
  }

  "reload_completed &&
   (REG_P (operands[0]) &&
   (REG_P (operands[1]) 
    /* If this is a load from the constant pool we split it into
       two double loads. */
    || (GET_CODE (operands[1]) == MEM
        && GET_CODE (XEXP (operands[1], 0)) == SYMBOL_REF
        && CONSTANT_POOL_ADDRESS_P (XEXP (operands[1], 0)))               
    /* If this is a load where the pointer register is a part
       of the register list, we must split it into two double
       loads in order for it to be exception safe. */
    || (GET_CODE (operands[1]) == MEM
        && register_operand (XEXP (operands[1], 0), SImode)
        && reg_overlap_mentioned_p (operands[0], XEXP (operands[1], 0)))               
    || GET_CODE (operands[1]) == CONST_INT
    || GET_CODE (operands[1]) == CONST_DOUBLE))"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))]
  {
    operands[2] = simplify_gen_subreg ( DImode, operands[0], 
                                        TImode, 0 );
    operands[0] = simplify_gen_subreg ( DImode, operands[0], 
                                        TImode, 8 );
    if ( REG_P(operands[1]) ){
      operands[3] = simplify_gen_subreg ( DImode, operands[1], 
                                          TImode, 0 );
      operands[1] = simplify_gen_subreg ( DImode, operands[1], 
                                          TImode, 8 );
    } else if ( GET_CODE(operands[1]) == CONST_DOUBLE 
                || GET_CODE(operands[1]) == CONST_INT ){
      rtx split_const[2];
      avr32_split_const_expr (TImode, DImode, operands[1], split_const);
      operands[3] = split_const[1];
      operands[1] = split_const[0];
    } else if (avr32_const_pool_ref_operand (operands[1], GET_MODE(operands[1]))){
      rtx split_const[2];
      rtx cop = avoid_constant_pool_reference (operands[1]);
      if (operands[1] == cop)
        cop = get_pool_constant (XEXP (operands[1], 0));
      avr32_split_const_expr (TImode, DImode, cop, split_const);
      operands[3] = force_const_mem (DImode, split_const[1]); 
      operands[1] = force_const_mem (DImode, split_const[0]); 
   } else {
      rtx ptr_reg = XEXP (operands[1], 0);
      operands[1] = gen_rtx_MEM (DImode, 
                                 gen_rtx_PLUS ( SImode,
                                                ptr_reg,
                                                GEN_INT (8) ));
      operands[3] = gen_rtx_MEM (DImode,
                                 ptr_reg);
              
      /* Check if the first load will clobber the pointer.
         If so, we must switch the order of the operations. */
      if ( reg_overlap_mentioned_p (operands[0], ptr_reg) )
        {
          /* We need to switch the order of the operations
             so that the pointer register does not get clobbered
             after the first double word load. */
          rtx tmp;
          tmp = operands[0];
          operands[0] = operands[2];
          operands[2] = tmp;
          tmp = operands[1];
          operands[1] = operands[3];
          operands[3] = tmp;
        }


   }
  }
  [(set_attr "length" "*,*,4,4,*,8")
   (set_attr "type" "*,*,load4,store4,*,load4")])


;;== float - 32 bits ==========================================================
(define_expand "movsf"
  [(set (match_operand:SF 0 "nonimmediate_operand" "")
	(match_operand:SF 1 "general_operand" ""))]
  ""
  {


    /* One of the ops has to be in a register.  */
    if (GET_CODE (operands[0]) != REG)
      operands[1] = force_reg (SFmode, operands[1]);

  })

(define_insn "*movsf_internal"
  [(set (match_operand:SF 0 "nonimmediate_operand"     "=r,r,r,r,m")
	(match_operand:SF 1 "general_operand"          "r, G,F,m,r"))]
  "(register_operand (operands[0], SFmode)
    || register_operand (operands[1], SFmode))"
  {
    switch (which_alternative) {
      case 0:
      case 1: return "mov\t%0, %1";
      case 2: 
       {
        HOST_WIDE_INT target_float[2];
        real_to_target (target_float, CONST_DOUBLE_REAL_VALUE (operands[1]), SFmode);
        if ( TARGET_V2_INSNS 
             && avr32_hi16_immediate_operand (GEN_INT (target_float[0]), VOIDmode) )
           return "movh\t%0, hi(%1)";
        else
           return "mov\t%0, lo(%1)\;orh\t%0, hi(%1)";
       }
      case 3:
        if ( (REG_P(XEXP(operands[1], 0))
              && REGNO(XEXP(operands[1], 0)) == SP_REGNUM)
             || (GET_CODE(XEXP(operands[1], 0)) == PLUS
                 && REGNO(XEXP(XEXP(operands[1], 0), 0)) == SP_REGNUM
	         && GET_CODE(XEXP(XEXP(operands[1], 0), 1)) == CONST_INT
	         && INTVAL(XEXP(XEXP(operands[1], 0), 1)) % 4 == 0
	         && INTVAL(XEXP(XEXP(operands[1], 0), 1)) <= 0x1FC) )
          return "lddsp\t%0, %1";
          else if ( avr32_const_pool_ref_operand(operands[1], GET_MODE(operands[1])) )
          return "lddpc\t%0, %1";
        else
          return "ld.w\t%0, %1";
      case 4:
        if ( (REG_P(XEXP(operands[0], 0))
              && REGNO(XEXP(operands[0], 0)) == SP_REGNUM)
             || (GET_CODE(XEXP(operands[0], 0)) == PLUS
                 && REGNO(XEXP(XEXP(operands[0], 0), 0)) == SP_REGNUM
	         && GET_CODE(XEXP(XEXP(operands[0], 0), 1)) == CONST_INT
	         && INTVAL(XEXP(XEXP(operands[0], 0), 1)) % 4 == 0
	         && INTVAL(XEXP(XEXP(operands[0], 0), 1)) <= 0x1FC) )
          return "stdsp\t%0, %1";
	else
          return "st.w\t%0, %1";
      default:
	abort();
    }
  }

  [(set_attr "length" "2,4,8,4,4")
   (set_attr "type" "alu,alu,alu2,load,store")
   (set_attr "cc" "none,none,clobber,none,none")])



;;== double - 64 bits =========================================================
(define_expand "movdf"
  [(set (match_operand:DF 0 "nonimmediate_operand" "")
	(match_operand:DF 1 "general_operand" ""))]
  ""
  {
    /* One of the ops has to be in a register.  */
    if (GET_CODE (operands[0]) != REG){
      operands[1] = force_reg (DFmode, operands[1]);
    }
  })


(define_insn_and_split "*movdf_internal"
  [(set (match_operand:DF 0 "nonimmediate_operand"     "=r,r,r,r,m")
	(match_operand:DF 1 "general_operand"          " r,G,F,m,r"))]
  "(register_operand (operands[0], DFmode)
       || register_operand (operands[1], DFmode))"
  {
    switch (which_alternative ){
    case 0:
    case 1:
    case 2: 
        return "#";
    case 3:
      if ( avr32_const_pool_ref_operand(operands[1], GET_MODE(operands[1])))
        return "ld.d\t%0, pc[%1 - .]";
      else
        return "ld.d\t%0, %1";
    case 4:
      return "st.d\t%0, %1";
    default:
      abort();
    }
  }
  "reload_completed
   && (REG_P (operands[0]) 
        && (REG_P (operands[1])
            || GET_CODE (operands[1]) == CONST_DOUBLE))"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 2) (match_dup 3))]
  "
   {
    operands[2] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[3] = gen_highpart(SImode, operands[1]);
    operands[1] = gen_lowpart(SImode, operands[1]);
   }
  "

  [(set_attr "length" "*,*,*,4,4")
   (set_attr "type" "*,*,*,load2,store2")
   (set_attr "cc" "*,*,*,none,none")])


;;=============================================================================
;; Conditional Moves
;;=============================================================================
(define_insn "ld<mode>_predicable"
  [(set (match_operand:MOVCC 0 "register_operand" "=r")
	(match_operand:MOVCC 1 "avr32_non_rmw_memory_operand" "<MOVCC:pred_mem_constraint>"))]
  "TARGET_V2_INSNS"
  "ld<MOVCC:load_postfix>%?\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "type" "load")
   (set_attr "predicable" "yes")]
)


(define_insn "st<mode>_predicable"
  [(set (match_operand:MOVCC 0 "avr32_non_rmw_memory_operand" "=<MOVCC:pred_mem_constraint>")
	(match_operand:MOVCC 1 "register_operand" "r"))]
  "TARGET_V2_INSNS"
  "st<MOVCC:store_postfix>%?\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "type" "store")
   (set_attr "predicable" "yes")]
)

(define_insn "mov<mode>_predicable"
  [(set (match_operand:MOVCC 0 "register_operand" "=r")
	(match_operand:MOVCC 1 "avr32_cond_register_immediate_operand" "rKs08"))]
  ""
  "mov%?\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "type" "alu")
   (set_attr "predicable" "yes")]
)


;;=============================================================================
;; Move chunks of memory
;;=============================================================================

(define_expand "movmemsi"
  [(match_operand:BLK 0 "general_operand" "")
   (match_operand:BLK 1 "general_operand" "")
   (match_operand:SI 2 "const_int_operand" "")
   (match_operand:SI 3 "const_int_operand" "")]
  ""
  "
   if (avr32_gen_movmemsi (operands))
     DONE;
   FAIL;
  "
  )




;;=============================================================================
;; Bit field instructions
;;-----------------------------------------------------------------------------
;; Instructions to insert or extract bit-fields
;;=============================================================================

(define_expand "insv"
  [ (set (zero_extract:SI (match_operand:SI 0 "register_operand" "")
                          (match_operand:SI 1 "immediate_operand" "")
                          (match_operand:SI 2 "immediate_operand" ""))
         (match_operand:SI 3 "register_operand" ""))]
  ""
  {
   if ( INTVAL(operands[1]) >= 32 )
      FAIL;
  }
)

(define_insn "insv_internal"
  [ (set (zero_extract:SI (match_operand:SI 0 "register_operand" "+r")
                          (match_operand:SI 1 "immediate_operand" "Ku05")
                          (match_operand:SI 2 "immediate_operand" "Ku05"))
         (match_operand 3 "register_operand" "r"))]
  ""
  "bfins\t%0, %3, %2, %1"
  [(set_attr "type" "alu")
   (set_attr "length" "4")
   (set_attr "cc" "set_ncz")])



(define_expand "extv"
  [ (set (match_operand:SI 0 "register_operand" "")
         (sign_extract:SI (match_operand:SI 1 "register_operand" "")
                          (match_operand:SI 2 "immediate_operand" "")
                          (match_operand:SI 3 "immediate_operand" "")))]
  ""
  {
   if ( INTVAL(operands[2]) >= 32 )
      FAIL;
  }
)

(define_expand "extzv"
  [ (set (match_operand:SI 0 "register_operand" "")
         (zero_extract:SI (match_operand:SI 1 "register_operand" "")
                          (match_operand:SI 2 "immediate_operand" "")
                          (match_operand:SI 3 "immediate_operand" "")))]
  ""
  {
   if ( INTVAL(operands[2]) >= 32 )
      FAIL;
  }
)

(define_insn "extv_internal"
  [ (set (match_operand:SI 0 "register_operand" "=r")
         (sign_extract:SI (match_operand:SI 1 "register_operand" "r")
                          (match_operand:SI 2 "immediate_operand" "Ku05")
                          (match_operand:SI 3 "immediate_operand" "Ku05")))]
  "INTVAL(operands[2]) < 32"
  "bfexts\t%0, %1, %3, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")
   (set_attr "cc" "set_ncz")])


(define_insn "extzv_internal"
  [ (set (match_operand:SI 0 "register_operand" "=r")
         (zero_extract:SI (match_operand:SI 1 "register_operand" "r")
                          (match_operand:SI 2 "immediate_operand" "Ku05")
                          (match_operand:SI 3 "immediate_operand" "Ku05")))]
  "INTVAL(operands[2]) < 32"
  "bfextu\t%0, %1, %3, %2"
  [(set_attr "type" "alu")
   (set_attr "length" "4")
   (set_attr "cc" "set_ncz")])



;;=============================================================================
;; Some peepholes for avoiding unnecessary cast instructions
;; followed by bfins.
;;-----------------------------------------------------------------------------

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (zero_extend:SI (match_operand:QI 1 "register_operand" "")))
   (set (zero_extract:SI (match_operand 2 "register_operand" "")
                         (match_operand:SI 3 "immediate_operand" "")
                         (match_operand:SI 4 "immediate_operand" ""))
        (match_dup 0))]
  "((peep2_reg_dead_p(2, operands[0]) &&
    (INTVAL(operands[3]) <= 8)))"
  [(set (zero_extract:SI (match_dup 2)
                         (match_dup 3)
                         (match_dup 4))
        (match_dup 1))]
  )

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (zero_extend:SI (match_operand:HI 1 "register_operand" "")))
   (set (zero_extract:SI (match_operand 2 "register_operand" "")
                         (match_operand:SI 3 "immediate_operand" "")
                         (match_operand:SI 4 "immediate_operand" ""))
        (match_dup 0))]
  "((peep2_reg_dead_p(2, operands[0]) &&
    (INTVAL(operands[3]) <= 16)))"
  [(set (zero_extract:SI (match_dup 2)
                         (match_dup 3)
                         (match_dup 4))
        (match_dup 1))]
  )

;;=============================================================================
;; push bytes
;;-----------------------------------------------------------------------------
;; Implements the push instruction
;;=============================================================================
(define_insn "pushm"
  [(set (mem:BLK (pre_dec:BLK (reg:SI SP_REGNUM)))
        (unspec:BLK [(match_operand 0 "const_int_operand" "")]
                    UNSPEC_PUSHM))]
  ""
  {
    if (INTVAL(operands[0])) {
      return "pushm\t%r0";
    } else {
      return "";
    }
  }
  [(set_attr "type" "store")
   (set_attr "length" "2")
   (set_attr "cc" "none")])

(define_insn "stm"
  [(unspec [(match_operand 0 "register_operand" "r")
            (match_operand 1 "const_int_operand" "")
            (match_operand 2 "const_int_operand" "")]
	   UNSPEC_STM)]
  ""
  {
    if (INTVAL(operands[1])) {
      if (INTVAL(operands[2]) != 0)
         return "stm\t--%0, %s1";
      else
         return "stm\t%0, %s1";
    } else {
      return "";
    }
  }
  [(set_attr "type" "store")
   (set_attr "length" "4")
   (set_attr "cc" "none")])



(define_insn "popm"
  [(unspec [(match_operand 0 "const_int_operand" "")]
	   UNSPEC_POPM)]
  ""
  {
    if (INTVAL(operands[0])) {
      return "popm   %r0";
    } else {
      return "";
    }
  }
  [(set_attr "type" "load")
   (set_attr "length" "2")])



;;=============================================================================
;; add
;;-----------------------------------------------------------------------------
;; Adds reg1 with reg2 and puts the result in reg0.
;;=============================================================================
(define_insn "add<mode>3"
  [(set (match_operand:INTM 0 "register_operand" "=r,r,r,r,r")
	(plus:INTM (match_operand:INTM 1 "register_operand" "%0,r,0,r,0")
                   (match_operand:INTM 2 "avr32_add_operand" "r,r,Is08,Is16,Is21")))]
  ""
  "@
   add     %0, %2
   add     %0, %1, %2
   sub     %0, %n2
   sub     %0, %1, %n2
   sub     %0, %n2"

  [(set_attr "length" "2,4,2,4,4")
   (set_attr "cc" "<INTM:alu_cc_attr>")])

(define_insn "add<mode>3_lsl"
  [(set (match_operand:INTM 0 "register_operand" "=r")
	(plus:INTM (ashift:INTM (match_operand:INTM 1 "register_operand" "r")
                                (match_operand:INTM 3 "avr32_add_shift_immediate_operand" "Ku02"))
                   (match_operand:INTM 2 "register_operand" "r")))]
  ""
  "add     %0, %2, %1 << %3"
  [(set_attr "length" "4")
   (set_attr "cc" "<INTM:alu_cc_attr>")])

(define_insn "add<mode>3_lsl2"
  [(set (match_operand:INTM 0 "register_operand" "=r")
	(plus:INTM (match_operand:INTM 1 "register_operand" "r")
                   (ashift:INTM (match_operand:INTM 2 "register_operand" "r")
                                (match_operand:INTM 3 "avr32_add_shift_immediate_operand" "Ku02"))))]
  ""
  "add     %0, %1, %2 << %3"
  [(set_attr "length" "4")
   (set_attr "cc" "<INTM:alu_cc_attr>")])


(define_insn "add<mode>3_mul"
  [(set (match_operand:INTM 0 "register_operand" "=r")
	(plus:INTM (mult:INTM (match_operand:INTM 1 "register_operand" "r")
                              (match_operand:INTM 3 "immediate_operand" "Ku04" ))
                   (match_operand:INTM 2 "register_operand" "r")))]
  "(INTVAL(operands[3]) == 0) || (INTVAL(operands[3]) == 2) ||
   (INTVAL(operands[3]) == 4) || (INTVAL(operands[3]) == 8)"
  "add     %0, %2, %1 << %p3"
  [(set_attr "length" "4")
   (set_attr "cc" "<INTM:alu_cc_attr>")])

(define_insn "add<mode>3_mul2"
  [(set (match_operand:INTM 0 "register_operand" "=r")
	(plus:INTM (match_operand:INTM 1 "register_operand" "r")
                   (mult:INTM (match_operand:INTM 2 "register_operand" "r")
                              (match_operand:INTM 3 "immediate_operand" "Ku04" ))))]
  "(INTVAL(operands[3]) == 0) || (INTVAL(operands[3]) == 2) ||
   (INTVAL(operands[3]) == 4) || (INTVAL(operands[3]) == 8)"
  "add     %0, %1, %2 << %p3"
  [(set_attr "length" "4")
   (set_attr "cc" "<INTM:alu_cc_attr>")])


(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (ashift:SI (match_operand:SI 1 "register_operand" "")
                   (match_operand:SI 2 "immediate_operand" "")))
   (set (match_operand:SI 3 "register_operand" "")
	(plus:SI (match_dup 0)
                 (match_operand:SI 4 "register_operand" "")))]
  "(peep2_reg_dead_p(2, operands[0]) &&
    (INTVAL(operands[2]) < 4 && INTVAL(operands[2]) > 0))"
  [(set (match_dup 3)
	(plus:SI (ashift:SI (match_dup 1)
                            (match_dup 2))
                 (match_dup 4)))]
  )

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (ashift:SI (match_operand:SI 1 "register_operand" "")
                   (match_operand:SI 2 "immediate_operand" "")))
   (set (match_operand:SI 3 "register_operand" "")
	(plus:SI (match_operand:SI 4 "register_operand" "")
                 (match_dup 0)))]
  "(peep2_reg_dead_p(2, operands[0]) &&
    (INTVAL(operands[2]) < 4 && INTVAL(operands[2]) > 0))"
  [(set (match_dup 3)
	(plus:SI (ashift:SI (match_dup 1)
                            (match_dup 2))
                 (match_dup 4)))]
  )

(define_insn "adddi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(plus:DI (match_operand:DI 1 "register_operand" "%0,r")
		 (match_operand:DI 2 "register_operand" "r,r")))]
  ""
  "@
   add     %0, %2\;adc    %m0, %m0, %m2
   add     %0, %1, %2\;adc    %m0, %m1, %m2"
  [(set_attr "length" "6,8")
   (set_attr "type" "alu2")
   (set_attr "cc" "set_vncz")])


(define_insn "add<mode>_imm_predicable"
  [(set (match_operand:INTM 0 "register_operand" "+r")
	(plus:INTM (match_dup 0)
                   (match_operand:INTM 1 "avr32_cond_immediate_operand" "%Is08")))]
  ""
  "sub%?\t%0, -%1"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "predicable" "yes")]
)

;;=============================================================================
;; subtract
;;-----------------------------------------------------------------------------
;; Subtract reg2 or immediate value from reg0 and puts the result in reg0.
;;=============================================================================

(define_insn "sub<mode>3"
  [(set (match_operand:INTM 0 "general_operand" "=r,r,r,r,r,r,r")
	(minus:INTM (match_operand:INTM 1 "register_const_int_operand" "0,r,0,r,0,r,Ks08")
		  (match_operand:INTM 2 "register_const_int_operand" "r,r,Ks08,Ks16,Ks21,0,r")))]
  ""
  "@
   sub     %0, %2
   sub     %0, %1, %2
   sub     %0, %2
   sub     %0, %1, %2
   sub     %0, %2
   rsub    %0, %1
   rsub    %0, %2, %1"
  [(set_attr "length" "2,4,2,4,4,2,4")
   (set_attr "cc" "<INTM:alu_cc_attr>")])

(define_insn "*sub<mode>3_mul"
  [(set (match_operand:INTM 0 "register_operand" "=r")
	(minus:INTM (match_operand:INTM 1 "register_operand" "r")
                    (mult:INTM (match_operand:INTM 2 "register_operand" "r")
                               (match_operand:SI 3 "immediate_operand" "Ku04" ))))]
  "(INTVAL(operands[3]) == 0) || (INTVAL(operands[3]) == 2) ||
   (INTVAL(operands[3]) == 4) || (INTVAL(operands[3]) == 8)"
  "sub     %0, %1, %2 << %p3"
  [(set_attr "length" "4")
   (set_attr "cc" "<INTM:alu_cc_attr>")])

(define_insn "*sub<mode>3_lsl"
  [(set (match_operand:INTM 0 "register_operand" "=r")
	(minus:INTM  (match_operand:INTM 1 "register_operand" "r")
                     (ashift:INTM (match_operand:INTM 2 "register_operand" "r")
                                  (match_operand:SI 3 "avr32_add_shift_immediate_operand" "Ku02"))))]
  ""
  "sub     %0, %1, %2 << %3"
  [(set_attr "length" "4")
   (set_attr "cc" "<INTM:alu_cc_attr>")])


(define_insn "subdi3"
  [(set (match_operand:DI 0 "register_operand" "=r,r")
	(minus:DI (match_operand:DI 1 "register_operand" "%0,r")
		 (match_operand:DI 2 "register_operand" "r,r")))]
  ""
  "@
   sub     %0, %2\;sbc    %m0, %m0, %m2
   sub     %0, %1, %2\;sbc    %m0, %m1, %m2"
  [(set_attr "length" "6,8")
   (set_attr "type" "alu2")
   (set_attr "cc" "set_vncz")])


(define_insn "sub<mode>_imm_predicable"
  [(set (match_operand:INTM 0 "register_operand" "+r")
	(minus:INTM (match_dup 0)
                    (match_operand:INTM 1 "avr32_cond_immediate_operand" "Ks08")))]
  ""
  "sub%?\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "predicable" "yes")])

(define_insn "rsub<mode>_imm_predicable"
  [(set (match_operand:INTM 0 "register_operand" "+r")
	(minus:INTM (match_operand:INTM 1 "avr32_cond_immediate_operand"  "Ks08")
                    (match_dup 0)))]
  ""
  "rsub%?\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "predicable" "yes")])

;;=============================================================================
;; multiply
;;-----------------------------------------------------------------------------
;; Multiply op1 and op2 and put the value in op0.
;;=============================================================================


(define_insn "mulqi3"
  [(set (match_operand:QI 0 "register_operand"         "=r,r,r")
	(mult:QI (match_operand:QI 1 "register_operand" "%0,r,r")
		 (match_operand:QI 2 "avr32_mul_operand" "r,r,Ks08")))]
  "!TARGET_NO_MUL_INSNS"
  {
   switch (which_alternative){
    case 0:
      return "mul     %0, %2";
    case 1:
      return "mul     %0, %1, %2";
    case 2:
      return "mul     %0, %1, %2";
    default:
      gcc_unreachable();
   }
  }
  [(set_attr "type" "mulww_w,mulww_w,mulwh")
   (set_attr "length" "2,4,4")
   (set_attr "cc" "none")])

(define_insn "mulsi3"
  [(set (match_operand:SI 0 "register_operand"         "=r,r,r")
	(mult:SI (match_operand:SI 1 "register_operand" "%0,r,r")
		 (match_operand:SI 2 "avr32_mul_operand" "r,r,Ks08")))]
  "!TARGET_NO_MUL_INSNS"
  {
   switch (which_alternative){
    case 0:
      return "mul     %0, %2";
    case 1:
      return "mul     %0, %1, %2";
    case 2:
      return "mul     %0, %1, %2";
    default:
      gcc_unreachable();
   }
  }
  [(set_attr "type" "mulww_w,mulww_w,mulwh")
   (set_attr "length" "2,4,4")
   (set_attr "cc" "none")])


(define_insn "mulhisi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mult:SI
	 (sign_extend:SI (match_operand:HI 1 "register_operand" "%r"))
	 (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "mulhh.w %0, %1:b, %2:b"
  [(set_attr "type" "mulhh")
   (set_attr "length" "4")
   (set_attr "cc" "none")])

(define_peephole2
  [(match_scratch:DI 6 "r")
   (set (match_operand:SI 0 "register_operand" "")
	(mult:SI
	 (sign_extend:SI (match_operand:HI 1 "register_operand" ""))
         (sign_extend:SI (match_operand:HI 2 "register_operand" ""))))
   (set (match_operand:SI 3 "register_operand" "")
        (ashiftrt:SI (match_dup 0)
                     (const_int 16)))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP
   && (peep2_reg_dead_p(1, operands[0]) || (REGNO(operands[0]) == REGNO(operands[3])))"
  [(set (match_dup 4) (sign_extend:SI (match_dup 1)))
   (set (match_dup 6)
        (ashift:DI (mult:DI (sign_extend:DI (match_dup 4))
                            (sign_extend:DI (match_dup 2)))
                   (const_int 16)))
   (set (match_dup 3) (match_dup 5))]

  "{
     operands[4] = gen_rtx_REG(SImode, REGNO(operands[1]));
     operands[5] = gen_highpart (SImode, operands[4]);
   }"
  )

(define_insn "mulnhisi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (mult:SI
         (sign_extend:SI (neg:HI (match_operand:HI 1 "register_operand" "r")))
         (sign_extend:SI (match_operand:HI 2 "register_operand" "r"))))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "mulnhh.w %0, %1:b, %2:b"
  [(set_attr "type" "mulhh")
   (set_attr "length" "4")
   (set_attr "cc" "none")])

(define_insn "machisi3"
  [(set (match_operand:SI 0 "register_operand" "+r")
	(plus:SI (mult:SI
                  (sign_extend:SI (match_operand:HI 1 "register_operand" "%r"))
                  (sign_extend:SI (match_operand:HI 2 "register_operand" "r")))
                 (match_dup 0)))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "machh.w %0, %1:b, %2:b"
  [(set_attr "type" "machh_w")
   (set_attr "length" "4")
   (set_attr "cc" "none")])



(define_insn "mulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI
	 (sign_extend:DI (match_operand:SI 1 "register_operand" "%r"))
	 (sign_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "!TARGET_NO_MUL_INSNS"
  "muls.d  %0, %1, %2"
  [(set_attr "type" "mulww_d")
   (set_attr "length" "4")
   (set_attr "cc" "none")])

(define_insn "umulsidi3"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(mult:DI
	 (zero_extend:DI (match_operand:SI 1 "register_operand" "%r"))
	 (zero_extend:DI (match_operand:SI 2 "register_operand" "r"))))]
  "!TARGET_NO_MUL_INSNS"
  "mulu.d  %0, %1, %2"
  [(set_attr "type" "mulww_d")
   (set_attr "length" "4")
   (set_attr "cc" "none")])

(define_insn "*mulaccsi3"
  [(set (match_operand:SI 0 "register_operand" "+r")
	(plus:SI (mult:SI (match_operand:SI 1 "register_operand" "%r")
			  (match_operand:SI 2 "register_operand" "r"))
		 (match_dup 0)))]
  "!TARGET_NO_MUL_INSNS"
  "mac     %0, %1, %2"
  [(set_attr "type" "macww_w")
   (set_attr "length" "4")
   (set_attr "cc" "none")])

(define_insn "*mulaccsidi3"
  [(set (match_operand:DI 0 "register_operand" "+r")
	(plus:DI (mult:DI
		  (sign_extend:DI (match_operand:SI 1 "register_operand" "%r"))
		  (sign_extend:DI (match_operand:SI 2 "register_operand" "r")))
		 (match_dup 0)))]
  "!TARGET_NO_MUL_INSNS"
  "macs.d  %0, %1, %2"
  [(set_attr "type" "macww_d")
   (set_attr "length" "4")
   (set_attr "cc" "none")])

(define_insn "*umulaccsidi3"
  [(set (match_operand:DI 0 "register_operand" "+r")
	(plus:DI (mult:DI
		  (zero_extend:DI (match_operand:SI 1 "register_operand" "%r"))
		  (zero_extend:DI (match_operand:SI 2 "register_operand" "r")))
		 (match_dup 0)))]
  "!TARGET_NO_MUL_INSNS"
  "macu.d  %0, %1, %2"
  [(set_attr "type" "macww_d")
   (set_attr "length" "4")
   (set_attr "cc" "none")])



;; Try to avoid Write-After-Write hazards for mul operations
;; if it can be done
(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(mult:SI
	 (sign_extend:SI (match_operand 1 "general_operand" ""))
         (sign_extend:SI (match_operand 2 "general_operand" ""))))
   (set (match_dup 0)
	(match_operator:SI 3 "alu_operator" [(match_dup 0)
                                             (match_operand 4 "general_operand" "")]))]
  "peep2_reg_dead_p(1, operands[2])"
  [(set (match_dup 5)
        (mult:SI
         (sign_extend:SI (match_dup 1))
         (sign_extend:SI (match_dup 2))))
   (set (match_dup 0)
	(match_op_dup 3 [(match_dup 5)
                         (match_dup 4)]))]
  "{operands[5] = gen_rtx_REG(SImode, REGNO(operands[2]));}"
  )



;;=============================================================================
;; DSP instructions
;;=============================================================================
(define_insn "mulsathh_h"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (ss_truncate:HI (ashiftrt:SI (mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "%r"))
                                              (sign_extend:SI (match_operand:HI 2 "register_operand" "r")))
                                     (const_int 15))))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "mulsathh.h\t%0, %1:b, %2:b"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "mulhh")])

(define_insn "mulsatrndhh_h"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (ss_truncate:HI (ashiftrt:SI
                         (plus:SI (mult:SI (sign_extend:SI (match_operand:HI 1 "register_operand" "%r"))
                                           (sign_extend:SI (match_operand:HI 2 "register_operand" "r")))
                                  (const_int 1073741824))
                         (const_int 15))))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "mulsatrndhh.h\t%0, %1:b, %2:b"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "mulhh")])

(define_insn "mulsathh_w"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ss_truncate:SI (ashift:DI (mult:DI (sign_extend:DI (match_operand:HI 1 "register_operand" "%r"))
                                            (sign_extend:DI (match_operand:HI 2 "register_operand" "r")))
                                   (const_int 1))))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "mulsathh.w\t%0, %1:b, %2:b"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "mulhh")])

(define_insn "mulsatwh_w"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ss_truncate:SI (ashiftrt:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
                                              (sign_extend:DI (match_operand:HI 2 "register_operand" "r")))
                                     (const_int 15))))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "mulsatwh.w\t%0, %1, %2:b"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "mulwh")])

(define_insn "mulsatrndwh_w"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ss_truncate:SI (ashiftrt:DI (plus:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
                                                       (sign_extend:DI (match_operand:HI 2 "register_operand" "r")))
                                              (const_int 1073741824))
                                     (const_int 15))))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "mulsatrndwh.w\t%0, %1, %2:b"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "mulwh")])

(define_insn "macsathh_w"
  [(set (match_operand:SI 0 "register_operand" "+r")
        (plus:SI (match_dup 0)
                 (ss_truncate:SI (ashift:DI (mult:DI (sign_extend:DI (match_operand:HI 1 "register_operand" "%r"))
                                                     (sign_extend:DI (match_operand:HI 2 "register_operand" "r")))
                                            (const_int 1)))))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "macsathh.w\t%0, %1:b, %2:b"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "mulhh")])


(define_insn "mulwh_d"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (ashift:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r"))
                            (sign_extend:DI (match_operand:HI 2 "register_operand" "r")))
                   (const_int 16)))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "mulwh.d\t%0, %1, %2:b"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "mulwh")])


(define_insn "mulnwh_d"
  [(set (match_operand:DI 0 "register_operand" "=r")
        (ashift:DI (mult:DI (not:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "r")))
                            (sign_extend:DI (match_operand:HI 2 "register_operand" "r")))
                   (const_int 16)))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "mulnwh.d\t%0, %1, %2:b"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "mulwh")])

(define_insn "macwh_d"
  [(set (match_operand:DI 0 "register_operand" "+r")
        (plus:DI (match_dup 0)
                 (ashift:DI (mult:DI (sign_extend:DI (match_operand:SI 1 "register_operand" "%r"))
                                     (sign_extend:DI (match_operand:HI 2 "register_operand" "r")))
                            (const_int 16))))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "macwh.d\t%0, %1, %2:b"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "mulwh")])

(define_insn "machh_d"
  [(set (match_operand:DI 0 "register_operand" "+r")
        (plus:DI (match_dup 0)
                 (mult:DI (sign_extend:DI (match_operand:HI 1 "register_operand" "%r"))
                          (sign_extend:DI (match_operand:HI 2 "register_operand" "r")))))]
  "!TARGET_NO_MUL_INSNS && TARGET_DSP"
  "machh.d\t%0, %1:b, %2:b"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "mulwh")])

(define_insn "satadd_w"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ss_plus:SI (match_operand:SI 1 "register_operand" "r")
                    (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_DSP"
  "satadd.w\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "alu_sat")])

(define_insn "satsub_w"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (ss_minus:SI (match_operand:SI 1 "register_operand" "r")
                     (match_operand:SI 2 "register_operand" "r")))]
  "TARGET_DSP"
  "satsub.w\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "alu_sat")])

(define_insn "satadd_h"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (ss_plus:HI (match_operand:HI 1 "register_operand" "r")
                    (match_operand:HI 2 "register_operand" "r")))]
  "TARGET_DSP"
  "satadd.h\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "alu_sat")])

(define_insn "satsub_h"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (ss_minus:HI (match_operand:HI 1 "register_operand" "r")
                     (match_operand:HI 2 "register_operand" "r")))]
  "TARGET_DSP"
  "satsub.h\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "cc" "none")
   (set_attr "type" "alu_sat")])


;;=============================================================================
;; smin
;;-----------------------------------------------------------------------------
;; Set reg0 to the smallest value of reg1 and reg2. It is used for signed
;; values in the registers.
;;=============================================================================
(define_insn "sminsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(smin:SI (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "register_operand" "r")))]
  ""
  "min     %0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "cc" "none")])

;;=============================================================================
;; smax
;;-----------------------------------------------------------------------------
;; Set reg0 to the largest value of reg1 and reg2. It is used for signed
;; values in the registers.
;;=============================================================================
(define_insn "smaxsi3"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(smax:SI (match_operand:SI 1 "register_operand" "r")
		 (match_operand:SI 2 "register_operand" "r")))]
  ""
  "max     %0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "cc" "none")])



;;=============================================================================
;; Logical operations
;;-----------------------------------------------------------------------------


;; Split up simple DImode logical operations.  Simply perform the logical
;; operation on the upper and lower halves of the registers.
(define_split
  [(set (match_operand:DI 0 "register_operand" "")
	(match_operator:DI 6 "logical_binary_operator"
	  [(match_operand:DI 1 "register_operand" "")
	   (match_operand:DI 2 "register_operand" "")]))]
  "reload_completed"
  [(set (match_dup 0) (match_op_dup:SI 6 [(match_dup 1) (match_dup 2)]))
   (set (match_dup 3) (match_op_dup:SI 6 [(match_dup 4) (match_dup 5)]))]
  "
  {
    operands[3] = gen_highpart (SImode, operands[0]);
    operands[0] = gen_lowpart (SImode, operands[0]);
    operands[4] = gen_highpart (SImode, operands[1]);
    operands[1] = gen_lowpart (SImode, operands[1]);
    operands[5] = gen_highpart (SImode, operands[2]);
    operands[2] = gen_lowpart (SImode, operands[2]);
  }"
)

;;=============================================================================
;; Logical operations with shifted operand
;;=============================================================================
(define_insn "<code>si_lshift"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (logical:SI (match_operator:SI 4 "logical_shift_operator"
                                       [(match_operand:SI 2 "register_operand" "r")
                                        (match_operand:SI 3 "immediate_operand" "Ku05")])
                    (match_operand:SI 1 "register_operand" "r")))]
  ""
  {
   if ( GET_CODE(operands[4]) == ASHIFT )
      return "<logical_insn>\t%0, %1, %2 << %3";
   else
      return "<logical_insn>\t%0, %1, %2 >> %3";
      }

  [(set_attr "cc" "set_z")]
)


;;************************************************
;; Peepholes for detecting logical operantions
;; with shifted operands
;;************************************************

(define_peephole
  [(set (match_operand:SI 3 "register_operand" "")
        (match_operator:SI 5 "logical_shift_operator"
                           [(match_operand:SI 1 "register_operand" "")
                            (match_operand:SI 2 "immediate_operand" "")]))
   (set (match_operand:SI 0 "register_operand" "")
        (logical:SI (match_operand:SI 4 "register_operand" "")
                    (match_dup 3)))]
  "(dead_or_set_p(insn, operands[3])) || (REGNO(operands[3]) == REGNO(operands[0]))"
  {
   if ( GET_CODE(operands[5]) == ASHIFT )
      return "<logical_insn>\t%0, %4, %1 << %2";
   else
      return "<logical_insn>\t%0, %4, %1 >> %2";
  }
  [(set_attr "cc" "set_z")]
  )

(define_peephole
  [(set (match_operand:SI 3 "register_operand" "")
        (match_operator:SI 5 "logical_shift_operator"
                           [(match_operand:SI 1 "register_operand" "")
                            (match_operand:SI 2 "immediate_operand" "")]))
   (set (match_operand:SI 0 "register_operand" "")
        (logical:SI (match_dup 3)
                    (match_operand:SI 4 "register_operand" "")))]
  "(dead_or_set_p(insn, operands[3])) || (REGNO(operands[3]) == REGNO(operands[0]))"
  {
   if ( GET_CODE(operands[5]) == ASHIFT )
      return "<logical_insn>\t%0, %4, %1 << %2";
   else
      return "<logical_insn>\t%0, %4, %1 >> %2";
  }
  [(set_attr "cc" "set_z")]
  )


(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (match_operator:SI 5 "logical_shift_operator"
                           [(match_operand:SI 1 "register_operand" "")
                            (match_operand:SI 2 "immediate_operand" "")]))
   (set (match_operand:SI 3 "register_operand" "")
        (logical:SI (match_operand:SI 4 "register_operand" "")
                    (match_dup 0)))]
  "(peep2_reg_dead_p(2, operands[0])) || (REGNO(operands[3]) == REGNO(operands[0]))"

  [(set (match_dup 3)
        (logical:SI  (match_op_dup:SI 5 [(match_dup 1) (match_dup 2)])
                     (match_dup 4)))]

  ""
)

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (match_operator:SI 5 "logical_shift_operator"
                           [(match_operand:SI 1 "register_operand" "")
                            (match_operand:SI 2 "immediate_operand" "")]))
   (set (match_operand:SI 3 "register_operand" "")
        (logical:SI (match_dup 0)
                    (match_operand:SI 4 "register_operand" "")))]
  "(peep2_reg_dead_p(2, operands[0])) || (REGNO(operands[3]) == REGNO(operands[0]))"

  [(set (match_dup 3)
        (logical:SI (match_op_dup:SI 5 [(match_dup 1) (match_dup 2)])
                    (match_dup 4)))]

  ""
)


;;=============================================================================
;; and
;;-----------------------------------------------------------------------------
;; Store the result after a bitwise logical-and between reg0 and reg2 in reg0.
;;=============================================================================

(define_insn "andnsi"
  [(set (match_operand:SI 0 "register_operand" "+r")
        (and:SI (match_dup 0)
                (not:SI (match_operand:SI 1 "register_operand" "r"))))]
  ""
  "andn    %0, %1"
  [(set_attr "cc" "set_z")
   (set_attr "length" "2")]
)


(define_insn "andsi3"
   [(set (match_operand:SI 0 "avr32_rmw_memory_or_register_operand"          "=Y,r,r,r,   r,   r,r,r,r,r")
 	(and:SI (match_operand:SI 1 "avr32_rmw_memory_or_register_operand"  "%0,r,0,0,   0,   0,0,0,0,r" )
 		(match_operand:SI 2 "nonmemory_operand"                     " N,M,N,Ku16,Ks17,J,L,r,i,r")))]
  ""
   "@
    memc\t%0, %z2
    bfextu\t%0, %1, 0, %z2
    cbr\t%0, %z2
    andl\t%0, %2, COH
    andl\t%0, lo(%2)
    andh\t%0, hi(%2), COH
    andh\t%0, hi(%2)
    and\t%0, %2
    andh\t%0, hi(%2)\;andl\t%0, lo(%2)
    and\t%0, %1, %2"

   [(set_attr "length" "4,4,2,4,4,4,4,2,8,4")
    (set_attr "cc" "none,set_z,set_z,set_z,set_z,set_z,set_z,set_z,set_z,set_z")])

  

(define_insn "anddi3"
  [(set (match_operand:DI 0 "register_operand" "=&r,&r")
	(and:DI (match_operand:DI 1 "register_operand" "%0,r")
                (match_operand:DI 2 "register_operand" "r,r")))]
  ""
  "#"
  [(set_attr "length" "8")
   (set_attr "cc" "clobber")]
)

;;=============================================================================
;; or
;;-----------------------------------------------------------------------------
;; Store the result after a bitwise inclusive-or between reg0 and reg2 in reg0.
;;=============================================================================

(define_insn "iorsi3"
  [(set (match_operand:SI 0 "avr32_rmw_memory_or_register_operand"          "=Y,r,r,   r,r,r,r")
	(ior:SI (match_operand:SI 1 "avr32_rmw_memory_or_register_operand"  "%0,0,0,   0,0,0,r" )
		(match_operand:SI 2 "nonmemory_operand"                     " O,O,Ku16,J,r,i,r")))]
  ""
  "@
   mems\t%0, %p2
   sbr\t%0, %p2
   orl\t%0, %2
   orh\t%0, hi(%2)
   or\t%0, %2
   orh\t%0, hi(%2)\;orl\t%0, lo(%2)
   or\t%0, %1, %2"

  [(set_attr "length" "4,2,4,4,2,8,4")
   (set_attr "cc" "none,set_z,set_z,set_z,set_z,set_z,set_z")])


(define_insn "iordi3"
  [(set (match_operand:DI 0 "register_operand" "=&r,&r")
	(ior:DI (match_operand:DI 1 "register_operand" "%0,r")
                (match_operand:DI 2 "register_operand" "r,r")))]
  ""
  "#"
  [(set_attr "length" "8")
   (set_attr "cc" "clobber")]
)

;;=============================================================================
;; xor bytes
;;-----------------------------------------------------------------------------
;; Store the result after a bitwise exclusive-or between reg0 and reg2 in reg0.
;;=============================================================================

(define_insn "xorsi3"
   [(set (match_operand:SI 0 "avr32_rmw_memory_or_register_operand"          "=Y,r,   r,r,r,r")
 	(xor:SI (match_operand:SI 1 "avr32_rmw_memory_or_register_operand"  "%0,0,   0,0,0,r" )
 		(match_operand:SI 2 "nonmemory_operand"                     " O,Ku16,J,r,i,r")))]
  ""
   "@
    memt\t%0, %p2
    eorl\t%0, %2
    eorh\t%0, hi(%2)
    eor\t%0, %2
    eorh\t%0, hi(%2)\;eorl\t%0, lo(%2)
    eor\t%0, %1, %2"

   [(set_attr "length" "4,4,4,2,8,4")
    (set_attr "cc" "none,set_z,set_z,set_z,set_z,set_z")])

(define_insn "xordi3"
  [(set (match_operand:DI 0 "register_operand" "=&r,&r")
	(xor:DI (match_operand:DI 1 "register_operand" "%0,r")
                (match_operand:DI 2 "register_operand" "r,r")))]
  ""
  "#"
  [(set_attr "length" "8")
   (set_attr "cc" "clobber")]
)

;;=============================================================================
;; Three operand predicable insns
;;=============================================================================

(define_insn "<predicable_insn3><mode>_predicable"
  [(set (match_operand:INTM 0 "register_operand" "=r")
	(predicable_op3:INTM (match_operand:INTM 1 "register_operand" "<predicable_commutative3>r")
                             (match_operand:INTM 2 "register_operand" "r")))]
  "TARGET_V2_INSNS"
  "<predicable_insn3>%?\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "predicable" "yes")]
)

(define_insn_and_split "<predicable_insn3><mode>_imm_clobber_predicable"
  [(parallel 
    [(set (match_operand:INTM 0 "register_operand" "=r")
          (predicable_op3:INTM (match_operand:INTM 1 "register_operand" "<predicable_commutative3>r")
                               (match_operand:INTM 2 "avr32_mov_immediate_operand" "JKs21")))
     (clobber (match_operand:INTM 3 "register_operand" "=&r"))])]
  "TARGET_V2_INSNS"
  {
   if ( current_insn_predicate != NULL_RTX ) 
      {
       if ( avr32_const_ok_for_constraint_p (INTVAL (operands[2]), 'K', "Ks08") )
          return "%! mov%?\t%3, %2\;<predicable_insn3>%?\t%0, %1, %3";
       else if ( avr32_const_ok_for_constraint_p (INTVAL (operands[2]), 'K', "Ks21") )
          return "%! mov\t%3, %2\;<predicable_insn3>%?\t%0, %1, %3";
       else
          return "%! movh\t%3, hi(%2)\;<predicable_insn3>%?\t%0, %1, %3";
       }
   else
      {
       if ( !avr32_cond_imm_clobber_splittable (insn, operands) )
          {
                if ( avr32_const_ok_for_constraint_p (INTVAL (operands[2]), 'K', "Ks08") )
                   return "mov%?\t%3, %2\;<predicable_insn3>%?\t%0, %1, %3";
                else if ( avr32_const_ok_for_constraint_p (INTVAL (operands[2]), 'K', "Ks21") )
                   return "mov\t%3, %2\;<predicable_insn3>%?\t%0, %1, %3";
                else
                   return "movh\t%3, hi(%2)\;<predicable_insn3>%?\t%0, %1, %3";
          }
       return "#";
      }
      
  }
  ;; If we find out that we could not actually do if-conversion on the block
  ;; containing this insn we convert it back to normal immediate format
  ;; to avoid outputing a redundant move insn
  ;; Do not split until after we have checked if we can make the insn 
  ;; conditional.
  "(GET_CODE (PATTERN (insn)) != COND_EXEC
    && cfun->machine->ifcvt_after_reload
    && avr32_cond_imm_clobber_splittable (insn, operands))"
  [(set (match_dup 0)
        (predicable_op3:INTM (match_dup 1)
                             (match_dup 2)))]
  ""
  [(set_attr "length" "8")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "predicable" "yes")]
  )


;;=============================================================================
;; Zero extend predicable insns
;;=============================================================================
(define_insn_and_split "zero_extendhisi_clobber_predicable"
  [(parallel 
    [(set (match_operand:SI 0 "register_operand" "=r")
          (zero_extend:SI (match_operand:HI 1 "register_operand" "r")))
     (clobber (match_operand:SI 2 "register_operand" "=&r"))])]
  "TARGET_V2_INSNS"
  {
   if ( current_insn_predicate != NULL_RTX ) 
      {
         return "%! mov\t%2, 0xffff\;and%?\t%0, %1, %2";
       }
   else
      {
       return "#";
      }
      
  }
  ;; If we find out that we could not actually do if-conversion on the block
  ;; containing this insn we convert it back to normal immediate format
  ;; to avoid outputing a redundant move insn
  ;; Do not split until after we have checked if we can make the insn 
  ;; conditional.
  "(GET_CODE (PATTERN (insn)) != COND_EXEC
    && cfun->machine->ifcvt_after_reload)"
  [(set (match_dup 0)
        (zero_extend:SI (match_dup 1)))]
  ""
  [(set_attr "length" "8")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "predicable" "yes")]
  )

(define_insn_and_split "zero_extendqisi_clobber_predicable"
  [(parallel 
    [(set (match_operand:SI 0 "register_operand" "=r")
          (zero_extend:SI (match_operand:QI 1 "register_operand" "r")))
     (clobber (match_operand:SI 2 "register_operand" "=&r"))])]
  "TARGET_V2_INSNS"
  {
   if ( current_insn_predicate != NULL_RTX ) 
      {
         return "%! mov\t%2, 0xff\;and%?\t%0, %1, %2";
       }
   else
      {
       return "#";
      }
      
  }
  ;; If we find out that we could not actually do if-conversion on the block
  ;; containing this insn we convert it back to normal immediate format
  ;; to avoid outputing a redundant move insn
  ;; Do not split until after we have checked if we can make the insn 
  ;; conditional.
  "(GET_CODE (PATTERN (insn)) != COND_EXEC
    && cfun->machine->ifcvt_after_reload)"
  [(set (match_dup 0)
        (zero_extend:SI (match_dup 1)))]
  ""
  [(set_attr "length" "8")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "predicable" "yes")]
  )

(define_insn_and_split "zero_extendqihi_clobber_predicable"
  [(parallel 
    [(set (match_operand:HI 0 "register_operand" "=r")
          (zero_extend:HI (match_operand:QI 1 "register_operand" "r")))
     (clobber (match_operand:SI 2 "register_operand" "=&r"))])]
  "TARGET_V2_INSNS"
  {
   if ( current_insn_predicate != NULL_RTX ) 
      {
         return "%! mov\t%2, 0xff\;and%?\t%0, %1, %2";
       }
   else
      {
       return "#";
      }
      
  }
  ;; If we find out that we could not actually do if-conversion on the block
  ;; containing this insn we convert it back to normal immediate format
  ;; to avoid outputing a redundant move insn
  ;; Do not split until after we have checked if we can make the insn 
  ;; conditional.
  "(GET_CODE (PATTERN (insn)) != COND_EXEC
    && cfun->machine->ifcvt_after_reload)"
  [(set (match_dup 0)
        (zero_extend:HI (match_dup 1)))]
  ""
  [(set_attr "length" "8")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "predicable" "yes")]
  )
;;=============================================================================
;; divmod
;;-----------------------------------------------------------------------------
;; Signed division that produces both a quotient and a remainder.
;;=============================================================================

(define_expand "divmodsi4"
  [(parallel [
     (parallel [
       (set (match_operand:SI 0 "register_operand" "=r")
	    (div:SI (match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "register_operand" "r")))
       (set (match_operand:SI 3 "register_operand" "=r")
	    (mod:SI (match_dup 1)
		    (match_dup 2)))])
     (use (match_dup 4))])]
  ""
  {
    if (can_create_pseudo_p ()) {
      operands[4] = gen_reg_rtx (DImode);
      emit_insn(gen_divmodsi4_internal(operands[4],operands[1],operands[2]));
      emit_move_insn(operands[0], gen_rtx_SUBREG( SImode, operands[4], 4));
      emit_move_insn(operands[3], gen_rtx_SUBREG( SImode, operands[4], 0));
      DONE;
    } else {
      FAIL;
    }
  })


(define_insn "divmodsi4_internal"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "register_operand" "r")]
		   UNSPEC_DIVMODSI4_INTERNAL))]
  ""
  "divs    %0, %1, %2"
  [(set_attr "type" "div")
   (set_attr "cc" "none")])


;;=============================================================================
;; udivmod
;;-----------------------------------------------------------------------------
;; Unsigned division that produces both a quotient and a remainder.
;;=============================================================================
(define_expand "udivmodsi4"
 [(parallel [
    (parallel [
      (set (match_operand:SI 0 "register_operand" "=r")
	   (udiv:SI (match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "register_operand" "r")))
      (set (match_operand:SI 3 "register_operand" "=r")
	   (umod:SI (match_dup 1)
		    (match_dup 2)))])
    (use (match_dup 4))])]
  ""
  {
    if (can_create_pseudo_p ()) {
      operands[4] = gen_reg_rtx (DImode);

      emit_insn(gen_udivmodsi4_internal(operands[4],operands[1],operands[2]));
      emit_move_insn(operands[0], gen_rtx_SUBREG( SImode, operands[4], 4));
      emit_move_insn(operands[3], gen_rtx_SUBREG( SImode, operands[4], 0));

      DONE;
    } else {
      FAIL;
    }
  })

(define_insn "udivmodsi4_internal"
  [(set (match_operand:DI 0 "register_operand" "=r")
	(unspec:DI [(match_operand:SI 1 "register_operand" "r")
		    (match_operand:SI 2 "register_operand" "r")]
		   UNSPEC_UDIVMODSI4_INTERNAL))]
  ""
  "divu    %0, %1, %2"
  [(set_attr "type" "div")
   (set_attr "cc" "none")])


;;=============================================================================
;; Arithmetic-shift left
;;-----------------------------------------------------------------------------
;; Arithmetic-shift reg0 left by reg2 or immediate value.
;;=============================================================================

(define_insn "ashlsi3"
  [(set (match_operand:SI 0 "register_operand"                      "=r,r,r")
	(ashift:SI (match_operand:SI 1 "register_operand"           "r,0,r")
		   (match_operand:SI 2 "register_const_int_operand" "r,Ku05,Ku05")))]
  ""
  "@
   lsl     %0, %1, %2
   lsl     %0, %2
   lsl     %0, %1, %2"
  [(set_attr "length" "4,2,4")
   (set_attr "cc" "set_ncz")])

;;=============================================================================
;; Arithmetic-shift right
;;-----------------------------------------------------------------------------
;; Arithmetic-shift reg0 right by an immediate value.
;;=============================================================================

(define_insn "ashrsi3"
  [(set (match_operand:SI 0 "register_operand"                        "=r,r,r")
	(ashiftrt:SI (match_operand:SI 1 "register_operand"           "r,0,r")
		     (match_operand:SI 2 "register_const_int_operand" "r,Ku05,Ku05")))]
  ""
  "@
   asr     %0, %1, %2
   asr     %0, %2
   asr     %0, %1, %2"
  [(set_attr "length" "4,2,4")
   (set_attr "cc" "set_ncz")])

;;=============================================================================
;; Logical shift right
;;-----------------------------------------------------------------------------
;; Logical shift reg0 right by an immediate value.
;;=============================================================================

(define_insn "lshrsi3"
  [(set (match_operand:SI 0 "register_operand"                        "=r,r,r")
	(lshiftrt:SI (match_operand:SI 1 "register_operand"           "r,0,r")
		     (match_operand:SI 2 "register_const_int_operand" "r,Ku05,Ku05")))]
  ""
  "@
   lsr     %0, %1, %2
   lsr     %0, %2
   lsr     %0, %1, %2"
  [(set_attr "length" "4,2,4")
   (set_attr "cc" "set_ncz")])


;;=============================================================================
;; neg
;;-----------------------------------------------------------------------------
;; Negate operand 1 and store the result in operand 0.
;;=============================================================================
(define_insn "negsi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(neg:SI (match_operand:SI 1 "register_operand" "0,r")))]
  ""
  "@
   neg\t%0
   rsub\t%0, %1, 0"
  [(set_attr "length" "2,4")
   (set_attr "cc" "set_vncz")])

(define_insn "negsi2_predicable"
  [(set (match_operand:SI 0 "register_operand" "+r")
	(neg:SI (match_dup 0)))]
  "TARGET_V2_INSNS"
  "rsub%?\t%0, 0"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "predicable" "yes")])

;;=============================================================================
;; abs
;;-----------------------------------------------------------------------------
;; Store the absolute value of operand 1 into operand 0.
;;=============================================================================
(define_insn "abssi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(abs:SI (match_operand:SI 1 "register_operand" "0")))]
  ""
  "abs\t%0"
  [(set_attr "length" "2")
   (set_attr "cc" "set_z")])


;;=============================================================================
;; one_cmpl
;;-----------------------------------------------------------------------------
;; Store the bitwise-complement of operand 1 into operand 0.
;;=============================================================================

(define_insn "one_cmplsi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r")
	(not:SI (match_operand:SI 1 "register_operand" "0,r")))]
  ""
  "@
   com\t%0
   rsub\t%0, %1, -1"
  [(set_attr "length" "2,4")
   (set_attr "cc" "set_z")])


(define_insn "one_cmplsi2_predicable"
  [(set (match_operand:SI 0 "register_operand" "+r")
	(not:SI (match_dup 0)))]
  "TARGET_V2_INSNS"
  "rsub%?\t%0, -1"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "predicable" "yes")])


;;=============================================================================
;; Bit load
;;-----------------------------------------------------------------------------
;; Load a bit into Z and C flags
;;=============================================================================
(define_insn "bldsi"
  [(set (cc0)
        (and:SI (match_operand:SI 0 "register_operand" "r")
                (match_operand:SI 1 "one_bit_set_operand" "i")))]
  ""
  "bld\t%0, %p1"
  [(set_attr "length" "4")
   (set_attr "cc" "bld")]
  )


;;=============================================================================
;; Compare
;;-----------------------------------------------------------------------------
;; Compare reg0 with reg1 or an immediate value.
;;=============================================================================

(define_expand "cmp<mode>"
  [(set (cc0)
	(compare:CMP
	 (match_operand:CMP 0 "register_operand" "")
	 (match_operand:CMP 1 "<CMP:cmp_predicate>"  "")))]
  ""
  "{
   avr32_compare_op0 = operands[0];
   avr32_compare_op1 = operands[1];
  }"
)

(define_insn "cmp<mode>_internal"
  [(set (cc0)
        (compare:CMP
         (match_operand:CMP 0 "register_operand" "r")
         (match_operand:CMP 1 "<CMP:cmp_predicate>" "<CMP:cmp_constraint>")))]
  ""
  {
   /* Check if the next insn already will output a compare. */
   if (!next_insn_emits_cmp (insn))  
     set_next_insn_cond(insn,
                        avr32_output_cmp(get_next_insn_cond(insn), GET_MODE (operands[0]), operands[0], operands[1]));
   return "";
  }
  [(set_attr "length" "4")
   (set_attr "cc" "compare")])

(define_expand "cmpsf"
  [(set (cc0)
	(compare:SF
	 (match_operand:SF 0 "general_operand" "")
	 (match_operand:SF 1 "general_operand"  "")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "{
   rtx tmpreg;
   if ( !REG_P(operands[0]) )
     operands[0] = force_reg(SFmode, operands[0]);

   if ( !REG_P(operands[1]) )
     operands[1] = force_reg(SFmode, operands[1]);

   avr32_compare_op0 = operands[0];
   avr32_compare_op1 = operands[1];
   emit_insn(gen_cmpsf_internal_uc3fp(operands[0], operands[1]));
   DONE;
  }"
)

;;;=============================================================================
;; Test if zero
;;-----------------------------------------------------------------------------
;; Compare reg against zero and set the condition codes.
;;=============================================================================


(define_expand "tstsi"
  [(set (cc0)
	(match_operand:SI 0 "register_operand" ""))]
  ""
  {
   avr32_compare_op0 = operands[0];
   avr32_compare_op1 = const0_rtx;
  }
)

(define_insn "tstsi_internal"
  [(set (cc0)
	(match_operand:SI 0 "register_operand" "r"))]
  ""
  {
   /* Check if the next insn already will output a compare. */
   if (!next_insn_emits_cmp (insn))  
     set_next_insn_cond(insn,
                        avr32_output_cmp(get_next_insn_cond(insn), SImode, operands[0], const0_rtx));

   return "";
  }
  [(set_attr "length" "2")
   (set_attr "cc" "compare")])


(define_expand "tstdi"
  [(set (cc0)
	(match_operand:DI 0 "register_operand" ""))]
  ""
  {
   avr32_compare_op0 = operands[0];
   avr32_compare_op1 = const0_rtx;
  }
)

(define_insn "tstdi_internal"
  [(set (cc0)
	(match_operand:DI 0 "register_operand" "r"))]
  ""
  {
   /* Check if the next insn already will output a compare. */
   if (!next_insn_emits_cmp (insn))  
     set_next_insn_cond(insn,
                        avr32_output_cmp(get_next_insn_cond(insn), DImode, operands[0], const0_rtx));
   return "";
  }
  [(set_attr "length" "4")
   (set_attr "type" "alu2")
   (set_attr "cc" "compare")])



;;=============================================================================
;; Convert operands
;;-----------------------------------------------------------------------------
;;
;;=============================================================================
(define_insn "truncdisi2"
  [(set (match_operand:SI 0 "general_operand" "")
	(truncate:SI (match_operand:DI 1 "general_operand" "")))]
  ""
  "truncdisi2")

;;=============================================================================
;; Extend
;;-----------------------------------------------------------------------------
;;
;;=============================================================================


(define_insn "extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(sign_extend:SI (match_operand:HI 1 "nonimmediate_operand" "0,r,<RKu00>,m")))]
  ""
  {
   switch ( which_alternative ){
     case 0:
       return    "casts.h\t%0";
     case 1:
       return    "bfexts\t%0, %1, 0, 16";
     case 2:
     case 3:
       return    "ld.sh\t%0, %1";
     default:
       abort();
   }
  }
  [(set_attr "length" "2,4,2,4")
   (set_attr "cc" "set_ncz,set_ncz,none,none")
   (set_attr "type" "alu,alu,load_rm,load_rm")])

(define_insn "extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(sign_extend:SI (match_operand:QI 1 "extendqi_operand" "0,r,RKu00,m")))]
  ""
  {
   switch ( which_alternative ){
     case 0:
       return    "casts.b\t%0";
     case 1:
       return    "bfexts\t%0, %1, 0, 8";
     case 2:
     case 3:
       return    "ld.sb\t%0, %1";
     default:
       abort();
   }
  }
  [(set_attr "length" "2,4,2,4")
   (set_attr "cc" "set_ncz,set_ncz,none,none")
   (set_attr "type" "alu,alu,load_rm,load_rm")])

(define_insn "extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,r")
	(sign_extend:HI (match_operand:QI 1 "extendqi_operand" "0,r,RKu00,m")))]
  ""
  {
   switch ( which_alternative ){
     case 0:
       return    "casts.b\t%0";
     case 1:
       return    "bfexts\t%0, %1, 0, 8";
     case 2:
     case 3:
       return    "ld.sb\t%0, %1";
     default:
       abort();
   }
  }
  [(set_attr "length" "2,4,2,4")
   (set_attr "cc" "set_ncz,set_ncz,none,none")
   (set_attr "type" "alu,alu,load_rm,load_rm")])


;;=============================================================================
;; Zero-extend
;;-----------------------------------------------------------------------------
;;
;;=============================================================================

(define_insn "zero_extendhisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(zero_extend:SI (match_operand:HI 1 "nonimmediate_operand" "0,r,<RKu00>,m")))]
  ""
  {
   switch ( which_alternative ){
     case 0:
       return    "castu.h\t%0";
     case 1:
       return    "bfextu\t%0, %1, 0, 16";
     case 2:
     case 3:
       return    "ld.uh\t%0, %1";
     default:
       abort();
   }
  }

  [(set_attr "length" "2,4,2,4")
   (set_attr "cc" "set_ncz,set_ncz,none,none")
   (set_attr "type" "alu,alu,load_rm,load_rm")])

(define_insn "zero_extendqisi2"
  [(set (match_operand:SI 0 "register_operand" "=r,r,r,r")
	(zero_extend:SI (match_operand:QI 1 "nonimmediate_operand" "0,r,<RKu00>,m")))]
  ""
  {
   switch ( which_alternative ){
     case 0:
       return    "castu.b\t%0";
     case 1:
       return    "bfextu\t%0, %1, 0, 8";
     case 2:
     case 3:
       return    "ld.ub\t%0, %1";
     default:
       abort();
   }
  }
  [(set_attr "length" "2,4,2,4")
   (set_attr "cc" "set_ncz, set_ncz, none, none")
   (set_attr "type" "alu, alu, load_rm, load_rm")])

(define_insn "zero_extendqihi2"
  [(set (match_operand:HI 0 "register_operand" "=r,r,r,r")
	(zero_extend:HI (match_operand:QI 1 "nonimmediate_operand" "0,r,<RKu00>,m")))]
  ""
  {
   switch ( which_alternative ){
     case 0:
       return    "castu.b\t%0";
     case 1:
       return    "bfextu\t%0, %1, 0, 8";
     case 2:
     case 3:
       return    "ld.ub\t%0, %1";
     default:
       abort();
   }
  }
  [(set_attr "length" "2,4,2,4")
   (set_attr "cc" "set_ncz, set_ncz, none, none")
   (set_attr "type" "alu, alu, load_rm, load_rm")])


;;=============================================================================
;; Conditional load and extend insns
;;=============================================================================
(define_insn "ldsi<mode>_predicable_se"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (sign_extend:SI 
         (match_operand:INTM 1 "memory_operand" "<INTM:pred_mem_constraint>")))]
  "TARGET_V2_INSNS"
  "ld<INTM:load_postfix_s>%?\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "type" "load")
   (set_attr "predicable" "yes")]
)

(define_insn "ldsi<mode>_predicable_ze"
  [(set (match_operand:SI 0 "register_operand" "=r")
        (zero_extend:SI 
         (match_operand:INTM 1 "memory_operand" "<INTM:pred_mem_constraint>")))]
  "TARGET_V2_INSNS"
  "ld<INTM:load_postfix_u>%?\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "type" "load")
   (set_attr "predicable" "yes")]
)

(define_insn "ldhi_predicable_ze"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (zero_extend:HI 
         (match_operand:QI 1 "memory_operand" "RKs10")))]
  "TARGET_V2_INSNS"
  "ld.ub%?\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "type" "load")
   (set_attr "predicable" "yes")]
)

(define_insn "ldhi_predicable_se"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (sign_extend:HI 
         (match_operand:QI 1 "memory_operand" "RKs10")))]
  "TARGET_V2_INSNS"
  "ld.sb%?\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "cc" "cmp_cond_insn")
   (set_attr "type" "load")
   (set_attr "predicable" "yes")]
)

;;=============================================================================
;; Conditional set register
;; sr{cond4}  rd
;;-----------------------------------------------------------------------------

;;Because of the same issue as with conditional moves and adds we must
;;not separate the compare instrcution from the scc instruction as
;;they might be sheduled "badly".

(define_insn "s<code>"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(any_cond:SI (cc0)
                     (const_int 0)))]
  ""
  "sr<cond>\t%0"
  [(set_attr "length" "2")
   (set_attr "cc" "none")])

(define_insn "smi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(cc0)
                    (const_int 0)] UNSPEC_COND_MI))]
  ""
  "srmi\t%0"
  [(set_attr "length" "2")
   (set_attr "cc" "none")])

(define_insn "spl"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(cc0)
                    (const_int 0)] UNSPEC_COND_PL))]
  ""
  "srpl\t%0"
  [(set_attr "length" "2")
   (set_attr "cc" "none")])


;;=============================================================================
;; Conditional branch
;;-----------------------------------------------------------------------------
;; Branch to label if the specified condition codes are set.
;;=============================================================================
; branch if negative
(define_insn "bmi"
  [(set (pc)
	(if_then_else (unspec:CC [(cc0) (const_int 0)] UNSPEC_COND_MI)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "brmi    %0"
  [(set_attr "type" "branch")
   (set (attr "length")
	(cond [(and (le (minus (match_dup 0) (pc)) (const_int 254))
		    (le (minus (pc) (match_dup 0)) (const_int 256)))
	       (const_int 2)] ; use compact branch
              (const_int 4))) ; use extended branch
   (set_attr "cc" "none")])

(define_insn "*bmi-reverse"
  [(set (pc)
	(if_then_else (unspec:CC [(cc0) (const_int 0)] UNSPEC_COND_MI)
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "brpl    %0"
  [(set_attr "type" "branch")
   (set (attr "length")
	(cond [(and (le (minus (match_dup 0) (pc)) (const_int 254))
		    (le (minus (pc) (match_dup 0)) (const_int 256)))
	       (const_int 2)] ; use compact branch
              (const_int 4))) ; use extended branch
   (set_attr "cc" "none")])

; branch if positive
(define_insn "bpl"
  [(set (pc)
	(if_then_else (unspec:CC [(cc0) (const_int 0)] UNSPEC_COND_PL)
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "brpl    %0"
  [(set_attr "type" "branch")
   (set (attr "length")
	(cond [(and (le (minus (match_dup 0) (pc)) (const_int 254))
		    (le (minus (pc) (match_dup 0)) (const_int 256)))
	       (const_int 2)] ; use compact branch
              (const_int 4))) ; use extended branch
   (set_attr "cc" "none")])

(define_insn "*bpl-reverse"
  [(set (pc)
	(if_then_else (unspec:CC [(cc0) (const_int 0)] UNSPEC_COND_PL)
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "brmi    %0"
  [(set_attr "type" "branch")
   (set (attr "length")
	(cond [(and (le (minus (match_dup 0) (pc)) (const_int 254))
		    (le (minus (pc) (match_dup 0)) (const_int 256)))
	       (const_int 2)] ; use compact branch
              (const_int 4))) ; use extended branch
   (set_attr "cc" "none")])

; branch if equal
(define_insn "b<code>"
  [(set (pc)
	(if_then_else (any_cond:CC (cc0)
			  (const_int 0))
		      (label_ref (match_operand 0 "" ""))
		      (pc)))]
  ""
  "br<cond>    %0 "
  [(set_attr "type" "branch")
   (set (attr "length")
	(cond [(and (le (minus (match_dup 0) (pc)) (const_int 254))
		    (le (minus (pc) (match_dup 0)) (const_int 256)))
	       (const_int 2)] ; use compact branch
              (const_int 4))) ; use extended branch
   (set_attr "cc" "none")])


(define_insn "*b<code>-reverse"
  [(set (pc)
	(if_then_else (any_cond:CC (cc0)
			  (const_int 0))
		      (pc)
		      (label_ref (match_operand 0 "" ""))))]
  ""
  "br<invcond>    %0 "
  [(set_attr "type" "branch")
   (set (attr "length")
	(cond [(and (le (minus (match_dup 0) (pc)) (const_int 254))
		    (le (minus (pc) (match_dup 0)) (const_int 256)))
	       (const_int 2)] ; use compact branch
              (const_int 4))) ; use extended branch
   (set_attr "cc" "none")])



;=============================================================================
; Conditional Add/Subtract
;-----------------------------------------------------------------------------
; sub{cond4}  Rd, imm
;=============================================================================


(define_expand "add<mode>cc"
  [(set (match_operand:ADDCC 0 "register_operand" "")
        (if_then_else:ADDCC (match_operator 1 "avr32_comparison_operator" 
                                            [(match_dup 4)
                                             (match_dup 5)])
                            (match_operand:ADDCC 2 "register_operand" "")
                            (plus:ADDCC 
                             (match_dup 2)
                             (match_operand:ADDCC 3 "" ""))))]
  ""
  {
   if ( !(GET_CODE (operands[3]) == CONST_INT
          || (TARGET_V2_INSNS && REG_P(operands[3]))) ){
      FAIL;
   }

   /* Delete compare instruction as it is merged into this instruction */
   remove_insn (get_last_insn_anywhere ());

   operands[4] = avr32_compare_op0;
   operands[5] = avr32_compare_op1;
   
   if ( TARGET_V2_INSNS 
        && REG_P(operands[3]) 
        && REGNO(operands[0]) != REGNO(operands[2]) ){
       emit_move_insn (operands[0], operands[2]);
       operands[2] = operands[0];
   }
  }
  )

(define_insn "add<ADDCC:mode>cc_cmp<CMP:mode>_reg"
  [(set (match_operand:ADDCC 0 "register_operand" "=r")
        (if_then_else:ADDCC (match_operator 1 "avr32_comparison_operator" 
                                            [(match_operand:CMP 4 "register_operand" "r")
                                             (match_operand:CMP 5 "<CMP:cmp_predicate>" "<CMP:cmp_constraint>")])
                            (match_dup 0)
                            (plus:ADDCC 
                             (match_operand:ADDCC 2 "register_operand" "r")
                             (match_operand:ADDCC 3 "register_operand" "r"))))]
  "TARGET_V2_INSNS"
  {
   operands[1] = avr32_output_cmp(operands[1], GET_MODE(operands[4]), operands[4], operands[5]);
   return "add%i1\t%0, %2, %3";
  }
  [(set_attr "length" "8")
   (set_attr "cc" "cmp_cond_insn")])

(define_insn "add<ADDCC:mode>cc_cmp<CMP:mode>"
  [(set (match_operand:ADDCC 0 "register_operand" "=r")
        (if_then_else:ADDCC (match_operator 1 "avr32_comparison_operator" 
                                            [(match_operand:CMP 4 "register_operand" "r")
                                             (match_operand:CMP 5 "<CMP:cmp_predicate>" "<CMP:cmp_constraint>")])
                            (match_operand:ADDCC 2 "register_operand" "0")
                            (plus:ADDCC 
                             (match_dup 2)
                             (match_operand:ADDCC 3 "avr32_cond_immediate_operand" "Is08"))))]
  ""
  {
   operands[1] = avr32_output_cmp(operands[1], GET_MODE(operands[4]), operands[4], operands[5]);
   return "sub%i1\t%0, -%3";
  }
  [(set_attr "length" "8")
   (set_attr "cc" "cmp_cond_insn")])

;=============================================================================
; Conditional Move
;-----------------------------------------------------------------------------
; mov{cond4}  Rd, (Rs/imm)
;=============================================================================
(define_expand "mov<mode>cc"
  [(set (match_operand:MOVCC 0 "register_operand" "")
        (if_then_else:MOVCC (match_operator 1 "avr32_comparison_operator" 
                                            [(match_dup 4)
                                             (match_dup 5)])
                            (match_operand:MOVCC 2 "avr32_cond_register_immediate_operand" "")
                            (match_operand:MOVCC 3 "avr32_cond_register_immediate_operand" "")))]
  ""
  {
   /* Delete compare instruction as it is merged into this instruction */
   remove_insn (get_last_insn_anywhere ());

   operands[4] = avr32_compare_op0;
   operands[5] = avr32_compare_op1;
  }
  )


(define_insn "mov<MOVCC:mode>cc_cmp<CMP:mode>"
  [(set (match_operand:MOVCC 0 "register_operand" "=r,r,r")
        (if_then_else:MOVCC (match_operator 1 "avr32_comparison_operator" 
                                            [(match_operand:CMP 4 "register_operand" "r,r,r")
                                             (match_operand:CMP 5 "<CMP:cmp_predicate>" "<CMP:cmp_constraint>,<CMP:cmp_constraint>,<CMP:cmp_constraint>")])
                            (match_operand:MOVCC 2 "avr32_cond_register_immediate_operand" "0, rKs08,rKs08")
                            (match_operand:MOVCC 3 "avr32_cond_register_immediate_operand" "rKs08,0,rKs08")))]
  ""
  {
   operands[1] = avr32_output_cmp(operands[1], GET_MODE(operands[4]), operands[4], operands[5]);
           
   switch( which_alternative ){
    case 0:
      return "mov%i1    %0, %3";
    case 1:
      return "mov%1    %0, %2";
    case 2:
      return "mov%1    %0, %2\;mov%i1    %0, %3";
    default:
      abort();
    }

  }
  [(set_attr "length" "8,8,12")
   (set_attr "cc" "cmp_cond_insn")])

  


;;=============================================================================
;; jump
;;-----------------------------------------------------------------------------
;; Jump inside a function; an unconditional branch to a label.
;;=============================================================================
(define_insn "jump"
  [(set (pc)
	(label_ref (match_operand 0 "" "")))]
  ""
  {
    if (get_attr_length(insn) > 4)
      return "Can't jump this far";
    return (get_attr_length(insn) == 2 ?
	    "rjmp    %0" : "bral    %0");
  }
  [(set_attr "type" "branch")
   (set (attr "length")
	(cond [(and (le (minus (match_dup 0) (pc)) (const_int 1022))
		    (le (minus (pc) (match_dup 0)) (const_int 1024)))
	       (const_int 2) ; use rjmp
	       (le (match_dup 0) (const_int 1048575))
	       (const_int 4)] ; use bral
	      (const_int 8))) ; do something else
   (set_attr "cc" "none")])

;;=============================================================================
;; call
;;-----------------------------------------------------------------------------
;; Subroutine call instruction returning no value.
;;=============================================================================
(define_insn "call_internal"
  [(parallel [(call (mem:SI (match_operand:SI 0 "avr32_call_operand" "r,U,T,W"))
                    (match_operand 1 "" ""))
              (clobber (reg:SI LR_REGNUM))])]
  ""
  {

    /* Check for a flashvault call. */
    if (avr32_flashvault_call (SYMBOL_REF_DECL (operands[0])))
      {
        /* Assembly is already emitted. */
        return "";
      }

    switch (which_alternative) {
      case 0:
        return "icall\t%0";
      case 1:
        return "rcall\t%0";
      case 2:
        return "mcall\t%0";
      case 3:
        if (TARGET_HAS_ASM_ADDR_PSEUDOS)
          return "call\t%0";
        else
          return "mcall\tr6[%0@got]";
      default:
        abort();
    }
  }
  [(set_attr "type" "call")
   (set_attr "length" "2,4,4,10")
   (set_attr "cc" "clobber")])


(define_expand "call"
  [(parallel [(call (match_operand:SI 0 "" "")
                    (match_operand 1 "" ""))
              (clobber (reg:SI LR_REGNUM))])]
  ""
  {
    rtx call_address;
    if ( GET_CODE(operands[0]) != MEM )
      FAIL;

    call_address = XEXP(operands[0], 0);

   /* If assembler supports call pseudo insn and the call address is a symbol then nothing special needs to be done. */
    if (TARGET_HAS_ASM_ADDR_PSEUDOS && (GET_CODE(call_address) == SYMBOL_REF) )
    {
       /* We must however mark the function as using the GOT if flag_pic is set, since the call insn might turn into a mcall using the GOT ptr register. */
       if (flag_pic)
       {
          current_function_uses_pic_offset_table = 1;
          emit_call_insn(gen_call_internal(call_address, operands[1]));
          DONE;
       }
    } 
    else 
    {
      if (flag_pic && GET_CODE(call_address) == SYMBOL_REF )
      {
        current_function_uses_pic_offset_table = 1;
        emit_call_insn(gen_call_internal(call_address, operands[1]));
        DONE;
      }

      if (!SYMBOL_REF_RCALL_FUNCTION_P(operands[0]) )
      {
        if (optimize_size && GET_CODE(call_address) == SYMBOL_REF )
        {
          call_address = force_const_mem(SImode, call_address);
        } 
        else 
        {
          call_address = force_reg(SImode, call_address);
        }
      }
    }
    emit_call_insn(gen_call_internal(call_address, operands[1]));
    DONE;

  }
)

;;=============================================================================
;; call_value
;;-----------------------------------------------------------------------------
;; Subroutine call instruction returning a value.
;;=============================================================================
(define_expand "call_value"
   [(parallel [(set (match_operand:SI 0 "" "")
                    (call (match_operand:SI 1 "" "")
                          (match_operand 2 "" "")))
               (clobber (reg:SI LR_REGNUM))])]
   ""
   {
    rtx call_address;
    if ( GET_CODE(operands[1]) != MEM )
      FAIL;

    call_address = XEXP(operands[1], 0);

   /* Check for a flashvault call. 
   if (GET_CODE (call_address) == SYMBOL_REF 
       && avr32_flashvault_call (SYMBOL_REF_DECL (call_address)))
     DONE;  
     
    */ 

    /* If assembler supports call pseudo insn and the call
       address is a symbol then nothing special needs to be done. */
    if ( TARGET_HAS_ASM_ADDR_PSEUDOS
         && (GET_CODE(call_address) == SYMBOL_REF) ){
       /* We must however mark the function as using the GOT if
          flag_pic is set, since the call insn might turn into
          a mcall using the GOT ptr register. */
       if ( flag_pic ) {
          current_function_uses_pic_offset_table = 1;
          emit_call_insn(gen_call_value_internal(operands[0], call_address, operands[2]));
          DONE;
       }
    } else {
      if ( flag_pic &&
           GET_CODE(call_address) == SYMBOL_REF ){
        current_function_uses_pic_offset_table = 1;
        emit_call_insn(gen_call_value_internal(operands[0], call_address, operands[2]));
        DONE;
      }

      if ( !SYMBOL_REF_RCALL_FUNCTION_P(operands[1]) ){
        if ( optimize_size &&
             GET_CODE(call_address) == SYMBOL_REF){
          call_address = force_const_mem(SImode, call_address);
        } else {
          call_address = force_reg(SImode, call_address);
        }
      }
    }
    emit_call_insn(gen_call_value_internal(operands[0], call_address,
                                           operands[2]));
    DONE;

   })

(define_insn "call_value_internal"
  [(parallel [(set (match_operand 0 "register_operand" "=r,r,r,r")
                   (call (mem:SI (match_operand:SI 1 "avr32_call_operand" "r,U,T,W"))
                         (match_operand 2 "" "")))
              (clobber (reg:SI LR_REGNUM))])]
  ;; Operand 2 not used on the AVR32.
  ""
  {
    /* Check for a flashvault call. */
    if (avr32_flashvault_call (SYMBOL_REF_DECL (operands[1])))
      {
        /* Assembly is already emitted. */
        return "";
      }


    switch (which_alternative) {
      case 0:
        return "icall\t%1";
      case 1:
        return "rcall\t%1";
      case 2:
        return "mcall\t%1";
      case 3:
        if ( TARGET_HAS_ASM_ADDR_PSEUDOS )
          return "call\t%1";
        else
          return "mcall\tr6[%1@got]";
      default:
        abort();
    }
  }
  [(set_attr "type" "call")
   (set_attr "length" "2,4,4,10")
   (set_attr "cc" "call_set")])


;;=============================================================================
;; untyped_call
;;-----------------------------------------------------------------------------
;; Subrutine call instruction returning a value of any type.
;; The code is copied from m68k.md (except gen_blockage is removed)
;; Fixme!
;;=============================================================================
(define_expand "untyped_call"
  [(parallel [(call (match_operand 0 "avr32_call_operand" "")
		    (const_int 0))
	      (match_operand 1 "" "")
	      (match_operand 2 "" "")])]
  ""
  {
    int i;

    emit_call_insn (GEN_CALL (operands[0], const0_rtx, NULL, const0_rtx));

    for (i = 0; i < XVECLEN (operands[2], 0); i++) {
      rtx set = XVECEXP (operands[2], 0, i);
      emit_move_insn (SET_DEST (set), SET_SRC (set));
    }

    /* The optimizer does not know that the call sets the function value
       registers we stored in the result block.  We avoid problems by
       claiming that all hard registers are used and clobbered at this
       point.  */
    emit_insn (gen_blockage ());

    DONE;
  })


;;=============================================================================
;; return
;;=============================================================================

(define_insn "return"
  [(return)]
  "USE_RETURN_INSN (FALSE)"
  {
   avr32_output_return_instruction(TRUE, FALSE, NULL, NULL);
   return "";
  }
  [(set_attr "length" "4")
   (set_attr "type" "call")]
  )


(define_insn "return_cond"
  [(set (pc) 
        (if_then_else (match_operand 0 "avr32_comparison_operand" "")
                      (return)
                      (pc)))]
  "USE_RETURN_INSN (TRUE)"
  "ret%0\tr12";
  [(set_attr "type" "call")])
  
(define_insn "return_cond_predicable"
  [(return)]
  "USE_RETURN_INSN (TRUE)"
  "ret%?\tr12";
  [(set_attr "type" "call")
   (set_attr "predicable" "yes")])


(define_insn "return_imm"
  [(parallel [(set (reg RETVAL_REGNUM) (match_operand 0 "immediate_operand" "i"))
              (use (reg RETVAL_REGNUM))
              (return)])]
  "USE_RETURN_INSN (FALSE) &&
   ((INTVAL(operands[0]) == -1) || (INTVAL(operands[0]) == 0) || (INTVAL(operands[0]) == 1))"
  {
   avr32_output_return_instruction(TRUE, FALSE, NULL, operands[0]);
   return "";
  }
  [(set_attr "length" "4")
   (set_attr "type" "call")]
  )

(define_insn "return_imm_cond"
  [(parallel [(set (reg RETVAL_REGNUM) (match_operand 0 "immediate_operand" "i"))
              (use (reg RETVAL_REGNUM))
              (set (pc) 
                   (if_then_else (match_operand 1 "avr32_comparison_operand" "")
                                 (return)
                                 (pc)))])]
  "USE_RETURN_INSN (TRUE) &&
   ((INTVAL(operands[0]) == -1) || (INTVAL(operands[0]) == 0) || (INTVAL(operands[0]) == 1))"
  "ret%1\t%0";
  [(set_attr "type" "call")]
  )

(define_insn "return_imm_predicable"
  [(parallel [(set (reg RETVAL_REGNUM) (match_operand 0 "immediate_operand" "i"))
              (use (reg RETVAL_REGNUM))
              (return)])]
  "USE_RETURN_INSN (TRUE) &&
   ((INTVAL(operands[0]) == -1) || (INTVAL(operands[0]) == 0) || (INTVAL(operands[0]) == 1))"
  "ret%?\t%0";
  [(set_attr "type" "call")
   (set_attr "predicable" "yes")])

(define_insn "return_<mode>reg"
  [(set (reg RETVAL_REGNUM) (match_operand:MOVM 0 "register_operand" "r"))
   (use (reg RETVAL_REGNUM))
   (return)]
  "USE_RETURN_INSN (TRUE)"
  "ret%?\t%0";
  [(set_attr "type" "call")
   (set_attr "predicable" "yes")])

(define_insn "return_<mode>reg_cond"
  [(set (reg RETVAL_REGNUM) (match_operand:MOVM 0 "register_operand" "r"))
   (use (reg RETVAL_REGNUM))
   (set (pc) 
        (if_then_else (match_operator 1 "avr32_comparison_operator"
                                      [(cc0) (const_int 0)])
                      (return)
                      (pc)))]
  "USE_RETURN_INSN (TRUE)"
  "ret%1\t%0";
  [(set_attr "type" "call")])
  
;;=============================================================================
;; nonlocal_goto_receiver
;;-----------------------------------------------------------------------------
;; For targets with a return stack we must make sure to flush the return stack
;; since it will be corrupt after a nonlocal goto.
;;=============================================================================
(define_expand "nonlocal_goto_receiver"
  [(const_int 0)]
  "TARGET_RETURN_STACK"
  "
   {
    emit_insn ( gen_frs() );
    DONE;
   }
  "
  )


;;=============================================================================
;; builtin_setjmp_receiver
;;-----------------------------------------------------------------------------
;; For pic code we need to reload the pic register.
;; For targets with a return stack we must make sure to flush the return stack
;; since it will probably be corrupted.
;;=============================================================================
(define_expand "builtin_setjmp_receiver"
  [(label_ref (match_operand 0 "" ""))]
  "flag_pic"
  "
   {
    if ( TARGET_RETURN_STACK ) 
     emit_insn ( gen_frs() );

    avr32_load_pic_register ();
    DONE;
   }
  "
)


;;=============================================================================
;; indirect_jump
;;-----------------------------------------------------------------------------
;; Jump to an address in reg or memory.
;;=============================================================================
(define_expand "indirect_jump"
  [(set (pc)
	(match_operand:SI 0 "register_operand" ""))]
  ""
  {
    /* One of the ops has to be in a register.  */
    if ( (flag_pic || TARGET_HAS_ASM_ADDR_PSEUDOS )
         && !avr32_legitimate_pic_operand_p(operands[0]) )
      operands[0] = legitimize_pic_address (operands[0], SImode, 0);
    else if ( flag_pic && avr32_address_operand(operands[0], GET_MODE(operands[0])) )
      /* If we have an address operand then this function uses the pic register. */
      current_function_uses_pic_offset_table = 1;
  })


(define_insn "indirect_jump_internal"
  [(set (pc)
	(match_operand:SI 0 "register_operand" "r,m,W"))]
  ""
  {
    switch( which_alternative ){
      case 0:
        return "mov\tpc, %0";
      case 1:
        if ( avr32_const_pool_ref_operand(operands[0], GET_MODE(operands[0])) )
          return "lddpc\tpc, %0";
        else
          return "ld.w\tpc, %0";
      case 2:
        if ( flag_pic )
          return "ld.w\tpc, r6[%0@got]";
        else
          return "lda.w\tpc, %0";
      default:
	abort();
    }
   }
  [(set_attr "length" "2,4,8")
   (set_attr "type" "call,call,call")
   (set_attr "cc" "none,none,clobber")])



;;=============================================================================
;; casesi and tablejump
;;=============================================================================
(define_insn "tablejump_add"
  [(set (pc)
	(plus:SI (match_operand:SI 0 "register_operand" "r")
                 (mult:SI (match_operand:SI 1 "register_operand" "r")
                          (match_operand:SI 2 "immediate_operand" "Ku04" ))))
   (use (label_ref (match_operand 3 "" "")))]
  "flag_pic &&
   ((INTVAL(operands[2]) == 0) || (INTVAL(operands[2]) == 2) ||
    (INTVAL(operands[2]) == 4) || (INTVAL(operands[2]) == 8))"
  "add\tpc, %0, %1 << %p2"
  [(set_attr "length" "4")
   (set_attr "cc" "clobber")])

(define_insn "tablejump_insn"
  [(set (pc) (match_operand:SI 0 "memory_operand" "m"))
   (use (label_ref (match_operand 1 "" "")))]
  "!flag_pic"
  "ld.w\tpc, %0"
  [(set_attr "length" "4")
   (set_attr "type" "call")
   (set_attr "cc" "none")])

(define_expand "casesi"
  [(match_operand:SI 0 "register_operand" "")	; index to jump on
   (match_operand:SI 1 "const_int_operand" "")	; lower bound
   (match_operand:SI 2 "const_int_operand" "")	; total range
   (match_operand:SI 3 "" "")			; table label
   (match_operand:SI 4 "" "")]			; Out of range label
  ""
  "
  {
    rtx reg;
    rtx index = operands[0];
    rtx low_bound = operands[1];
    rtx range = operands[2];
    rtx table_label = operands[3];
    rtx oor_label = operands[4];

    index = force_reg ( SImode, index );
    if (low_bound != const0_rtx)
      {
        if (!avr32_const_ok_for_constraint_p(INTVAL (low_bound), 'I', \"Is21\")){
          reg = force_reg(SImode, GEN_INT (INTVAL (low_bound)));
	  emit_insn (gen_subsi3 (reg, index,
			         reg));
        } else {
          reg = gen_reg_rtx (SImode);
          emit_insn (gen_addsi3 (reg, index,
		         	 GEN_INT (-INTVAL (low_bound))));
        }
	index = reg;
      }

    if (!avr32_const_ok_for_constraint_p (INTVAL (range), 'K', \"Ks21\"))
      range = force_reg (SImode, range);

    emit_cmp_and_jump_insns ( index, range, GTU, NULL_RTX, SImode, 1, oor_label );
    reg = gen_reg_rtx (SImode);
    emit_move_insn ( reg, gen_rtx_LABEL_REF (VOIDmode, table_label));

    if ( flag_pic ) 
       emit_jump_insn ( gen_tablejump_add ( reg, index, GEN_INT(4), table_label));
    else
       emit_jump_insn ( 
           gen_tablejump_insn ( gen_rtx_MEM ( SImode, 
                                              gen_rtx_PLUS ( SImode, 
                                                             reg, 
                                                             gen_rtx_MULT ( SImode, 
                                                                            index, 
                                                                            GEN_INT(4)))),
                                table_label));
    DONE;
  }"
)



(define_insn "prefetch"
  [(prefetch (match_operand:SI 0 "avr32_ks16_address_operand" "p")
	     (match_operand 1 "const_int_operand" "")
	     (match_operand 2 "const_int_operand" ""))]
  ""
  {
     return "pref\t%0";
  }

  [(set_attr "length" "4")
   (set_attr "type" "load")
   (set_attr "cc" "none")])



;;=============================================================================
;; prologue
;;-----------------------------------------------------------------------------
;; This pattern, if defined, emits RTL for entry to a function. The function
;; entry i responsible for setting up the stack frame, initializing the frame
;; pointer register, saving callee saved registers, etc.
;;=============================================================================
(define_expand "prologue"
  [(clobber (const_int 0))]
  ""
  "
  avr32_expand_prologue();
  DONE;
  "
  )

;;=============================================================================
;; eh_return
;;-----------------------------------------------------------------------------
;; This pattern, if defined, affects the way __builtin_eh_return, and
;; thence the call frame exception handling library routines, are
;; built. It is intended to handle non-trivial actions needed along
;; the abnormal return path.
;;
;; The address of the exception handler to which the function should
;; return is passed as operand to this pattern. It will normally need
;; to copied by the pattern to some special register or memory
;; location. If the pattern needs to determine the location of the
;; target call frame in order to do so, it may use
;; EH_RETURN_STACKADJ_RTX, if defined; it will have already been
;; assigned.
;;
;; If this pattern is not defined, the default action will be to
;; simply copy the return address to EH_RETURN_HANDLER_RTX. Either
;; that macro or this pattern needs to be defined if call frame
;; exception handling is to be used.

;; We can't expand this before we know where the link register is stored.
(define_insn_and_split "eh_return"
  [(unspec_volatile [(match_operand:SI 0 "register_operand" "r")]
		    VUNSPEC_EH_RETURN)
   (clobber (match_scratch:SI 1 "=&r"))]
  ""
  "#"
  "reload_completed"
  [(const_int 0)]
  "
  {
    avr32_set_return_address (operands[0], operands[1]);
    DONE;
  }"
  )


;;=============================================================================
;; ffssi2
;;-----------------------------------------------------------------------------
(define_insn "ffssi2"
  [ (set (match_operand:SI 0 "register_operand" "=r")
         (ffs:SI (match_operand:SI 1 "register_operand" "r"))) ]
  ""
  "mov    %0, %1
   brev   %0
   clz    %0, %0
   sub    %0, -1
   cp     %0, 33
   moveq  %0, 0"
  [(set_attr "length" "18")
   (set_attr "cc" "clobber")]
  )



;;=============================================================================
;; swap_h
;;-----------------------------------------------------------------------------
(define_insn "*swap_h"
  [ (set (match_operand:SI 0 "register_operand" "=r")
         (ior:SI (ashift:SI (match_dup 0) (const_int 16))
                 (lshiftrt:SI (match_dup 0) (const_int 16))))]
  ""
  "swap.h    %0"
  [(set_attr "length" "2")]
  )

(define_insn_and_split "bswap_16"
  [ (set (match_operand:HI 0 "avr32_bswap_operand" "=r,RKs13,r")
         (ior:HI (and:HI (lshiftrt:HI (match_operand:HI 1 "avr32_bswap_operand" "r,r,RKs13")
                                      (const_int 8))
                         (const_int 255))
                 (ashift:HI (and:HI (match_dup 1)
                                    (const_int 255))
                            (const_int 8))))]
  ""
  {
   switch ( which_alternative ){
     case 0:
       if ( REGNO(operands[0]) == REGNO(operands[1]))
         return "swap.bh\t%0";
       else
         return "mov\t%0, %1\;swap.bh\t%0";
     case 1:
       return "stswp.h\t%0, %1";
     case 2:
       return "ldswp.sh\t%0, %1";
     default:
       abort();
     }
  }

  "(reload_completed &&
     REG_P(operands[0]) && REG_P(operands[1])
     && (REGNO(operands[0]) != REGNO(operands[1])))"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0)
        (ior:HI (and:HI (lshiftrt:HI (match_dup 0)
                                     (const_int 8))
                        (const_int 255))
                (ashift:HI (and:HI (match_dup 0)
                                   (const_int 255))
                           (const_int 8))))]
  ""

  [(set_attr "length" "4,4,4")
   (set_attr "type" "alu,store,load_rm")]
  )

(define_insn_and_split "bswap_32"
  [ (set (match_operand:SI 0 "avr32_bswap_operand" "=r,RKs14,r")
         (ior:SI (ior:SI (lshiftrt:SI (and:SI (match_operand:SI 1 "avr32_bswap_operand" "r,r,RKs14")
                                              (const_int -16777216))
                                      (const_int 24))
                         (lshiftrt:SI (and:SI (match_dup 1)
                                              (const_int 16711680))
                                      (const_int 8)))
                 (ior:SI (ashift:SI (and:SI (match_dup 1)
                                            (const_int 65280))
                                    (const_int 8))
                         (ashift:SI (and:SI (match_dup 1)
                                            (const_int 255))
                                    (const_int 24)))))]
  ""
  {
    switch ( which_alternative ){
     case 0:
       if ( REGNO(operands[0]) == REGNO(operands[1]))
         return "swap.b\t%0";
       else
         return "#";
     case 1:
       return "stswp.w\t%0, %1";
     case 2:
       return "ldswp.w\t%0, %1";
     default:
       abort();
    }
  }
  "(reload_completed &&
    REG_P(operands[0]) && REG_P(operands[1])
    && (REGNO(operands[0]) != REGNO(operands[1])))"
  [(set (match_dup 0) (match_dup 1))
   (set (match_dup 0)
        (ior:SI (ior:SI (lshiftrt:SI (and:SI (match_dup 0)
                                             (const_int -16777216))
                                     (const_int 24))
                        (lshiftrt:SI (and:SI (match_dup 0)
                                             (const_int 16711680))
                                     (const_int 8)))
                (ior:SI (ashift:SI (and:SI (match_dup 0)
                                           (const_int 65280))
                                   (const_int 8))
                        (ashift:SI (and:SI (match_dup 0)
                                           (const_int 255))
                                   (const_int 24)))))]
  ""

  [(set_attr "length" "4,4,4")
   (set_attr "type" "alu,store,load_rm")]
  )


;;=============================================================================
;; blockage
;;-----------------------------------------------------------------------------
;; UNSPEC_VOLATILE is considered to use and clobber all hard registers and
;; all of memory.  This blocks insns from being moved across this point.

(define_insn "blockage"
  [(unspec_volatile [(const_int 0)] VUNSPEC_BLOCKAGE)]
  ""
  ""
  [(set_attr "length" "0")]
)

;;=============================================================================
;; clzsi2
;;-----------------------------------------------------------------------------
(define_insn "clzsi2"
  [ (set (match_operand:SI 0 "register_operand" "=r")
         (clz:SI (match_operand:SI 1 "register_operand" "r"))) ]
  ""
  "clz    %0, %1"
  [(set_attr "length" "4")
   (set_attr "cc" "set_z")]
  )

;;=============================================================================
;; ctzsi2
;;-----------------------------------------------------------------------------
(define_insn "ctzsi2"
  [ (set (match_operand:SI 0 "register_operand" "=r,r")
         (ctz:SI (match_operand:SI 1 "register_operand" "0,r"))) ]
  ""
  "@
   brev\t%0\;clz\t%0, %0
   mov\t%0, %1\;brev\t%0\;clz\t%0, %0"
  [(set_attr "length" "8")
   (set_attr "cc" "set_z")]
  )

;;=============================================================================
;; cache instructions
;;-----------------------------------------------------------------------------
(define_insn "cache"
  [ (unspec_volatile [(match_operand:SI 0 "avr32_ks11_address_operand" "p")
                      (match_operand:SI 1 "immediate_operand" "Ku05")] VUNSPEC_CACHE)]
  ""
  "cache    %0, %1"
  [(set_attr "length" "4")]
  )

(define_insn "sync"
  [ (unspec_volatile [(match_operand:SI 0 "immediate_operand" "Ku08")] VUNSPEC_SYNC)]
  ""
  "sync    %0"
  [(set_attr "length" "4")]
  )

;;=============================================================================
;; TLB instructions
;;-----------------------------------------------------------------------------
(define_insn "tlbr"
  [ (unspec_volatile [(const_int 0)] VUNSPEC_TLBR)]
  ""
  "tlbr"
  [(set_attr "length" "2")]
  )

(define_insn "tlbw"
  [ (unspec_volatile [(const_int 0)] VUNSPEC_TLBW)]
  ""
  "tlbw"
  [(set_attr "length" "2")]
  )

(define_insn "tlbs"
  [ (unspec_volatile [(const_int 0)] VUNSPEC_TLBS)]
  ""
  "tlbs"
  [(set_attr "length" "2")]
  )

;;=============================================================================
;; Breakpoint instruction
;;-----------------------------------------------------------------------------
(define_insn "breakpoint"
  [ (unspec_volatile [(const_int 0)] VUNSPEC_BREAKPOINT)]
  ""
  "breakpoint"
  [(set_attr "length" "2")]
  )


;;=============================================================================
;; mtsr/mfsr instruction
;;-----------------------------------------------------------------------------
(define_insn "mtsr"
  [ (unspec_volatile [(match_operand 0 "immediate_operand" "i")
                      (match_operand:SI 1 "register_operand" "r")] VUNSPEC_MTSR)]
  ""
  "mtsr\t%0, %1"
  [(set_attr "length" "4")]
  )

(define_insn "mfsr"
  [ (set (match_operand:SI 0 "register_operand" "=r")
         (unspec_volatile:SI [(match_operand 1 "immediate_operand" "i")] VUNSPEC_MFSR)) ]
  ""
  "mfsr\t%0, %1"
  [(set_attr "length" "4")]
  )

;;=============================================================================
;; mtdr/mfdr instruction
;;-----------------------------------------------------------------------------
(define_insn "mtdr"
  [ (unspec_volatile [(match_operand 0 "immediate_operand" "i")
                      (match_operand:SI 1 "register_operand" "r")] VUNSPEC_MTDR)]
  ""
  "mtdr\t%0, %1"
  [(set_attr "length" "4")]
  )

(define_insn "mfdr"
  [ (set (match_operand:SI 0 "register_operand" "=r")
         (unspec_volatile:SI [(match_operand 1 "immediate_operand" "i")] VUNSPEC_MFDR)) ]
  ""
  "mfdr\t%0, %1"
  [(set_attr "length" "4")]
  )

;;=============================================================================
;; musfr
;;-----------------------------------------------------------------------------
(define_insn "musfr"
  [ (unspec_volatile [(match_operand:SI 0 "register_operand" "r")] VUNSPEC_MUSFR)]
  ""
  "musfr\t%0"
  [(set_attr "length" "2")
   (set_attr "cc" "clobber")]
  )

(define_insn "mustr"
  [ (set (match_operand:SI 0 "register_operand" "=r")
         (unspec_volatile:SI [(const_int 0)] VUNSPEC_MUSTR)) ]
  ""
  "mustr\t%0"
  [(set_attr "length" "2")]
  )

(define_insn "ssrf"
  [ (unspec_volatile [(match_operand:SI 0 "immediate_operand" "Ku05")] VUNSPEC_SSRF)]
  ""
  "ssrf    %0"
  [(set_attr "length" "2")
   (set_attr "cc" "clobber")]
  )

(define_insn "csrf"
  [ (unspec_volatile [(match_operand:SI 0 "immediate_operand" "Ku05")] VUNSPEC_CSRF)]
  ""
  "csrf    %0"
  [(set_attr "length" "2")
   (set_attr "cc" "clobber")]
  )

;;=============================================================================
;; Flush Return Stack instruction
;;-----------------------------------------------------------------------------
(define_insn "frs"
  [ (unspec_volatile [(const_int 0)] VUNSPEC_FRS)]
  ""
  "frs"
  [(set_attr "length" "2")
   (set_attr "cc" "none")]
  )


;;=============================================================================
;; Saturation Round Scale instruction
;;-----------------------------------------------------------------------------
(define_insn "sats"
  [ (set (match_operand:SI 0 "register_operand" "+r")
         (unspec:SI [(match_dup 0)
                     (match_operand 1 "immediate_operand" "Ku05")
                     (match_operand 2 "immediate_operand" "Ku05")]
                    UNSPEC_SATS)) ]
  "TARGET_DSP"
  "sats\t%0 >> %1, %2"
  [(set_attr "type" "alu_sat")
   (set_attr "length" "4")]
  )

(define_insn "satu"
  [ (set (match_operand:SI 0 "register_operand" "+r")
         (unspec:SI [(match_dup 0)
                     (match_operand 1 "immediate_operand" "Ku05")
                     (match_operand 2 "immediate_operand" "Ku05")]
                    UNSPEC_SATU)) ]
  "TARGET_DSP"
  "satu\t%0 >> %1, %2"
  [(set_attr "type" "alu_sat")
   (set_attr "length" "4")]
  )

(define_insn "satrnds"
  [ (set (match_operand:SI 0 "register_operand" "+r")
         (unspec:SI [(match_dup 0)
                     (match_operand 1 "immediate_operand" "Ku05")
                     (match_operand 2 "immediate_operand" "Ku05")]
                    UNSPEC_SATRNDS)) ]
  "TARGET_DSP"
  "satrnds\t%0 >> %1, %2"
  [(set_attr "type" "alu_sat")
   (set_attr "length" "4")]
  )

(define_insn "satrndu"
  [ (set (match_operand:SI 0 "register_operand" "+r")
         (unspec:SI [(match_dup 0)
                     (match_operand 1 "immediate_operand" "Ku05")
                     (match_operand 2 "immediate_operand" "Ku05")]
                    UNSPEC_SATRNDU)) ]
  "TARGET_DSP"
  "sats\t%0 >> %1, %2"
  [(set_attr "type" "alu_sat")
   (set_attr "length" "4")]
  )

(define_insn "sleep"
  [(unspec_volatile [(const_int 0)] VUNSPEC_SLEEP)
  (match_operand:SI 0 "const_int_operand" "")]
  ""
  "sleep	%0"
  [(set_attr "length" "1")
   (set_attr "cc"  "none")
  ])

(define_expand "delay_cycles"
  [(unspec_volatile [(match_operand:SI 0 "const_int_operand" "i")]
                    VUNSPEC_DELAY_CYCLES)]
  ""
  "
  unsigned int cycles = UINTVAL (operands[0]);
  if (IN_RANGE(cycles,0x10000 ,0xFFFFFFFF))
   {
     unsigned int msb = (cycles & 0xFFFF0000);
     unsigned int shift = 16;
     msb = (msb >> shift);
     unsigned int cycles_used = (msb*0x10000);
     emit_insn (gen_delay_cycles_2 (gen_int_mode (msb, SImode)));
     cycles -= cycles_used;
   }
  if (IN_RANGE(cycles, 4, 0xFFFF))
   {
     unsigned int loop_count = (cycles/ 4);
     unsigned int cycles_used = (loop_count*4);
     emit_insn (gen_delay_cycles_1 (gen_int_mode (loop_count, SImode)));
     cycles -= cycles_used;
   }
  while (cycles >= 3)
    {
      emit_insn (gen_nop3 ());
      cycles -= 3;
    }
  if (cycles == 1 || cycles == 2)
    {
      while (cycles--)
        emit_insn (gen_nop ());
    }
  DONE;
  ")

(define_insn "delay_cycles_1"
[(unspec_volatile [(const_int 0)] VUNSPEC_DELAY_CYCLES_1)
  (match_operand:SI 0 "immediate_operand" "")
  (clobber (match_scratch:SI 1 "=&r"))]
  ""
  "mov\t%1, %0
    1:  sub\t%1, 1
        brne\t1b
        nop"
)

(define_insn "delay_cycles_2"
[(unspec_volatile [(const_int 0)] VUNSPEC_DELAY_CYCLES_2)
  (match_operand:SI 0 "immediate_operand" "")
  (clobber (match_scratch:SI 1 "=&r"))
  (clobber (match_scratch:SI 2 "=&r"))]
  ""
  "mov\t%1, %0
    1:  mov\t%2, 16383 
    2:  sub\t%2, 1	
        brne\t2b
        nop
        sub\t%1, 1
        brne\t1b
        nop"
)

;; CPU instructions

;;=============================================================================
;; nop
;;-----------------------------------------------------------------------------
;; No-op instruction.
;;=============================================================================
(define_insn "nop"
  [(unspec_volatile [(const_int 0)] VUNSPEC_NOP)]
  ""
  "nop"
  [(set_attr "length" "1")
   (set_attr "type" "alu")
  (set_attr "cc" "none")])

;; NOP3
(define_insn "nop3"
  [(unspec_volatile [(const_int 0)] VUNSPEC_NOP3)]
  ""
  "rjmp\t2"
  [(set_attr "length" "3")
   (set_attr "type" "alu")
  (set_attr "cc" "none")])

;; Special patterns for dealing with the constant pool

(define_insn "align_4"
  [(unspec_volatile [(const_int 0)] VUNSPEC_ALIGN)]
  ""
  {
   assemble_align (32);
   return "";
  }
  [(set_attr "length" "2")]
)


(define_insn "consttable_start"
  [(unspec_volatile [(const_int 0)] VUNSPEC_POOL_START)]
  ""
  {
   return ".cpool";
  }
  [(set_attr "length" "0")]
  )

(define_insn "consttable_end"
  [(unspec_volatile [(const_int 0)] VUNSPEC_POOL_END)]
  ""
  {
   making_const_table = FALSE;
   return "";
  }
  [(set_attr "length" "0")]
)


(define_insn "consttable_4"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_4)]
  ""
  {
    making_const_table = TRUE;
    switch (GET_MODE_CLASS (GET_MODE (operands[0])))
      {
      case MODE_FLOAT:
      {
        REAL_VALUE_TYPE r;
        char real_string[1024];
        REAL_VALUE_FROM_CONST_DOUBLE (r, operands[0]);
        real_to_decimal(real_string, &r, 1024, 0, 1);
        asm_fprintf (asm_out_file, "\t.float\t%s\n", real_string);
        break;
      }
      default:
        assemble_integer (operands[0], 4, 0, 1);
        break;
      }
    return "";
  }
  [(set_attr "length" "4")]
)

(define_insn "consttable_8"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_8)]
  ""
  {
    making_const_table = TRUE;
    switch (GET_MODE_CLASS (GET_MODE (operands[0])))
      {
       case MODE_FLOAT:
        {
         REAL_VALUE_TYPE r; 
         char real_string[1024];
         REAL_VALUE_FROM_CONST_DOUBLE (r, operands[0]);
         real_to_decimal(real_string, &r, 1024, 0, 1);
         asm_fprintf (asm_out_file, "\t.double\t%s\n", real_string);
         break;
        }
       default:
         assemble_integer(operands[0], 8, 0, 1);
        break;
     }
    return "";
  }
  [(set_attr "length" "8")]
)

(define_insn "consttable_16"
  [(unspec_volatile [(match_operand 0 "" "")] VUNSPEC_POOL_16)]
  ""
  {
    making_const_table = TRUE;
    assemble_integer(operands[0], 16, 0, 1);
    return "";
  }
  [(set_attr "length" "16")]
)

;;=============================================================================
;; coprocessor instructions
;;-----------------------------------------------------------------------------
(define_insn "cop"
  [ (unspec_volatile [(match_operand 0 "immediate_operand" "Ku03")
                      (match_operand 1 "immediate_operand" "Ku04")
                      (match_operand 2 "immediate_operand" "Ku04")
                      (match_operand 3 "immediate_operand" "Ku04")
                      (match_operand 4 "immediate_operand" "Ku07")] VUNSPEC_COP)]
  ""
  "cop\tcp%0, cr%1, cr%2, cr%3, %4"
  [(set_attr "length" "4")]
  )

(define_insn "mvcrsi"
  [ (set (match_operand:SI 0 "avr32_cop_move_operand" "=r,<,Z")
         (unspec_volatile:SI [(match_operand 1 "immediate_operand" "Ku03,Ku03,Ku03")
                              (match_operand 2 "immediate_operand" "Ku04,Ku04,Ku04")]
                             VUNSPEC_MVCR)) ]
  ""
  "@
   mvcr.w\tcp%1, %0, cr%2
   stcm.w\tcp%1, %0, cr%2
   stc.w\tcp%1, %0, cr%2"
  [(set_attr "length" "4")]
  )

(define_insn "mvcrdi"
  [ (set (match_operand:DI 0 "avr32_cop_move_operand" "=r,<,Z")
         (unspec_volatile:DI [(match_operand 1 "immediate_operand" "Ku03,Ku03,Ku03")
                              (match_operand 2 "immediate_operand" "Ku04,Ku04,Ku04")]
                             VUNSPEC_MVCR)) ]
  ""
  "@
   mvcr.d\tcp%1, %0, cr%2
   stcm.d\tcp%1, %0, cr%2-cr%i2
   stc.d\tcp%1, %0, cr%2"
  [(set_attr "length" "4")]
  )

(define_insn "mvrcsi"
  [ (unspec_volatile:SI [(match_operand 0 "immediate_operand" "Ku03,Ku03,Ku03")
                         (match_operand 1 "immediate_operand" "Ku04,Ku04,Ku04")
                         (match_operand:SI 2 "avr32_cop_move_operand" "r,>,Z")]
                        VUNSPEC_MVRC)]
  ""
  {
   switch (which_alternative){
    case 0:
      return "mvrc.w\tcp%0, cr%1, %2";
    case 1:
      return "ldcm.w\tcp%0, %2, cr%1";
    case 2:
      return "ldc.w\tcp%0, cr%1, %2";
    default:
      abort();
   }
  }
  [(set_attr "length" "4")]
  )

(define_insn "mvrcdi"
  [ (unspec_volatile:DI [(match_operand 0 "immediate_operand" "Ku03,Ku03,Ku03")
                         (match_operand 1 "immediate_operand" "Ku04,Ku04,Ku04")
                         (match_operand:DI 2 "avr32_cop_move_operand" "r,>,Z")]
                        VUNSPEC_MVRC)]
  ""
  {
   switch (which_alternative){
    case 0:
      return "mvrc.d\tcp%0, cr%1, %2";
    case 1:
      return "ldcm.d\tcp%0, %2, cr%1-cr%i1";
    case 2:
      return "ldc.d\tcp%0, cr%1, %2";
    default:
      abort();
   }
  }
  [(set_attr "length" "4")]
  )

;;=============================================================================
;; epilogue
;;-----------------------------------------------------------------------------
;; This pattern emits RTL for exit from a function. The function exit is
;; responsible for deallocating the stack frame, restoring callee saved
;; registers and emitting the return instruction.
;; ToDo: using TARGET_ASM_FUNCTION_PROLOGUE instead.
;;=============================================================================
(define_expand "epilogue"
  [(unspec_volatile [(return)] VUNSPEC_EPILOGUE)]
  ""
  "
  if (USE_RETURN_INSN (FALSE)){
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

(define_insn "*epilogue_insns"
  [(unspec_volatile [(return)] VUNSPEC_EPILOGUE)]
  ""
  {
    avr32_output_return_instruction (FALSE, FALSE, NULL, NULL);
    return "";
  }
  ; Length is absolute worst case
  [(set_attr "type" "branch")
   (set_attr "length" "12")]
  )

(define_insn "*epilogue_insns_ret_imm"
  [(parallel [(set (reg RETVAL_REGNUM) (match_operand 0 "immediate_operand" "i"))
              (use (reg RETVAL_REGNUM))
              (unspec_volatile [(return)] VUNSPEC_EPILOGUE)])]
  "((INTVAL(operands[0]) == -1) || (INTVAL(operands[0]) == 0) || (INTVAL(operands[0]) == 1))"
  {
    avr32_output_return_instruction (FALSE, FALSE, NULL, operands[0]);
    return "";
  }
  ; Length is absolute worst case
  [(set_attr "type" "branch")
   (set_attr "length" "12")]
  )

(define_insn "sibcall_epilogue"
  [(unspec_volatile [(const_int 0)] VUNSPEC_EPILOGUE)]
  ""
  {
   avr32_output_return_instruction (FALSE, FALSE,  NULL, NULL);
   return "";
  }
;; Length is absolute worst case
  [(set_attr "type" "branch")
   (set_attr "length" "12")]
  )

(define_insn "*sibcall_epilogue_insns_ret_imm"
  [(parallel [(set (reg RETVAL_REGNUM) (match_operand 0 "immediate_operand" "i"))
              (use (reg RETVAL_REGNUM))
              (unspec_volatile [(const_int 0)] VUNSPEC_EPILOGUE)])]
  "((INTVAL(operands[0]) == -1) || (INTVAL(operands[0]) == 0) || (INTVAL(operands[0]) == 1))"
  {
    avr32_output_return_instruction (FALSE, FALSE, NULL, operands[0]);
    return "";
  }
  ; Length is absolute worst case
  [(set_attr "type" "branch")
   (set_attr "length" "12")]
  )

(define_insn "ldxi"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(mem:SI (plus:SI
                 (match_operand:SI 1 "register_operand" "r")
                 (mult:SI (zero_extract:SI (match_operand:SI 2 "register_operand" "r")
                                           (const_int 8)
                                           (match_operand:SI 3 "immediate_operand" "Ku05"))
                          (const_int 4)))))]
  "(INTVAL(operands[3]) == 24 || INTVAL(operands[3]) == 16 || INTVAL(operands[3]) == 8
   || INTVAL(operands[3]) == 0)"
  {
   switch ( INTVAL(operands[3]) ){
    case 0:
         return "ld.w    %0, %1[%2:b << 2]";
    case 8:
         return "ld.w    %0, %1[%2:l << 2]";
    case 16:
         return "ld.w    %0, %1[%2:u << 2]";
    case 24:
         return "ld.w    %0, %1[%2:t << 2]";
    default:
         internal_error("illegal operand for ldxi");
   }
  }
  [(set_attr "type" "load")
   (set_attr "length" "4")
   (set_attr "cc" "none")])






;;=============================================================================
;; Peephole optimizing
;;-----------------------------------------------------------------------------
;; Changing
;;   sub     r8, r7, 8
;;   st.w    r8[0x0], r12
;; to
;;   sub     r8, r7, 8
;;   st.w    r7[-0x8], r12
;;=============================================================================
; (set (reg:SI 9 r8)
;      (plus:SI (reg/f:SI 6 r7)
;               (const_int ...)))
; (set (mem:SI (reg:SI 9 r8))
;      (reg:SI 12 r12))
(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "immediate_operand" "")))
   (set (mem:SI (match_dup 0))
	(match_operand:SI 3 "register_operand" ""))]
  "REGNO(operands[0]) != REGNO(operands[1]) && avr32_const_ok_for_constraint_p(INTVAL(operands[2]), 'K', \"Ks16\")"
  [(set (match_dup 0)
	(plus:SI (match_dup 1)
		 (match_dup 2)))
   (set (mem:SI (plus:SI (match_dup 1)
			 (match_dup 2)))
	(match_dup 3))]
  "")

;;=============================================================================
;; Peephole optimizing
;;-----------------------------------------------------------------------------
;; Changing
;;   sub     r6, r7, 4
;;   ld.w    r6, r6[0x0]
;; to
;;   sub     r6, r7, 4
;;   ld.w    r6, r7[-0x4]
;;=============================================================================
; (set (reg:SI 7 r6)
;      (plus:SI (reg/f:SI 6 r7)
;               (const_int -4 [0xfffffffc])))
; (set (reg:SI 7 r6)
;      (mem:SI (reg:SI 7 r6)))
(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_operand:SI 1 "register_operand" "")
		 (match_operand:SI 2 "immediate_operand" "")))
   (set (match_operand:SI 3 "register_operand" "")
	(mem:SI (match_dup 0)))]
  "REGNO(operands[0]) != REGNO(operands[1]) && avr32_const_ok_for_constraint_p(INTVAL(operands[2]), 'K', \"Ks16\")"
  [(set (match_dup 0)
	(plus:SI (match_dup 1)
		 (match_dup 2)))
   (set (match_dup 3)
	(mem:SI (plus:SI (match_dup 1)
			 (match_dup 2))))]
  "")

;;=============================================================================
;; Peephole optimizing
;;-----------------------------------------------------------------------------
;; Changing
;;   ld.sb   r0, r7[-0x6]
;;   cashs.b r0
;; to
;;   ld.sb   r0, r7[-0x6]
;;=============================================================================
(define_peephole2
  [(set (match_operand:QI 0 "register_operand" "")
	(match_operand:QI 1 "load_sb_memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
	(sign_extend:SI (match_dup 0)))]
  "(REGNO(operands[0]) == REGNO(operands[2]) || peep2_reg_dead_p(2, operands[0]))"
  [(set (match_dup 2)
	(sign_extend:SI (match_dup 1)))]
  "")

;;=============================================================================
;; Peephole optimizing
;;-----------------------------------------------------------------------------
;; Changing
;;   ld.ub   r0, r7[-0x6]
;;   cashu.b r0
;; to
;;   ld.ub   r0, r7[-0x6]
;;=============================================================================
(define_peephole2
  [(set (match_operand:QI 0 "register_operand" "")
	(match_operand:QI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
	(zero_extend:SI (match_dup 0)))]
  "(REGNO(operands[0]) == REGNO(operands[2])) || peep2_reg_dead_p(2, operands[0])"
  [(set (match_dup 2)
	(zero_extend:SI (match_dup 1)))]
  "")

;;=============================================================================
;; Peephole optimizing
;;-----------------------------------------------------------------------------
;; Changing
;;   ld.sh   r0, r7[-0x6]
;;   casts.h r0
;; to
;;   ld.sh   r0, r7[-0x6]
;;=============================================================================
(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
	(match_operand:HI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
	(sign_extend:SI (match_dup 0)))]
  "(REGNO(operands[0]) == REGNO(operands[2])) || peep2_reg_dead_p(2, operands[0])"
  [(set (match_dup 2)
	(sign_extend:SI (match_dup 1)))]
  "")

;;=============================================================================
;; Peephole optimizing
;;-----------------------------------------------------------------------------
;; Changing
;;   ld.uh   r0, r7[-0x6]
;;   castu.h r0
;; to
;;   ld.uh   r0, r7[-0x6]
;;=============================================================================
(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
	(match_operand:HI 1 "memory_operand" ""))
   (set (match_operand:SI 2 "register_operand" "")
	(zero_extend:SI (match_dup 0)))]
  "(REGNO(operands[0]) == REGNO(operands[2])) || peep2_reg_dead_p(2, operands[0])"
  [(set (match_dup 2)
	(zero_extend:SI (match_dup 1)))]
  "")

;;=============================================================================
;; Peephole optimizing
;;-----------------------------------------------------------------------------
;; Changing
;;   mul     rd, rx, ry
;;   add     rd2, rd  
;; or
;;   add     rd2, rd, rd2  
;; to
;;   mac     rd2, rx, ry
;;=============================================================================
(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (mult:SI (match_operand:SI 1 "register_operand" "")
                (match_operand:SI 2 "register_operand" "")))
   (set (match_operand:SI 3 "register_operand" "")
        (plus:SI (match_dup 3)
                 (match_dup 0)))]
  "peep2_reg_dead_p(2, operands[0])"
  [(set (match_dup 3)
	(plus:SI (mult:SI (match_dup 1)
			  (match_dup 2))
		 (match_dup 3)))]
  "")

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (mult:SI (match_operand:SI 1 "register_operand" "")
                (match_operand:SI 2 "register_operand" "")))
   (set (match_operand:SI 3 "register_operand" "")
        (plus:SI (match_dup 0)
                 (match_dup 3)))]
  "peep2_reg_dead_p(2, operands[0])"
  [(set (match_dup 3)
	(plus:SI (mult:SI (match_dup 1)
			  (match_dup 2))
		 (match_dup 3)))]
  "")


;;=============================================================================
;; Peephole optimizing
;;-----------------------------------------------------------------------------
;; Changing
;;   bfextu  rd, rs, k5, 1 or and(h/l) rd, one_bit_set_mask
;; to
;;   bld     rs, k5
;;
;; If rd is dead after the operation.
;;=============================================================================
(define_peephole2
  [ (set (match_operand:SI 0 "register_operand" "")
         (zero_extract:SI (match_operand:SI 1 "register_operand" "")
                          (const_int 1)
                          (match_operand:SI 2 "immediate_operand" "")))
    (set (cc0)
         (match_dup 0))]
  "peep2_reg_dead_p(2, operands[0])"
  [(set (cc0)
        (and:SI (match_dup 1)
                (match_dup 2)))]
  "operands[2] = GEN_INT(1 << INTVAL(operands[2]));")

(define_peephole2
  [ (set (match_operand:SI 0 "register_operand" "")
         (and:SI (match_operand:SI 1 "register_operand" "")
                 (match_operand:SI 2 "one_bit_set_operand" "")))
    (set (cc0)
         (match_dup 0))]
  "peep2_reg_dead_p(2, operands[0])"
  [(set (cc0)
        (and:SI (match_dup 1)
                (match_dup 2)))]
  "")

;;=============================================================================
;; Peephole optimizing
;;-----------------------------------------------------------------------------
;; Load with extracted index: ld.w  Rd, Rb[Ri:{t/u/b/l} << 2]
;;
;;=============================================================================


(define_peephole
  [(set (match_operand:SI 0 "register_operand" "")
        (zero_extract:SI (match_operand:SI 1 "register_operand" "")
                         (const_int 8)
                         (match_operand:SI 2 "avr32_extract_shift_operand" "")))
   (set (match_operand:SI 3 "register_operand" "")
        (mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
                         (match_operand:SI 4 "register_operand" ""))))]

  "(dead_or_set_p(insn, operands[0]))"
  {
   switch ( INTVAL(operands[2]) ){
    case 0:
         return "ld.w    %3, %4[%1:b << 2]";
    case 8:
         return "ld.w    %3, %4[%1:l << 2]";
    case 16:
         return "ld.w    %3, %4[%1:u << 2]";
    case 24:
         return "ld.w    %3, %4[%1:t << 2]";
    default:
         internal_error("illegal operand for ldxi");
   }
  }
  [(set_attr "type" "load")
   (set_attr "length" "4")
   (set_attr "cc" "clobber")]
  )



(define_peephole
  [(set (match_operand:SI 0 "register_operand" "")
        (and:SI (match_operand:SI 1 "register_operand" "") (const_int 255)))
   (set (match_operand:SI 2 "register_operand" "")
        (mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
                         (match_operand:SI 3 "register_operand" ""))))]

  "(dead_or_set_p(insn, operands[0]))"

  "ld.w    %2, %3[%1:b << 2]"
  [(set_attr "type" "load")
   (set_attr "length" "4")
   (set_attr "cc" "clobber")]
  )


(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (zero_extract:SI (match_operand:SI 1 "register_operand" "")
                         (const_int 8)
                         (match_operand:SI 2 "avr32_extract_shift_operand" "")))
   (set (match_operand:SI 3 "register_operand" "")
        (mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
                         (match_operand:SI 4 "register_operand" ""))))]

  "(peep2_reg_dead_p(2, operands[0]))
   || (REGNO(operands[0]) == REGNO(operands[3]))"
  [(set (match_dup 3)
	(mem:SI (plus:SI
                 (match_dup 4)
                 (mult:SI (zero_extract:SI (match_dup 1)
                                           (const_int 8)
                                           (match_dup 2))
                          (const_int 4)))))]
  )

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (zero_extend:SI (match_operand:QI 1 "register_operand" "")))
   (set (match_operand:SI 2 "register_operand" "")
        (mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
                         (match_operand:SI 3 "register_operand" ""))))]

  "(peep2_reg_dead_p(2, operands[0]))
   || (REGNO(operands[0]) == REGNO(operands[2]))"
  [(set (match_dup 2)
	(mem:SI (plus:SI
                 (match_dup 3)
                 (mult:SI (zero_extract:SI (match_dup 1)
                                           (const_int 8)
                                           (const_int 0))
                          (const_int 4)))))]
  "operands[1] = gen_rtx_REG(SImode, REGNO(operands[1]));"
  )


(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (and:SI (match_operand:SI 1 "register_operand" "")
                (const_int 255)))
   (set (match_operand:SI 2 "register_operand" "")
        (mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
                         (match_operand:SI 3 "register_operand" ""))))]

  "(peep2_reg_dead_p(2, operands[0]))
   || (REGNO(operands[0]) == REGNO(operands[2]))"
  [(set (match_dup 2)
	(mem:SI (plus:SI
                 (match_dup 3)
                 (mult:SI (zero_extract:SI (match_dup 1)
                                           (const_int 8)
                                           (const_int 0))
                          (const_int 4)))))]
  ""
  )



(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (lshiftrt:SI (match_operand:SI 1 "register_operand" "")
                     (const_int 24)))
   (set (match_operand:SI 2 "register_operand" "")
        (mem:SI (plus:SI (mult:SI (match_dup 0) (const_int 4))
                         (match_operand:SI 3 "register_operand" ""))))]

  "(peep2_reg_dead_p(2, operands[0]))
   || (REGNO(operands[0]) == REGNO(operands[2]))"
  [(set (match_dup 2)
	(mem:SI (plus:SI
                 (match_dup 3)
                 (mult:SI (zero_extract:SI (match_dup 1)
                                           (const_int 8)
                                           (const_int 24))
                          (const_int 4)))))]
  ""
  )


;;************************************************
;; ANDN
;;
;;************************************************


(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (not:SI (match_operand:SI 1 "register_operand" "")))
   (set (match_operand:SI 2 "register_operand" "")
        (and:SI (match_dup 2)
                (match_dup 0)))]
  "peep2_reg_dead_p(2, operands[0])"

  [(set (match_dup 2)
        (and:SI  (match_dup 2)
                 (not:SI (match_dup 1))
                 ))]
  ""
)

(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
        (not:SI (match_operand:SI 1 "register_operand" "")))
   (set (match_operand:SI 2 "register_operand" "")
        (and:SI (match_dup 0)
                (match_dup 2)
                ))]
  "peep2_reg_dead_p(2, operands[0])"

  [(set (match_dup 2)
        (and:SI  (match_dup 2)
                 (not:SI (match_dup 1))
                 ))]

  ""
)


;;=================================================================
;; Addabs peephole
;;=================================================================

(define_peephole
  [(set (match_operand:SI 2 "register_operand" "=r")
 	(abs:SI (match_operand:SI 1 "register_operand" "r")))
   (set (match_operand:SI 0 "register_operand" "=r")
 	(plus:SI (match_operand:SI 3 "register_operand" "r")
 		 (match_dup 2)))]
  "dead_or_set_p(insn, operands[2])"
  "addabs  %0, %3, %1"
  [(set_attr "length" "4")
   (set_attr "cc" "set_z")])

(define_peephole
  [(set (match_operand:SI 2 "register_operand" "=r")
 	(abs:SI (match_operand:SI 1 "register_operand" "r")))
   (set (match_operand:SI 0 "register_operand" "=r")
 	(plus:SI (match_dup 2)
                 (match_operand:SI 3 "register_operand" "r")))]
  "dead_or_set_p(insn, operands[2])"
  "addabs  %0, %3, %1"
  [(set_attr "length" "4")
   (set_attr "cc" "set_z")])


;;=================================================================
;; Detect roundings
;;=================================================================

(define_insn "*round"
  [(set (match_operand:SI 0 "register_operand" "+r")
        (ashiftrt:SI (plus:SI (match_dup 0)
                              (match_operand:SI 1 "immediate_operand" "i"))
                     (match_operand:SI 2 "immediate_operand" "i")))]
  "avr32_rnd_operands(operands[1], operands[2])"

  "satrnds    %0 >> %2, 31"

  [(set_attr "type" "alu_sat")
   (set_attr "length" "4")]

  )


(define_peephole2
  [(set (match_operand:SI 0 "register_operand" "")
	(plus:SI (match_dup 0)
                 (match_operand:SI 1 "immediate_operand" "")))
   (set (match_dup 0)
	(ashiftrt:SI (match_dup 0)
                     (match_operand:SI 2 "immediate_operand" "")))]
  "avr32_rnd_operands(operands[1], operands[2])"

  [(set (match_dup 0)
        (ashiftrt:SI (plus:SI (match_dup 0)
                              (match_dup 1))
                     (match_dup 2)))]
  )

(define_peephole
  [(set (match_operand:SI 0 "register_operand" "r")
	(plus:SI (match_dup 0)
                 (match_operand:SI 1 "immediate_operand" "i")))
   (set (match_dup 0)
	(ashiftrt:SI (match_dup 0)
                     (match_operand:SI 2 "immediate_operand" "i")))]
  "avr32_rnd_operands(operands[1], operands[2])"

  "satrnds    %0 >> %2, 31"

  [(set_attr "type" "alu_sat")
   (set_attr "length" "4")
   (set_attr "cc" "clobber")]

  )


;;=================================================================
;; mcall
;;=================================================================
(define_peephole
  [(set (match_operand:SI 0 "register_operand"        "")
	(match_operand 1 "avr32_const_pool_ref_operand"  ""))
   (parallel [(call (mem:SI (match_dup 0))
                    (match_operand 2 "" ""))
              (clobber (reg:SI LR_REGNUM))])]
  "dead_or_set_p(insn, operands[0])"
  "mcall    %1"
  [(set_attr "type" "call")
   (set_attr "length" "4")
   (set_attr "cc" "clobber")]
)

(define_peephole
  [(set (match_operand:SI 2 "register_operand"        "")
	(match_operand 1 "avr32_const_pool_ref_operand"  ""))
   (parallel [(set (match_operand 0 "register_operand" "")
                   (call (mem:SI (match_dup 2))
                         (match_operand 3 "" "")))
              (clobber (reg:SI LR_REGNUM))])]
  "dead_or_set_p(insn, operands[2])"
  "mcall    %1"
  [(set_attr "type" "call")
   (set_attr "length" "4")
   (set_attr "cc" "call_set")]
)


(define_peephole2
  [(set (match_operand:SI 0 "register_operand"    "")
	(match_operand 1 "avr32_const_pool_ref_operand"  ""))
   (parallel [(call (mem:SI (match_dup 0))
                    (match_operand 2 "" ""))
              (clobber (reg:SI LR_REGNUM))])]
  "peep2_reg_dead_p(2, operands[0])"
  [(parallel [(call (mem:SI (match_dup 1))
                    (match_dup 2))
              (clobber (reg:SI LR_REGNUM))])]
  ""
)

(define_peephole2
  [(set (match_operand:SI 0 "register_operand"        "")
	(match_operand 1 "avr32_const_pool_ref_operand"  ""))
   (parallel [(set (match_operand 2 "register_operand" "")
                   (call (mem:SI (match_dup 0))
                         (match_operand 3 "" "")))
              (clobber (reg:SI LR_REGNUM))])]
  "(peep2_reg_dead_p(2, operands[0]) || (REGNO(operands[2]) == REGNO(operands[0])))"
  [(parallel [(set (match_dup 2)
                   (call (mem:SI (match_dup 1))
                         (match_dup 3)))
              (clobber (reg:SI LR_REGNUM))])]
  ""
)

;;=================================================================
;; Returning a value
;;=================================================================


(define_peephole
  [(set (match_operand 0 "register_operand" "")
        (match_operand 1 "register_operand" ""))
   (return)]
  "USE_RETURN_INSN (TRUE) && (REGNO(operands[0]) == RETVAL_REGNUM)
   && (REGNO(operands[1]) != LR_REGNUM)
   && (REGNO_REG_CLASS(REGNO(operands[1])) == GENERAL_REGS)"
  "retal    %1"
  [(set_attr "type" "call")
   (set_attr "length" "2")]
  )


(define_peephole
  [(set (match_operand 0 "register_operand" "r")
        (match_operand 1 "immediate_operand" "i"))
   (return)]
  "(USE_RETURN_INSN (FALSE) && (REGNO(operands[0]) == RETVAL_REGNUM) &&
   ((INTVAL(operands[1]) == -1) || (INTVAL(operands[1]) == 0) || (INTVAL(operands[1]) == 1)))"
  {
    avr32_output_return_instruction (TRUE, FALSE, NULL, operands[1]);
    return "";
  }
  [(set_attr "type" "call")
   (set_attr "length" "4")]
  )

(define_peephole
  [(set (match_operand 0 "register_operand" "r")
        (match_operand 1 "immediate_operand" "i"))
   (unspec_volatile [(return)] VUNSPEC_EPILOGUE)]
  "(REGNO(operands[0]) == RETVAL_REGNUM) &&
   ((INTVAL(operands[1]) == -1) || (INTVAL(operands[1]) == 0) || (INTVAL(operands[1]) == 1))"
  {
    avr32_output_return_instruction (FALSE, FALSE, NULL, operands[1]);
    return "";
  }
  ; Length is absolute worst case
  [(set_attr "type" "branch")
   (set_attr "length" "12")]
  )

(define_peephole
  [(set (match_operand 0 "register_operand" "=r")
        (if_then_else (match_operator 1 "avr32_comparison_operator"
                                      [(match_operand 4 "register_operand" "r")
                                       (match_operand 5 "register_immediate_operand" "rKs21")])
                      (match_operand 2 "avr32_cond_register_immediate_operand" "rKs08")
                      (match_operand 3 "avr32_cond_register_immediate_operand" "rKs08")))
   (return)]
  "USE_RETURN_INSN (TRUE) && (REGNO(operands[0]) == RETVAL_REGNUM)"
  {
   operands[1] = avr32_output_cmp(operands[1], GET_MODE(operands[4]), operands[4], operands[5]);

   if ( GET_CODE(operands[2]) == REG
        && GET_CODE(operands[3]) == REG
        && REGNO(operands[2]) != LR_REGNUM
        && REGNO(operands[3]) != LR_REGNUM ){
      return "ret%1    %2\;ret%i1    %3";
   } else if ( GET_CODE(operands[2]) == REG
               && GET_CODE(operands[3]) == CONST_INT ){
      if ( INTVAL(operands[3]) == -1
           || INTVAL(operands[3]) == 0
           || INTVAL(operands[3]) == 1 ){
        return "ret%1    %2\;ret%i1    %d3";
      } else {
        return "mov%1    r12, %2\;mov%i1    r12, %3\;retal    r12";
      }
   } else if ( GET_CODE(operands[2]) == CONST_INT
               && GET_CODE(operands[3]) == REG ){
      if ( INTVAL(operands[2]) == -1
           || INTVAL(operands[2]) == 0
           || INTVAL(operands[2]) == 1 ){
        return "ret%1    %d2\;ret%i1    %3";
      } else {
        return "mov%1    r12, %2\;mov%i1    r12, %3\;retal    r12";
      }
   } else {
      if ( (INTVAL(operands[2]) == -1
            || INTVAL(operands[2]) == 0
            || INTVAL(operands[2]) == 1 )
           && (INTVAL(operands[3]) == -1
               || INTVAL(operands[3]) == 0
               || INTVAL(operands[3]) == 1 )){
        return "ret%1    %d2\;ret%i1    %d3";
      } else {
        return "mov%1    r12, %2\;mov%i1    r12, %3\;retal    r12";
      }
   }
  }

  [(set_attr "length" "10")
   (set_attr "cc" "none")
   (set_attr "type" "call")])
  


;;=================================================================
;; mulnhh.w
;;=================================================================

(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
        (neg:HI (match_operand:HI 1 "register_operand" "")))
   (set (match_operand:SI 2 "register_operand" "")
        (mult:SI
         (sign_extend:SI (match_dup 0))
         (sign_extend:SI (match_operand:HI 3 "register_operand" ""))))]
  "(peep2_reg_dead_p(2, operands[0])) || (REGNO(operands[2]) == REGNO(operands[0]))"
  [ (set (match_dup 2)
         (mult:SI
          (sign_extend:SI (neg:HI (match_dup 1)))
          (sign_extend:SI (match_dup 3))))]
  ""
  )

(define_peephole2
  [(set (match_operand:HI 0 "register_operand" "")
        (neg:HI (match_operand:HI 1 "register_operand" "")))
   (set (match_operand:SI 2 "register_operand" "")
        (mult:SI
         (sign_extend:SI (match_operand:HI 3 "register_operand" ""))
         (sign_extend:SI (match_dup 0))))]
  "(peep2_reg_dead_p(2, operands[0])) || (REGNO(operands[2]) == REGNO(operands[0]))"
  [ (set (match_dup 2)
         (mult:SI
          (sign_extend:SI (neg:HI (match_dup 1)))
          (sign_extend:SI (match_dup 3))))]
  ""
  )



;;=================================================================
;; Vector set and extract operations
;;=================================================================
(define_insn "vec_setv2hi_hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
        (vec_merge:V2HI
         (match_dup 0)
         (vec_duplicate:V2HI
          (match_operand:HI 1 "register_operand" "r"))
         (const_int 1)))]
  ""
  "bfins\t%0, %1, 16, 16"
  [(set_attr "type" "alu")
   (set_attr "length" "4")
   (set_attr "cc" "clobber")])

(define_insn "vec_setv2hi_lo"
  [(set (match_operand:V2HI 0 "register_operand" "+r")
        (vec_merge:V2HI
         (match_dup 0)
         (vec_duplicate:V2HI
          (match_operand:HI 1 "register_operand" "r"))
         (const_int 2)))]
  ""
  "bfins\t%0, %1, 0, 16"
  [(set_attr "type" "alu")
   (set_attr "length" "4")
   (set_attr "cc" "clobber")])

(define_expand "vec_setv2hi"
  [(set (match_operand:V2HI 0 "register_operand" "")
        (vec_merge:V2HI
         (match_dup 0)
         (vec_duplicate:V2HI
          (match_operand:HI 1 "register_operand" ""))
         (match_operand 2 "immediate_operand" "")))]
  ""
  { operands[2] = GEN_INT(INTVAL(operands[2]) + 1); }
  )

(define_insn "vec_extractv2hi"
  [(set (match_operand:HI 0 "register_operand" "=r")
        (vec_select:HI
         (match_operand:V2HI 1 "register_operand" "r")
         (parallel [(match_operand:SI 2 "immediate_operand" "i")])))]
  ""
  {
   if ( INTVAL(operands[2]) == 0 )
      return "bfextu\t%0, %1, 16, 16";
   else
      return "bfextu\t%0, %1, 0, 16";
  }
  [(set_attr "type" "alu")
   (set_attr "length" "4")
   (set_attr "cc" "clobber")])

(define_insn "vec_extractv4qi"
  [(set (match_operand:QI 0 "register_operand" "=r")
        (vec_select:QI
         (match_operand:V4QI 1 "register_operand" "r")
         (parallel [(match_operand:SI 2 "immediate_operand" "i")])))]
  ""
  {
   switch ( INTVAL(operands[2]) ){
     case 0:
       return "bfextu\t%0, %1, 24, 8";
     case 1:
       return "bfextu\t%0, %1, 16, 8";
     case 2:
       return "bfextu\t%0, %1, 8, 8";
     case 3:
       return "bfextu\t%0, %1, 0, 8";
     default:
       abort();
   }
  }
  [(set_attr "type" "alu")
   (set_attr "length" "4")
   (set_attr "cc" "clobber")])


(define_insn "concatv2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r, r, r")
        (vec_concat:V2HI
         (match_operand:HI 1 "register_operand" "r, r, 0")
         (match_operand:HI 2 "register_operand" "r, 0, r")))]
  ""
  "@
   mov\t%0, %1\;bfins\t%0, %2, 0, 16
   bfins\t%0, %2, 0, 16
   bfins\t%0, %1, 16, 16"
  [(set_attr "length" "6, 4, 4")
   (set_attr "type" "alu")])


;; Load the atomic operation description
(include "sync.md")

;; Load the SIMD description
(include "simd.md")

;; Include the FPU for uc3
(include "uc3fpu.md")
