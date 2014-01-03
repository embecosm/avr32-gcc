;;   AVR32 machine description file for Floating-Point instructions.
;;   Copyright 2003-2006 Atmel Corporation.
;;
;;   Written by Ronny Pedersen, Atmel Norway, <rpedersen@atmel.com>
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

;;******************************************************************************
;; Automaton pipeline description for floating-point coprocessor insns
;;******************************************************************************
(define_cpu_unit "fid,fm1,fm2,fm3,fm4,fwb,fcmp,fcast" "avr32_ap")

(define_insn_reservation "fmv_op" 1
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "fmv"))
  "is,da,d,fid,fwb")

(define_insn_reservation "fmul_op" 5
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "fmul"))
  "is,da,d,fid,fm1,fm2,fm3,fm4,fwb")

(define_insn_reservation "fcmps_op" 1
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "fcmps"))
  "is,da,d,fid,fcmp")

(define_insn_reservation "fcmpd_op" 2
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "fcmpd"))
  "is,da,d,fid*2,fcmp")

(define_insn_reservation "fcast_op" 3
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "fcast"))
  "is,da,d,fid,fcmp,fcast,fwb")

(define_insn_reservation "fmvcpu_op" 2
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "fmvcpu"))
  "is,da,d")

(define_insn_reservation "fldd_op" 1
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "fldd"))
  "is,da,d,fwb")

(define_insn_reservation "flds_op" 1
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "flds"))
  "is,da,d,fwb")

(define_insn_reservation "fsts_op" 0
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "fsts"))
  "is,da*2,d")

(define_insn_reservation "fstd_op" 0
  (and (eq_attr "pipeline" "ap")
       (eq_attr "type" "fstd"))
  "is,da*2,d")


(define_insn "*movsf_fpcp"
  [(set (match_operand:SF 0 "nonimmediate_operand"     "=f,f,r,f,m,r,r,r,m")
	(match_operand:SF 1 "general_operand"          " f,r,f,m,f,r,G,m,r"))]
  "TARGET_HARD_FLOAT"
  "@
   fmov.s\t%0, %1
   fmov.s\t%0, %1
   fmov.s\t%0, %1
   fld.s\t%0, %1
   fst.s\t%0, %1
   mov\t%0, %1
   mov\t%0, %1
   ld.w\t%0, %1
   st.w\t%0, %1"
  [(set_attr "length" "4,4,4,4,4,2,4,4,4")
   (set_attr "type" "fmv,flds,fmvcpu,flds,fsts,alu,alu,load,store")])

(define_insn_and_split "*movdf_fpcp"
  [(set (match_operand:DF 0 "nonimmediate_operand"     "=f,f,r,f,m,r,r,m")
	(match_operand:DF 1 "general_operand"          " f,r,f,m,f,r,m,r"))]
  "TARGET_HARD_FLOAT"
  "@
   fmov.d\t%0, %1
   fmov.d\t%0, %1
   fmov.d\t%0, %1
   fld.d\t%0, %1
   fst.d\t%0, %1
   mov\t%0, %1\;mov\t%m0, %m1
   ld.d\t%0, %1
   st.d\t%0, %1"

  "TARGET_HARD_FLOAT
   && reload_completed
   && (REG_P(operands[0]) &&  (REGNO_REG_CLASS(REGNO(operands[0])) == GENERAL_REGS))
   && (REG_P(operands[1]) &&  (REGNO_REG_CLASS(REGNO(operands[1])) == GENERAL_REGS))"
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

  [(set_attr "length" "4,4,4,4,4,4,4,4")
   (set_attr "type" "fmv,fldd,fmvcpu,fldd,fstd,alu2,load2,store2")])


(define_insn "mulsf3"
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "=f")
	(mult:SF (match_operand:SF 1 "avr32_fp_register_operand" "f")
		 (match_operand:SF 2 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fmul.s\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "nmulsf3"
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "=f")
	(neg:SF (mult:SF (match_operand:SF 1 "avr32_fp_register_operand" "f")
                         (match_operand:SF 2 "avr32_fp_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "fnmul.s\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_peephole2
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "")
	(mult:SF (match_operand:SF 1 "avr32_fp_register_operand" "")
		 (match_operand:SF 2 "avr32_fp_register_operand" "")))
   (set (match_operand:SF          3 "avr32_fp_register_operand" "")
	(neg:SF (match_dup 0)))]
  "TARGET_HARD_FLOAT &&
   (peep2_reg_dead_p(2, operands[0]) || (REGNO(operands[3]) == REGNO(operands[0])))"
  [(set (match_dup 3)
	(neg:SF (mult:SF (match_dup 1)
			 (match_dup 2))))]
)


(define_insn "macsf3"
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "=f")
	(plus:SF (mult:SF (match_operand:SF 1 "avr32_fp_register_operand" "f")
                          (match_operand:SF 2 "avr32_fp_register_operand" "f"))
                 (match_operand:SF 3 "avr32_fp_register_operand" "0")))]
  "TARGET_HARD_FLOAT"
  "fmac.s\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "nmacsf3"
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "=f")
	(plus:SF  (neg:SF (mult:SF (match_operand:SF 1 "avr32_fp_register_operand" "f")
                                   (match_operand:SF 2 "avr32_fp_register_operand" "f")))
                  (match_operand:SF 3 "avr32_fp_register_operand" "0")))]
  "TARGET_HARD_FLOAT"
  "fnmac.s\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_peephole2
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "")
	(mult:SF (match_operand:SF 1 "avr32_fp_register_operand" "")
		 (match_operand:SF 2 "avr32_fp_register_operand" "")))
   (set (match_operand:SF          3 "avr32_fp_register_operand" "")
	(minus:SF
	 (match_dup 3)
	 (match_dup 0)))]
  "TARGET_HARD_FLOAT && peep2_reg_dead_p(2, operands[0])"
  [(set (match_dup 3)
	(plus:SF  (neg:SF (mult:SF (match_dup 1)
                                   (match_dup 2)))
                  (match_dup 3)))]
)


(define_insn "msubacsf3"
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "=f")
	(minus:SF (mult:SF (match_operand:SF 1 "avr32_fp_register_operand" "f")
                           (match_operand:SF 2 "avr32_fp_register_operand" "f"))
                  (match_operand:SF 3 "avr32_fp_register_operand" "0")))]
  "TARGET_HARD_FLOAT"
  "fmsc.s\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_peephole2
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "")
	(mult:SF (match_operand:SF 1 "avr32_fp_register_operand" "")
		 (match_operand:SF 2 "avr32_fp_register_operand" "")))
   (set (match_operand:SF          3 "avr32_fp_register_operand" "")
	(minus:SF
	 (match_dup 0)
	 (match_dup 3)))]
  "TARGET_HARD_FLOAT && peep2_reg_dead_p(2, operands[0])"
  [(set (match_dup 3)
	(minus:SF  (mult:SF (match_dup 1)
			    (match_dup 2))
		   (match_dup 3)))]
)

(define_insn "nmsubacsf3"
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "=f")
	(minus:SF  (neg:SF (mult:SF (match_operand:SF 1 "avr32_fp_register_operand" "f")
                                    (match_operand:SF 2 "avr32_fp_register_operand" "f")))
                   (match_operand:SF 3 "avr32_fp_register_operand" "0")))]
  "TARGET_HARD_FLOAT"
  "fnmsc.s\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])



(define_insn "addsf3"
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "=f")
	(plus:SF (match_operand:SF 1 "avr32_fp_register_operand" "f")
		 (match_operand:SF 2 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fadd.s\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "subsf3"
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "=f")
	(minus:SF (match_operand:SF 1 "avr32_fp_register_operand" "f")
                  (match_operand:SF 2 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fsub.s\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])


(define_insn "negsf2"
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "=f")
	(neg:SF (match_operand:SF 1 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fneg.s\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "type" "fmv")])

(define_insn "abssf2"
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "=f")
	(abs:SF (match_operand:SF 1 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fabs.s\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "type" "fmv")])

(define_insn "truncdfsf2"
  [(set (match_operand:SF          0 "avr32_fp_register_operand" "=f")
	(float_truncate:SF
         (match_operand:DF 1 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcastd.s\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "type" "fcast")])

(define_insn "extendsfdf2"
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "=f")
	(float_extend:DF
         (match_operand:SF 1 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fcasts.d\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "type" "fcast")])

(define_insn "muldf3"
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "=f")
	(mult:DF (match_operand:DF 1 "avr32_fp_register_operand" "f")
		 (match_operand:DF 2 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fmul.d\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "nmuldf3"
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "=f")
	(neg:DF (mult:DF (match_operand:DF 1 "avr32_fp_register_operand" "f")
                         (match_operand:DF 2 "avr32_fp_register_operand" "f"))))]
  "TARGET_HARD_FLOAT"
  "fnmul.d\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_peephole2
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "")
	(mult:DF (match_operand:DF 1 "avr32_fp_register_operand" "")
		 (match_operand:DF 2 "avr32_fp_register_operand" "")))
   (set (match_operand:DF          3 "avr32_fp_register_operand" "")
	(neg:DF (match_dup 0)))]
  "TARGET_HARD_FLOAT &&
   (peep2_reg_dead_p(2, operands[0]) || (REGNO(operands[3]) == REGNO(operands[0])))"
  [(set (match_dup 3)
	(neg:DF (mult:DF (match_dup 1)
			 (match_dup 2))))]
)

(define_insn "macdf3"
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "=f")
	(plus:DF (mult:DF (match_operand:DF 1 "avr32_fp_register_operand" "f")
                          (match_operand:DF 2 "avr32_fp_register_operand" "f"))
                 (match_operand:DF 3 "avr32_fp_register_operand" "0")))]
  "TARGET_HARD_FLOAT"
  "fmac.d\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "msubacdf3"
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "=f")
	(minus:DF (mult:DF (match_operand:DF 1 "avr32_fp_register_operand" "f")
                           (match_operand:DF 2 "avr32_fp_register_operand" "f"))
                  (match_operand:DF 3 "avr32_fp_register_operand" "0")))]
  "TARGET_HARD_FLOAT"
  "fmsc.d\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_peephole2
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "")
	(mult:DF (match_operand:DF 1 "avr32_fp_register_operand" "")
		 (match_operand:DF 2 "avr32_fp_register_operand" "")))
   (set (match_operand:DF          3 "avr32_fp_register_operand" "")
	(minus:DF
	 (match_dup 0)
	 (match_dup 3)))]
  "TARGET_HARD_FLOAT && peep2_reg_dead_p(2, operands[0])"
  [(set (match_dup 3)
	(minus:DF  (mult:DF (match_dup 1)
			    (match_dup 2))
		   (match_dup 3)))]
  )

(define_insn "nmsubacdf3"
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "=f")
	(minus:DF  (neg:DF (mult:DF (match_operand:DF 1 "avr32_fp_register_operand" "f")
                                    (match_operand:DF 2 "avr32_fp_register_operand" "f")))
                   (match_operand:DF 3 "avr32_fp_register_operand" "0")))]
  "TARGET_HARD_FLOAT"
  "fnmsc.d\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "nmacdf3"
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "=f")
	(plus:DF  (neg:DF (mult:DF (match_operand:DF 1 "avr32_fp_register_operand" "f")
                                   (match_operand:DF 2 "avr32_fp_register_operand" "f")))
                  (match_operand:DF 3 "avr32_fp_register_operand" "0")))]
  "TARGET_HARD_FLOAT"
  "fnmac.d\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_peephole2
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "")
	(mult:DF (match_operand:DF 1 "avr32_fp_register_operand" "")
		 (match_operand:DF 2 "avr32_fp_register_operand" "")))
   (set (match_operand:DF          3 "avr32_fp_register_operand" "")
	(minus:DF
	 (match_dup 3)
	 (match_dup 0)))]
  "TARGET_HARD_FLOAT && peep2_reg_dead_p(2, operands[0])"
  [(set (match_dup 3)
	(plus:DF  (neg:DF (mult:DF (match_dup 1)
                                   (match_dup 2)))
                  (match_dup 3)))]
)

(define_insn "adddf3"
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "=f")
	(plus:DF (match_operand:DF 1 "avr32_fp_register_operand" "f")
		 (match_operand:DF 2 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fadd.d\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "subdf3"
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "=f")
	(minus:DF (match_operand:DF 1 "avr32_fp_register_operand" "f")
                  (match_operand:DF 2 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fsub.d\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "negdf2"
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "=f")
	(neg:DF (match_operand:DF 1 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fneg.d\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "type" "fmv")])

(define_insn "absdf2"
  [(set (match_operand:DF          0 "avr32_fp_register_operand" "=f")
	(abs:DF (match_operand:DF 1 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  "fabs.d\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "type" "fmv")])


(define_expand "cmpdf"
  [(set (cc0)
	(compare:DF
	 (match_operand:DF 0 "general_operand" "")
	 (match_operand:DF 1 "general_operand"  "")))]
  "TARGET_HARD_FLOAT"
  "{
   rtx tmpreg;
   if ( !REG_P(operands[0]) )
     operands[0] = force_reg(DFmode, operands[0]);

   if ( !REG_P(operands[1]) )
     operands[1] = force_reg(DFmode, operands[1]);

   avr32_compare_op0 = operands[0];
   avr32_compare_op1 = operands[1];

   emit_insn(gen_cmpdf_internal(operands[0], operands[1]));

   tmpreg = gen_reg_rtx(SImode);
   emit_insn(gen_fpcc_to_reg(tmpreg));
   emit_insn(gen_reg_to_cc(tmpreg));

   DONE;
  }"
)

(define_insn "cmpdf_internal"
  [(set (reg:CC FPCC_REGNUM)
	(compare:CC
	 (match_operand:DF 0 "avr32_fp_register_operand" "f")
	 (match_operand:DF 1 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  {
   if (!rtx_equal_p(cc_prev_status.mdep.fpvalue, SET_SRC(PATTERN (insn))) )
      return "fcmp.d\t%0, %1";
   return "";
  }
  [(set_attr "length" "4")
   (set_attr "type" "fcmpd")
   (set_attr "cc" "fpcompare")])

(define_expand "cmpsf"
  [(set (cc0)
	(compare:SF
	 (match_operand:SF 0 "general_operand" "")
	 (match_operand:SF 1 "general_operand"  "")))]
  "TARGET_HARD_FLOAT"
  "{
   rtx tmpreg;
   if ( !REG_P(operands[0]) )
     operands[0] = force_reg(SFmode, operands[0]);

   if ( !REG_P(operands[1]) )
     operands[1] = force_reg(SFmode, operands[1]);

   avr32_compare_op0 = operands[0];
   avr32_compare_op1 = operands[1];

   emit_insn(gen_cmpsf_internal(operands[0], operands[1]));

   tmpreg = gen_reg_rtx(SImode);
   emit_insn(gen_fpcc_to_reg(tmpreg));
   emit_insn(gen_reg_to_cc(tmpreg));

   DONE;
  }"
)

(define_insn "cmpsf_internal"
  [(set (reg:CC FPCC_REGNUM)
	(compare:CC
	 (match_operand:SF 0 "avr32_fp_register_operand" "f")
	 (match_operand:SF 1 "avr32_fp_register_operand" "f")))]
  "TARGET_HARD_FLOAT"
  {
   if (!rtx_equal_p(cc_prev_status.mdep.fpvalue, SET_SRC(PATTERN (insn))) )
      return "fcmp.s\t%0, %1";
   return "";
  }
  [(set_attr "length" "4")
   (set_attr "type" "fcmps")
   (set_attr "cc" "fpcompare")])

(define_insn "fpcc_to_reg"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unspec:SI [(reg:CC FPCC_REGNUM)]
		   UNSPEC_FPCC_TO_REG))]
  "TARGET_HARD_FLOAT"
  "fmov.s\t%0, fsr"
  [(set_attr "length" "4")
   (set_attr "type" "fmvcpu")])

(define_insn "reg_to_cc"
  [(set (cc0)
	(unspec:SI [(match_operand:SI 0 "register_operand" "r")]
		   UNSPEC_REG_TO_CC))]
  "TARGET_HARD_FLOAT"
  "musfr\t%0"
  [(set_attr "length" "2")
   (set_attr "type" "alu")
   (set_attr "cc" "from_fpcc")])

(define_insn "stm_fp"
  [(unspec [(match_operand 0 "register_operand" "r")
            (match_operand 1 "const_int_operand" "")
            (match_operand 2 "const_int_operand" "")]
	   UNSPEC_STMFP)]
  "TARGET_HARD_FLOAT"
  {
    int cop_reglist = INTVAL(operands[1]);

    if (INTVAL(operands[2]) != 0)
      return "stcm.w\tcp0, --%0, %C1";
    else
      return "stcm.w\tcp0, %0, %C1";

    if ( cop_reglist & ~0xff ){
      operands[1] = GEN_INT(cop_reglist & ~0xff);
      if (INTVAL(operands[2]) != 0)
         return "stcm.d\tcp0, --%0, %D1";
      else
         return "stcm.d\tcp0, %0, %D1";
    }
  }
  [(set_attr "type" "fstm")
   (set_attr "length" "4")
   (set_attr "cc" "none")])
