;;   AVR32 machine description file for Floating-Point instructions.
;;   Copyright 2003-2006 Atmel Corporation.
;;
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

(define_insn "*movsf_uc3fp"
  [(set (match_operand:SF 0 "nonimmediate_operand"     "=r,r,r,m")
	(match_operand:SF 1 "general_operand"          "r,G,m,r"))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "@
   mov\t%0, %1
   mov\t%0, %1
   ld.w\t%0, %1
   st.w\t%0, %1"
  [(set_attr "length" "2,4,4,4")
   (set_attr "type" "alu,alu,load,store")])

(define_insn "mulsf3"
  [(set (match_operand:SF          0 "register_operand" "=r")
	(mult:SF (match_operand:SF 1 "register_operand" "r")
		 (match_operand:SF 2 "register_operand" "r")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "fmul.s\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "nmulsf3"
  [(set (match_operand:SF          0 "register_operand" "=r")
	(neg:SF (mult:SF (match_operand:SF 1 "register_operand" "%r")
                         (match_operand:SF 2 "register_operand" "r"))))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "fnmul.s\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "macsf3"
  [(set (match_operand:SF          0 "register_operand" "=r")
	(plus:SF (mult:SF (match_operand:SF 1 "register_operand" "r")
                          (match_operand:SF 2 "register_operand" "r"))
                 (match_operand:SF 3 "register_operand" "r")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "fmac.s\t%0, %3, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

;(define_insn "nmacsf3"
;  [(set (match_operand:SF          0 "register_operand" "=r")
;	(plus:SF  (neg:SF (match_operand:SF 1 "register_operand" "r"))
;                            (mult:SF(match_operand:SF 2 "register_operand" "r")
;                                    (match_operand:SF 3 "register_operand" "r"))))]
;  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
;  "fnmac.s\t%0, %1, %2, %3"
;  [(set_attr "length" "4")
;   (set_attr "type" "fmul")])

(define_insn "nmacsf3"
  [(set (match_operand:SF          0 "register_operand" "=r")
	(minus:SF  (mult:SF (match_operand:SF 2 "register_operand" "r")
                        (match_operand:SF 3 "register_operand" "r"))
	                    (match_operand:SF 1 "register_operand" "r")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "fnmac.s\t%0, %1, %2, %3"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "msubacsf3"
  [(set (match_operand:SF          0 "register_operand" "=r")
	(minus:SF (match_operand:SF 3 "register_operand" "r")
	          (mult:SF (match_operand:SF 1 "register_operand" "r")
                       (match_operand:SF 2 "register_operand" "r"))))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "fmsc.s\t%0, %3, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "nmsubacsf3"
  [(set (match_operand:SF          0 "register_operand" "=r")
	(minus:SF  (neg:SF (mult:SF (match_operand:SF 1 "register_operand" "r")
                                    (match_operand:SF 2 "register_operand" "r")))
                   (match_operand:SF 3 "register_operand" "r")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "fnmsc.s\t%0, %3, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "addsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(plus:SF (match_operand:SF 1 "register_operand" "%r")
                   (match_operand:SF 2 "register_operand" "r")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "fadd.s\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "subsf3"
  [(set (match_operand:SF          0 "register_operand" "=r")
	(minus:SF (match_operand:SF 1 "register_operand" "r")
                  (match_operand:SF 2 "register_operand" "r")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "fsub.s\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "fmul")])

(define_insn "fixuns_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(unsigned_fix:SI (match_operand:SF 1 "register_operand" "r")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "fcastrs.uw\t%0, %1"
  [(set_attr "length" "4")])

(define_insn "fix_truncsfsi2"
  [(set (match_operand:SI 0 "register_operand" "=r")
	(fix:SI (match_operand:SF 1 "register_operand" "r")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "fcastrs.sw\t%0, %1"
  [(set_attr "length" "4")])

(define_insn "floatunssisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (unsigned_float:SF (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "fcastuw.s\t%0, %1"
  [(set_attr "length" "4")])

(define_insn "floatsisf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
        (float:SF (match_operand:SI 1 "register_operand" "r")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "fcastsw.s\t%0, %1"
  [(set_attr "length" "4")])

(define_insn "cmpsf_internal_uc3fp"
  [(set (cc0)
        (compare:CC
         (match_operand:SF 0 "register_operand" "r")
         (match_operand:SF 1 "register_operand" "r")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  {
        avr32_branch_type = CMP_SF;
   if (!rtx_equal_p(cc_prev_status.mdep.value, SET_SRC(PATTERN (insn))) )
      return "fcmp.s\t%0, %1";
   return "";
  }
  [(set_attr "length" "4")
   (set_attr "cc" "compare")])

(define_expand "divsf3"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(div:SF (match_operand:SF 1 "register_operand" "r")
		 (match_operand:SF 2 "register_operand" "r")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT && flag_unsafe_math_optimizations"
  "{
    emit_insn(gen_frcpa_internal(operands[0],operands[2]));
    emit_insn(gen_mulsf3(operands[0],operands[0],operands[1]));
    DONE;
  }"  
)

(define_insn "frcpa_internal"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(unspec:SF [(match_operand:SF 1 "register_operand" "r")] UNSPEC_FRCPA))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "frcpa.s %0,%1"
  [(set_attr "length" "4")])

(define_expand "sqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "")
	(sqrt:SF (match_operand:SF 1 "register_operand" "")))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT && flag_unsafe_math_optimizations"
  "
{
  rtx scratch = gen_reg_rtx (SFmode);
  emit_insn (gen_rsqrtsf2 (scratch, operands[1], CONST1_RTX (SFmode)));
  emit_insn (gen_divsf3(operands[0], force_reg (SFmode, CONST1_RTX (SFmode)),
			 scratch));
  DONE;
}")

(define_insn "rsqrtsf2"
  [(set (match_operand:SF 0 "register_operand" "=r")
	(div:SF (match_operand:SF 2 "const_1f_operand" "F")
		(sqrt:SF (match_operand:SF 1 "register_operand" "?r"))))]
  "TARGET_ARCH_FPU && TARGET_HARD_FLOAT"
  "frsqrta.s %1, %0")
