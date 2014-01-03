;;   AVR32 predicates file.
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


;; True if the operand is a memory reference which contains an
;; Address consisting of a single pointer register
(define_predicate "avr32_indirect_register_operand"
  (and (match_code "mem")
       (match_test "register_operand(XEXP(op, 0), SImode)")))



;; Address expression with a base pointer offset with
;; a register displacement
(define_predicate "avr32_indexed_memory_operand"
  (and (match_code "mem")
       (match_test "GET_CODE(XEXP(op, 0)) == PLUS"))
  {

   rtx op0 = XEXP(XEXP(op, 0), 0);
   rtx op1 = XEXP(XEXP(op, 0), 1);

   return ((avr32_address_register_rtx_p (op0, 0)
            && avr32_legitimate_index_p (GET_MODE(op), op1, 0))
	   || (avr32_address_register_rtx_p (op1, 0)
            && avr32_legitimate_index_p (GET_MODE(op), op0, 0)));

 })

;; Operand suitable for the ld.sb instruction
(define_predicate "load_sb_memory_operand"
  (ior (match_operand 0 "avr32_indirect_register_operand")
       (match_operand 0 "avr32_indexed_memory_operand")))


;; Operand suitable as operand to insns sign extending QI values
(define_predicate "extendqi_operand"
  (ior (match_operand 0 "load_sb_memory_operand")
       (match_operand 0 "register_operand")))

(define_predicate "post_inc_memory_operand"
  (and (match_code "mem")
       (match_test "(GET_CODE(XEXP(op, 0)) == POST_INC)
                     && REG_P(XEXP(XEXP(op, 0), 0))")))

(define_predicate "pre_dec_memory_operand"
  (and (match_code "mem")
       (match_test "(GET_CODE(XEXP(op, 0)) == PRE_DEC)
                     && REG_P(XEXP(XEXP(op, 0), 0))")))

;; Operand suitable for add instructions
(define_predicate "avr32_add_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_operand 0 "immediate_operand")
            (match_test "CONST_OK_FOR_CONSTRAINT_P(INTVAL(op), 'I', \"Is21\")"))))

;; Operand is a power of two immediate
(define_predicate "power_of_two_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT value = INTVAL (op);

  return value != 0 && (value & (value - 1)) == 0;
})

;; Operand is a multiple of 8 immediate
(define_predicate "multiple_of_8_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT value = INTVAL (op);

  return (value & 0x7) == 0 ;
})

;; Operand is a multiple of 16 immediate
(define_predicate "multiple_of_16_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT value = INTVAL (op);

  return (value & 0xf) == 0 ;
})

;; Operand is a mask used for masking away upper bits of a reg
(define_predicate "avr32_mask_upper_bits_operand"
  (match_code "const_int")
{
  HOST_WIDE_INT value = INTVAL (op) + 1;

  return value != 1 && value != 0 && (value & (value - 1)) == 0;
})


;; Operand suitable for mul instructions
(define_predicate "avr32_mul_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_operand 0 "immediate_operand")
            (match_test "CONST_OK_FOR_CONSTRAINT_P(INTVAL(op), 'K', \"Ks08\")"))))

;; True for logical binary operators.
(define_predicate "logical_binary_operator"
  (match_code "ior,xor,and"))

;; True for logical shift operators
(define_predicate "logical_shift_operator"
  (match_code "ashift,lshiftrt"))

;; True for shift operand for logical and, or and eor insns
(define_predicate "avr32_logical_shift_operand"
  (and (match_code "ashift,lshiftrt")
       (ior (and (match_test "GET_CODE(XEXP(op, 1)) == CONST_INT")
                 (match_test "register_operand(XEXP(op, 0), GET_MODE(XEXP(op, 0)))"))
            (and (match_test "GET_CODE(XEXP(op, 0)) == CONST_INT")
                 (match_test "register_operand(XEXP(op, 1), GET_MODE(XEXP(op, 1)))"))))
  )


;; Predicate for second operand to and, ior and xor insn patterns
(define_predicate "avr32_logical_insn_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "avr32_logical_shift_operand"))
)


;; True for avr32 comparison operators
(define_predicate "avr32_comparison_operator"
  (ior (match_code "eq, ne, gt, ge, lt, le, gtu, geu, ltu, leu")
       (and (match_code "unspec")
            (match_test "(XINT(op, 1) == UNSPEC_COND_MI)
                         || (XINT(op, 1) == UNSPEC_COND_PL)"))))

(define_predicate "avr32_cond3_comparison_operator"
  (ior (match_code "eq, ne, ge, lt, geu, ltu")
       (and (match_code "unspec")
            (match_test "(XINT(op, 1) == UNSPEC_COND_MI)
                         || (XINT(op, 1) == UNSPEC_COND_PL)"))))

;; True for avr32 comparison operand
(define_predicate "avr32_comparison_operand"
  (ior (and (match_code "eq, ne, gt, ge, lt, le, gtu, geu, ltu, leu")
            (match_test "(CC0_P (XEXP(op,0)) && rtx_equal_p (XEXP(op,1), const0_rtx))"))
       (and (match_code "unspec")
            (match_test "(XINT(op, 1) == UNSPEC_COND_MI)
                         || (XINT(op, 1) == UNSPEC_COND_PL)"))))

;; True if this is a const_int with one bit set
(define_predicate "one_bit_set_operand"
  (match_code "const_int")
  {
   int i;
   int value;
   int ones = 0;

   value = INTVAL(op);
   for ( i = 0 ; i < 32; i++ ){
     if ( value & ( 1 << i ) ){
        ones++;
      }
   }

   return ( ones == 1 );
  })


;; True if this is a const_int with one bit cleared
(define_predicate "one_bit_cleared_operand"
  (match_code "const_int")
  {
   int i;
   int value;
   int zeroes = 0;

   value = INTVAL(op);
   for ( i = 0 ; i < 32; i++ ){
     if ( !(value & ( 1 << i )) ){
        zeroes++;
      }
   }

   return ( zeroes == 1 );
  })


;; Immediate all the low 16-bits cleared
(define_predicate "avr32_hi16_immediate_operand"
  (match_code "const_int")
  {
   /* If the low 16-bits are zero then this
      is a hi16 immediate. */
   return ((INTVAL(op) & 0xffff) == 0);
   }
)

;; True if this is a register or immediate operand
(define_predicate "register_immediate_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "immediate_operand")))

;; True if this is a register or const_int operand
(define_predicate "register_const_int_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_operand 0 "const_int_operand")
            (match_operand 0 "immediate_operand"))))

;; True if this is a register or const_double operand
(define_predicate "register_const_double_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "const_double_operand")))

;; True if this is an operand containing a label_ref.
(define_predicate "avr32_label_ref_operand"
  (and (match_code "mem")
       (match_test "avr32_find_symbol(op)
                    && (GET_CODE(avr32_find_symbol(op)) == LABEL_REF)")))

;; True if this is a valid symbol pointing to the constant pool.
(define_predicate "avr32_const_pool_operand"
  (and (match_code "symbol_ref")
       (match_test "CONSTANT_POOL_ADDRESS_P(op)"))
  {
        return (flag_pic ? (!(symbol_mentioned_p (get_pool_constant (op))
                        || label_mentioned_p (get_pool_constant (op)))
                       || avr32_got_mentioned_p(get_pool_constant (op)))
                    : true);
  }
)

;; True if this is a memory reference to the constant or mini pool.
(define_predicate "avr32_const_pool_ref_operand"
  (ior (match_operand 0 "avr32_label_ref_operand")
       (and (match_code "mem")
            (match_test "avr32_const_pool_operand(XEXP(op,0), GET_MODE(XEXP(op,0)))"))))


;; Legal source operand for movti insns
(define_predicate "avr32_movti_src_operand"
  (ior (match_operand 0 "avr32_const_pool_ref_operand")
       (ior (ior (match_operand 0 "register_immediate_operand")
                 (match_operand 0 "avr32_indirect_register_operand"))
            (match_operand 0 "post_inc_memory_operand"))))
  
;; Legal destination operand for movti insns
(define_predicate "avr32_movti_dst_operand"
  (ior (ior (match_operand 0 "register_operand")
            (match_operand 0 "avr32_indirect_register_operand"))
       (match_operand 0 "pre_dec_memory_operand")))


;; True if this is a k12 offseted memory operand.
(define_predicate "avr32_k12_memory_operand"
  (and (match_code "mem")
       (ior (match_test "REG_P(XEXP(op, 0))")
            (match_test "GET_CODE(XEXP(op, 0)) == PLUS
                         && REG_P(XEXP(XEXP(op, 0), 0))
                         && (GET_CODE(XEXP(XEXP(op, 0), 1)) == CONST_INT)
                         && (CONST_OK_FOR_CONSTRAINT_P(INTVAL(XEXP(XEXP(op, 0), 0)),
                                'K', (mode == SImode) ? \"Ks14\" : ((mode == HImode) ? \"Ks13\" : \"Ks12\")))"))))

;; True if this is a memory operand with an immediate displacement.
(define_predicate "avr32_imm_disp_memory_operand"
  (and (match_code "mem")
       (match_test "GET_CODE(XEXP(op, 0)) == PLUS
                    && REG_P(XEXP(XEXP(op, 0), 0))
                    && (GET_CODE(XEXP(XEXP(op, 0), 1)) == CONST_INT)")))

;; True if this is a bswap operand.
(define_predicate "avr32_bswap_operand"
  (ior (match_operand 0 "avr32_k12_memory_operand")
       (match_operand 0 "register_operand")))

;; True if this is a valid coprocessor insn memory operand.
(define_predicate "avr32_cop_memory_operand"
  (and (match_operand 0 "memory_operand")
       (not (match_test "GET_CODE(XEXP(op, 0)) == PLUS
                         && REG_P(XEXP(XEXP(op, 0), 0))
                         && (GET_CODE(XEXP(XEXP(op, 0), 1)) == CONST_INT)
                         && !(CONST_OK_FOR_CONSTRAINT_P(INTVAL(XEXP(XEXP(op, 0), 0)), 'K', \"Ku10\"))"))))

;; True if this is a valid source/destination operand.
;; for moving values to/from a coprocessor
(define_predicate "avr32_cop_move_operand"
  (ior (match_operand 0 "register_operand")
       (match_operand 0 "avr32_cop_memory_operand")))


;; True if this is a valid extract byte offset for use in
;; load extracted index insns.
(define_predicate "avr32_extract_shift_operand"
  (and (match_operand 0 "const_int_operand")
       (match_test "(INTVAL(op) == 0) || (INTVAL(op) == 8)
                    || (INTVAL(op) == 16) || (INTVAL(op) == 24)")))

;; True if this is a valid avr32 symbol operand.
(define_predicate "avr32_symbol_operand"
   (and (match_code "label_ref, symbol_ref, const")
        (match_test "avr32_find_symbol(op)")))

;; True if this is a valid operand for the lda.w and call pseudo insns.
(define_predicate "avr32_address_operand"
   (and (and (match_code "label_ref, symbol_ref")
             (match_test "avr32_find_symbol(op)"))
       (ior (match_test "TARGET_HAS_ASM_ADDR_PSEUDOS")
            (match_test "flag_pic")) ))

;; An immediate k16 address operand
(define_predicate "avr32_ks16_address_operand"
  (and (match_operand 0 "address_operand")
       (ior (match_test "REG_P(op)")
            (match_test "GET_CODE(op) == PLUS
                         && ((GET_CODE(XEXP(op,0)) == CONST_INT)
                             || (GET_CODE(XEXP(op,1)) == CONST_INT))")) ))

;; An offset k16 memory operand
(define_predicate "avr32_ks16_memory_operand"
  (and (match_code "mem")
       (match_test "avr32_ks16_address_operand (XEXP (op, 0), GET_MODE (XEXP (op, 0)))")))

;; An immediate k11 address operand
(define_predicate "avr32_ks11_address_operand"
  (and (match_operand 0 "address_operand")
       (ior (match_test "REG_P(op)")
            (match_test "GET_CODE(op) == PLUS
                         && (((GET_CODE(XEXP(op,0)) == CONST_INT)
                              && avr32_const_ok_for_constraint_p(INTVAL(XEXP(op,0)), 'K', \"Ks11\"))
                             || ((GET_CODE(XEXP(op,1)) == CONST_INT)
                                 && avr32_const_ok_for_constraint_p(INTVAL(XEXP(op,1)), 'K', \"Ks11\")))")) ))

;; True if this is a avr32 call operand
(define_predicate "avr32_call_operand"
  (ior (ior (match_operand 0 "register_operand")
            (ior (match_operand 0 "avr32_const_pool_ref_operand")
                 (match_operand 0 "avr32_address_operand")))
       (match_test "SYMBOL_REF_RCALL_FUNCTION_P(op)")))

;; Return true for operators performing ALU operations

(define_predicate "alu_operator"
  (match_code "ior, xor, and, plus, minus, ashift, lshiftrt, ashiftrt"))

(define_predicate "avr32_add_shift_immediate_operand"
  (and (match_operand 0 "immediate_operand")
       (match_test "CONST_OK_FOR_CONSTRAINT_P(INTVAL(op), 'K', \"Ku02\")")))

(define_predicate "avr32_cond_register_immediate_operand"
  (ior (match_operand 0 "register_operand")
       (and (match_operand 0 "immediate_operand")
            (match_test "CONST_OK_FOR_CONSTRAINT_P(INTVAL(op), 'K', \"Ks08\")"))))

(define_predicate "avr32_cond_immediate_operand"
  (and (match_operand 0 "immediate_operand")
       (match_test "CONST_OK_FOR_CONSTRAINT_P(INTVAL(op), 'I', \"Is08\")")))


(define_predicate "avr32_cond_move_operand"
  (ior (ior (match_operand 0 "register_operand")
            (and (match_operand 0 "immediate_operand")
                 (match_test "CONST_OK_FOR_CONSTRAINT_P(INTVAL(op), 'K', \"Ks08\")")))
       (and (match_test "TARGET_V2_INSNS")
            (match_operand 0 "memory_operand"))))

(define_predicate "avr32_mov_immediate_operand"
  (and (match_operand 0 "immediate_operand")
       (match_test "avr32_const_ok_for_move(INTVAL(op))")))


(define_predicate "avr32_rmw_address_operand"
  (ior (and (match_code "symbol_ref") 
            (match_test "({rtx symbol = avr32_find_symbol(op); \
                               symbol && (GET_CODE (symbol) == SYMBOL_REF) && SYMBOL_REF_RMW_ADDR(symbol);})"))
       (and (match_operand 0 "immediate_operand")
            (match_test "CONST_OK_FOR_CONSTRAINT_P(INTVAL(op), 'K', \"Ks17\")")))
  {
     return TARGET_RMW && !flag_pic;
  }
)
 
(define_predicate "avr32_rmw_memory_operand"
  (and (match_code "mem") 
       (match_test "!volatile_refs_p(op) && (GET_MODE(op) == SImode) && 
                    avr32_rmw_address_operand(XEXP(op, 0), GET_MODE(XEXP(op, 0)))")))

(define_predicate "avr32_rmw_memory_or_register_operand"
  (ior (match_operand 0 "avr32_rmw_memory_operand")
       (match_operand 0 "register_operand")))

(define_predicate "avr32_non_rmw_memory_operand"
  (and (not (match_operand 0 "avr32_rmw_memory_operand"))
       (match_operand 0 "memory_operand")))

(define_predicate "avr32_non_rmw_general_operand"
  (and (not (match_operand 0 "avr32_rmw_memory_operand"))
       (match_operand 0 "general_operand")))

(define_predicate "avr32_non_rmw_nonimmediate_operand"
  (and (not (match_operand 0 "avr32_rmw_memory_operand"))
       (match_operand 0 "nonimmediate_operand")))

;; Return true if the operand is the 1.0f constant.

(define_predicate "const_1f_operand"
  (match_code "const_int,const_double")
{
  return (op == CONST1_RTX (SFmode));
})
