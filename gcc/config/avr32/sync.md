;;=================================================================
;; Atomic operations
;;=================================================================


(define_insn "sync_compare_and_swapsi"
  [(set (match_operand:SI 0 "register_operand" "=&r,&r")
	(match_operand:SI 1 "memory_operand" "+RKs16,+RKs16"))
   (set (match_dup 1)
	(unspec_volatile:SI
	  [(match_dup 1)
	   (match_operand:SI 2 "register_immediate_operand" "r,Ks21")
	   (match_operand:SI 3 "register_operand" "r,r")]
	  VUNSPEC_SYNC_CMPXCHG))   ]
  ""
  "0:
   ssrf\t5
   ld.w\t%0,%1
   cp.w\t%0,%2
   brne\t0f
   stcond\t%1, %3
   brne\t0b
   0:
  "
  [(set_attr "length" "16,18")
   (set_attr "cc" "clobber")]
  )
 

(define_code_iterator atomic_op [plus minus and ior xor])
(define_code_attr  atomic_asm_insn [(plus "add") (minus "sub") (and "and") (ior "or") (xor "eor")])
(define_code_attr  atomic_insn [(plus "add") (minus "sub") (and "and") (ior "ior") (xor "xor")])

(define_insn "sync_loadsi"
  ; NB! Put an early clobber on the destination operand to 
  ; avoid gcc using the same register in the source and 
  ; destination. This is done in order to avoid gcc to 
  ; clobber the source operand since these instructions
  ; are actually inside a "loop".
  [(set (match_operand:SI 0 "register_operand" "=&r")
	(unspec_volatile:SI
         [(match_operand:SI 1 "avr32_ks16_memory_operand" "RKs16")
          (label_ref (match_operand 2 "" ""))]
         VUNSPEC_SYNC_SET_LOCK_AND_LOAD) )]
  ""
  "%2:
   ssrf\t5
   ld.w\t%0,%1"
  [(set_attr "length" "6")
   (set_attr "cc" "clobber")]
  )
  
(define_insn "sync_store_if_lock"
  [(set (match_operand:SI 0 "avr32_ks16_memory_operand" "=RKs16")
        (unspec_volatile:SI
         [(match_operand:SI 1 "register_operand" "r")
          (label_ref (match_operand 2 "" ""))]
         VUNSPEC_SYNC_STORE_IF_LOCK) )]
  ""
  "stcond\t%0, %1
   brne\t%2"
  [(set_attr "length" "6")
   (set_attr "cc" "clobber")]
  )


(define_expand "sync_<atomic_insn>si"
  [(set (match_dup 2)
	(unspec_volatile:SI
         [(match_operand:SI 0 "avr32_ks16_memory_operand" "")
          (match_dup 3)]
         VUNSPEC_SYNC_SET_LOCK_AND_LOAD))
   (set (match_dup 2) 
        (atomic_op:SI (match_dup 2)
                      (match_operand:SI 1 "register_immediate_operand" "")))
   (set (match_dup 0)
        (unspec_volatile:SI
         [(match_dup 2)
          (match_dup 3)]
         VUNSPEC_SYNC_STORE_IF_LOCK) )
   (use (match_dup 1))
   (use (match_dup 4))]
  ""
  {
   rtx *mem_expr = &operands[0];
   rtx ptr_reg;
   if ( !avr32_ks16_memory_operand (*mem_expr, GET_MODE (*mem_expr)) )
    {
      ptr_reg = force_reg (Pmode, XEXP (*mem_expr, 0));
      XEXP (*mem_expr, 0) = ptr_reg;
    } 
   else 
    {
      rtx address = XEXP (*mem_expr, 0);
      if ( REG_P (address) )
         ptr_reg = address;
      else if ( REG_P (XEXP (address, 0)) ) 
         ptr_reg = XEXP (address, 0);
      else 
         ptr_reg = XEXP (address, 1);
    }

   operands[2] = gen_reg_rtx (SImode);
   operands[3] = gen_rtx_LABEL_REF(Pmode, gen_label_rtx ());
   operands[4] = ptr_reg;   

  }
  )



(define_expand "sync_old_<atomic_insn>si"
  [(set (match_operand:SI 0 "register_operand" "")
	(unspec_volatile:SI
         [(match_operand:SI 1 "avr32_ks16_memory_operand" "")
          (match_dup 4)]
         VUNSPEC_SYNC_SET_LOCK_AND_LOAD))
   (set (match_dup 3) 
        (atomic_op:SI (match_dup 0)
                      (match_operand:SI 2 "register_immediate_operand" "")))
   (set (match_dup 1)
        (unspec_volatile:SI
         [(match_dup 3)
          (match_dup 4)]
         VUNSPEC_SYNC_STORE_IF_LOCK) )
   (use (match_dup 2))
   (use (match_dup 5))]
  ""
  {
   rtx *mem_expr = &operands[1];
   rtx ptr_reg;
   if ( !avr32_ks16_memory_operand (*mem_expr, GET_MODE (*mem_expr)) )
    {
      ptr_reg = force_reg (Pmode, XEXP (*mem_expr, 0));
      XEXP (*mem_expr, 0) = ptr_reg;
    } 
   else 
    {
      rtx address = XEXP (*mem_expr, 0);
      if ( REG_P (address) )
         ptr_reg = address;
      else if ( REG_P (XEXP (address, 0)) ) 
         ptr_reg = XEXP (address, 0);
      else 
         ptr_reg = XEXP (address, 1);
    }

   operands[3] = gen_reg_rtx (SImode);
   operands[4] = gen_rtx_LABEL_REF(Pmode, gen_label_rtx ());
   operands[5] = ptr_reg;
  }
  )

(define_expand "sync_new_<atomic_insn>si"
  [(set (match_operand:SI 0 "register_operand" "")
	(unspec_volatile:SI
         [(match_operand:SI 1 "avr32_ks16_memory_operand" "")
          (match_dup 3)]
         VUNSPEC_SYNC_SET_LOCK_AND_LOAD))
   (set (match_dup 0) 
        (atomic_op:SI (match_dup 0)
                      (match_operand:SI 2 "register_immediate_operand" "")))
   (set (match_dup 1)
        (unspec_volatile:SI
         [(match_dup 0)
          (match_dup 3)]
         VUNSPEC_SYNC_STORE_IF_LOCK) )
   (use (match_dup 2))
   (use (match_dup 4))]
  ""
  {
   rtx *mem_expr = &operands[1];
   rtx ptr_reg;
   if ( !avr32_ks16_memory_operand (*mem_expr, GET_MODE (*mem_expr)) )
    {
      ptr_reg = force_reg (Pmode, XEXP (*mem_expr, 0));
      XEXP (*mem_expr, 0) = ptr_reg;
    } 
   else 
    {
      rtx address = XEXP (*mem_expr, 0);
      if ( REG_P (address) )
         ptr_reg = address;
      else if ( REG_P (XEXP (address, 0)) ) 
         ptr_reg = XEXP (address, 0);
      else 
         ptr_reg = XEXP (address, 1);
    }

   operands[3] = gen_rtx_LABEL_REF(Pmode, gen_label_rtx ());
   operands[4] = ptr_reg;
  }
  )


;(define_insn "sync_<atomic_insn>si"
;  [(set (match_operand:SI 0 "memory_operand" "+RKs16")
;	(unspec_volatile:SI
;         [(atomic_op:SI (match_dup 0)
;                        (match_operand:SI 1 "register_operand" "r"))]
;         VUNSPEC_SYNC_CMPXCHG))
;   (clobber (match_scratch:SI 2 "=&r"))]
;  ""
;  "0:
;   ssrf\t5
;   ld.w\t%2,%0
;   <atomic_asm_insn>\t%2,%1
;   stcond\t%0, %2
;   brne\t0b
;  "
;  [(set_attr "length" "14")
;   (set_attr "cc" "clobber")]
;  )
;
;(define_insn "sync_new_<atomic_insn>si"
;  [(set (match_operand:SI 1 "memory_operand" "+RKs16")
;	(unspec_volatile:SI
;         [(atomic_op:SI (match_dup 1)
;                        (match_operand:SI 2 "register_operand" "r"))]
;         VUNSPEC_SYNC_CMPXCHG))
;   (set (match_operand:SI 0 "register_operand" "=&r")
;	(atomic_op:SI (match_dup 1)
;                      (match_dup 2)))]
;  ""
;  "0:
;   ssrf\t5
;   ld.w\t%0,%1
;   <atomic_asm_insn>\t%0,%2
;   stcond\t%1, %0
;   brne\t0b
;  "
;  [(set_attr "length" "14")
;   (set_attr "cc" "clobber")]
;  )

(define_insn "sync_lock_test_and_setsi"
  [ (set (match_operand:SI 0 "register_operand" "=&r")
         (match_operand:SI 1 "memory_operand" "+RKu00"))
    (set (match_dup 1)
         (match_operand:SI 2 "register_operand" "r")) ]
  ""
  "xchg\t%0, %p1, %2"
  [(set_attr "length" "4")]
  )
