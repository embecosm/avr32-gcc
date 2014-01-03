/* Macro for moving immediate value to register. */	
.macro mov_imm	reg, imm
.if	(((\imm & 0xfffff) == \imm) || ((\imm | 0xfff00000) == \imm))
	mov	\reg, \imm
#if __AVR32_UC__ >= 2
.elseif	((\imm & 0xffff) == 0)
	movh	\reg, hi(\imm)

#endif
.else
	mov	\reg, lo(\imm)
	orh	\reg, hi(\imm)
.endif
.endm
	
	
	 
/* Adjust the unpacked double number if it is a subnormal number.
   The exponent and mantissa pair are stored
   in [mant_hi,mant_lo] and [exp]. A register with the correct sign bit in
   the MSB is passed in [sign]. Needs two scratch
   registers [scratch1] and [scratch2]. An adjusted and packed double float
   is present in [mant_hi,mant_lo] after macro has executed */
.macro  adjust_subnormal_df     exp, mant_lo, mant_hi, sign, scratch1, scratch2 
        /* We have an exponent which is <=0 indicating a subnormal number
           As it should be stored as if the exponent was 1 (although the
           exponent field is all zeros to indicate a subnormal number)
           we have to shift down the mantissa to its correct position. */
        neg     \exp
        sub     \exp,-1                   /* amount to shift down */
        cp.w    \exp,54
        brlo    50f                     /* if more than 53 shift steps, the
                                           entire mantissa will disappear
                                           without any rounding to occur */
        mov     \mant_hi, 0
        mov     \mant_lo, 0
        rjmp    52f
50:     
        sub     \exp,-10                /* do the shift to position the
                                           mantissa at the same time
                                           note! this does not include the
                                           final 1 step shift to add the sign */
 
        /* when shifting, save all shifted out bits in [scratch2]. we may need to
           look at them to make correct rounding. */
 
        rsub    \scratch1,\exp,32       /* get inverted shift count */
        cp.w    \exp,32                 /* handle shifts >= 32 separately */
        brhs    51f
 
        /* small (<32) shift amount, both words are part of the shift */
        lsl     \scratch2,\mant_lo,\scratch1               /* save bits to shift out from lsw*/
        lsl     \scratch1,\mant_hi,\scratch1               /* get bits from msw destined for lsw*/
        lsr     \mant_lo,\mant_lo,\exp                     /* shift down lsw */
        lsr     \mant_hi,\mant_hi,\exp                     /* shift down msw */
        or      \mant_hi,\scratch1                         /* add bits from msw with prepared lsw */
        rjmp    50f
 
        /* large (>=32) shift amount, only lsw will have bits left after shift.
           note that shift operations will use ((shift count) mod 32) so
           we do not need to subtract 32 from shift count. */
51:
        lsl     \scratch2,\mant_hi,\scratch1               /* save bits to shift out from msw */
        or      \scratch2,\mant_lo                         /* also save all bits from lsw */
        mov     \mant_lo,\mant_hi                          /* msw -> lsw (i.e. "shift 32 first") */
        mov     \mant_hi,0                                 /* clear msw */
        lsr     \mant_lo,\mant_lo,\exp                     /* make rest of shift inside lsw */
 
50:
        /* result is almost ready to return, except that least significant bit
           and the part we already shifted out may cause the result to be
           rounded */
        bld     \mant_lo,0                   /* get bit to be shifted out */
        brcc    51f                          /* if bit was 0, no rounding */
 
        /* msb of part to remove is 1, so rounding depends on rest of bits */
        tst     \scratch2,\scratch2                   /* get shifted out tail */
        brne    50f     /* if rest > 0, do round */
        bld     \mant_lo,1                   /* we have to look at lsb in result */
        brcc    51f   /* if lsb is 0, don't round */
 
50:
        /* subnormal result requires rounding
           rounding may cause subnormal to become smallest normal number
           luckily, smallest normal number has exactly the representation
           we got by rippling a one bit up from mantissa into exponent field. */
        sub     \mant_lo,-1
        subcc   \mant_hi,-1
 
51:
        /* shift and return packed double with correct sign */
        rol     \sign
        ror     \mant_hi
        ror     \mant_lo        
52:     
.endm
 
 
/* Adjust subnormal single float number with exponent [exp]
   and mantissa [mant] and round.    */
.macro  adjust_subnormal_sf     sf, exp, mant, sign, scratch
        /* subnormal number */
        rsub    \exp,\exp, 1            /* shift amount */
        cp.w    \exp, 25
        movhs   \mant, 0                
        brhs    90f                     /* Return zero */
        rsub    \scratch, \exp, 32
        lsl     \scratch, \mant,\scratch/* Check if there are any bits set
                                           in the bits discarded in the mantissa */
        srne    \scratch                /* If so set the lsb of the shifted mantissa */ 
        lsr     \mant,\mant,\exp        /* Shift the mantissa */
        or      \mant, \scratch         /* Round lsb if any bits were shifted out  */
        /* Rounding :   For explaination, see round_sf. */
        mov     \scratch, 0x7f          /* Set rounding constant */
        bld     \mant, 8                
        subeq   \scratch, -1            /* For odd numbers use rounding constant 0x80 */
        add     \mant, \scratch         /* Add rounding constant to mantissa */
        /* We can't overflow because mantissa is at least shifted one position
           to the right so the implicit bit is zero. We can however get the implicit
           bit set after rounding which means that we have the lowest normal number
           but this is ok since this bit has the same position as the LSB of the
           exponent */
        lsr     \sf, \mant, 7
        /* Rotate in sign */
        lsl     \sign, 1
        ror     \sf
90:     
.endm
 
 
/* Round the unpacked df number with exponent [exp] and
   mantissa [mant_hi, mant_lo]. Uses scratch register
   [scratch] */
.macro  round_df        exp, mant_lo, mant_hi, scratch
        mov     \scratch, 0x3ff         /* Rounding constant */
        bld     \mant_lo,11             /* Check if lsb in the final result is  
                                           set */
        subeq   \scratch, -1            /* Adjust rounding constant to 0x400
                                           if rounding 0.5 upwards */   
        add     \mant_lo, \scratch      /* Round */
        acr     \mant_hi                /* If overflowing we know that
                                           we have all zeros in the bits not
                                           scaled out so we can leave them
                                           but we must increase the exponent with
                                           two since we had an implicit bit
                                           which is lost + the extra overflow bit */
        subcs   \exp, -2                /* Update exponent */
.endm           
 
/* Round single float number stored in [mant] and [exp] */
.macro  round_sf        exp, mant, scratch
        /* Round:       
                For 0.5 we round to nearest even integer
                for all other cases we round to nearest integer.
                This means that if the digit left of the "point" (.)
                is 1 we can add 0x80 to the mantissa since the
                corner case 0x180 will round up to 0x200. If the
                digit left of the "point" is 0 we will have to
                add 0x7f since this will give 0xff and hence a
                truncation/rounding downwards for the corner
                case when the 9 lowest bits are 0x080 */
        mov     \scratch, 0x7f  /* Set rounding constant */
        /* Check if the mantissa is even or odd */
        bld     \mant, 8
        subeq   \scratch, -1    /* Rounding constant should be 0x80 */
        add     \mant, \scratch
        subcs   \exp, -2        /* Adjust exponent if we overflowed */          
.endm
 
                 
 
/* Pack a single float number stored in [mant] and [exp]
   into a single float number in [sf]  */
.macro  pack_sf sf, exp, mant
        bld     \mant,31                  /* implicit bit to z */
        subne   \exp,1                   /* if subnormal (implicit bit 0)
                                          adjust exponent to storage format */
        
        lsr     \sf, \mant, 7
        bfins   \sf, \exp, 24, 8
.endm   
 
/* Pack exponent [exp] and mantissa [mant_hi, mant_lo]
   into [df_hi, df_lo].  [df_hi] is shifted
   one bit up so the sign bit can be shifted into it */
        
.macro  pack_df         exp, mant_lo, mant_hi, df_lo, df_hi
        bld     \mant_hi,31                  /* implicit bit to z */
        subne   \exp,1                   /* if subnormal (implicit bit 0)
                                          adjust exponent to storage format */
 
        lsr     \mant_lo,11                  /* shift back lsw */
        or      \df_lo,\mant_lo,\mant_hi<<21          /* combine with low bits from msw */
        lsl     \mant_hi,1                   /* get rid of implicit bit */
        lsr     \mant_hi,11                  /* shift back msw except for one step*/
        or      \df_hi,\mant_hi,\exp<<21          /* combine msw with exponent */
.endm
 
/* Normalize single float number stored in [mant] and [exp]
   using scratch register [scratch] */
.macro  normalize_sf    exp, mant, scratch
        /* Adjust exponent and mantissa */
        clz     \scratch, \mant
        sub     \exp, \scratch
        lsl     \mant, \mant, \scratch
.endm
 
/* Normalize the exponent and mantissa pair stored
   in [mant_hi,mant_lo] and [exp]. Needs two scratch
   registers [scratch1] and [scratch2]. */
.macro  normalize_df            exp, mant_lo, mant_hi, scratch1, scratch2
        clz     \scratch1,\mant_hi     /* Check if we have zeros in high bits */
        breq    80f                     /* No need for scaling if no zeros in high bits */
        brcs    81f                     /* Check for all zeros */           
 
        /* shift amount is smaller than 32, and involves both msw and lsw*/
        rsub    \scratch2,\scratch1,32  /* shift mantissa */
        lsl     \mant_hi,\mant_hi,\scratch1
        lsr     \scratch2,\mant_lo,\scratch2
        or      \mant_hi,\scratch2
        lsl     \mant_lo,\mant_lo,\scratch1
        sub     \exp,\scratch1          /* adjust exponent */
        rjmp    80f                     /* Finished */  
81:
        /* shift amount is greater than 32 */
        clz     \scratch1,\mant_lo      /* shift mantissa */
        movcs   \scratch1, 0
        subcc   \scratch1,-32
        lsl     \mant_hi,\mant_lo,\scratch1
        mov     \mant_lo,0
        sub     \exp,\scratch1          /* adjust exponent */
80:     
.endm
        
 
/* Fast but approximate multiply of two 64-bit numbers to give a 64 bit result.
   The multiplication of [al]x[bl] is discarded.
   Operands in [ah], [al], [bh], [bl].
   Scratch registers in [sh], [sl].
   Returns results in registers [rh], [rl].*/
.macro  mul_approx_df   ah, al, bh, bl, rh, rl, sh, sl
        mulu.d  \sl, \ah, \bl
        macu.d  \sl, \al, \bh
        mulu.d  \rl, \ah, \bh
        add     \rl, \sh
        acr     \rh
.endm           
 
 
        
#if defined(L_avr32_f64_mul) || defined(L_avr32_f64_mul_fast)
        .align  2
#if defined(L_avr32_f64_mul)
        .global __avr32_f64_mul
        .type  __avr32_f64_mul,@function
__avr32_f64_mul:
#else 
        .global __avr32_f64_mul_fast
        .type  __avr32_f64_mul_fast,@function
__avr32_f64_mul_fast:
#endif                
        or      r12, r10, r11 << 1 
        breq   __avr32_f64_mul_op1_zero        

#if defined(L_avr32_f64_mul)
	pushm	r4-r7, lr
#else
        stm     --sp, r5,r6,r7,lr       
#endif

#define AVR32_F64_MUL_OP1_INT_BITS 1
#define AVR32_F64_MUL_OP2_INT_BITS 10
#define AVR32_F64_MUL_RES_INT_BITS 11
	
        /* op1 in {r11,r10}*/
        /* op2 in {r9,r8}*/
        eor     lr, r11, r9             /* MSB(lr) = Sign(op1) ^ Sign(op2) */
 
        /* Unpack op1 to 1.63 format*/        
        /* exp: r7 */
        /* sf:  r11, r10 */
	bfextu	r7, r11, 20, 11 /* Extract exponent */

	mov	r5, 1

        /* Check if normalization is needed */
        breq    __avr32_f64_mul_op1_subnormal /*If number is subnormal, normalize it */ 

        lsl     r11, (12-AVR32_F64_MUL_OP1_INT_BITS-1) /* Extract mantissa, leave room for implicit bit */ 
        or      r11, r11, r10>>(32-(12-AVR32_F64_MUL_OP1_INT_BITS-1))
        lsl     r10, (12-AVR32_F64_MUL_OP1_INT_BITS-1)
	bfins	r11, r5, 32 - (1 + AVR32_F64_MUL_OP1_INT_BITS), 1 + AVR32_F64_MUL_OP1_INT_BITS /* Insert implicit bit */


22:     
        /* Unpack op2 to 10.54 format */
        /* exp: r6 */
        /* sf:  r9, r8 */
	bfextu	r6, r9, 20, 11 /* Extract exponent */

        /* Check if normalization is needed */
        breq    __avr32_f64_mul_op2_subnormal /*If number is subnormal, normalize it */ 

	lsl	r8, 1 /* Extract mantissa, leave room for implicit bit */
	rol	r9	
	bfins	r9, r5, 32 - (1 + AVR32_F64_MUL_OP2_INT_BITS), 1 + AVR32_F64_MUL_OP2_INT_BITS /* Insert implicit bit */

23:     
 
        /* Check if any operands are NaN or INF */
        cp      r7, 0x7ff
        breq    __avr32_f64_mul_op_nan_or_inf /* Check op1 for NaN or Inf */
        cp      r6, 0x7ff
        breq    __avr32_f64_mul_op_nan_or_inf  /* Check op2 for NaN or Inf */
 
 
        /* Calculate new exponent in r12*/
        add     r12, r7, r6
        sub     r12, (1023-1)
 
#if defined(L_avr32_f64_mul)
	/* Do the multiplication.
           Place result in [r11, r10, r7, r6]. The result is in 11.117 format.  */
        mulu.d  r4, r11, r8
        macu.d  r4, r10, r9
        mulu.d  r6, r10, r8
        mulu.d  r10, r11, r9
	add	r7, r4
	adc	r10, r10, r5	
	acr	r11
#else
	/* Do the multiplication using approximate calculation. discard the al x bl
	   calculation.
           Place result in [r11, r10, r7]. The result is in 11.85 format.  */

        /* Do the multiplication using approximate calculation.
         Place result in r11, r10. Use r7, r6 as scratch registers */
        mulu.d  r6, r11, r8
        macu.d  r6, r10, r9
        mulu.d  r10, r11, r9
        add     r10, r7
        acr     r11
#endif 
        /* Adjust exponent and mantissa */
        /* [r12]:exp, [r11, r10]:mant [r7, r6]:sticky bits */
        /* Mantissa may be of the format 00000000000.0xxx or 00000000000.1xxx. */
        /* In the first case, shift one pos to left.*/
        bld     r11, 32-AVR32_F64_MUL_RES_INT_BITS-1
	breq	0f	
	lsl	r7, 1
	rol	r10
	rol	r11
	sub	r12, 1
0:	
        cp      r12, 0  
        brle    __avr32_f64_mul_res_subnormal /*Result was subnormal.*/
 
        /* Check for Inf. */
        cp.w    r12, 0x7ff
        brge    __avr32_f64_mul_res_inf

	/* Insert exponent. */
	bfins	r11, r12, 20, 11  

        /* Result was not subnormal. Perform rounding. */
        /* For the fast version we discard the sticky bits and always round
	   the halfwaycase up. */
24:	
#if defined(L_avr32_f64_mul)
	or	r6, r6, r10 << 31 /* Or in parity bit into stickybits */
	or	r7, r7, r6 >> 1   /* Or together sticky and still make the msb 
				     of r7 represent the halfway bit. */
	eorh	r7, 0x8000	  /* Toggle halfway bit. */
	/* We should now round up by adding one for the following cases:

		halfway   sticky|parity  round-up
		   0            x           no
		   1            0           no
	           1            1           yes

	   Since we have inverted the halfway bit we can use the satu instruction
           by saturating to 1 bit to implement this. 
	*/ 
	satu	r7 >> 0, 1
#else
	lsr	r7, 31
#endif	
	add	r10, r7
	acr	r11	
        
        /* Insert sign bit*/
        bld     lr, 31
        bst     r11, 31
        
        /* Return result in [r11,r10] */
#if defined(L_avr32_f64_mul)
	popm	r4-r7, pc
#else
        ldm     sp++, r5, r6, r7,pc
#endif
 
 
__avr32_f64_mul_op1_subnormal:
	andh	r11, 0x000f /* Remove sign bit and exponent */
        clz     r12, r10    /* Count leading zeros in lsw */
        clz     r6, r11     /* Count leading zeros in msw */
        subcs	r12, -32 + AVR32_F64_MUL_OP1_INT_BITS 
	movcs	r6, r12
	subcc	r6, AVR32_F64_MUL_OP1_INT_BITS
	cp.w	r6, 32
	brge	0f
		
        /* shifting involves both msw and lsw*/
        rsub    r12, r6, 32  /* shift mantissa */
        lsl     r11, r11, r6
        lsr     r12, r10, r12
        or      r11, r12
        lsl     r10, r10, r6
	sub	r6, 12-AVR32_F64_MUL_OP1_INT_BITS
        sub     r7, r6          /* adjust exponent */
        rjmp    22b             /* Finished */  
0:
        /* msw is zero so only need to consider lsw */
        lsl     r11, r10, r6
	breq	__avr32_f64_mul_res_zero
        mov     r10, 0
	sub	r6, 12-AVR32_F64_MUL_OP1_INT_BITS
        sub     r7, r6            /* adjust exponent */
        rjmp    22b

 
__avr32_f64_mul_op2_subnormal:
	andh	r9, 0x000f  /* Remove sign bit and exponent */
        clz     r12, r8    /* Count leading zeros in lsw */
        clz     r5, r9     /* Count leading zeros in msw */
        subcs	r12, -32 + AVR32_F64_MUL_OP2_INT_BITS 
	movcs	r5, r12
	subcc	r5, AVR32_F64_MUL_OP2_INT_BITS
	cp.w	r5, 32
	brge	0f
		
        /* shifting involves both msw and lsw*/
        rsub    r12, r5, 32  /* shift mantissa */
        lsl     r9, r9, r5
        lsr     r12, r8, r12
        or      r9, r12
        lsl     r8, r8, r5
	sub	r5, 12 - AVR32_F64_MUL_OP2_INT_BITS
        sub     r6, r5          /* adjust exponent */
        rjmp    23b             /* Finished */  
0:
        /* msw is zero so only need to consider lsw */
        lsl     r9, r8, r5
	breq	__avr32_f64_mul_res_zero
        mov     r8, 0
	sub	r5, 12 - AVR32_F64_MUL_OP2_INT_BITS
        sub     r6, r5            /* adjust exponent */
        rjmp    23b
                
 
__avr32_f64_mul_op_nan_or_inf:
        /* Same code for OP1 and OP2*/
        /* Since we are here, at least one of the OPs were NaN or INF*/
	andh	r9, 0x000f  /* Remove sign bit and exponent */
	andh	r11, 0x000f  /* Remove sign bit and exponent */
        /* Merge the regs in each operand to check for zero*/
        or      r11, r10 /* op1 */
        or      r9, r8 /* op2 */
        /* Check if op1 is NaN or INF */
        cp      r7, 0x7ff
        brne    __avr32_f64_mul_op1_not_naninf
        /* op1 was NaN or INF.*/
        cp      r11, 0
        brne    __avr32_f64_mul_res_nan /* op1 was NaN. Result will be NaN*/
        /*op1 was INF. check if op2 is NaN or INF*/
        cp      r6, 0x7ff
        brne    __avr32_f64_mul_res_inf /*op1 was INF, op2 was neither NaN nor INF*/
        /* op1 is INF, op2 is either NaN or INF*/
        cp      r9, 0
        breq    __avr32_f64_mul_res_inf /*op2 was also INF*/
        rjmp    __avr32_f64_mul_res_nan /*op2 was NaN*/
 
__avr32_f64_mul_op1_not_naninf:
        /* op1 was not NaN nor INF. Then op2 must be NaN or INF*/
        cp      r9, 0
        breq    __avr32_f64_mul_res_inf /*op2 was INF, return INF*/
        rjmp   __avr32_f64_mul_res_nan /*else return NaN*/
        
__avr32_f64_mul_res_subnormal:/* Multiply result was subnormal. */
#if defined(L_avr32_f64_mul)
	/* Check how much we must scale down the mantissa. */
	neg	r12
	sub	r12, -1     /* We do no longer have an implicit bit. */
	satu	r12 >> 0, 6 /* Saturate shift amount to max 63. */
	cp.w	r12, 32
	brge	0f
	/* Shift amount <32 */
	rsub	r8, r12, 32
	or	r6, r7 
	lsr	r7, r7, r12
	lsl	r9, r10, r8
	or	r7, r9
	lsr	r10, r10, r12
	lsl	r9, r11, r8
	or	r10, r9
	lsr	r11, r11, r12
	rjmp	24b
0:
	/* Shift amount >=32 */
	rsub	r8, r12, 32
	moveq	r9, 0
	breq	0f
	lsl	r9, r11, r8
0:	
	or	r6, r7
	or	r6, r6, r10 << 1 
	lsr	r10, r10, r12
	or	r7, r9, r10
	lsr	r10, r11, r12
	mov	r11, 0	
	rjmp	24b				
#else
	/* Flush to zero for the fast version. */
        mov     r11, lr /*Get correct sign*/
        andh    r11, 0x8000, COH
        mov     r10, 0
        ldm     sp++, r5, r6, r7,pc
#endif

__avr32_f64_mul_res_zero:/* Multiply result is zero. */
        mov     r11, lr /*Get correct sign*/
        andh    r11, 0x8000, COH
        mov     r10, 0
#if defined(L_avr32_f64_mul)
	popm	r4-r7, pc
#else
        ldm     sp++, r5, r6, r7,pc
#endif
 
__avr32_f64_mul_res_nan:        /* Return NaN. */
        mov     r11, -1
        mov     r10, -1
#if defined(L_avr32_f64_mul)
	popm	r4-r7, pc
#else
        ldm     sp++, r5, r6, r7,pc
#endif
        
__avr32_f64_mul_res_inf:        /* Return INF. */
	mov	r11, 0xfff00000
        bld     lr, 31
        bst     r11, 31
        mov     r10, 0
#if defined(L_avr32_f64_mul)
	popm	r4-r7, pc
#else
        ldm     sp++, r5, r6, r7,pc
#endif

__avr32_f64_mul_op1_zero:
        /* Get sign */
        eor     r11, r11, r9
        andh    r11, 0x8000, COH  
        /* Check if op2 is Inf or NaN. */
        bfextu  r12, r9, 20, 11
        cp.w    r12, 0x7ff
        retne   r12     /* Return 0.0 */
        /* Return NaN */
        mov     r10, -1
        mov     r11, -1
        ret     r12
         

 
#endif
                
 
#if  defined(L_avr32_f64_addsub) || defined(L_avr32_f64_addsub_fast)
        .align  2

__avr32_f64_sub_from_add:
        /* Switch sign on op2 */
        eorh    r9, 0x8000

#if  defined(L_avr32_f64_addsub_fast)
        .global __avr32_f64_sub_fast
        .type  __avr32_f64_sub_fast,@function
__avr32_f64_sub_fast:
#else	
        .global __avr32_f64_sub
        .type  __avr32_f64_sub,@function
__avr32_f64_sub:
#endif
        
        /* op1 in {r11,r10}*/
        /* op2 in {r9,r8}*/

#if  defined(L_avr32_f64_addsub_fast)
        /* If op2 is zero just return op1 */
        or      r12, r8, r9 << 1
        reteq   r12 
#endif
 
        /* Check signs */
        eor     r12, r11, r9
        /* Different signs, use addition. */
        brmi    __avr32_f64_add_from_sub
 
        stm     --sp, r5, r6, r7, lr
 
        /* Get sign of op1 into r12 */
        mov     r12, r11
        andh    r12, 0x8000, COH                
 
        /* Remove sign from operands */
        cbr     r11, 31
        cbr     r9, 31
 
        /* Put the largest number in [r11, r10]
           and the smallest number in [r9, r8] */
        cp      r10, r8
        cpc     r11, r9
        brhs    1f /* Skip swap if operands already correctly ordered*/
        /* Operands were not correctly ordered, swap them*/
        mov     r7, r11
        mov     r11, r9
        mov     r9, r7
        mov     r7, r10
        mov     r10, r8
        mov     r8, r7
        eorh    r12, 0x8000 /* Invert sign in r12*/
1:      
        /* Unpack largest operand - opH */      
        /* exp: r7 */
        /* sf:  r11, r10 */
        lsr     r7, r11, 20 /* Extract exponent */
        lsl     r11, 11 /* Extract mantissa, leave room for implicit bit */ 
        or      r11, r11, r10>>21
        lsl     r10, 11
        sbr     r11, 31 /* Insert implicit bit */
 
        
        /* Unpack smallest operand - opL */
        /* exp: r6 */
        /* sf:  r9, r8 */
        lsr     r6, r9, 20 /* Extract exponent */
        breq    __avr32_f64_sub_opL_subnormal /* If either zero or subnormal */
        lsl     r9, 11 /* Extract mantissa, leave room for implicit bit */ 
        or      r9, r9, r8>>21
        lsl     r8, 11
        sbr     r9, 31 /* Insert implicit bit */
 

__avr32_f64_sub_opL_subnormal_done:     
        /* opH is NaN or Inf. */
        cp.w    r7, 0x7ff
        breq    __avr32_f64_sub_opH_nan_or_inf

        /* Get shift amount to scale mantissa of op2. */
        rsub    r6, r7
        breq    __avr32_f64_sub_shift_done /* No need to shift, exponents are equal*/
 
        /* Scale mantissa [r9, r8] with amount [r6].
        Uses scratch registers [r5] and [lr].
        In IEEE mode:Must not forget the sticky bits we intend to shift out. */
 
        rsub    r5,r6,32 /* get (32 - shift count)
                            (if shift count > 32 we get a
                            negative value, but that will
                            work as well in the code below.) */
 
        cp.w    r6,32       /* handle shifts >= 32 separately */
        brhs    __avr32_f64_sub_longshift
 
        /* small (<32) shift amount, both words are part of the shift
           first remember whether part that is lost contains any 1 bits ... */
        lsl     lr,r8,r5  /* shift away bits that are part of
                             final mantissa. only part that goes
                             to lr are bits that will be lost */
 
        /* ... and now to the actual shift */
        lsl     r5,r9,r5  /* get bits from msw destined for lsw*/
        lsr     r8,r8,r6  /* shift down lsw of mantissa */
        lsr     r9,r9,r6  /* shift down msw of mantissa */
        or      r8,r5     /* combine these bits with prepared lsw*/
#if  defined(L_avr32_f64_addsub)
        cp.w    lr,0      /* if any '1' bit in part we lost ...*/
        srne    lr
        or      r8, lr     /* ... we need to set sticky bit*/
#endif
        
__avr32_f64_sub_shift_done:     
        /* Now subtract the mantissas. */
        sub     r10, r8
        sbc     r11, r11, r9
 
        /* Normalize the exponent and mantissa pair stored in
        [r11,r10] and exponent in [r7]. Needs two scratch registers [r6] and [lr]. */
        clz     r6,r11     /* Check if we have zeros in high bits */
        breq    __avr32_f64_sub_longnormalize_done  /* No need for scaling if no zeros in high bits */
        brcs    __avr32_f64_sub_longnormalize
 
	
        /* shift amount is smaller than 32, and involves both msw and lsw*/
        rsub    lr,r6,32  /* shift mantissa */
        lsl     r11,r11,r6
        lsr     lr,r10,lr
        or      r11,lr
        lsl     r10,r10,r6
 
        sub     r7,r6    /* adjust exponent */
        brle    __avr32_f64_sub_subnormal_result
__avr32_f64_sub_longnormalize_done:     
        
#if defined(L_avr32_f64_addsub)
        /* Insert the bits we will remove from the mantissa r9[31:21] */
        lsl     r9, r10, (32 - 11)
#else
        /* Keep the last bit shifted out. */
        bfextu  r9, r10, 10, 1
#endif
 
        /* Pack final result*/
        /* Input: [r7]:exp, [r11, r10]:mant, [r12]:sign in MSB */
        /* Result in [r11,r10] */
        /* Insert mantissa */
        lsr     r10, 11
        or      r10, r10, r11<<21
        lsr     r11, 11
        /* Insert exponent and sign bit*/
	bfins	r11, r7, 20, 11
        or      r11, r12
        
        /* Round */     
__avr32_f64_sub_round:
#if defined(L_avr32_f64_addsub)
	mov_imm	r7, 0x80000000
        bld     r10, 0
        subne   r7, -1  
 
        cp.w    r9, r7
        srhs    r9
#endif
        add     r10, r9
        acr     r11
        
        /* Return result in [r11,r10] */
        ldm     sp++, r5, r6, r7,pc
 
 
 
__avr32_f64_sub_opL_subnormal:
        /* Extract the of mantissa */
        lsl     r9, 11 /* Extract mantissa, leave room for implicit bit */ 
        or      r9, r9, r8>>21
        lsl     r8, 11
 
        /* Set exponent to 1 if we do not have a zero. */
        or      lr, r9, r8
        movne   r6,1
	
        /* Check if opH is also subnormal. If so, clear implicit bit in r11*/
        rsub    lr, r7, 0
        moveq   r7,1
        bst     r11, 31
	
        /* Check if op1 is zero, if so set exponent to 0. */
        or      lr, r11, r10
        moveq   r7,0
	                 
        rjmp    __avr32_f64_sub_opL_subnormal_done
 
__avr32_f64_sub_opH_nan_or_inf: 
        /* Check if opH is NaN, if so return NaN */
        cbr     r11, 31
        or      lr, r11, r10
        brne    __avr32_f64_sub_return_nan
 
        /* opH is Inf. */
        /* Check if opL is Inf. or NaN */
        cp.w    r6, 0x7ff
        breq    __avr32_f64_sub_return_nan
	/* Return infinity with correct sign. */	
	or      r11, r12, r7 << 20
        ldm     sp++, r5, r6, r7, pc/* opL not Inf or NaN, return opH */
__avr32_f64_sub_return_nan:     
        mov     r10, -1 /* Generate NaN in r11, r10 */
        mov     r11, -1
        ldm     sp++, r5, r6, r7, pc/* opL Inf or NaN, return NaN */
 
 
__avr32_f64_sub_subnormal_result:
#if defined(L_avr32_f64_addsub)
	/* Check how much we must scale down the mantissa. */
	neg	r7
	sub	r7, -1     /* We do no longer have an implicit bit. */
	satu	r7 >> 0, 6 /* Saturate shift amount to max 63. */
	cp.w	r7, 32
	brge	0f
	/* Shift amount <32 */
	rsub	r8, r7, 32
	lsl	r9, r10, r8
	srne	r6
	lsr	r10, r10, r7
	or	r10, r6		/* Sticky bit from the
				   part that was shifted out. */
	lsl	r9, r11, r8
	or	r10, r10, r9
	lsr	r11, r10, r7
	/* Set exponent */
	mov	r7, 0
	rjmp	__avr32_f64_sub_longnormalize_done
0:
	/* Shift amount >=32 */
	rsub	r8, r7, 64
	lsl	r9, r11, r8
	or	r9, r10
	srne	r6
	lsr	r10, r11, r7
	or	r10, r6		/* Sticky bit from the
				   part that was shifted out. */
	mov	r11, 0
	/* Set exponent */
	mov	r7, 0
	rjmp	__avr32_f64_sub_longnormalize_done
#else
        /* Just flush subnormals to zero. */
        mov     r10, 0
        mov     r11, 0
#endif
        ldm     sp++, r5, r6, r7, pc
 
__avr32_f64_sub_longshift:
        /* large (>=32) shift amount, only lsw will have bits left after shift.
           note that shift operations will use ((shift count=r6) mod 32) so
           we do not need to subtract 32 from shift count. */
        /* Saturate the shift amount to 63. If the amount
           is any larger op2 is insignificant. */
        satu    r6 >> 0, 6
	
#if defined(L_avr32_f64_addsub)
        /* first remember whether part that is lost contains any 1 bits ... */
	moveq	lr, r8	   /* If shift amount is 32, no bits from msw are lost. */
	breq	0f
        lsl     lr,r9,r5   /* save all lost bits from msw */
        or      lr,r8      /* also save lost bits (all) from lsw
                              now lr != 0 if we lose any bits */
#endif  
0:	
        /* ... and now to the actual shift */
        lsr     r8,r9,r6   /* Move msw to lsw and shift. */
        mov     r9,0       /* clear msw */
#if defined(L_avr32_f64_addsub)
        cp.w    lr,0       /* if any '1' bit in part we lost ...*/
        srne    lr
        or      r8, lr      /* ... we need to set sticky bit*/
#endif
        rjmp    __avr32_f64_sub_shift_done
 
__avr32_f64_sub_longnormalize:
        /* shift amount is greater than 32 */
        clz     r6,r10      /* shift mantissa */
        /* If the resulting mantissa is zero the result is 
           zero so force exponent to zero. */
        movcs   r7, 0
        movcs   r6, 0
        movcs   r12, 0  /* Also clear sign bit. A zero result from subtraction
			   always is +0.0 */
        subcc   r6,-32
        lsl     r11,r10,r6
        mov     r10,0
        sub     r7,r6          /* adjust exponent */
        brle    __avr32_f64_sub_subnormal_result
        rjmp    __avr32_f64_sub_longnormalize_done
        
 
        
	 .align  2
__avr32_f64_add_from_sub:
        /* Switch sign on op2 */
        eorh    r9, 0x8000

#if defined(L_avr32_f64_addsub_fast)
        .global __avr32_f64_add_fast
        .type  __avr32_f64_add_fast,@function
__avr32_f64_add_fast:
#else	
        .global __avr32_f64_add
        .type  __avr32_f64_add,@function
__avr32_f64_add:
#endif
        
        /* op1 in {r11,r10}*/
        /* op2 in {r9,r8}*/
 
#if defined(L_avr32_f64_addsub_fast)
        /* If op2 is zero just return op1 */
        or      r12, r8, r9 << 1
        reteq   r12 
#endif

        /* Check signs */
        eor     r12, r11, r9
        /* Different signs, use subtraction. */
        brmi    __avr32_f64_sub_from_add
 
        stm     --sp, r5, r6, r7, lr
 
        /* Get sign of op1 into r12 */
        mov     r12, r11
        andh    r12, 0x8000, COH                
 
        /* Remove sign from operands */
        cbr     r11, 31
        cbr     r9, 31
 
        /* Put the number with the largest exponent in [r11, r10]
           and the number with the smallest exponent in [r9, r8] */
        cp      r11, r9
        brhs    1f /* Skip swap if operands already correctly ordered */
        /* Operands were not correctly ordered, swap them */
        mov     r7, r11
        mov     r11, r9
        mov     r9, r7
        mov     r7, r10
        mov     r10, r8
        mov     r8, r7
1:      
	mov	lr, 0 /* Set sticky bits to zero */
        /* Unpack largest operand - opH */      
        /* exp: r7 */
        /* sf:  r11, r10 */
	bfextu	R7, R11, 20, 11 /* Extract exponent */
	bfextu	r11, r11, 0, 20 /* Extract mantissa */
        sbr     r11, 20 /* Insert implicit bit */
 
        /* Unpack smallest operand - opL */
        /* exp: r6 */
        /* sf:  r9, r8 */
	bfextu	R6, R9, 20, 11	/* Extract exponent */
	breq	__avr32_f64_add_op2_subnormal
	bfextu	r9, r9, 0, 20   /* Extract mantissa */
        sbr     r9, 20		/* Insert implicit bit */

2:		 
        /* opH is NaN or Inf. */
        cp.w    r7, 0x7ff
        breq    __avr32_f64_add_opH_nan_or_inf

        /* Get shift amount to scale mantissa of op2. */
        rsub    r6, r7
        breq    __avr32_f64_add_shift_done /* No need to shift, exponents are equal*/
 
        /* Scale mantissa [r9, r8] with amount [r6].
        Uses scratch registers [r5] and [lr].
        In IEEE mode:Must not forget the sticky bits we intend to shift out. */
        rsub    r5,r6,32 /* get (32 - shift count)
                            (if shift count > 32 we get a
                            negative value, but that will
                            work as well in the code below.) */
 
        cp.w    r6,32       /* handle shifts >= 32 separately */
        brhs    __avr32_f64_add_longshift
 
        /* small (<32) shift amount, both words are part of the shift
           first remember whether part that is lost contains any 1 bits ... */
        lsl     lr,r8,r5  /* shift away bits that are part of
                             final mantissa. only part that goes
                             to lr are bits that will be lost */
 
        /* ... and now to the actual shift */
        lsl     r5,r9,r5  /* get bits from msw destined for lsw*/
        lsr     r8,r8,r6  /* shift down lsw of mantissa */
        lsr     r9,r9,r6  /* shift down msw of mantissa */
        or      r8,r5     /* combine these bits with prepared lsw*/
        
__avr32_f64_add_shift_done:     
        /* Now add the mantissas. */
        add     r10, r8
        adc     r11, r11, r9

        /* Check if we overflowed. */
	bld	r11, 21 
        breq	__avr32_f64_add_res_of:

__avr32_f64_add_res_of_done:    
        
        /* Pack final result*/
        /* Input: [r7]:exp, [r11, r10]:mant, [r12]:sign in MSB */
        /* Result in [r11,r10] */
        /* Insert exponent and sign bit*/
	bfins	r11, r7, 20, 11
	or	r11, r12
        
        /* Round */     
__avr32_f64_add_round:
#if defined(L_avr32_f64_addsub)
	bfextu	r12, r10, 0, 1 /* Extract parity bit.*/
	or	lr, r12	       /* or it together with the sticky bits. */	
	eorh	lr, 0x8000     /* Toggle round bit. */	
	/* We should now round up by adding one for the following cases:

		halfway   sticky|parity  round-up
		   0            x           no
		   1            0           no
	           1            1           yes

	   Since we have inverted the halfway bit we can use the satu instruction
           by saturating to 1 bit to implement this. 
	*/ 
	satu	lr >> 0, 1
#else
	lsr	lr, 31
#endif
        add     r10, lr
        acr     r11
        
        /* Return result in [r11,r10] */
        ldm     sp++, r5, r6, r7,pc
 
  
__avr32_f64_add_opH_nan_or_inf: 
        /* Check if opH is NaN, if so return NaN */
        cbr     r11, 20
        or      lr, r11, r10
        brne    __avr32_f64_add_return_nan
 
        /* opH is Inf. */
        /* Check if opL is Inf. or NaN */
        cp.w    r6, 0x7ff
        breq    __avr32_f64_add_opL_nan_or_inf
        ldm     sp++, r5, r6, r7, pc/* opL not Inf or NaN, return opH */
__avr32_f64_add_opL_nan_or_inf:
        cbr     r9, 20
        or      lr, r9, r8
        brne    __avr32_f64_add_return_nan
        mov     r10, 0  /* Generate Inf in r11, r10 */
	mov_imm r11, 0x7ff00000
        ldm     sp++, r5, r6, r7, pc/* opL Inf, return Inf */
__avr32_f64_add_return_nan:     
        mov     r10, -1 /* Generate NaN in r11, r10 */
        mov     r11, -1
        ldm     sp++, r5, r6, r7, pc/* opL Inf or NaN, return NaN */
 
 
__avr32_f64_add_longshift:
        /* large (>=32) shift amount, only lsw will have bits left after shift.
           note that shift operations will use ((shift count=r6) mod 32) so
           we do not need to subtract 32 from shift count. */
        /* Saturate the shift amount to 63. If the amount
           is any larger op2 is insignificant. */
        satu    r6 >> 0, 6
	/* If shift amount is 32 there are no bits from the msw that are lost. */
	moveq	lr, r8
	breq	0f	
        /* first remember whether part that is lost contains any 1 bits ... */
        lsl     lr,r9,r5   /* save all lost bits from msw */
#if defined(L_avr32_f64_addsub)
	cp.w	r8, 0
	srne	r8	
        or      lr,r8      /* also save lost bits (all) from lsw
                              now lr != 0 if we lose any bits */
#endif  
0:	
        /* ... and now to the actual shift */
        lsr     r8,r9,r6   /* msw -> lsw and make rest of shift inside lsw*/
        mov     r9,0       /* clear msw */
        rjmp    __avr32_f64_add_shift_done
 
__avr32_f64_add_res_of:
	/* We overflowed. Scale down mantissa by shifting right one position. */
	or	lr, lr, lr << 1 /* Remember stickybits*/
	lsr	r11, 1
	ror	r10
	ror	lr
	sub	r7, -1	/* Increment exponent */
 
        /* Clear mantissa to set result to Inf if the exponent is 255. */
        cp.w    r7, 0x7ff
        moveq   r10, 0
        moveq   r11, 0
        moveq   lr, 0
        rjmp    __avr32_f64_add_res_of_done
        
__avr32_f64_add_op2_subnormal:	
	/* Set epxponent to 1 */
	mov	r6, 1

	/* Check if op2 is also subnormal. */
	cp.w	r7, 0
	brne	2b

	cbr	r11, 20
	/* Both operands are subnormal. Just addd the mantissas
	   and the exponent will automatically be set to 1 if
	   we overflow into a normal number. */
	add	r10, r8
	adc	r11, r11, r9

	/* Add sign bit */
	or	r11, r12
	
        /* Return result in [r11,r10] */
        ldm     sp++, r5, r6, r7,pc
	
			
	 
#endif
 
#ifdef L_avr32_f64_to_u32
        /* This goes into L_fixdfsi */
#endif
        
 
#ifdef L_avr32_f64_to_s32
        .global __avr32_f64_to_u32
        .type  __avr32_f64_to_u32,@function
__avr32_f64_to_u32:
        cp.w    r11, 0
        retmi   0       /* Negative returns 0 */
 
        /* Fallthrough to df to signed si conversion */ 
        .global __avr32_f64_to_s32
        .type  __avr32_f64_to_s32,@function
__avr32_f64_to_s32:
        lsl     r12,r11,1
        lsr     r12,21                  /* extract exponent*/
        sub     r12,1023                /* convert to unbiased exponent.*/
        retlo   0                       /* too small exponent implies zero. */
 
1:      
        rsub    r12,r12,31              /* shift count = 31 - exponent */
        mov     r9,r11                  /* save sign for later...*/
        lsl     r11,11                  /* remove exponent and sign*/
        sbr     r11,31                  /* add implicit bit*/
        or      r11,r11,r10>>21         /* get rest of bits from lsw of double */
        lsr     r11,r11,r12             /* shift down mantissa to final place */
        lsl     r9,1                    /* sign -> carry */
        retcc   r11                     /* if positive, we are done */
        neg     r11                     /* if negative float, negate result */
        ret     r11
 
#endif  /* L_fixdfsi*/
 
#ifdef L_avr32_f64_to_u64
        /* Actual function is in L_fixdfdi */
#endif
        
#ifdef L_avr32_f64_to_s64
        .global __avr32_f64_to_u64
        .type  __avr32_f64_to_u64,@function
__avr32_f64_to_u64:
        cp.w    r11,0
        /* Negative numbers return zero */
        movmi   r10, 0
        movmi   r11, 0
        retmi   r11
 
        
 
        /* Fallthrough */
        .global __avr32_f64_to_s64
        .type  __avr32_f64_to_s64,@function
__avr32_f64_to_s64:
        lsl     r9,r11,1
        lsr     r9,21                   /* get exponent*/
        sub     r9,1023                 /* convert to correct range*/
        /* Return zero if exponent to small */
        movlo   r10, 0
        movlo   r11, 0
        retlo   r11
 
        mov     r8,r11                  /* save sign for later...*/
1:      
        lsl     r11,11                  /* remove exponent */
        sbr     r11,31                  /* add implicit bit*/
        or      r11,r11,r10>>21         /* get rest of bits from lsw of double*/
        lsl     r10,11                  /* align lsw correctly as well */
        rsub    r9,r9,63                /* shift count = 63 - exponent */
        breq    1f
 
        cp.w    r9,32                   /* is shift count more than one reg? */
        brhs    0f
 
        mov     r12,r11                 /* save msw */
        lsr     r10,r10,r9              /* small shift count, shift down lsw */
        lsr     r11,r11,r9              /* small shift count, shift down msw */
        rsub    r9,r9,32                /* get 32-size of shifted out tail */
        lsl     r12,r12,r9              /* align part to move from msw to lsw */
        or      r10,r12                 /* combine to get new lsw */
        rjmp    1f
 
0:
        lsr     r10,r11,r9              /* large shift count,only lsw get bits
                                           note that shift count is modulo 32*/
        mov     r11,0                   /* msw will be 0 */
 
1:
        lsl     r8,1                    /* sign -> carry */
        retcc   r11                     /* if positive, we are done */
 
        neg     r11                     /* if negative float, negate result */
        neg     r10
        scr     r11
        ret     r11 
 
#endif
 
#ifdef L_avr32_u32_to_f64
        /* Code located in L_floatsidf */
#endif
        
#ifdef L_avr32_s32_to_f64
        .global __avr32_u32_to_f64
        .type  __avr32_u32_to_f64,@function
__avr32_u32_to_f64:
        sub     r11, r12, 0 /* Move to r11 and force Z flag to be updated */
        mov     r12, 0      /* always positive */
        rjmp    0f          /* Jump to common code for floatsidf */
        
        .global __avr32_s32_to_f64
        .type  __avr32_s32_to_f64,@function
__avr32_s32_to_f64:
        mov     r11, r12        /* Keep original value in r12 for sign */
        abs     r11             /* Absolute value if r12 */
0:      
        mov     r10,0           /* let remaining bits be zero */
        reteq   r11             /* zero long will return zero float */
 
        pushm   lr
        mov     r9,31+1023              /* set exponent */
                
        normalize_df    r9 /*exp*/, r10, r11 /* mantissa */, r8, lr /* scratch */
 
        /* Check if a subnormal result was created */
        cp.w    r9, 0
        brgt    0f
        
        adjust_subnormal_df     r9 /* exp */, r10, r11 /* Mantissa */, r12 /*sign*/, r8, lr /* scratch */
        popm    pc
0:
        
        /* Round result */
        round_df        r9 /*exp*/, r10, r11 /* Mantissa */, r8 /*scratch*/
        cp.w    r9,0x7ff
        brlt    0f
        /*Return infinity */
        mov     r10, 0
	mov_imm	r11, 0xffe00000
        rjmp    __floatsidf_return_op1
        
0:
 
        /* Pack */
        pack_df r9 /*exp*/, r10, r11 /* mantissa */, r10, r11 /* Output df number*/
__floatsidf_return_op1: 
        lsl     r12,1                  /* shift in sign bit */
        ror     r11
 
        popm    pc
#endif
 
 
#ifdef L_avr32_f32_cmp_eq
        .global __avr32_f32_cmp_eq
        .type  __avr32_f32_cmp_eq,@function
__avr32_f32_cmp_eq:     
        cp.w    r12, r11
        breq    0f      
        /* If not equal check for +/-0 */
        /* Or together the two values and shift out the sign bit.
           If the result is zero, then the two values are both zero. */
        or      r12, r11
        lsl     r12, 1
        reteq   1
        ret     0
0:                      
        /* Numbers were equal. Check for NaN or Inf */
	mov_imm	r11, 0xff000000
        lsl     r12, 1
        cp.w    r12, r11
        retls   1     /* 0 if NaN, 1 otherwise */
        ret     0     
#endif
        
#if defined(L_avr32_f32_cmp_ge) || defined(L_avr32_f32_cmp_lt)
#ifdef L_avr32_f32_cmp_ge
        .global __avr32_f32_cmp_ge
        .type  __avr32_f32_cmp_ge,@function
__avr32_f32_cmp_ge:
#endif  
#ifdef L_avr32_f32_cmp_lt
        .global __avr32_f32_cmp_lt
        .type  __avr32_f32_cmp_lt,@function
__avr32_f32_cmp_lt:
#endif  
        lsl     r10, r12, 1     /* Remove sign bits */
        lsl     r9, r11, 1
	subfeq	r10, 0
#ifdef L_avr32_f32_cmp_ge
	reteq	1		/* Both number are zero. Return true. */
#endif	
#ifdef L_avr32_f32_cmp_lt
	reteq	0		/* Both number are zero. Return false. */
#endif	
	mov_imm	r8, 0xff000000
        cp.w    r10, r8
        rethi   0               /* Op0 is NaN */                
        cp.w    r9, r8
        rethi   0               /* Op1 is Nan */
 
        eor     r8, r11, r12
        bld     r12, 31
#ifdef L_avr32_f32_cmp_ge
        srcc    r8      /* Set result to true if op0 is positive*/
#endif
#ifdef L_avr32_f32_cmp_lt
        srcs    r8      /* Set result to true if op0 is negative*/
#endif
        retmi   r8      /* Return if signs are different */
        brcs    0f      /* Both signs negative? */
 
        /* Both signs positive */
        cp.w    r12, r11
#ifdef L_avr32_f32_cmp_ge
        reths    1
        retlo    0
#endif
#ifdef L_avr32_f32_cmp_lt
        reths    0
        retlo    1
#endif
0:
        /* Both signs negative */
        cp.w    r11, r12
#ifdef L_avr32_f32_cmp_ge
        reths    1
        retlo    0
#endif
#ifdef L_avr32_f32_cmp_lt
        reths    0
        retlo    1
#endif
#endif
        
 
#ifdef L_avr32_f64_cmp_eq
        .global __avr32_f64_cmp_eq
        .type  __avr32_f64_cmp_eq,@function
__avr32_f64_cmp_eq:     
        cp.w    r10,r8
        cpc     r11,r9
        breq    0f
        
        /* Args were not equal*/
        /* Both args could be zero with different sign bits */
        lsl     r11,1                   /* get rid of sign bits */
        lsl     r9,1
        or      r11,r10                 /* Check if all bits are zero */
        or      r11,r9
        or      r11,r8
        reteq   1                       /* If all zeros the arguments are equal
                                           so return 1 else return 0 */
        ret     0
0:      
        /* check for NaN */
        lsl     r11,1
	mov_imm	r12, 0xffe00000
        cp.w    r10,0
        cpc     r11,r12                 /* check if nan or inf */
        retls   1                       /* If Arg is NaN return 0 else 1*/
        ret     0                       /* Return  */
 
#endif
 
 
#if   defined(L_avr32_f64_cmp_ge) || defined(L_avr32_f64_cmp_lt)
 
#ifdef L_avr32_f64_cmp_ge
        .global __avr32_f64_cmp_ge
        .type  __avr32_f64_cmp_ge,@function
__avr32_f64_cmp_ge:
#endif  
#ifdef L_avr32_f64_cmp_lt
        .global __avr32_f64_cmp_lt
        .type  __avr32_f64_cmp_lt,@function
__avr32_f64_cmp_lt:
#endif  
 
        /* compare magnitude of op1 and op2 */
        lsl     r11,1                   /* Remove sign bit of op1 */
        srcs    r12                     /* Sign op1 to lsb of r12*/
	subfeq	r10, 0
	breq	3f			/* op1 zero */
        lsl     r9,1                    /* Remove sign bit of op2 */
        rol     r12                     /* Sign op2 to lsb of lr, sign bit op1 bit 1 of r12*/
	
 
        /* Check for Nan */
	pushm	lr
	mov_imm	lr, 0xffe00000
        cp.w    r10,0
        cpc     r11,lr
        brhi    0f      /* We have NaN */
        cp.w    r8,0
        cpc     r9,lr
        brhi    0f      /* We have NaN */
	popm	lr
 
        cp.w    r12,3                   /* both operands negative ?*/    
        breq    1f
 
        cp.w    r12,1                   /* both operands positive? */
        brlo    2f
 
        /* Different signs. If sign of op1 is negative the difference
           between op1 and op2 will always be negative, and if op1 is
           positive the difference will always be positive */           
#ifdef L_avr32_f64_cmp_ge
	reteq	1
	retne	0
#endif
#ifdef L_avr32_f64_cmp_lt
	reteq	0
	retne	1
#endif
  
2:
        /* Both operands positive. Just compute the difference */
        cp.w    r10,r8
        cpc     r11,r9
#ifdef L_avr32_f64_cmp_ge
	reths	1
	retlo	0
#endif
#ifdef L_avr32_f64_cmp_lt
	reths	0
	retlo	1
#endif
                
1:
        /* Both operands negative. Compute the difference with operands switched */
        cp     r8,r10
        cpc    r9,r11
#ifdef L_avr32_f64_cmp_ge
	reths	1
	retlo	0
#endif
#ifdef L_avr32_f64_cmp_lt
	reths	0
	retlo	1
#endif

0:      
        popm    pc, r12=0
#endif
 
3:
        lsl     r9,1                   /* Remove sign bit of op1 */
#ifdef L_avr32_f64_cmp_ge
        srcs    r12		       /* If op2 is negative then op1 >= op2. */	
#endif
#ifdef L_avr32_f64_cmp_lt
        srcc    r12		       /* If op2 is positve then op1 <= op2. */
#endif
	subfeq	r8, 0		       
#ifdef L_avr32_f64_cmp_ge
	reteq	1		       /* Both operands are zero. Return true. */
#endif
#ifdef L_avr32_f64_cmp_lt
	reteq	0		       /* Both operands are zero. Return false. */
#endif
	ret	r12
				        
 
#if defined(L_avr32_f64_div) || defined(L_avr32_f64_div_fast)
        .align  2

#if defined(L_avr32_f64_div_fast)
        .global __avr32_f64_div_fast
        .type  __avr32_f64_div_fast,@function
__avr32_f64_div_fast:
#else
        .global __avr32_f64_div
        .type  __avr32_f64_div,@function
__avr32_f64_div:
#endif
        stm     --sp, r0, r1, r2, r3, r4, r5, r6, r7,lr 
        /* op1 in {r11,r10}*/
        /* op2 in {r9,r8}*/
        eor     lr, r11, r9             /* MSB(lr) = Sign(op1) ^ Sign(op2) */
 
        
        /* Unpack op1 to 2.62 format*/  
        /* exp: r7 */
        /* sf:  r11, r10 */
        lsr     r7, r11, 20 /* Extract exponent */
                
        lsl     r11, 9 /* Extract mantissa, leave room for implicit bit */ 
        or      r11, r11, r10>>23
        lsl     r10, 9
        sbr     r11, 29 /* Insert implicit bit */
        andh    r11, 0x3fff /*Mask last part of exponent since we use 2.62 format*/
 
        cbr     r7, 11       /* Clear sign bit */
        /* Check if normalization is needed */
        breq    11f /*If number is subnormal, normalize it */
22:     
        cp      r7, 0x7ff
        brge    2f  /* Check op1 for NaN or Inf */

        /* Unpack op2 to 2.62 format*/
        /* exp: r6 */
        /* sf:  r9, r8 */
        lsr     r6, r9, 20 /* Extract exponent */
                
        lsl     r9, 9 /* Extract mantissa, leave room for implicit bit */ 
        or      r9, r9, r8>>23
        lsl     r8, 9
        sbr     r9, 29 /* Insert implicit bit */
        andh    r9, 0x3fff /*Mask last part of exponent since we use 2.62 format*/
 
        cbr     r6, 11       /* Clear sign bit */
        /* Check if normalization is needed */
        breq    13f /*If number is subnormal, normalize it */
23:             
        cp      r6, 0x7ff
        brge    3f  /* Check op2 for NaN or Inf */

        /* Calculate new exponent */
        sub     r7, r6
        sub     r7,-1023
 
        /* Divide */
        /* Approximating 1/d with the following recurrence: */
        /* R[j+1] = R[j]*(2-R[j]*d) */
        /* Using 2.62 format */
        /* TWO:  r12 */
        /* d = op2 = divisor (2.62 format): r9,r8 */
        /* Multiply result :     r5, r4 */
        /* Initial guess :       r3, r2 */
        /* New approximations :  r3, r2 */
        /* op1 = Dividend (2.62 format) : r11, r10 */
 
	mov_imm	r12, 0x80000000
         
        /* Load initial guess, using look-up table */
        /* Initial guess is of format 01.XY, where XY is constructed as follows: */
        /* Let d be of following format: 00.1xy....., then XY=~xy */
        /* For d=00.100 = 0,5   -> initial guess=01.11 = 1,75 */
        /* For d=00.101 = 0,625 -> initial guess=01.11 = 1,5  */
        /* For d=00.110 = 0,75  -> initial guess=01.11 = 1,25 */
        /* For d=00.111 = 0,875 -> initial guess=01.11 = 1,0  */
        /* r2 is also part of the reg pair forming initial guess, but it*/
        /* is kept uninitialized to save one cycle since it has so low significance*/
 
        lsr     r3, r12, 1
        bfextu  r4, r9, 27, 2
        com     r4
        bfins   r3, r4, 28, 2
 
        /* First approximation */
        /* Approximating to 32 bits */
        /* r5 = R[j]*d */
        mulu.d  r4, r3, r9
        /* r5 = 2-R[j]*d */
        sub    r5, r12, r5<<2
        /* r3 = R[j]*(2-R[j]*d) */
        mulu.d  r4, r3, r5
        lsl     r3, r5, 2
         
        /* Second approximation */
        /* Approximating to 32 bits */
        /* r5 = R[j]*d */
        mulu.d  r4, r3, r9
        /* r5 = 2-R[j]*d */
        sub    r5, r12, r5<<2
        /* r3 = R[j]*(2-R[j]*d) */
        mulu.d  r4, r3, r5
        lsl     r3, r5, 2
         
        /* Third approximation */
        /* Approximating to 32 bits */
        /* r5 = R[j]*d */
        mulu.d  r4, r3, r9
        /* r5 = 2-R[j]*d */
        sub    r5, r12, r5<<2
        /* r3 = R[j]*(2-R[j]*d) */
        mulu.d  r4, r3, r5
        lsl     r3, r5, 2
 
        /* Fourth approximation */
        /* Approximating to 64 bits */
        /* r5,r4 = R[j]*d */
        mul_approx_df        r3 /*ah*/, r2 /*al*/, r9 /*bh*/, r8 /*bl*/, r5 /*rh*/, r4 /*rl*/, r1 /*sh*/, r0 /*sl*/
        lsl    r5, 2
        or     r5, r5, r4>>30
        lsl    r4, 2
        /* r5,r4 = 2-R[j]*d */
        neg    r4
        sbc    r5, r12, r5
        /* r3,r2 = R[j]*(2-R[j]*d) */
        mul_approx_df        r3 /*ah*/, r2 /*al*/, r5 /*bh*/, r4 /*bl*/, r5 /*rh*/, r4 /*rl*/, r1 /*sh*/, r0 /*sl*/
        lsl    r3, r5, 2
        or     r3, r3, r4>>30
        lsl    r2, r4, 2
 
 
        /* Fifth approximation */
        /* Approximating to 64 bits */
        /* r5,r4 = R[j]*d */
        mul_approx_df        r3 /*ah*/, r2 /*al*/, r9 /*bh*/, r8 /*bl*/, r5 /*rh*/, r4 /*rl*/, r1 /*sh*/, r0 /*sl*/
        lsl    r5, 2
        or     r5, r5, r4>>30
        lsl    r4, 2
        /* r5,r4 = 2-R[j]*d */
        neg    r4
        sbc    r5, r12, r5
        /* r3,r2 = R[j]*(2-R[j]*d) */
        mul_approx_df        r3 /*ah*/, r2 /*al*/, r5 /*bh*/, r4 /*bl*/, r5 /*rh*/, r4 /*rl*/, r1 /*sh*/, r0 /*sl*/
        lsl    r3, r5, 2
        or     r3, r3, r4>>30
        lsl    r2, r4, 2
 
 
        /* Multiply with dividend to get quotient */
        mul_approx_df        r3 /*ah*/, r2 /*al*/, r11 /*bh*/, r10 /*bl*/, r3 /*rh*/, r2 /*rl*/, r1 /*sh*/, r0 /*sl*/
 
 
        /* To increase speed, this result is not corrected before final rounding.*/
        /* This may give a difference to IEEE compliant code of 1 ULP.*/
		
 
        /* Adjust exponent and mantissa */
        /* r7:exp, [r3, r2]:mant, [r5, r4]:scratch*/
        /* Mantissa may be of the format 0.xxxx or 1.xxxx. */
        /* In the first case, shift one pos to left.*/
        bld     r3, 31-3
	breq	0f
	lsl	r2, 1
	rol	r3
	sub	r7, 1
#if defined(L_avr32_f64_div)
	/* We must scale down the dividend to 5.59 format. */
	lsr	r10, 3
	or	r10, r10, r11 << 29
	lsr	r11, 3
	rjmp    1f
#endif	
0:	
#if defined(L_avr32_f64_div)
	/* We must scale down the dividend to 6.58 format. */
	lsr	r10, 4
	or	r10, r10, r11 << 28
	lsr	r11, 4
1:	
#endif
        cp      r7, 0   
        brle    __avr32_f64_div_res_subnormal /* Result was subnormal. */
 
 
#if defined(L_avr32_f64_div)
	/* In order to round correctly we calculate the remainder:	
	   Remainder = dividend[11:r10] - divisor[r9:r8]*quotient[r3:r2] 
	   for the case when the quotient is halfway between the round-up
	   value and the round down value. If the remainder then is negative
	   it means that the quotient was to big and that it should not be
           rounded up, if the remainder is positive the quotient was to small
	   and we need to round up. If the remainder is zero it means that the
	   quotient is exact but since we need to remove the guard bit we should
	   round to even. */

	/* Truncate and add guard bit. */
	andl	r2, 0xff00
	orl	r2, 0x0080	
	

	/* Now do the multiplication. The quotient has the format 4.60
	   while the divisor has the format 2.62 which gives a result
	   of 6.58 */
        mulu.d  r0, r3, r8
        macu.d  r0, r2, r9
        mulu.d  r4, r2, r8
        mulu.d  r8, r3, r9
	add	r5, r0
	adc	r8, r8, r1	
	acr	r9


	/* Check if remainder is positive, negative or equal. */
	bfextu	r12, r2, 8, 1  /* Get parity bit into bit 0 of r0 */ 
	cp	r4, 0
	cpc	r5
__avr32_f64_div_round_subnormal:	
	cpc	r8, r10
	cpc	r9, r11
	srlo	r6	/* Remainder positive:	 we need to round up.*/
	moveq	r6, r12  /* Remainder zero:	 round up if mantissa odd. */
#else
	bfextu	r6, r2, 7, 1  /* Get guard bit */ 	
#endif
	/* Final packing, scale down mantissa. */
	lsr	r10, r2, 8
        or      r10, r10, r3<<24
        lsr     r11, r3, 8
	/* Insert exponent and sign bit*/
	bfins	r11, r7, 20, 11
        bld     lr, 31
        bst     r11, 31

	/* Final rounding */
	add	r10, r6
	acr	r11		
	        
        /* Return result in [r11,r10] */
        ldm     sp++, r0, r1, r2, r3, r4, r5, r6, r7,pc
 
                
2:
        /* Op1 is NaN or inf */
        andh    r11, 0x000f /* Extract mantissa */
        or      r11, r10
        brne    16f     /* Return NaN if op1 is NaN */
        /* Op1 is inf check op2 */
        lsr     r6, r9, 20 /* Extract exponent */
        cbr     r6, 8       /* Clear sign bit */
        cp      r6, 0x7ff
        brne    17f     /* Inf/number gives inf, return inf */
        rjmp    16f     /* The rest gives NaN*/
        
3:      
        /* Op1 is a valid number. Op 2 is NaN or inf */
        andh    r9, 0x000f /* Extract mantissa */
        or      r9, r8
        brne    16f     /* Return NaN if op2 is NaN */
        rjmp    15f     /* Op2 was inf, return zero*/
                
11:     /* Op1 was denormal. Fix it. */
        lsl     r11, 3
        or      r11, r11, r10 >> 29
        lsl     r10, 3
        /* Check if op1 is zero. */
        or      r4, r10, r11
        breq    __avr32_f64_div_op1_zero
        normalize_df    r7 /*exp*/, r10, r11 /*Mantissa*/, r4, r5 /*scratch*/
        lsr     r10, 2
        or      r10, r10, r11 << 30
        lsr     r11, 2
        rjmp    22b
 
 
13:     /* Op2 was denormal. Fix it */
        lsl     r9, 3
        or      r9, r9, r8 >> 29
        lsl     r8, 3
        /* Check if op2 is zero. */
        or      r4, r9, r8
        breq    17f     /* Divisor is zero -> return Inf */
        normalize_df    r6 /*exp*/, r8, r9 /*Mantissa*/, r4, r5 /*scratch*/     
        lsr     r8, 2
        or      r8, r8, r9 << 30
        lsr     r9, 2
        rjmp    23b
        
 
__avr32_f64_div_res_subnormal:/* Divide result was subnormal. */
#if defined(L_avr32_f64_div)
	/* Check how much we must scale down the mantissa. */
	neg	r7
	sub	r7, -1     /* We do no longer have an implicit bit. */
	satu	r7 >> 0, 6 /* Saturate shift amount to max 63. */
	cp.w	r7, 32
	brge	0f
	/* Shift amount <32 */
	/* Scale down quotient */
	rsub	r6, r7, 32
	lsr	r2, r2, r7
	lsl	r12, r3, r6
	or	r2, r12
	lsr	r3, r3, r7
	/* Scale down the dividend to match the scaling of the quotient. */
	lsl	r1, r10, r6
	lsr	r10, r10, r7
	lsl	r12, r11, r6
	or	r10, r12
	lsr	r11, r11, r7
	mov	r0, 0
	rjmp	1f
0:
	/* Shift amount >=32 */
	rsub	r6, r7, 32
	moveq	r0, 0
	moveq	r12, 0
	breq	0f
	lsl	r0, r10, r6
	lsl	r12, r11, r6
0:	
	lsr	r2, r3, r7
	mov	r3, 0
	/* Scale down the dividend to match the scaling of the quotient. */
	lsr	r1, r10, r7
	or	r1, r12
	lsr	r10, r11, r7
	mov	r11, 0
1:	
	/* Start performing the same rounding as done for normal numbers
	   but this time we have scaled the quotient and dividend and hence
	   need a little different comparison. */
	/* Truncate and add guard bit. */
	andl	r2, 0xff00
	orl	r2, 0x0080	
	
	/* Now do the multiplication. */
        mulu.d  r6, r3, r8
        macu.d  r6, r2, r9
        mulu.d  r4, r2, r8
        mulu.d  r8, r3, r9
	add	r5, r6
	adc	r8, r8, r7	
	acr	r9

	/* Set exponent to 0 */
	mov	r7, 0	

	/* Check if remainder is positive, negative or equal. */
	bfextu	r12, r2, 8, 1  /* Get parity bit into bit 0 of r0 */ 
	cp	r4, r0
	cpc	r5, r1
	/* Now the rest of the rounding is the same as for normals. */
	rjmp	__avr32_f64_div_round_subnormal
	
#endif
15:	
	/* Flush to zero for the fast version. */
        mov     r11, lr /*Get correct sign*/
        andh    r11, 0x8000, COH
        mov     r10, 0
        ldm     sp++, r0, r1, r2, r3, r4, r5, r6, r7,pc
	 
16:     /* Return NaN. */
        mov     r11, -1
        mov     r10, -1
        ldm     sp++, r0, r1, r2, r3, r4, r5, r6, r7,pc
        
17:     /* Return INF. */
        mov     r11, lr /*Get correct sign*/
        andh    r11, 0x8000, COH
        orh     r11, 0x7ff0
        mov     r10, 0
        ldm     sp++, r0, r1, r2, r3, r4, r5, r6, r7,pc

__avr32_f64_div_op1_zero:
        or      r5, r8, r9 << 1
        breq    16b             /* 0.0/0.0 -> NaN */
        bfextu  r4, r9, 20, 11
        cp      r4, 0x7ff
        brne    15b             /* Return zero */
        /* Check if divisor is Inf or NaN */
        or      r5, r8, r9 << 12
        breq    15b             /* Divisor is inf -> return zero */
        rjmp    16b             /* Return NaN */                
        
        
        

#endif  
                
#if defined(L_avr32_f32_addsub) || defined(L_avr32_f32_addsub_fast)

        .align  2
__avr32_f32_sub_from_add:
        /* Switch sign on op2 */
        eorh    r11, 0x8000

#if defined(L_avr32_f32_addsub_fast)
        .global __avr32_f32_sub_fast
        .type  __avr32_f32_sub_fast,@function
__avr32_f32_sub_fast:
#else
        .global __avr32_f32_sub
        .type  __avr32_f32_sub,@function
__avr32_f32_sub:
#endif 
 
        /* Check signs */
        eor     r8, r11, r12
        /* Different signs, use subtraction. */
        brmi    __avr32_f32_add_from_sub
 
        /* Get sign of op1 */
        mov     r8, r12
        andh    r12, 0x8000, COH                
 
        /* Remove sign from operands */
        cbr     r11, 31
#if defined(L_avr32_f32_addsub_fast)
        reteq   r8      /* If op2 is zero return op1 */
#endif
        cbr     r8, 31
 
        /* Put the number with the largest exponent in r10
           and the number with the smallest exponent in r9 */
        max     r10, r8, r11
        min     r9, r8, r11
        cp      r10, r8 /*If largest operand (in R10) is not equal to op1*/
        subne   r12, 1 /* Subtract 1 from sign, which will invert MSB of r12*/
        andh    r12, 0x8000, COH /*Mask all but MSB*/
 
        /* Unpack exponent and mantissa of op1 */
        lsl     r8, r10, 8
        sbr     r8, 31  /* Set implicit bit. */
        lsr     r10, 23 
                
        /* op1 is NaN or Inf. */
        cp.w    r10, 0xff
        breq    __avr32_f32_sub_op1_nan_or_inf
        
        /* Unpack exponent and mantissa of op2 */
        lsl     r11, r9, 8
        sbr     r11, 31  /* Set implicit bit. */
        lsr     r9, 23  
 
#if defined(L_avr32_f32_addsub)
        /* Keep sticky bit for correct IEEE rounding */
        st.w    --sp, r12
 
        /* op2 is either zero or subnormal. */
        breq    __avr32_f32_sub_op2_subnormal
0:      
        /* Get shift amount to scale mantissa of op2. */
        sub     r12, r10, r9                 

	breq	__avr32_f32_sub_shift_done
 
        /* Saturate the shift amount to 31. If the amount
           is any larger op2 is insignificant. */
        satu    r12 >> 0, 5      

        /* Put the remaining bits into r9.*/
        rsub    r9, r12, 32
        lsl     r9, r11, r9
	
	/* If the remaining bits are non-zero then we must subtract one
	   more from opL.  */
	subne	r8, 1
	srne	r9	/* LSB of r9 represents sticky bits. */

        /* Shift mantissa of op2 to same decimal point as the mantissa
           of op1. */
        lsr     r11, r11, r12
 

__avr32_f32_sub_shift_done:	
        /* Now subtract the mantissas. */
        sub     r8, r11
 
        ld.w    r12, sp++
 
        /* Normalize resulting mantissa. */
        clz     r11, r8

	retcs	0
        lsl     r8, r8, r11
        sub     r10, r11
        brle    __avr32_f32_sub_subnormal_result

        /* Insert the bits we will remove from the mantissa into r9[31:24] */
	or	r9, r9, r8 << 24
#else
        /* Ignore sticky bit to simplify and speed up rounding */
        /* op2 is either zero or subnormal. */
        breq    __avr32_f32_sub_op2_subnormal
0:      
        /* Get shift amount to scale mantissa of op2. */
        rsub    r9, r10                 
 
        /* Saturate the shift amount to 31. If the amount
           is any larger op2 is insignificant. */
        satu    r9 >> 0, 5      
 
        /* Shift mantissa of op2 to same decimal point as the mantissa
           of op1. */
        lsr     r11, r11, r9
 
        /* Now subtract the mantissas. */
        sub     r8, r11
 
        /* Normalize resulting mantissa. */
        clz     r9, r8
	retcs	0
        lsl     r8, r8, r9
        sub     r10, r9
        brle    __avr32_f32_sub_subnormal_result        
#endif
        
        /* Pack result. */
        or      r12, r12, r8 >> 8
        bfins   r12, r10, 23, 8         
 
        /* Round */     
__avr32_f32_sub_round:
#if defined(L_avr32_f32_addsub)
	mov_imm	r10, 0x80000000
        bld     r12, 0
        subne   r10, -1 
        cp.w    r9, r10
        subhs   r12, -1
#else
        bld     r8, 7 
        acr     r12
#endif  
        
        ret     r12     
 
 
__avr32_f32_sub_op2_subnormal:
        /* Fix implicit bit and adjust exponent of subnormals. */
        cbr     r11, 31
        /* Set exponent to 1 if we do not have a zero. */
        movne   r9,1
 
        /* Check if op1 is also subnormal. */
        cp.w    r10, 0
        brne    0b
 
        cbr     r8, 31
         /* If op1 is not zero set exponent to 1. */
        movne   r10,1
                
        rjmp    0b
 
__avr32_f32_sub_op1_nan_or_inf: 
        /* Check if op1 is NaN, if so return NaN */
        lsl     r11, r8, 1
        retne   -1
 
        /* op1 is Inf. */
        bfins   r12, r10, 23, 8 /* Generate Inf in r12 */
 
        /* Check if op2 is Inf. or NaN */
        lsr     r11, r9, 23
        cp.w    r11, 0xff
        retne   r12             /* op2 not Inf or NaN, return op1 */
 
        ret     -1              /* op2 Inf or NaN, return NaN */
 
__avr32_f32_sub_subnormal_result:
        /* Check if the number is so small that
           it will be represented with zero. */
        rsub    r10, r10, 9
        rsub    r11, r10, 32
        retcs   0
 
        /* Shift the mantissa into the correct position.*/
        lsr     r10, r8, r10
        /* Add sign bit. */
        or      r12, r10

        /* Put the shifted out bits in the most significant part
           of r8. */
        lsl     r8, r8, r11
 
#if defined(L_avr32_f32_addsub)
        /* Add all the remainder bits used for rounding into r9 */
        or      r9, r8
#else
        lsr     r8, 24 
#endif
        rjmp    __avr32_f32_sub_round
 
                                
        .align  2

__avr32_f32_add_from_sub:
        /* Switch sign on op2 */
        eorh    r11, 0x8000

#if defined(L_avr32_f32_addsub_fast)
        .global __avr32_f32_add_fast
        .type  __avr32_f32_add_fast,@function
__avr32_f32_add_fast:
#else
        .global __avr32_f32_add
        .type  __avr32_f32_add,@function
__avr32_f32_add:
#endif 
	
        /* Check signs */
        eor     r8, r11, r12
        /* Different signs, use subtraction. */
        brmi    __avr32_f32_sub_from_add
 
        /* Get sign of op1 */
        mov     r8, r12
        andh    r12, 0x8000, COH                
 
        /* Remove sign from operands */
        cbr     r11, 31
#if defined(L_avr32_f32_addsub_fast)
        reteq   r8      /* If op2 is zero return op1 */
#endif
        cbr     r8, 31
 
        /* Put the number with the largest exponent in r10
           and the number with the smallest exponent in r9 */
        max     r10, r8, r11
        min     r9, r8, r11
 
        /* Unpack exponent and mantissa of op1 */
        lsl     r8, r10, 8
        sbr     r8, 31  /* Set implicit bit. */
        lsr     r10, 23 
                
        /* op1 is NaN or Inf. */
        cp.w    r10, 0xff
        breq    __avr32_f32_add_op1_nan_or_inf
        
        /* Unpack exponent and mantissa of op2 */
        lsl     r11, r9, 8
        sbr     r11, 31  /* Set implicit bit. */
        lsr     r9, 23  
 
#if defined(L_avr32_f32_addsub)
        /* op2 is either zero or subnormal. */
        breq    __avr32_f32_add_op2_subnormal
0:      
        /* Keep sticky bit for correct IEEE rounding */
        st.w    --sp, r12
 
        /* Get shift amount to scale mantissa of op2. */
        rsub    r9, r10                 
 
        /* Saturate the shift amount to 31. If the amount
           is any larger op2 is insignificant. */
        satu    r9 >> 0, 5      
 
        /* Shift mantissa of op2 to same decimal point as the mantissa
           of op1. */
        lsr     r12, r11, r9
 
        /* Put the remainding bits into r11[23:..].*/
        rsub    r9, r9, (32-8)
        lsl     r11, r11, r9
        /* Insert the bits we will remove from the mantissa into r11[31:24] */
        bfins   r11, r12, 24, 8
 
        /* Now add the mantissas. */
        add     r8, r12
 
        ld.w    r12, sp++
#else
        /* Ignore sticky bit to simplify and speed up rounding */
        /* op2 is either zero or subnormal. */
        breq    __avr32_f32_add_op2_subnormal
0:      
        /* Get shift amount to scale mantissa of op2. */
        rsub    r9, r10                 
 
        /* Saturate the shift amount to 31. If the amount
           is any larger op2 is insignificant. */
        satu    r9 >> 0, 5      
 
        /* Shift mantissa of op2 to same decimal point as the mantissa
           of op1. */
        lsr     r11, r11, r9
 
        /* Now add the mantissas. */
        add     r8, r11
        
#endif
        /* Check if we overflowed. */
        brcs    __avr32_f32_add_res_of
1:      
        /* Pack result. */
        or      r12, r12, r8 >> 8
        bfins   r12, r10, 23, 8         
 
        /* Round */     
#if defined(L_avr32_f32_addsub)
	mov_imm	r10, 0x80000000
        bld     r12, 0
        subne   r10, -1 
        cp.w    r11, r10
        subhs   r12, -1
#else
        bld     r8, 7 
        acr     r12
#endif  

        ret     r12     
 
__avr32_f32_add_op2_subnormal:
        /* Fix implicit bit and adjust exponent of subnormals. */
        cbr     r11, 31
        /* Set exponent to 1 if we do not have a zero. */
        movne   r9,1
 
        /* Check if op1 is also subnormal. */
        cp.w    r10, 0
        brne    0b
	/* Both operands subnormal, just add the mantissas and 
	   pack. If the addition of the subnormal numbers results
	   in a normal number then the exponent will automatically
	   be set to 1 by the addition. */
        cbr     r8, 31
	add	r11, r8
	or	r12, r12, r11 >> 8
	ret	r12
 
__avr32_f32_add_op1_nan_or_inf: 
        /* Check if op1 is NaN, if so return NaN */
        lsl     r11, r8, 1
        retne   -1
 
        /* op1 is Inf. */
        bfins   r12, r10, 23, 8 /* Generate Inf in r12 */
 
        /* Check if op2 is Inf. or NaN */
        lsr     r11, r9, 23
        cp.w    r11, 0xff
        retne   r12             /* op2 not Inf or NaN, return op1 */
 
        lsl     r9, 9
        reteq   r12             /* op2 Inf return op1 */
        ret     -1              /* op2 is NaN, return NaN */ 
 
__avr32_f32_add_res_of:
        /* We overflowed. Increase exponent and shift mantissa.*/
        lsr     r8, 1
        sub     r10, -1
 
        /* Clear mantissa to set result to Inf if the exponent is 255. */
        cp.w    r10, 255
        moveq   r8, 0
        moveq   r11, 0
        rjmp    1b      
        
        
#endif

	
#if defined(L_avr32_f32_div) || defined(L_avr32_f32_div_fast)
	.align	2

#if defined(L_avr32_f32_div_fast)
        .global __avr32_f32_div_fast
        .type  __avr32_f32_div_fast,@function
__avr32_f32_div_fast:
#else
        .global __avr32_f32_div
        .type  __avr32_f32_div,@function
__avr32_f32_div:
#endif
	 
        eor     r8, r11, r12            /* MSB(r8) = Sign(op1) ^ Sign(op2) */
 
        /* Unpack */
        lsl     r12,1
        reteq   0                       /* Return zero if op1 is zero */
        lsl     r11,1
        breq    4f                      /* Check op2 for zero */
        
        /* Unpack op1*/ 
        /* exp: r9 */
        /* sf:  r12 */
        lsr     r9, r12, 24
        breq    11f /*If number is subnormal*/
        cp      r9, 0xff
        brhs    2f  /* Check op1 for NaN or Inf */      
        lsl     r12, 7
        sbr     r12, 31 /*Implicit bit*/
12:                     
 
        /* Unpack op2*/
        /* exp: r10 */
        /* sf:  r11 */
        lsr     r10, r11, 24
        breq    13f /*If number is subnormal*/
        cp      r10, 0xff
        brhs    3f  /* Check op2 for NaN or Inf */      
        
        lsl     r11,7
        sbr     r11, 31 /*Implicit bit*/
14:     
 
        /* For UC3, store with predecrement is faster than stm */
        st.w    --sp, r5
        st.d    --sp, r6
 
        /* Calculate new exponent */
        sub     r9, r10
        sub     r9,-127
 
        /* Divide */
        /* Approximating 1/d with the following recurrence: */
        /* R[j+1] = R[j]*(2-R[j]*d) */
        /* Using 2.30 format */
        /* TWO:  r10 */
        /* d:    r5 */
        /* Multiply result :     r6, r7 */
        /* Initial guess :       r11 */
        /* New approximations :  r11 */
        /* Dividend :            r12 */

	/* Load TWO */
	mov_imm	r10, 0x80000000 
         
        lsr     r12, 2     /* Get significand of Op1 in 2.30 format */
        lsr     r5, r11, 2 /* Get significand of Op2 (=d) in 2.30 format */
 
        /* Load initial guess, using look-up table */
        /* Initial guess is of format 01.XY, where XY is constructed as follows: */
        /* Let d be of following format: 00.1xy....., then XY=~xy */
        /* For d=00.100 = 0,5   -> initial guess=01.11 = 1,75 */
        /* For d=00.101 = 0,625 -> initial guess=01.11 = 1,5  */
        /* For d=00.110 = 0,75  -> initial guess=01.11 = 1,25 */
        /* For d=00.111 = 0,875 -> initial guess=01.11 = 1,0  */
 
        lsr     r11, r10, 1
        bfextu  r6, r5, 27, 2
        com     r6
        bfins   r11, r6, 28, 2
 
        /* First approximation */
        /* r7 = R[j]*d */
        mulu.d  r6, r11, r5
        /* r7 = 2-R[j]*d */
        sub    r7, r10, r7<<2
        /* r11 = R[j]*(2-R[j]*d) */
        mulu.d  r6, r11, r7
        lsl     r11, r7, 2
         
        /* Second approximation */
        /* r7 = R[j]*d */
        mulu.d  r6, r11, r5
        /* r7 = 2-R[j]*d */
        sub    r7, r10, r7<<2
        /* r11 = R[j]*(2-R[j]*d) */
        mulu.d  r6, r11, r7
        lsl     r11, r7, 2
         
        /* Third approximation */
        /* r7 = R[j]*d */
        mulu.d  r6, r11, r5
        /* r7 = 2-R[j]*d */
        sub    r7, r10, r7<<2
        /* r11 = R[j]*(2-R[j]*d) */
        mulu.d  r6, r11, r7
        lsl     r11, r7, 2
 
        /* Fourth approximation */
        /* r7 = R[j]*d */
        mulu.d  r6, r11, r5
        /* r7 = 2-R[j]*d */
        sub    r7, r10, r7<<2
        /* r11 = R[j]*(2-R[j]*d) */
        mulu.d  r6, r11, r7
        lsl     r11, r7, 2
 
 
        /* Multiply with dividend to get quotient, r7 = sf(op1)/sf(op2) */
        mulu.d  r6, r11, r12
 
        /* Shift by 3 to get result in 1.31 format, as required by the exponent. */
        /* Note that 1.31 format is already used by the exponent in r9, since */
        /* a bias of 127 was added to the result exponent, even though the implicit */
        /* bit was inserted. This gives the exponent an additional bias of 1, which */
        /* supports 1.31 format. */
	//lsl     r10, r7, 3

	/* Adjust exponent and mantissa in case the result is of format
	   0000.1xxx to 0001.xxx*/	
#if defined(L_avr32_f32_div)
	lsr	r12, 4	/* Scale dividend to 6.26 format to match the
			   result of the multiplication of the divisor and 
			   quotient to get the remainder. */
#endif
	bld	r7, 31-3
	breq	0f
	lsl	r7, 1	
	sub	r9, 1
#if defined(L_avr32_f32_div)
	lsl	r12, 1	/* Scale dividend to 5.27 format to match the
			   result of the multiplication of the divisor and 
			   quotient to get the remainder. */
#endif
0:		
        cp      r9, 0   
        brle    __avr32_f32_div_res_subnormal /* Result was subnormal. */

		
#if defined(L_avr32_f32_div)
	/* In order to round correctly we calculate the remainder:	
	   Remainder = dividend[r12] - divisor[r5]*quotient[r7] 
	   for the case when the quotient is halfway between the round-up
	   value and the round down value. If the remainder then is negative
	   it means that the quotient was to big and that it should not be
           rounded up, if the remainder is positive the quotient was to small
	   and we need to round up. If the remainder is zero it means that the
	   quotient is exact but since we need to remove the guard bit we should
	   round to even. */
	andl	r7, 0xffe0
	orl	r7, 0x0010

	/* Now do the multiplication. The quotient has the format 4.28
	   while the divisor has the format 2.30 which gives a result
	   of 6.26 */
	mulu.d	r10, r5, r7

	/* Check if remainder is positive, negative or equal. */
	bfextu	r5, r7, 5, 1  /* Get parity bit into bit 0 of r5 */ 
	cp	r10, 0
__avr32_f32_div_round_subnormal:	
	cpc	r11, r12
	srlo	r11	/* Remainder positive:	 we need to round up.*/
	moveq	r11, r5  /* Remainder zero:	 round up if mantissa odd. */
#else
	bfextu	r11, r7, 4, 1  /* Get guard bit */ 	
#endif
                               
        /* Pack final result*/
        lsr     r12, r7, 5
        bfins   r12, r9, 23, 8
        /* For UC3, load with postincrement is faster than ldm */
        ld.d    r6, sp++
        ld.w    r5, sp++
        bld     r8, 31
        bst     r12, 31
	/* Rounding add. */
	add	r12, r11
        ret     r12

__divsf_return_op1:     
        lsl     r8, 1
        ror     r12
        ret     r12
 
 
2:
        /* Op1 is NaN or inf */
        retne   -1      /* Return NaN if op1 is NaN */
        /* Op1 is inf check op2 */
	mov_imm	r9, 0xff000000
        cp      r11, r9
        brlo    __divsf_return_op1 /* inf/number gives inf */
        ret     -1      /* The rest gives NaN*/
3:      
        /* Op2 is NaN or inf */
        reteq   0       /* Return zero if number/inf*/
        ret     -1      /* Return NaN*/
4:
        /* Op2 is zero ? */
        tst     r12,r12
        reteq   -1      /* 0.0/0.0 is NaN */
        /* Nonzero/0.0 is Inf. Sign bit will be shifted in before returning*/
	mov_imm	r12, 0xff000000
        rjmp    __divsf_return_op1
                
11:     /* Op1 was denormal. Fix it. */
        lsl     r12,7
        clz     r9,r12
        lsl     r12,r12,r9
        rsub    r9,r9,1
        rjmp    12b
 
13:     /* Op2 was denormal. Fix it. */ 
        lsl     r11,7
        clz     r10,r11
        lsl     r11,r11,r10
        rsub    r10,r10,1
        rjmp    14b
        
 
__avr32_f32_div_res_subnormal:     /* Divide result was subnormal */
#if defined(L_avr32_f32_div)
	/* Check how much we must scale down the mantissa. */
	neg	r9
	sub	r9, -1     /* We do no longer have an implicit bit. */
	satu	r9 >> 0, 5 /* Saturate shift amount to max 32. */
	/* Scale down quotient */
	rsub	r10, r9, 32
	lsr	r7, r7, r9
	/* Scale down the dividend to match the scaling of the quotient. */
	lsl	r6, r12, r10	/* Make the divident 64-bit and put the lsw in r6 */
	lsr	r12, r12, r9

	/* Start performing the same rounding as done for normal numbers
	   but this time we have scaled the quotient and dividend and hence
	   need a little different comparison. */
	andl	r7, 0xffe0
	orl	r7, 0x0010

	/* Now do the multiplication. The quotient has the format 4.28
	   while the divisor has the format 2.30 which gives a result
	   of 6.26 */
	mulu.d	r10, r5, r7

	/* Set exponent to 0 */
	mov	r9, 0	

	/* Check if remainder is positive, negative or equal. */
	bfextu	r5, r7, 5, 1  /* Get parity bit into bit 0 of r5 */ 
	cp	r10, r6
	rjmp	__avr32_f32_div_round_subnormal

#else
        ld.d    r6, sp++
        ld.w    r5, sp++
        /*Flush to zero*/
	ret	0
#endif
#endif
 
#ifdef L_avr32_f32_mul
        .global __avr32_f32_mul
        .type  __avr32_f32_mul,@function
 
                
__avr32_f32_mul:
        mov     r8, r12
        eor     r12, r11                /* MSB(r8) = Sign(op1) ^ Sign(op2) */
        andh    r12, 0x8000, COH
        
        /* arrange operands so that that op1 >= op2 */
        cbr     r8, 31
        breq    __avr32_f32_mul_op1_zero
        cbr     r11, 31
 
        /* Put the number with the largest exponent in r10
           and the number with the smallest exponent in r9 */
        max     r10, r8, r11
        min     r9, r8, r11
 
        /* Unpack exponent and mantissa of op1 */
        lsl     r8, r10, 8
        sbr     r8, 31  /* Set implicit bit. */
        lsr     r10, 23 
                
        /* op1 is NaN or Inf. */
        cp.w    r10, 0xff
        breq    __avr32_f32_mul_op1_nan_or_inf
        
        /* Unpack exponent and mantissa of op2 */
        lsl     r11, r9, 8
        sbr     r11, 31  /* Set implicit bit. */
        lsr     r9, 23  
 
        /* op2 is either zero or subnormal. */
        breq    __avr32_f32_mul_op2_subnormal
0:      
        /* Calculate new exponent */
        add     r9,r10
 
        /* Do the multiplication */
        mulu.d  r10,r8,r11
 
        /* We might need to scale up by two if the MSB of the result is
           zero. */
        lsl     r8, r11, 1
        movcc   r11, r8
        subcc   r9, 1
 
        /* Put the shifted out bits of the mantissa into r10 */
        lsr     r10, 8
        bfins   r10, r11, 24, 8
                
        sub     r9,(127-1)              /* remove extra exponent bias */
        brle    __avr32_f32_mul_res_subnormal
 
        /* Check for Inf. */
        cp.w    r9, 0xff
        brge    1f
 
        /* Pack result. */
        or      r12, r12, r11 >> 8
        bfins   r12, r9, 23, 8          
 
        /* Round */     
__avr32_f32_mul_round:
	mov_imm	r8, 0x80000000
        bld     r12, 0
        subne   r8, -1  
 
        cp.w    r10, r8
        subhs   r12, -1
        
        ret     r12     
 
1:      
        /* Return Inf */        
        orh     r12, 0x7f80
        ret     r12
 
__avr32_f32_mul_op2_subnormal:
        cbr     r11, 31
        clz     r9, r11
        retcs   0       /* op2 is zero. Return 0 */
        sub     r9, 8
        lsl     r11, r11, r9
        rsub    r9, r9, 1
                
        /* Check if op2 is subnormal. */
        tst     r10, r10
        brne    0b
 
        /* op2 is subnormal */  
        cbr     r8, 31
        clz     r10, r11
        retcs   0       /* op1 is zero. Return 0 */
        lsl     r8, r8, r10
        rsub    r10, r10, 1
                        
        rjmp    0b
                
 
__avr32_f32_mul_op1_nan_or_inf:
        /* Check if op1 is NaN, if so return NaN */
        lsl     r11, r8, 1
        retne   -1
 
        /* op1 is Inf. */
        tst     r9, r9
        reteq   -1      /* Inf * 0 -> NaN */
 
        bfins   r12, r10, 23, 8 /* Generate Inf in r12 */
 
        /* Check if op2 is Inf. or NaN */
        lsr     r11, r9, 23
        cp.w    r11, 0xff
        retne   r12             /* op2 not Inf or NaN, return Info */
 
        lsl     r9, 9
        reteq   r12             /* op2 Inf return Inf */
        ret     -1              /* op2 is NaN, return NaN */ 
        
__avr32_f32_mul_res_subnormal:
        /* Check if the number is so small that
           it will be represented with zero. */
        rsub    r9, r9, 9
        rsub    r8, r9, 32
        retcs   0
 
        /* Shift the mantissa into the correct position.*/
        lsr     r9, r11, r9
        /* Add sign bit. */
        or      r12, r9
        /* Put the shifted out bits in the most significant part
           of r8. */
        lsl     r11, r11, r8
 
        /* Add all the remainder bits used for rounding into r11 */
        andh    r10, 0x00FF     
        or      r10, r11
        rjmp    __avr32_f32_mul_round

__avr32_f32_mul_op1_zero:
        bfextu  r10, r11, 23, 8
        cp.w    r10, 0xff
        retne   r12
        reteq   -1        
 
#endif  
 
        
#ifdef L_avr32_s32_to_f32
        .global __avr32_s32_to_f32
        .type  __avr32_s32_to_f32,@function
__avr32_s32_to_f32:
        cp      r12, 0
        reteq   r12     /* If zero then return zero float */
        mov     r11, r12 /* Keep the sign */
        abs     r12     /* Compute the absolute value */
        mov     r10, 31 + 127   /* Set the correct exponent */
        
        /* Normalize */
        normalize_sf    r10 /*exp*/, r12 /*mant*/, r9 /*scratch*/       
 
        /* Check for subnormal result */
        cp.w    r10, 0
        brle    __avr32_s32_to_f32_subnormal
 
        round_sf        r10 /*exp*/, r12 /*mant*/, r9 /*scratch*/       
        pack_sf         r12 /*sf*/, r10 /*exp*/, r12 /*mant*/
        lsl     r11, 1
        ror     r12
        ret     r12             
 
__avr32_s32_to_f32_subnormal:
        /* Adjust a subnormal result */
        adjust_subnormal_sf     r12/*sf*/, r10 /*exp*/, r12 /*mant*/, r11/*sign*/, r9 /*scratch*/
        ret     r12
        
#endif
 
#ifdef L_avr32_u32_to_f32
        .global __avr32_u32_to_f32
        .type  __avr32_u32_to_f32,@function
__avr32_u32_to_f32:
        cp      r12, 0
        reteq   r12     /* If zero then return zero float */
        mov     r10, 31 + 127   /* Set the correct exponent */
        
        /* Normalize */
        normalize_sf    r10 /*exp*/, r12 /*mant*/, r9 /*scratch*/       
 
        /* Check for subnormal result */
        cp.w    r10, 0
        brle    __avr32_u32_to_f32_subnormal
 
        round_sf        r10 /*exp*/, r12 /*mant*/, r9 /*scratch*/       
        pack_sf         r12 /*sf*/, r10 /*exp*/, r12 /*mant*/
        lsr     r12,1   /* Sign bit is 0 for unsigned int */
        ret     r12             
 
__avr32_u32_to_f32_subnormal:
        /* Adjust a subnormal result */
        mov     r8, 0
        adjust_subnormal_sf     r12/*sf*/,r10 /*exp*/, r12 /*mant*/,r8/*sign*/, r9 /*scratch*/
        ret     r12
        
        
#endif
        
 
#ifdef L_avr32_f32_to_s32
        .global __avr32_f32_to_s32
        .type  __avr32_f32_to_s32,@function
__avr32_f32_to_s32:
        bfextu  r11, r12, 23, 8
        sub     r11,127                 /* Fix bias */
        retlo   0                       /* Negative exponent yields zero integer */
 
        /* Shift mantissa into correct position */
        rsub    r11,r11,31      /* Shift amount */
        lsl     r10,r12,8       /* Get mantissa */
        sbr     r10,31          /* Add implicit bit */
        lsr     r10,r10,r11     /* Perform shift */
        lsl     r12,1           /* Check sign */
        retcc   r10             /* if positive, we are done */
        neg     r10             /* if negative float, negate result */
        ret     r10
 
#endif  
        
#ifdef L_avr32_f32_to_u32
        .global __avr32_f32_to_u32
        .type  __avr32_f32_to_u32,@function
__avr32_f32_to_u32:
        cp      r12,0
        retmi   0                       /* Negative numbers gives 0 */
        bfextu  r11, r12, 23, 8         /* Extract exponent */
        sub     r11,127                 /* Fix bias */
        retlo   0                       /* Negative exponent yields zero integer */
 
        /* Shift mantissa into correct position */
        rsub    r11,r11,31      /* Shift amount */
        lsl     r12,8           /* Get mantissa */
        sbr     r12,31          /* Add implicit bit */
        lsr     r12,r12,r11     /* Perform shift */
        ret     r12
 
#endif  
 
#ifdef L_avr32_f32_to_f64
        .global __avr32_f32_to_f64
        .type  __avr32_f32_to_f64,@function
 
__avr32_f32_to_f64:
        lsl     r11,r12,1               /* Remove sign bit, keep original value in r12*/
        moveq   r10, 0
        reteq   r11                     /* Return zero if input is zero */
 
        bfextu  r9,r11,24,8              /* Get exponent */
        cp.w    r9,0xff                 /* check for NaN or inf */
        breq    0f
 
        lsl     r11,7                   /* Convert sf mantissa to df format */
        mov     r10,0
 
        /* Check if implicit bit should be set */
        cp.w    r9, 0
        subeq   r9,-1                    /* Adjust exponent if it was 0 */
        srne    r8
        or      r11, r11, r8 << 31      /* Set implicit bit if needed */
        sub     r9,(127-0x3ff)          /* Convert exponent to df format exponent */
 
        /*We know that low register of mantissa is 0, and will be unaffected by normalization.*/
        /*We can therefore use the faster normalize_sf function instead of normalize_df.*/
        normalize_sf    r9 /*exp*/, r11 /*mantissa*/, r8 /*scratch*/
        pack_df         r9 /*exp*/, r10, r11 /*mantissa*/, r10, r11 /*df*/
 
__extendsfdf_return_op1:        
        /* Rotate in sign bit */
        lsl     r12, 1
        ror     r11
        ret     r11
                        
0:
        /* Inf or NaN*/
	mov_imm	r10, 0xffe00000
        lsl     r11,8                   /* check mantissa */
        movne   r11, -1                 /* Return NaN */
        moveq   r11, r10                /* Return inf */
        rjmp    __extendsfdf_return_op1
#endif                  
 
 
#ifdef L_avr32_f64_to_f32
        .global __avr32_f64_to_f32
        .type  __avr32_f64_to_f32,@function
 
__avr32_f64_to_f32:
        /* Unpack */
        lsl     r9,r11,1                /* Unpack exponent */
        lsr     r9,21
 
        reteq   0                       /* If exponent is 0 the number is so small
                                           that the conversion to single float gives
                                           zero */
 
        lsl     r8,r11,10                  /* Adjust mantissa */
        or      r12,r8,r10>>22
 
        lsl     r10,10                  /* Check if there are any remaining bits
                                           in the low part of the mantissa.*/
        neg     r10
        rol     r12                     /* If there were remaining bits then set lsb
                                           of mantissa to 1 */
 
        cp      r9,0x7ff
        breq    2f                      /* Check for NaN or inf */
 
        sub     r9,(0x3ff-127)          /* Adjust bias of exponent */
        sbr     r12,31                  /* set the implicit bit.*/
 
        cp.w    r9, 0                   /* Check for subnormal number */
        brle    3f
 
        round_sf        r9 /*exp*/, r12 /*mant*/, r10 /*scratch*/       
        pack_sf         r12 /*sf*/, r9 /*exp*/, r12 /*mant*/
__truncdfsf_return_op1: 
        /* Rotate in sign bit */
        lsl     r11, 1
        ror     r12
        ret     r12             
        
2:
        /* NaN or inf */
        cbr     r12,31                  /* clear implicit bit */
        retne   -1                      /* Return NaN if mantissa not zero */
	mov_imm	r12, 0xff000000
        ret     r12                     /* Return inf */
 
3:      /* Result is subnormal. Adjust it.*/
        adjust_subnormal_sf     r12/*sf*/,r9 /*exp*/, r12 /*mant*/, r11/*sign*/, r10 /*scratch*/
        ret     r12
        
                
#endif
 
#if defined(L_mulsi3) && defined(__AVR32_NO_MUL__)
	.global __mulsi3
	.type __mulsi3,@function

__mulsi3:
	mov r9, 0
0:
	lsr r11, 1
	addcs r9, r9, r12
	breq 1f
	lsl r12, 1
	rjmp 0b
1:
	ret r9
#endif
