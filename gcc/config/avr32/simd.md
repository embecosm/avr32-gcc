;;   AVR32 machine description file for SIMD instructions.
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


;; Vector modes
(define_mode_iterator VECM [V2HI V4QI])
(define_mode_attr  size [(V2HI "h") (V4QI "b")])

(define_insn "add<mode>3"
  [(set (match_operand:VECM 0 "register_operand" "=r")
	(plus:VECM (match_operand:VECM 1 "register_operand" "r")
                   (match_operand:VECM 2 "register_operand" "r")))]
  "TARGET_SIMD"
  "padd.<size>\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "alu")])


(define_insn "sub<mode>3"
  [(set (match_operand:VECM 0 "register_operand" "=r")
	(minus:VECM (match_operand:VECM 1 "register_operand" "r")
                    (match_operand:VECM 2 "register_operand" "r")))]
  "TARGET_SIMD"
  "psub.<size>\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "alu")])


(define_insn "abs<mode>2"
  [(set (match_operand:VECM 0 "register_operand" "=r")
	(abs:VECM (match_operand:VECM 1 "register_operand" "r")))]
  "TARGET_SIMD"
  "pabs.s<size>\t%0, %1"
  [(set_attr "length" "4")
   (set_attr "type" "alu")])

(define_insn "ashl<mode>3"
  [(set (match_operand:VECM 0 "register_operand"           "=r")
	(ashift:VECM (match_operand:VECM 1 "register_operand" "r")
                     (match_operand:SI 2 "immediate_operand" "Ku04")))]
  "TARGET_SIMD"
  "plsl.<size>\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "alu")])

(define_insn "ashr<mode>3"
  [(set (match_operand:VECM 0 "register_operand"           "=r")
	(ashiftrt:VECM (match_operand:VECM 1 "register_operand" "r")
                       (match_operand:SI 2 "immediate_operand" "Ku04")))]
  "TARGET_SIMD"
  "pasr.<size>\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "alu")])

(define_insn "lshr<mode>3"
  [(set (match_operand:VECM 0 "register_operand"           "=r")
	(lshiftrt:VECM (match_operand:VECM 1 "register_operand" "r")
                       (match_operand:SI 2 "immediate_operand" "Ku04")))]
  "TARGET_SIMD"
  "plsr.<size>\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "alu")])

(define_insn "smaxv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(smax:V2HI (match_operand:V2HI 1 "register_operand" "r")
                        (match_operand:V2HI 2 "register_operand" "r")))]

  "TARGET_SIMD"
  "pmax.sh\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "alu")])

(define_insn "sminv2hi3"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
	(smin:V2HI (match_operand:V2HI 1 "register_operand" "r")
                        (match_operand:V2HI 2 "register_operand" "r")))]

  "TARGET_SIMD"
  "pmin.sh\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "alu")])

(define_insn "umaxv4qi3"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
	(umax:V4QI (match_operand:V4QI 1 "register_operand" "r")
                   (match_operand:V4QI 2 "register_operand" "r")))]

  "TARGET_SIMD"
  "pmax.ub\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "alu")])

(define_insn "uminv4qi3"
  [(set (match_operand:V4QI 0 "register_operand" "=r")
	(umin:V4QI (match_operand:V4QI 1 "register_operand" "r")
                   (match_operand:V4QI 2 "register_operand" "r")))]

  "TARGET_SIMD"
  "pmin.ub\t%0, %1, %2"
  [(set_attr "length" "4")
   (set_attr "type" "alu")])


(define_insn "addsubv2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
        (vec_concat:V2HI
         (plus:HI (match_operand:HI 1 "register_operand" "r")
                  (match_operand:HI 2 "register_operand" "r"))
         (minus:HI (match_dup 1) (match_dup 2))))]
  "TARGET_SIMD"
  "paddsub.h\t%0, %1:b, %2:b"
  [(set_attr "length" "4")
   (set_attr "type" "alu")])

(define_insn "subaddv2hi"
  [(set (match_operand:V2HI 0 "register_operand" "=r")
        (vec_concat:V2HI
         (minus:HI (match_operand:HI 1 "register_operand" "r")
                  (match_operand:HI 2 "register_operand" "r"))
         (plus:HI (match_dup 1) (match_dup 2))))]
  "TARGET_SIMD"
  "psubadd.h\t%0, %1:b, %2:b"
  [(set_attr "length" "4")
   (set_attr "type" "alu")])
