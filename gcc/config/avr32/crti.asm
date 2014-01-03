/*
   Init/fini stuff for AVR32.
   Copyright 2003-2006 Atmel Corporation.

   Written by Ronny Pedersen, Atmel Norway, <rpedersen@atmel.com>

   This file is part of GCC.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

	
/* The code in sections .init and .fini is supposed to be a single
   regular function.  The function in .init is called directly from
   start in crt1.asm.  The function in .fini is atexit()ed in crt1.asm
   too.

   crti.asm contributes the prologue of a function to these sections,
   and crtn.asm comes up the epilogue.  STARTFILE_SPEC should list
   crti.o before any other object files that might add code to .init
   or .fini sections, and ENDFILE_SPEC should list crtn.o after any
   such object files.  */
		
	.file		"crti.asm"

	.section	".init"
/* Just load the GOT */
	.align 2
	.global	_init
_init:
	stm	--sp, r6, lr
	lddpc	r6, 1f		
0:	
	rsub	r6, pc
	rjmp	2f
	.align	2
1:	.long	0b - _GLOBAL_OFFSET_TABLE_		
2:	
				
	.section	".fini"
/* Just load the GOT */
	.align	2
	.global	_fini
_fini:
	stm	--sp, r6, lr
	lddpc	r6, 1f		
0:	
	rsub	r6, pc
	rjmp	2f
	.align	2
1:	.long	0b - _GLOBAL_OFFSET_TABLE_		
2:	

