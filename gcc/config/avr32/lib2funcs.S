	.align	4
	.global __nonlocal_goto
	.type  __nonlocal_goto,@function

/* __nonlocal_goto:	This function handles nonlocal_goto's in gcc.

	parameter 0 (r12) = New Frame Pointer
	parameter 1 (r11) = Address to goto
	parameter 2 (r10) = New Stack Pointer

	This function invalidates the return stack, since it returns from a
	function without using a return instruction.
*/
__nonlocal_goto:
	mov	r7, r12
	mov	sp, r10
	frs			# Flush return stack
	mov	pc, r11


		
