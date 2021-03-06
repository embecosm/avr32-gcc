/* { dg-do compile { target { { sh-*-* sh[1234ble]*-*-* } && nonpic } } } */
/* { dg-options "-O" } */
extern void foo ();
#pragma interrupt
#pragma nosave_low_regs
void
isr()
{
  foo ();
}

/* { dg-final { scan-assembler-times "rte" 1} } */
/* A call will clobber all call-saved registers, but because of
   #pragma nosave_low_regs, r0..r7 need not be saved/restored.
   One of these registers will also do fine to hold the function address.
   Call-saved registers r8..r13 also don't need to be restored.  */
/* { dg-final { scan-assembler-not "\[^f\]r\[0-9\]\[ \t\]*," } } */
/* { dg-final { scan-assembler-not "\[^f\]r\[89\]" } } */
/* { dg-final { scan-assembler-not "\[^f\]r1\[,0-3\]" } } */
/* { dg-final { scan-assembler-times "macl" 2} } */
