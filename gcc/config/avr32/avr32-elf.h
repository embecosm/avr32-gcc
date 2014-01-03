/*
   Elf specific definitions.
   Copyright 2003,2004,2005,2006,2007,2008,2009 Atmel Corporation.

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


/*****************************************************************************
 * Controlling the Compiler Driver, 'gcc'
 *****************************************************************************/

/* Run-time Target Specification.  */
#undef  TARGET_VERSION
#define TARGET_VERSION  fputs (" (AVR32 GNU with ELF)", stderr);

/*
Another C string constant used much like LINK_SPEC.  The
difference between the two is that STARTFILE_SPEC is used at
the very beginning of the command given to the linker.

If this macro is not defined, a default is provided that loads the
standard C startup file from the usual place.  See gcc.c.
*/
#if 0
#undef  STARTFILE_SPEC
#define STARTFILE_SPEC "crt0%O%s crti%O%s crtbegin%O%s"
#endif
#undef  STARTFILE_SPEC 
#define STARTFILE_SPEC "%{mflashvault: crtfv.o%s} %{!mflashvault: crt0.o%s} \
			crti.o%s crtbegin.o%s"

#undef LINK_SPEC
#define LINK_SPEC "%{muse-oscall:--defsym __do_not_use_oscall_coproc__=0} %{mrelax|O*:%{mno-relax|O0|O1: ;:--relax}} %{mpart=uc3a3revd:-mavr32elf_uc3a3256s;:%{mpart=*:-mavr32elf_%*}} %{mcpu=*:-mavr32elf_%*}"


/*
Another C string constant used much like LINK_SPEC.  The
difference between the two is that ENDFILE_SPEC is used at
the very end of the command given to the linker.

Do not define this macro if it does not need to do anything.
*/
#undef  ENDFILE_SPEC
#define ENDFILE_SPEC "crtend%O%s crtn%O%s"


/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()								\
  do															\
    {															\
      builtin_define ("__avr32__");								\
      builtin_define ("__AVR32__");								\
      builtin_define ("__AVR32_ELF__");							\
      builtin_define (avr32_part->macro);						\
      builtin_define (avr32_arch->macro);						\
      if (avr32_arch->uarch_type == UARCH_TYPE_AVR32A)			\
        builtin_define ("__AVR32_AVR32A__");					\
      else														\
        builtin_define ("__AVR32_AVR32B__");					\
      if (TARGET_UNALIGNED_WORD)								\
        builtin_define ("__AVR32_HAS_UNALIGNED_WORD__");		\
      if (TARGET_SIMD)											\
        builtin_define ("__AVR32_HAS_SIMD__");					\
      if (TARGET_DSP)											\
        builtin_define ("__AVR32_HAS_DSP__");					\
      if (TARGET_RMW)											\
        builtin_define ("__AVR32_HAS_RMW__");					\
      if (TARGET_BRANCH_PRED)									\
        builtin_define ("__AVR32_HAS_BRANCH_PRED__");			\
      if (TARGET_FAST_FLOAT)                                    \
        builtin_define ("__AVR32_FAST_FLOAT__");                \
      if (TARGET_FLASHVAULT)                                    \
        builtin_define ("__AVR32_FLASHVAULT__");                \
      if (TARGET_NO_MUL_INSNS)                                  \
        builtin_define ("__AVR32_NO_MUL__");                    \
    }															\
  while (0)
