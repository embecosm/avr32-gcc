/*
   Linux/Elf specific definitions.
   Copyright 2003-2006 Atmel Corporation.

   Written by Ronny Pedersen, Atmel Norway, <rpedersen@atmel.com>
   and H�vard Skinnemoen, Atmel Norway, <hskinnemoen@atmel.com>

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



/* elfos.h should have already been included.  Now just override
   any conflicting definitions and add any extras.  */

/* Run-time Target Specification.  */
#undef  TARGET_VERSION
#define TARGET_VERSION  fputs (" (AVR32 GNU/Linux with ELF)", stderr);

/* Do not assume anything about header files.  */
#define NO_IMPLICIT_EXTERN_C

/* The GNU C++ standard library requires that these macros be defined.  */
#undef CPLUSPLUS_CPP_SPEC
#define CPLUSPLUS_CPP_SPEC "-D_GNU_SOURCE %(cpp)"

/* Now we define the strings used to build the spec file.  */
#undef  LIB_SPEC
#define LIB_SPEC \
  "%{pthread:-lpthread} \
   %{shared:-lc} \
   %{!shared:%{profile:-lc_p}%{!profile:-lc}}"

/* Provide a STARTFILE_SPEC appropriate for GNU/Linux.  Here we add
   the GNU/Linux magical crtbegin.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main'.  */

#undef  STARTFILE_SPEC
#define STARTFILE_SPEC \
  "%{!shared: \
     %{pg:gcrt1.o%s} %{!pg:%{p:gcrt1.o%s} \
		       %{!p:%{profile:gcrt1.o%s} \
			 %{!profile:crt1.o%s}}}} \
   crti.o%s %{!shared:crtbegin.o%s} %{shared:crtbeginS.o%s}"

/* Provide a ENDFILE_SPEC appropriate for GNU/Linux.  Here we tack on
   the GNU/Linux magical crtend.o file (see crtstuff.c) which
   provides part of the support for getting C++ file-scope static
   object constructed before entering `main', followed by a normal
   GNU/Linux "finalizer" file, `crtn.o'.  */

#undef  ENDFILE_SPEC
#define ENDFILE_SPEC \
  "%{!shared:crtend.o%s} %{shared:crtendS.o%s} crtn.o%s"

#undef ASM_SPEC
#define ASM_SPEC "%{!mno-pic:%{!fno-pic:--pic}} %{mrelax|O*:%{mno-relax|O0|O1: ;:--linkrelax}} %{mcpu=*:-mcpu=%*}"
 
#undef  LINK_SPEC
#define LINK_SPEC "%{version:-v} \
   %{static:-Bstatic} \
   %{shared:-shared} \
   %{symbolic:-Bsymbolic} \
   %{rdynamic:-export-dynamic} \
   %{!dynamic-linker:-dynamic-linker /lib/ld-uClibc.so.0} \
   %{mrelax|O*:%{mno-relax|O0|O1: ;:--relax}}"

#define TARGET_OS_CPP_BUILTINS() LINUX_TARGET_OS_CPP_BUILTINS()

/* This is how we tell the assembler that two symbols have the same value.  */
#define ASM_OUTPUT_DEF(FILE, NAME1, NAME2) \
  do					   \
    {					   \
      assemble_name (FILE, NAME1); 	   \
      fputs (" = ", FILE);		   \
      assemble_name (FILE, NAME2);	   \
      fputc ('\n', FILE);		   \
    }					   \
  while (0)



#undef  CC1_SPEC
#define CC1_SPEC "%{profile:-p}"

/* Target CPU builtins.  */
#define TARGET_CPU_CPP_BUILTINS()				\
  do								\
    {								\
      builtin_define ("__avr32__");				\
      builtin_define ("__AVR32__");				\
      builtin_define ("__AVR32_LINUX__");			\
      builtin_define (avr32_part->macro);			\
      builtin_define (avr32_arch->macro);			\
      if (avr32_arch->uarch_type == UARCH_TYPE_AVR32A)		\
	builtin_define ("__AVR32_AVR32A__");			\
      else							\
	builtin_define ("__AVR32_AVR32B__");			\
      if (TARGET_UNALIGNED_WORD)				\
	builtin_define ("__AVR32_HAS_UNALIGNED_WORD__");	\
      if (TARGET_SIMD)						\
	builtin_define ("__AVR32_HAS_SIMD__");			\
      if (TARGET_DSP)						\
	builtin_define ("__AVR32_HAS_DSP__");			\
      if (TARGET_RMW)						\
	builtin_define ("__AVR32_HAS_RMW__");			\
      if (TARGET_BRANCH_PRED)					\
	builtin_define ("__AVR32_HAS_BRANCH_PRED__");		\
      if (TARGET_FAST_FLOAT)                                    \
        builtin_define ("__AVR32_FAST_FLOAT__");                \
    }								\
  while (0)



/* Call the function profiler with a given profile label.  */
#undef  FUNCTION_PROFILER
#define FUNCTION_PROFILER(STREAM, LABELNO)				\
  do									\
    {									\
      fprintf (STREAM, "\tmov\tlr, lo(mcount)\n\torh\tlr, hi(mcount)\n"); \
      fprintf (STREAM, "\ticall lr\n");					\
    }									\
  while (0)

#define NO_PROFILE_COUNTERS 1

/* For dynamic libraries to work */
/* #define PLT_REG_CALL_CLOBBERED 1 */
#define AVR32_ALWAYS_PIC 1

/* uclibc does not implement sinf, cosf etc. */
#undef TARGET_C99_FUNCTIONS
#define TARGET_C99_FUNCTIONS 0

#define LINK_GCC_C_SEQUENCE_SPEC \
  "%{static:--start-group} %G %L %{static:--end-group}%{!static:%G}"
