/*
   Definitions of target machine for AVR32.
   Copyright 2003,2004,2005,2006,2007,2008,2009,2010 Atmel Corporation.

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

#ifndef GCC_AVR32_H
#define GCC_AVR32_H


#ifndef OBJECT_FORMAT_ELF
#error avr32.h included before elfos.h
#endif

#ifndef LOCAL_LABEL_PREFIX
#define LOCAL_LABEL_PREFIX "."
#endif

#ifndef SUBTARGET_CPP_SPEC
#define SUBTARGET_CPP_SPEC  "-D__ELF__"
#endif


extern struct rtx_def *avr32_compare_op0;
extern struct rtx_def *avr32_compare_op1;

/* comparison type */
enum avr32_cmp_type {
  CMP_QI,				/* 1 byte ->char */
  CMP_HI,				/* 2 byte->half word */
  CMP_SI,				/* four byte->word*/
  CMP_DI,				/* eight byte->double word */
  CMP_SF,				/* single precision floats */
  CMP_MAX				/* max comparison type */
};

extern enum avr32_cmp_type avr32_branch_type;	/* type of branch to use */


extern struct rtx_def *avr32_acc_cache;

/* cache instruction op5 codes */
#define AVR32_CACHE_INVALIDATE_ICACHE 1

/* 
These bits describe the different types of function supported by the AVR32
backend. They are exclusive, e.g. a function cannot be both a normal function
and an interworked function.  Knowing the type of a function is important for
determining its prologue and epilogue sequences. Note value 7 is currently 
unassigned.  Also note that the interrupt function types all have bit 2 set, 
so that they can be tested for easily. Note that 0 is deliberately chosen for
AVR32_FT_UNKNOWN so that when the machine_function structure is initialized
(to zero) func_type will default to unknown. This will force the first use of
avr32_current_func_type to call avr32_compute_func_type. 
*/
#define AVR32_FT_UNKNOWN           0  /* Type has not yet been determined. */
#define AVR32_FT_NORMAL            1  /* Normal function. */
#define AVR32_FT_ACALL             2  /* An acall function. */
#define AVR32_FT_EXCEPTION_HANDLER 3  /* A C++ exception handler. */
#define AVR32_FT_ISR_FULL          4  /* A fully shadowed interrupt mode. */
#define AVR32_FT_ISR_HALF          5  /* A half shadowed interrupt mode. */
#define AVR32_FT_ISR_NONE          6  /* No shadow registers. */

#define AVR32_FT_TYPE_MASK	((1 << 3) - 1)

/* In addition functions can have several type modifiers, outlined by these bit masks: */
#define AVR32_FT_INTERRUPT       (1 << 2)  /* Note overlap with FT_ISR and above. */
#define AVR32_FT_NAKED           (1 << 3)  /* No prologue or epilogue. */
#define AVR32_FT_VOLATILE        (1 << 4)  /* Does not return. */
#define AVR32_FT_NESTED          (1 << 5)  /* Embedded inside another func. */
#define AVR32_FT_FLASHVAULT      (1 << 6)  /* Flashvault function call. */
#define AVR32_FT_FLASHVAULT_IMPL (1 << 7)  /* Function definition in FlashVault. */


/* Some macros to test these flags.  */
#define AVR32_FUNC_TYPE(t)     (t & AVR32_FT_TYPE_MASK)
#define IS_INTERRUPT(t)        (t & AVR32_FT_INTERRUPT)
#define IS_NAKED(t)            (t & AVR32_FT_NAKED)
#define IS_VOLATILE(t)         (t & AVR32_FT_VOLATILE)
#define IS_NESTED(t)           (t & AVR32_FT_NESTED)
#define IS_FLASHVAULT(t)       (t & AVR32_FT_FLASHVAULT)
#define IS_FLASHVAULT_IMPL(t)  (t & AVR32_FT_FLASHVAULT_IMPL)

#define SYMBOL_FLAG_RMW_ADDR_SHIFT    SYMBOL_FLAG_MACH_DEP_SHIFT
#define SYMBOL_REF_RMW_ADDR(RTX)                                        \
  ((SYMBOL_REF_FLAGS (RTX) & (1 << SYMBOL_FLAG_RMW_ADDR_SHIFT)) != 0)


typedef struct minipool_labels
GTY ((chain_next ("%h.next"), chain_prev ("%h.prev")))
{
  rtx label;
  struct minipool_labels *prev;
  struct minipool_labels *next;
} minipool_labels;

/* A C structure for machine-specific, per-function data.
   This is added to the cfun structure.  */

typedef struct machine_function
GTY (())
{
  /* Records the type of the current function.  */
  unsigned long func_type;
  /* List of minipool labels, use for checking if code label is valid in a
     memory expression */
  minipool_labels *minipool_label_head;
  minipool_labels *minipool_label_tail;
  int ifcvt_after_reload;
} machine_function;

/* Initialize data used by insn expanders.  This is called from insn_emit,
   once for every function before code is generated.  */
#define INIT_EXPANDERS avr32_init_expanders ()

/******************************************************************************
 * SPECS
 *****************************************************************************/

#ifndef ASM_SPEC
#define ASM_SPEC "%{fpic:--pic} %{mrelax|O*:%{mno-relax|O0|O1: ;:--linkrelax}} %{march=ucr2nomul:-march=ucr2;:%{march=*:-march=%*}} %{mpart=uc3a3revd:-mpart=uc3a3256s;:%{mpart=*:-mpart=%*}}"
#endif

#ifndef MULTILIB_DEFAULTS
#define MULTILIB_DEFAULTS { "march=ap", "" }
#endif

/******************************************************************************
 * Run-time Target Specification
 *****************************************************************************/
#ifndef TARGET_VERSION
#define TARGET_VERSION fprintf(stderr, " (AVR32, GNU assembler syntax)");
#endif


/* Part types. Keep this in sync with the order of avr32_part_types in avr32.c*/
enum part_type
{
  PART_TYPE_AVR32_NONE,
  PART_TYPE_AVR32_AP7000,
  PART_TYPE_AVR32_AP7001,
  PART_TYPE_AVR32_AP7002,
  PART_TYPE_AVR32_AP7200,
  PART_TYPE_AVR32_UC3A0128,
  PART_TYPE_AVR32_UC3A0256,
  PART_TYPE_AVR32_UC3A0512,
  PART_TYPE_AVR32_UC3A0512ES,
  PART_TYPE_AVR32_UC3A1128,
  PART_TYPE_AVR32_UC3A1256,
  PART_TYPE_AVR32_UC3A1512,
  PART_TYPE_AVR32_UC3A1512ES,
  PART_TYPE_AVR32_UC3A3REVD,
  PART_TYPE_AVR32_UC3A364,
  PART_TYPE_AVR32_UC3A364S,
  PART_TYPE_AVR32_UC3A3128,
  PART_TYPE_AVR32_UC3A3128S,
  PART_TYPE_AVR32_UC3A3256,
  PART_TYPE_AVR32_UC3A3256S,
  PART_TYPE_AVR32_UC3A464,
  PART_TYPE_AVR32_UC3A464S,
  PART_TYPE_AVR32_UC3A4128,
  PART_TYPE_AVR32_UC3A4128S,
  PART_TYPE_AVR32_UC3A4256,
  PART_TYPE_AVR32_UC3A4256S,
  PART_TYPE_AVR32_UC3B064,
  PART_TYPE_AVR32_UC3B0128,
  PART_TYPE_AVR32_UC3B0256,
  PART_TYPE_AVR32_UC3B0256ES,
  PART_TYPE_AVR32_UC3B0512,
  PART_TYPE_AVR32_UC3B0512REVC,
  PART_TYPE_AVR32_UC3B164,
  PART_TYPE_AVR32_UC3B1128,
  PART_TYPE_AVR32_UC3B1256,
  PART_TYPE_AVR32_UC3B1256ES,
  PART_TYPE_AVR32_UC3B1512,
  PART_TYPE_AVR32_UC3B1512REVC,
  PART_TYPE_AVR32_UC64D3,
  PART_TYPE_AVR32_UC128D3,
  PART_TYPE_AVR32_UC64D4,
  PART_TYPE_AVR32_UC128D4,
  PART_TYPE_AVR32_UC3C0512CREVC,
  PART_TYPE_AVR32_UC3C1512CREVC,
  PART_TYPE_AVR32_UC3C2512CREVC,
  PART_TYPE_AVR32_UC3L0256,
  PART_TYPE_AVR32_UC3L0128,
  PART_TYPE_AVR32_UC3L064,
  PART_TYPE_AVR32_UC3L032,
  PART_TYPE_AVR32_UC3L016,
  PART_TYPE_AVR32_UC3L064REVB,
  PART_TYPE_AVR32_UC64L3U,
  PART_TYPE_AVR32_UC128L3U,
  PART_TYPE_AVR32_UC256L3U,
  PART_TYPE_AVR32_UC64L4U,
  PART_TYPE_AVR32_UC128L4U,
  PART_TYPE_AVR32_UC256L4U,
  PART_TYPE_AVR32_UC3C064C,
  PART_TYPE_AVR32_UC3C0128C,
  PART_TYPE_AVR32_UC3C0256C,
  PART_TYPE_AVR32_UC3C0512C,
  PART_TYPE_AVR32_UC3C164C,
  PART_TYPE_AVR32_UC3C1128C,
  PART_TYPE_AVR32_UC3C1256C,
  PART_TYPE_AVR32_UC3C1512C,
  PART_TYPE_AVR32_UC3C264C,
  PART_TYPE_AVR32_UC3C2128C,
  PART_TYPE_AVR32_UC3C2256C,
  PART_TYPE_AVR32_UC3C2512C,
  PART_TYPE_AVR32_MXT768E
};

/* Microarchitectures. */
enum microarchitecture_type
{
  UARCH_TYPE_AVR32A,
  UARCH_TYPE_AVR32B,
  UARCH_TYPE_NONE
};

/* Architectures types which specifies the pipeline.
 Keep this in sync with avr32_arch_types in avr32.c
 and the pipeline attribute in avr32.md */
enum architecture_type
{
  ARCH_TYPE_AVR32_AP,
  ARCH_TYPE_AVR32_UCR1,
  ARCH_TYPE_AVR32_UCR2,
  ARCH_TYPE_AVR32_UCR2NOMUL,
  ARCH_TYPE_AVR32_UCR3,
  ARCH_TYPE_AVR32_UCR3FP,
  ARCH_TYPE_AVR32_NONE
};

/* Flag specifying if the cpu has support for DSP instructions.*/
#define FLAG_AVR32_HAS_DSP (1 << 0)
/* Flag specifying if the cpu has support for Read-Modify-Write
   instructions.*/
#define FLAG_AVR32_HAS_RMW (1 << 1)
/* Flag specifying if the cpu has support for SIMD instructions. */
#define FLAG_AVR32_HAS_SIMD (1 << 2)
/* Flag specifying if the cpu has support for unaligned memory word access. */
#define FLAG_AVR32_HAS_UNALIGNED_WORD (1 << 3)
/* Flag specifying if the cpu has support for branch prediction. */
#define FLAG_AVR32_HAS_BRANCH_PRED (1 << 4)
/* Flag specifying if the cpu has support for a return stack. */
#define FLAG_AVR32_HAS_RETURN_STACK (1 << 5)
/* Flag specifying if the cpu has caches. */
#define FLAG_AVR32_HAS_CACHES (1 << 6)
/* Flag specifying if the cpu has support for v2 insns. */
#define FLAG_AVR32_HAS_V2_INSNS (1 << 7)
/* Flag specifying that the cpu has buggy mul insns. */
#define FLAG_AVR32_HAS_NO_MUL_INSNS (1 << 8)
/* Flag specifying that the device has FPU instructions according 
   to AVR32002 specifications*/
#define FLAG_AVR32_HAS_FPU (1 << 9)

/* Structure for holding information about different avr32 CPUs/parts */
struct part_type_s
{
  const char *const name;
  enum part_type part_type;
  enum architecture_type arch_type;
  /* Must lie outside user's namespace.  NULL == no macro.  */
  const char *const macro;
};

/* Structure for holding information about different avr32 pipeline
 architectures. */
struct arch_type_s
{
  const char *const name;
  enum architecture_type arch_type;
  enum microarchitecture_type uarch_type;
  const unsigned long feature_flags;
  /* Must lie outside user's namespace.  NULL == no macro.  */
  const char *const macro;
};

extern const struct part_type_s *avr32_part;
extern const struct arch_type_s *avr32_arch;
extern int avr32_emit_fpu_insns;

#define TARGET_SIMD  (avr32_arch->feature_flags & FLAG_AVR32_HAS_SIMD)
#define TARGET_DSP  (avr32_arch->feature_flags & FLAG_AVR32_HAS_DSP)
#define TARGET_RMW  (avr32_arch->feature_flags & FLAG_AVR32_HAS_RMW)
#define TARGET_UNALIGNED_WORD  (avr32_arch->feature_flags & FLAG_AVR32_HAS_UNALIGNED_WORD)
#define TARGET_BRANCH_PRED  (avr32_arch->feature_flags & FLAG_AVR32_HAS_BRANCH_PRED)
#define TARGET_RETURN_STACK  (avr32_arch->feature_flags & FLAG_AVR32_HAS_RETURN_STACK)
#define TARGET_V2_INSNS  (avr32_arch->feature_flags & FLAG_AVR32_HAS_V2_INSNS)
#define TARGET_CACHES  (avr32_arch->feature_flags & FLAG_AVR32_HAS_CACHES)
#define TARGET_NO_MUL_INSNS  (avr32_arch->feature_flags & FLAG_AVR32_HAS_NO_MUL_INSNS)
#define TARGET_ARCH_AP (avr32_arch->arch_type == ARCH_TYPE_AVR32_AP)
#define TARGET_ARCH_UCR1 (avr32_arch->arch_type == ARCH_TYPE_AVR32_UCR1)
#define TARGET_ARCH_UCR2 (avr32_arch->arch_type == ARCH_TYPE_AVR32_UCR2)
#define TARGET_ARCH_UC (TARGET_ARCH_UCR1 || TARGET_ARCH_UCR2)
#define TARGET_UARCH_AVR32A (avr32_arch->uarch_type == UARCH_TYPE_AVR32A)
#define TARGET_UARCH_AVR32B (avr32_arch->uarch_type == UARCH_TYPE_AVR32B)
#define TARGET_ARCH_FPU (avr32_arch->feature_flags & FLAG_AVR32_HAS_FPU)
#define TARGET_EMIT_FPU_INSNS (avr32_emit_fpu_insns)

#define CAN_DEBUG_WITHOUT_FP




/******************************************************************************
 * Storage Layout
 *****************************************************************************/

/*
Define this macro to have the value 1 if the most significant bit in a
byte has the lowest number; otherwise define it to have the value zero.
This means that bit-field instructions count from the most significant
bit.  If the machine has no bit-field instructions, then this must still
be defined, but it doesn't matter which value it is defined to.  This
macro need not be a constant.

This macro does not affect the way structure fields are packed into
bytes or words; that is controlled by BYTES_BIG_ENDIAN.
*/
#define BITS_BIG_ENDIAN 0

/*
Define this macro to have the value 1 if the most significant byte in a
word has the lowest number. This macro need not be a constant.
*/
/*
  Data is stored in an big-endian way.
*/
#define BYTES_BIG_ENDIAN 1

/*
Define this macro to have the value 1 if, in a multiword object, the
most significant word has the lowest number.  This applies to both
memory locations and registers; GCC fundamentally assumes that the
order of words in memory is the same as the order in registers.  This
macro need not be a constant.
*/
/*
  Data is stored in an bin-endian way.
*/
#define WORDS_BIG_ENDIAN 1

/*
Define this macro if WORDS_BIG_ENDIAN is not constant.  This must be a
constant value with the same meaning as WORDS_BIG_ENDIAN, which will be
used only when compiling libgcc2.c.  Typically the value will be set
based on preprocessor defines.
*/
#define LIBGCC2_WORDS_BIG_ENDIAN WORDS_BIG_ENDIAN

/*
Define this macro to have the value 1 if DFmode, XFmode or
TFmode floating point numbers are stored in memory with the word
containing the sign bit at the lowest address; otherwise define it to
have the value 0.  This macro need not be a constant.

You need not define this macro if the ordering is the same as for
multi-word integers.
*/
/* #define FLOAT_WORDS_BIG_ENDIAN 1 */

/*
Define this macro to be the number of bits in an addressable storage
unit (byte); normally 8.
*/
#define BITS_PER_UNIT 8

/*
Number of bits in a word; normally 32.
*/
#define BITS_PER_WORD 32

/*
Maximum number of bits in a word.  If this is undefined, the default is
BITS_PER_WORD.  Otherwise, it is the constant value that is the
largest value that BITS_PER_WORD can have at run-time.
*/
/* MAX_BITS_PER_WORD not defined*/

/*
Number of storage units in a word; normally 4.
*/
#define UNITS_PER_WORD 4

/*
Minimum number of units in a word.  If this is undefined, the default is
UNITS_PER_WORD.  Otherwise, it is the constant value that is the
smallest value that UNITS_PER_WORD can have at run-time.
*/
/* MIN_UNITS_PER_WORD not defined */

/*
Width of a pointer, in bits.  You must specify a value no wider than the
width of Pmode.  If it is not equal to the width of Pmode,
you must define POINTERS_EXTEND_UNSIGNED.
*/
#define POINTER_SIZE 32

/*
A C expression whose value is greater than zero if pointers that need to be
extended from being POINTER_SIZE bits wide to Pmode are to
be zero-extended and zero if they are to be sign-extended.  If the value
is less then zero then there must be an "ptr_extend" instruction that
extends a pointer from POINTER_SIZE to Pmode.

You need not define this macro if the POINTER_SIZE is equal
to the width of Pmode.
*/
/* #define POINTERS_EXTEND_UNSIGNED */

/*
A Macro to update M and UNSIGNEDP when an object whose type
is TYPE and which has the specified mode and signedness is to be
stored in a register.  This macro is only called when TYPE is a
scalar type.

On most RISC machines, which only have operations that operate on a full
register, define this macro to set M to word_mode if
M is an integer mode narrower than BITS_PER_WORD.  In most
cases, only integer modes should be widened because wider-precision
floating-point operations are usually more expensive than their narrower
counterparts.

For most machines, the macro definition does not change UNSIGNEDP.
However, some machines, have instructions that preferentially handle
either signed or unsigned quantities of certain modes.  For example, on
the DEC Alpha, 32-bit loads from memory and 32-bit add instructions
sign-extend the result to 64 bits.  On such machines, set
UNSIGNEDP according to which kind of extension is more efficient.

Do not define this macro if it would never modify M.
*/
#define PROMOTE_MODE(M, UNSIGNEDP, TYPE)                                \
  {                                                                     \
    if (!AGGREGATE_TYPE_P (TYPE)                                        \
        && GET_MODE_CLASS (mode) == MODE_INT                            \
        && GET_MODE_SIZE (mode) < 4)                                    \
      {                                                                 \
        if (M == QImode)                                                \
          (UNSIGNEDP) = 1;                                              \
        else if (M == HImode)                                           \
          (UNSIGNEDP) = 0;                                              \
        (M) = SImode;                                                   \
      }                                                                 \
  }

#define PROMOTE_FUNCTION_MODE(M, UNSIGNEDP, TYPE)  \
        PROMOTE_MODE(M, UNSIGNEDP, TYPE)

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */
#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, UNKNOWN if not known.  */
#define LOAD_EXTEND_OP(MODE)				\
   (((MODE) == QImode) ? ZERO_EXTEND			\
   : ((MODE) == HImode) ? SIGN_EXTEND : UNKNOWN)


/*
Normal alignment required for function parameters on the stack, in
bits.  All stack parameters receive at least this much alignment
regardless of data type.  On most machines, this is the same as the
size of an integer.
*/
#define PARM_BOUNDARY 32

/*
Define this macro to the minimum alignment enforced by hardware for the
stack pointer on this machine.  The definition is a C expression for the
desired alignment (measured in bits).  This value is used as a default
if PREFERRED_STACK_BOUNDARY is not defined.  On most machines,
this should be the same as PARM_BOUNDARY.
*/
#define STACK_BOUNDARY 32

/*
Define this macro if you wish to preserve a certain alignment for the
stack pointer, greater than what the hardware enforces.  The definition
is a C expression for the desired alignment (measured in bits).  This
macro must evaluate to a value equal to or larger than
STACK_BOUNDARY.
*/
#define PREFERRED_STACK_BOUNDARY (TARGET_FORCE_DOUBLE_ALIGN ? 64 : 32 )

/*
Alignment required for a function entry point, in bits.
*/
#define FUNCTION_BOUNDARY 16

/*
Biggest alignment that any data type can require on this machine, in bits.
*/
#define BIGGEST_ALIGNMENT  (TARGET_FORCE_DOUBLE_ALIGN ? 64 : 32 )

/*
If defined, the smallest alignment, in bits, that can be given to an
object that can be referenced in one operation, without disturbing any
nearby object.  Normally, this is BITS_PER_UNIT, but may be larger
on machines that don't have byte or half-word store operations.
*/
#define MINIMUM_ATOMIC_ALIGNMENT BITS_PER_UNIT


/*
An integer expression for the size in bits of the largest integer machine mode that
should actually be used. All integer machine modes of this size or smaller can be
used for structures and unions with the appropriate sizes. If this macro is undefined,
GET_MODE_BITSIZE (DImode) is assumed.*/
#define MAX_FIXED_MODE_SIZE  GET_MODE_BITSIZE (DImode)


/*
If defined, a C expression to compute the alignment given to a constant
that is being placed in memory.  CONSTANT is the constant and
BASIC_ALIGN is the alignment that the object would ordinarily
have.  The value of this macro is used instead of that alignment to
align the object.

If this macro is not defined, then BASIC_ALIGN is used.

The typical use of this macro is to increase alignment for string
constants to be word aligned so that strcpy calls that copy
constants can be done inline.
*/
#define CONSTANT_ALIGNMENT(CONSTANT, BASIC_ALIGN) \
 ((TREE_CODE(CONSTANT) == STRING_CST) ? BITS_PER_WORD : BASIC_ALIGN)

/* Try to align string to a word. */
#define DATA_ALIGNMENT(TYPE, ALIGN)                                     \
  ({(TREE_CODE (TYPE) == ARRAY_TYPE                                     \
     && TYPE_MODE (TREE_TYPE (TYPE)) == QImode                          \
     && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN));})

/* Try to align local store strings to a word. */
#define LOCAL_ALIGNMENT(TYPE, ALIGN)                                    \
  ({(TREE_CODE (TYPE) == ARRAY_TYPE                                     \
     && TYPE_MODE (TREE_TYPE (TYPE)) == QImode                          \
     && (ALIGN) < BITS_PER_WORD ? BITS_PER_WORD : (ALIGN));})

/*
Define this macro to be the value 1 if instructions will fail to work
if given data not on the nominal alignment.  If instructions will merely
go slower in that case, define this macro as 0.
*/
#define STRICT_ALIGNMENT 1

/*
Define this if you wish to imitate the way many other C compilers handle
alignment of bit-fields and the structures that contain them.

The behavior is that the type written for a bit-field (int,
short, or other integer type) imposes an alignment for the
entire structure, as if the structure really did contain an ordinary
field of that type.  In addition, the bit-field is placed within the
structure so that it would fit within such a field, not crossing a
boundary for it.

Thus, on most machines, a bit-field whose type is written as int
would not cross a four-byte boundary, and would force four-byte
alignment for the whole structure.  (The alignment used may not be four
bytes; it is controlled by the other alignment parameters.)

If the macro is defined, its definition should be a C expression;
a nonzero value for the expression enables this behavior.

Note that if this macro is not defined, or its value is zero, some
bit-fields may cross more than one alignment boundary.  The compiler can
support such references if there are insv, extv, and
extzv insns that can directly reference memory.

The other known way of making bit-fields work is to define
STRUCTURE_SIZE_BOUNDARY as large as BIGGEST_ALIGNMENT.
Then every structure can be accessed with fullwords.

Unless the machine has bit-field instructions or you define
STRUCTURE_SIZE_BOUNDARY that way, you must define
PCC_BITFIELD_TYPE_MATTERS to have a nonzero value.

If your aim is to make GCC use the same conventions for laying out
bit-fields as are used by another compiler, here is how to investigate
what the other compiler does.  Compile and run this program:

struct foo1
{
  char x;
  char :0;
  char y;
};

struct foo2
{
  char x;
  int :0;
  char y;
};

main ()
{
  printf ("Size of foo1 is %d\n",
          sizeof (struct foo1));
  printf ("Size of foo2 is %d\n",
          sizeof (struct foo2));
  exit (0);
}

If this prints 2 and 5, then the compiler's behavior is what you would
get from PCC_BITFIELD_TYPE_MATTERS.
*/
#define PCC_BITFIELD_TYPE_MATTERS 1


/******************************************************************************
 * Layout of Source Language Data Types
 *****************************************************************************/

/*
A C expression for the size in bits of the type int on the
target machine.  If you don't define this, the default is one word.
*/
#define INT_TYPE_SIZE 32

/*
A C expression for the size in bits of the type short on the
target machine.  If you don't define this, the default is half a word. (If
this would be less than one storage unit, it is rounded up to one unit.)
*/
#define SHORT_TYPE_SIZE 16

/*
A C expression for the size in bits of the type long on the
target machine.  If you don't define this, the default is one word.
*/
#define LONG_TYPE_SIZE 32


/*
A C expression for the size in bits of the type long long on the
target machine.  If you don't define this, the default is two
words.  If you want to support GNU Ada on your machine, the value of this
macro must be at least 64.
*/
#define LONG_LONG_TYPE_SIZE 64

/*
A C expression for the size in bits of the type char on the
target machine.  If you don't define this, the default is
BITS_PER_UNIT.
*/
#define CHAR_TYPE_SIZE 8


/*
A C expression for the size in bits of the C++ type bool and
C99 type _Bool on the target machine.  If you don't define
this, and you probably shouldn't, the default is CHAR_TYPE_SIZE.
*/
#define BOOL_TYPE_SIZE 8


/*
An expression whose value is 1 or 0, according to whether the type
char should be signed or unsigned by default.  The user can
always override this default with the options -fsigned-char
and -funsigned-char.
*/
/* We are using unsigned char */
#define DEFAULT_SIGNED_CHAR 0


/*
A C expression for a string describing the name of the data type to use
for size values.  The typedef name size_t is defined using the
contents of the string.

The string can contain more than one keyword.  If so, separate them with
spaces, and write first any length keyword, then unsigned if
appropriate, and finally int.  The string must exactly match one
of the data type names defined in the function
init_decl_processing in the file c-decl.c.  You may not
omit int or change the order - that would cause the compiler to
crash on startup.

If you don't define this macro, the default is "long unsigned int".
*/
#define SIZE_TYPE "long unsigned int"

/*
A C expression for a string describing the name of the data type to use
for the result of subtracting two pointers.  The typedef name
ptrdiff_t is defined using the contents of the string.  See
SIZE_TYPE above for more information.

If you don't define this macro, the default is "long int".
*/
#define PTRDIFF_TYPE "long int"


/*
A C expression for the size in bits of the data type for wide
characters.  This is used in cpp, which cannot make use of
WCHAR_TYPE.
*/
#define WCHAR_TYPE_SIZE 32


/*
A C expression for a string describing the name of the data type to
use for wide characters passed to printf and returned from
getwc.  The typedef name wint_t is defined using the
contents of the string.  See SIZE_TYPE above for more
information.

If you don't define this macro, the default is "unsigned int".
*/
#define WINT_TYPE "unsigned int"

/*
A C expression for a string describing the name of the data type that
can represent any value of any standard or extended signed integer type.
The typedef name intmax_t is defined using the contents of the
string.  See SIZE_TYPE above for more information.

If you don't define this macro, the default is the first of
"int", "long int", or "long long int" that has as
much precision as long long int.
*/
#define INTMAX_TYPE "long long int"

/*
A C expression for a string describing the name of the data type that
can represent any value of any standard or extended unsigned integer
type.  The typedef name uintmax_t is defined using the contents
of the string.  See SIZE_TYPE above for more information.

If you don't define this macro, the default is the first of
"unsigned int", "long unsigned int", or "long long unsigned int"
that has as much precision as long long unsigned int.
*/
#define UINTMAX_TYPE "long long unsigned int"


/******************************************************************************
 * Register Usage
 *****************************************************************************/

/* Convert from gcc internal register number to register number
   used in assembly code */
#define ASM_REGNUM(reg) (LAST_REGNUM - (reg))

/* Convert between register number used in assembly to gcc
   internal register number  */
#define INTERNAL_REGNUM(reg) (LAST_REGNUM - (reg))

/** Basic Characteristics of Registers **/

/*
Number of hardware registers known to the compiler.  They receive
numbers 0 through FIRST_PSEUDO_REGISTER-1; thus, the first
pseudo register's number really is assigned the number
FIRST_PSEUDO_REGISTER.
*/
#define FIRST_PSEUDO_REGISTER (LAST_REGNUM + 1)

#define FIRST_REGNUM 0
#define LAST_REGNUM 15

/*
An initializer that says which registers are used for fixed purposes
all throughout the compiled code and are therefore not available for
general allocation.  These would include the stack pointer, the frame
pointer (except on machines where that can be used as a general
register when no frame pointer is needed), the program counter on
machines where that is considered one of the addressable registers,
and any other numbered register with a standard use.

This information is expressed as a sequence of numbers, separated by
commas and surrounded by braces.  The nth number is 1 if
register n is fixed, 0 otherwise.

The table initialized from this macro, and the table initialized by
the following one, may be overridden at run time either automatically,
by the actions of the macro CONDITIONAL_REGISTER_USAGE, or by
the user with the command options -ffixed-[reg],
-fcall-used-[reg] and -fcall-saved-[reg].
*/

/* The internal gcc register numbers are reversed
   compared to the real register numbers since
   gcc expects data types stored over multiple
   registers in the register file to be big endian
   if the memory layout is big endian. But this
   is not the case for avr32 so we fake a big
   endian register file. */

#define FIXED_REGISTERS {	\
  1, /* Program Counter */	\
  0, /* Link Register */	\
  1, /* Stack Pointer */	\
  0, /* r12 */			\
  0, /* r11 */			\
  0, /* r10 */			\
  0, /* r9 */			\
  0, /* r8 */			\
  0, /* r7 */			\
  0, /* r6 */			\
  0, /* r5 */			\
  0, /* r4 */			\
  0, /* r3 */			\
  0, /* r2 */			\
  0, /* r1 */			\
  0, /* r0 */			\
}

/*
Like FIXED_REGISTERS but has 1 for each register that is
clobbered (in general) by function calls as well as for fixed
registers.  This macro therefore identifies the registers that are not
available for general allocation of values that must live across
function calls.

If a register has 0 in CALL_USED_REGISTERS, the compiler
automatically saves it on function entry and restores it on function
exit, if the register is used within the function.
*/
#define CALL_USED_REGISTERS {	\
  1, /* Program Counter */	\
  0, /* Link Register */	\
  1, /* Stack Pointer */	\
  1, /* r12 */			\
  1, /* r11 */			\
  1, /* r10 */			\
  1, /* r9 */			\
  1, /* r8 */			\
  0, /* r7 */			\
  0, /* r6 */			\
  0, /* r5 */			\
  0, /* r4 */			\
  0, /* r3 */			\
  0, /* r2 */			\
  0, /* r1 */			\
  0, /* r0 */			\
}

/* Interrupt functions can only use registers that have already been
   saved by the prologue, even if they would normally be
   call-clobbered.  */
#define HARD_REGNO_RENAME_OK(SRC, DST)					\
	(! IS_INTERRUPT (cfun->machine->func_type) ||			\
         df_regs_ever_live_p (DST))


/*
Zero or more C statements that may conditionally modify five variables
fixed_regs, call_used_regs, global_regs,
reg_names, and reg_class_contents, to take into account
any dependence of these register sets on target flags.  The first three
of these are of type char [] (interpreted as Boolean vectors).
global_regs is a const char *[], and
reg_class_contents is a HARD_REG_SET.  Before the macro is
called, fixed_regs, call_used_regs,
reg_class_contents, and reg_names have been initialized
from FIXED_REGISTERS, CALL_USED_REGISTERS,
REG_CLASS_CONTENTS, and REGISTER_NAMES, respectively.
global_regs has been cleared, and any -ffixed-[reg],
-fcall-used-[reg] and -fcall-saved-[reg]
command options have been applied.

You need not define this macro if it has no work to do.

If the usage of an entire class of registers depends on the target
flags, you may indicate this to GCC by using this macro to modify
fixed_regs and call_used_regs to 1 for each of the
registers in the classes which should not be used by GCC.  Also define
the macro REG_CLASS_FROM_LETTER to return NO_REGS if it
is called with a letter for a class that shouldn't be used.

 (However, if this class is not included in GENERAL_REGS and all
of the insn patterns whose constraints permit this class are
controlled by target switches, then GCC will automatically avoid using
these registers when the target switches are opposed to them.)
*/
#define CONDITIONAL_REGISTER_USAGE                              \
  do								\
    {								\
      if (flag_pic)						\
	{							\
	  fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;		\
	  call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;		\
	}							\
    }								\
  while (0)


/*
If the program counter has a register number, define this as that
register number.  Otherwise, do not define it.
*/

#define LAST_AVR32_REGNUM 16


/** Order of Allocation of Registers **/

/*
If defined, an initializer for a vector of integers, containing the
numbers of hard registers in the order in which GCC should prefer
to use them (from most preferred to least).

If this macro is not defined, registers are used lowest numbered first
(all else being equal).

One use of this macro is on machines where the highest numbered
registers must always be saved and the save-multiple-registers
instruction supports only sequences of consecutive registers.  On such
machines, define REG_ALLOC_ORDER to be an initializer that lists
the highest numbered allocable register first.
*/
#define REG_ALLOC_ORDER 	\
{				\
  INTERNAL_REGNUM(8),		\
  INTERNAL_REGNUM(9),		\
  INTERNAL_REGNUM(10),		\
  INTERNAL_REGNUM(11),		\
  INTERNAL_REGNUM(12),		\
  LR_REGNUM,			\
  INTERNAL_REGNUM(7),		\
  INTERNAL_REGNUM(6),		\
  INTERNAL_REGNUM(5),		\
  INTERNAL_REGNUM(4),		\
  INTERNAL_REGNUM(3),		\
  INTERNAL_REGNUM(2),		\
  INTERNAL_REGNUM(1),		\
  INTERNAL_REGNUM(0),		\
  SP_REGNUM,           		\
  PC_REGNUM			\
}


/** How Values Fit in Registers **/

/*
A C expression for the number of consecutive hard registers, starting
at register number REGNO, required to hold a value of mode
MODE.

On a machine where all registers are exactly one word, a suitable
definition of this macro is

#define HARD_REGNO_NREGS(REGNO, MODE)            \
   ((GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1)  \
    / UNITS_PER_WORD)
*/
#define HARD_REGNO_NREGS(REGNO, MODE) \
  ((unsigned int)((GET_MODE_SIZE(MODE) + UNITS_PER_WORD -1 ) / UNITS_PER_WORD))

/*
A C expression that is nonzero if it is permissible to store a value
of mode MODE in hard register number REGNO (or in several
registers starting with that one).  For a machine where all registers
are equivalent, a suitable definition is

  #define HARD_REGNO_MODE_OK(REGNO, MODE) 1

You need not include code to check for the numbers of fixed registers,
because the allocation mechanism considers them to be always occupied.

On some machines, double-precision values must be kept in even/odd
register pairs.  You can implement that by defining this macro to reject
odd register numbers for such modes.

The minimum requirement for a mode to be OK in a register is that the
mov[mode] instruction pattern support moves between the
register and other hard register in the same class and that moving a
value into the register and back out not alter it.

Since the same instruction used to move word_mode will work for
all narrower integer modes, it is not necessary on any machine for
HARD_REGNO_MODE_OK to distinguish between these modes, provided
you define patterns movhi, etc., to take advantage of this.  This
is useful because of the interaction between HARD_REGNO_MODE_OK
and MODES_TIEABLE_P; it is very desirable for all integer modes
to be tieable.

Many machines have special registers for floating point arithmetic.
Often people assume that floating point machine modes are allowed only
in floating point registers.  This is not true.  Any registers that
can hold integers can safely hold a floating point machine
mode, whether or not floating arithmetic can be done on it in those
registers.  Integer move instructions can be used to move the values.

On some machines, though, the converse is true: fixed-point machine
modes may not go in floating registers.  This is true if the floating
registers normalize any value stored in them, because storing a
non-floating value there would garble it.  In this case,
HARD_REGNO_MODE_OK should reject fixed-point machine modes in
floating registers.  But if the floating registers do not automatically
normalize, if you can store any bit pattern in one and retrieve it
unchanged without a trap, then any machine mode may go in a floating
register, so you can define this macro to say so.

The primary significance of special floating registers is rather that
they are the registers acceptable in floating point arithmetic
instructions.  However, this is of no concern to
HARD_REGNO_MODE_OK.  You handle it by writing the proper
constraints for those instructions.

On some machines, the floating registers are especially slow to access,
so that it is better to store a value in a stack frame than in such a
register if floating point arithmetic is not being done.  As long as the
floating registers are not in class GENERAL_REGS, they will not
be used unless some pattern's constraint asks for one.
*/
#define HARD_REGNO_MODE_OK(REGNO, MODE) avr32_hard_regno_mode_ok(REGNO, MODE)

/*
A C expression that is nonzero if a value of mode
MODE1 is accessible in mode MODE2 without copying.

If HARD_REGNO_MODE_OK(R, MODE1) and
HARD_REGNO_MODE_OK(R, MODE2) are always the same for
any R, then MODES_TIEABLE_P(MODE1, MODE2)
should be nonzero.  If they differ for any R, you should define
this macro to return zero unless some other mechanism ensures the
accessibility of the value in a narrower mode.

You should define this macro to return nonzero in as many cases as
possible since doing so will allow GCC to perform better register
allocation.
*/
#define MODES_TIEABLE_P(MODE1, MODE2)  \
  (GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2))



/******************************************************************************
 * Register Classes
 *****************************************************************************/

/*
An enumeral type that must be defined with all the register class names
as enumeral values.  NO_REGS must be first.  ALL_REGS
must be the last register class, followed by one more enumeral value,
LIM_REG_CLASSES, which is not a register class but rather
tells how many classes there are.

Each register class has a number, which is the value of casting
the class name to type int.  The number serves as an index
in many of the tables described below.
*/
enum reg_class
{
  NO_REGS,
  GENERAL_REGS,
  ALL_REGS,
  LIM_REG_CLASSES
};

/*
The number of distinct register classes, defined as follows:
  #define N_REG_CLASSES (int) LIM_REG_CLASSES
*/
#define N_REG_CLASSES (int)LIM_REG_CLASSES

/*
An initializer containing the names of the register classes as C string
constants.  These names are used in writing some of the debugging dumps.
*/
#define REG_CLASS_NAMES		\
{				\
  "NO_REGS",			\
  "GENERAL_REGS",		\
  "ALL_REGS"			\
}

/*
An initializer containing the contents of the register classes, as integers
which are bit masks.  The nth integer specifies the contents of class
n.  The way the integer mask is interpreted is that
register r is in the class if mask & (1 << r) is 1.

When the machine has more than 32 registers, an integer does not suffice.
Then the integers are replaced by sub-initializers, braced groupings containing
several integers.  Each sub-initializer must be suitable as an initializer
for the type HARD_REG_SET which is defined in hard-reg-set.h.
In this situation, the first integer in each sub-initializer corresponds to
registers 0 through 31, the second integer to registers 32 through 63, and
so on.
*/
#define REG_CLASS_CONTENTS {		\
  {0x00000000}, /* NO_REGS */		\
  {0x0000FFFF}, /* GENERAL_REGS */	\
  {0x7FFFFFFF}, /* ALL_REGS */		\
}


/*
A C expression whose value is a register class containing hard register
REGNO.  In general there is more than one such class; choose a class
which is minimal, meaning that no smaller class also contains the
register.
*/
#define REGNO_REG_CLASS(REGNO) (GENERAL_REGS)

/*
A macro whose definition is the name of the class to which a valid
base register must belong.  A base register is one used in an address
which is the register value plus a displacement.
*/
#define BASE_REG_CLASS GENERAL_REGS

/*
This is a variation of the BASE_REG_CLASS macro which allows
the selection of a base register in a mode depenedent manner.  If
mode is VOIDmode then it should return the same value as
BASE_REG_CLASS.
*/
#define MODE_BASE_REG_CLASS(MODE) BASE_REG_CLASS

/*
A macro whose definition is the name of the class to which a valid
index register must belong.  An index register is one used in an
address where its value is either multiplied by a scale factor or
added to another register (as well as added to a displacement).
*/
#define INDEX_REG_CLASS BASE_REG_CLASS

/*
A C expression which defines the machine-dependent operand constraint
letters for register classes.  If CHAR is such a letter, the
value should be the register class corresponding to it.  Otherwise,
the value should be NO_REGS.  The register letter r,
corresponding to class GENERAL_REGS, will not be passed
to this macro; you do not need to handle it.
*/
#define REG_CLASS_FROM_LETTER(CHAR) NO_REGS

/* These assume that REGNO is a hard or pseudo reg number.
   They give nonzero only if REGNO is a hard reg of the suitable class
   or a pseudo reg currently allocated to a suitable hard reg.
   Since they use reg_renumber, they are safe only once reg_renumber
   has been allocated, which happens in local-alloc.c.  */
#define TEST_REGNO(R, TEST, VALUE) \
  ((R TEST VALUE) || ((unsigned) reg_renumber[R] TEST VALUE))

/*
A C expression which is nonzero if register number num is suitable for use as a base
register in operand addresses. It may be either a suitable hard register or a pseudo
register that has been allocated such a hard register.
*/
#define REGNO_OK_FOR_BASE_P(NUM)  TEST_REGNO(NUM, <=, LAST_REGNUM)

/* The following macro defines cover classes for Integrated Register
   Allocator.  Cover classes is a set of non-intersected register
   classes covering all hard registers used for register allocation
   purpose.  Any move between two registers of a cover class should be
   cheaper than load or store of the registers.  The macro value is
   array of register classes with LIM_REG_CLASSES used as the end
   marker.  */

#define IRA_COVER_CLASSES               \
{                                       \
  GENERAL_REGS, LIM_REG_CLASSES         \
}

/*
A C expression which is nonzero if register number NUM is
suitable for use as an index register in operand addresses.  It may be
either a suitable hard register or a pseudo register that has been
allocated such a hard register.

The difference between an index register and a base register is that
the index register may be scaled.  If an address involves the sum of
two registers, neither one of them scaled, then either one may be
labeled the ``base'' and the other the ``index''; but whichever
labeling is used must fit the machine's constraints of which registers
may serve in each capacity.  The compiler will try both labelings,
looking for one that is valid, and will reload one or both registers
only if neither labeling works.
*/
#define REGNO_OK_FOR_INDEX_P(NUM) TEST_REGNO(NUM, <=, LAST_REGNUM)

/*
A C expression that places additional restrictions on the register class
to use when it is necessary to copy value X into a register in class
CLASS.  The value is a register class; perhaps CLASS, or perhaps
another, smaller class.  On many machines, the following definition is
safe: #define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS

Sometimes returning a more restrictive class makes better code.  For
example, on the 68000, when X is an integer constant that is in range
for a 'moveq' instruction, the value of this macro is always
DATA_REGS as long as CLASS includes the data registers.
Requiring a data register guarantees that a 'moveq' will be used.

If X is a const_double, by returning NO_REGS
you can force X into a memory constant.  This is useful on
certain machines where immediate floating values cannot be loaded into
certain kinds of registers.
*/
#define PREFERRED_RELOAD_CLASS(X, CLASS)  CLASS



/*
A C expression for the maximum number of consecutive registers
of class CLASS needed to hold a value of mode MODE.

This is closely related to the macro HARD_REGNO_NREGS.  In fact,
the value of the macro CLASS_MAX_NREGS(CLASS, MODE)
should be the maximum value of HARD_REGNO_NREGS(REGNO, MODE)
for all REGNO values in the class CLASS.

This macro helps control the handling of multiple-word values
in the reload pass.
*/
#define CLASS_MAX_NREGS(CLASS, MODE) /* ToDo:fixme */ \
  (unsigned int)((GET_MODE_SIZE(MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)


/*
  Using CONST_OK_FOR_CONSTRAINT_P instead of CONS_OK_FOR_LETTER_P
  in order to support constraints with more than one letter.
  Only two letters are then used for constant constraints,
  the letter 'K' and the letter 'I'. The constraint starting with
  these letters must consist of four characters. The character following
  'K' or 'I' must be either 'u' (unsigned) or 's' (signed) to specify
  if the constant is zero or sign extended. The last two characters specify
  the length in bits of the constant. The base constraint letter 'I' means
  that this is an negated constant, meaning that actually -VAL should be
  checked to lie withing the valid range instead of VAL which is used when
  'K' is the base constraint letter.

*/

#define CONSTRAINT_LEN(C, STR)				\
  ( ((C) == 'K' || (C) == 'I') ?  4 :			\
    ((C) == 'R') ?  5 :					\
    ((C) == 'P') ? -1 :                                 \
    DEFAULT_CONSTRAINT_LEN((C), (STR)) )

#define CONST_OK_FOR_CONSTRAINT_P(VALUE, C, STR)	\
  avr32_const_ok_for_constraint_p(VALUE, C, STR)

/*
A C expression that defines the machine-dependent operand constraint
letters that specify particular ranges of const_double values ('G' or 'H').

If C is one of those letters, the expression should check that
VALUE, an RTX of code const_double, is in the appropriate
range and return 1 if so, 0 otherwise.  If C is not one of those
letters, the value should be 0 regardless of VALUE.

const_double is used for all floating-point constants and for
DImode fixed-point constants.  A given letter can accept either
or both kinds of values.  It can use GET_MODE to distinguish
between these kinds.
*/
#define CONST_DOUBLE_OK_FOR_LETTER_P(OP, C) \
  ((C) == 'G' ? avr32_const_double_immediate(OP) : 0)

/*
A C expression that defines the optional machine-dependent constraint
letters that can be used to segregate specific types of operands, usually
memory references, for the target machine.  Any letter that is not
elsewhere defined and not matched by REG_CLASS_FROM_LETTER
may be used.  Normally this macro will not be defined.

If it is required for a particular target machine, it should return 1
if VALUE corresponds to the operand type represented by the
constraint letter C.  If C is not defined as an extra
constraint, the value returned should be 0 regardless of VALUE.

For example, on the ROMP, load instructions cannot have their output
in r0 if the memory reference contains a symbolic address.  Constraint
letter 'Q' is defined as representing a memory address that does
not contain a symbolic address.  An alternative is specified with
a 'Q' constraint on the input and 'r' on the output.  The next
alternative specifies 'm' on the input and a register class that
does not include r0 on the output.
*/
#define EXTRA_CONSTRAINT_STR(OP, C, STR)				\
  ((C) == 'W' ? avr32_address_operand(OP, GET_MODE(OP)) :		\
   (C) == 'R' ? (avr32_indirect_register_operand(OP, GET_MODE(OP)) ||	\
                 (avr32_imm_disp_memory_operand(OP, GET_MODE(OP))	\
                  && avr32_const_ok_for_constraint_p(			\
				INTVAL(XEXP(XEXP(OP, 0), 1)),		\
				(STR)[1], &(STR)[1]))) :		\
   (C) == 'S' ? avr32_indexed_memory_operand(OP, GET_MODE(OP)) :	\
   (C) == 'T' ? avr32_const_pool_ref_operand(OP, GET_MODE(OP)) :	\
   (C) == 'U' ? SYMBOL_REF_RCALL_FUNCTION_P(OP) :			\
   (C) == 'Z' ? avr32_cop_memory_operand(OP, GET_MODE(OP)) :		\
   (C) == 'Q' ? avr32_non_rmw_memory_operand(OP, GET_MODE(OP)) :		\
   (C) == 'Y' ? avr32_rmw_memory_operand(OP, GET_MODE(OP)) :            \
   0)


#define EXTRA_MEMORY_CONSTRAINT(C, STR) ( ((C) == 'R') ||               \
                                          ((C) == 'Q') ||               \
                                          ((C) == 'S') ||               \
                                          ((C) == 'Y') ||               \
                                          ((C) == 'Z') )


/* Returns nonzero if op is a function SYMBOL_REF which
   can be called using an rcall instruction */
#define SYMBOL_REF_RCALL_FUNCTION_P(op)  \
  ( GET_CODE(op) == SYMBOL_REF           \
    && SYMBOL_REF_FUNCTION_P(op)         \
    && SYMBOL_REF_LOCAL_P(op)            \
    && !SYMBOL_REF_EXTERNAL_P(op)        \
    && !TARGET_HAS_ASM_ADDR_PSEUDOS )

/******************************************************************************
 * Stack Layout and Calling Conventions
 *****************************************************************************/

/** Basic Stack Layout **/

/*
Define this macro if pushing a word onto the stack moves the stack
pointer to a smaller address.

When we say, ``define this macro if ...,'' it means that the
compiler checks this macro only with #ifdef so the precise
definition used does not matter.
*/
/* pushm decrece SP: *(--SP) <-- Rx */
#define STACK_GROWS_DOWNWARD

/*
This macro defines the operation used when something is pushed
on the stack.  In RTL, a push operation will be
(set (mem (STACK_PUSH_CODE (reg sp))) ...)

The choices are PRE_DEC, POST_DEC, PRE_INC,
and POST_INC.  Which of these is correct depends on
the stack direction and on whether the stack pointer points
to the last item on the stack or whether it points to the
space for the next item on the stack.

The default is PRE_DEC when STACK_GROWS_DOWNWARD is
defined, which is almost always right, and PRE_INC otherwise,
which is often wrong.
*/
/* pushm: *(--SP) <-- Rx */
#define STACK_PUSH_CODE PRE_DEC

/* Define this to nonzero if the nominal address of the stack frame
   is at the high-address end of the local variables;
   that is, each additional local variable allocated
   goes at a more negative offset in the frame.  */
#define FRAME_GROWS_DOWNWARD 1


/*
Offset from the frame pointer to the first local variable slot to be allocated.

If FRAME_GROWS_DOWNWARD, find the next slot's offset by
subtracting the first slot's length from STARTING_FRAME_OFFSET.
Otherwise, it is found by adding the length of the first slot to the
value STARTING_FRAME_OFFSET.
  (i'm not sure if the above is still correct.. had to change it to get
   rid of an overfull.  --mew 2feb93 )
*/
#define STARTING_FRAME_OFFSET 0

/*
Offset from the stack pointer register to the first location at which
outgoing arguments are placed.  If not specified, the default value of
zero is used.  This is the proper value for most machines.

If ARGS_GROW_DOWNWARD, this is the offset to the location above
the first location at which outgoing arguments are placed.
*/
#define STACK_POINTER_OFFSET 0

/*
Offset from the argument pointer register to the first argument's
address.  On some machines it may depend on the data type of the
function.

If ARGS_GROW_DOWNWARD, this is the offset to the location above
the first argument's address.
*/
#define FIRST_PARM_OFFSET(FUNDECL) 0


/*
A C expression whose value is RTL representing the address in a stack
frame where the pointer to the caller's frame is stored.  Assume that
FRAMEADDR is an RTL expression for the address of the stack frame
itself.

If you don't define this macro, the default is to return the value
of FRAMEADDR - that is, the stack frame address is also the
address of the stack word that points to the previous frame.
*/
#define DYNAMIC_CHAIN_ADDRESS(FRAMEADDR) plus_constant ((FRAMEADDR), 4)


/*
A C expression whose value is RTL representing the value of the return
address for the frame COUNT steps up from the current frame, after
the prologue.  FRAMEADDR is the frame pointer of the COUNT
frame, or the frame pointer of the COUNT - 1 frame if
RETURN_ADDR_IN_PREVIOUS_FRAME is defined.

The value of the expression must always be the correct address when
COUNT is zero, but may be NULL_RTX if there is not way to
determine the return address of other frames.
*/
#define RETURN_ADDR_RTX(COUNT, FRAMEADDR) avr32_return_addr(COUNT, FRAMEADDR)


/*
A C expression whose value is RTL representing the location of the
incoming return address at the beginning of any function, before the
prologue.  This RTL is either a REG, indicating that the return
value is saved in 'REG', or a MEM representing a location in
the stack.

You only need to define this macro if you want to support call frame
debugging information like that provided by DWARF 2.

If this RTL is a REG, you should also define
DWARF_FRAME_RETURN_COLUMN to DWARF_FRAME_REGNUM (REGNO).
*/
#define INCOMING_RETURN_ADDR_RTX gen_rtx_REG (Pmode, LR_REGNUM)

/*
A C expression whose value is an integer giving the offset, in bytes,
from the value of the stack pointer register to the top of the stack
frame at the beginning of any function, before the prologue.  The top of
the frame is defined to be the value of the stack pointer in the
previous frame, just before the call instruction.

You only need to define this macro if you want to support call frame
debugging information like that provided by DWARF 2.
*/
#define INCOMING_FRAME_SP_OFFSET 0


/** Exception Handling Support **/

/* Use setjump/longjump for exception handling. */
#define DWARF2_UNWIND_INFO 0
#define MUST_USE_SJLJ_EXCEPTIONS 1

/*
A C expression whose value is the Nth register number used for
data by exception handlers, or INVALID_REGNUM if fewer than
N registers are usable.

The exception handling library routines communicate with the exception
handlers via a set of agreed upon registers.  Ideally these registers
should be call-clobbered; it is possible to use call-saved registers,
but may negatively impact code size.  The target must support at least
2 data registers, but should define 4 if there are enough free registers.

You must define this macro if you want to support call frame exception
handling like that provided by DWARF 2.
*/
/*
  Use r9-r11
*/
#define EH_RETURN_DATA_REGNO(N)                                         \
  ((N<3) ? INTERNAL_REGNUM(N+9) : INVALID_REGNUM)

/*
A C expression whose value is RTL representing a location in which
to store a stack adjustment to be applied before function return.
This is used to unwind the stack to an exception handler's call frame.
It will be assigned zero on code paths that return normally.

Typically this is a call-clobbered hard register that is otherwise
untouched by the epilogue, but could also be a stack slot.

You must define this macro if you want to support call frame exception
handling like that provided by DWARF 2.
*/
/*
  Use r8
*/
#define EH_RETURN_STACKADJ_REGNO INTERNAL_REGNUM(8)
#define EH_RETURN_STACKADJ_RTX gen_rtx_REG(SImode, EH_RETURN_STACKADJ_REGNO)

/*
A C expression whose value is RTL representing a location in which
to store the address of an exception handler to which we should
return.  It will not be assigned on code paths that return normally.

Typically this is the location in the call frame at which the normal
return address is stored.  For targets that return by popping an
address off the stack, this might be a memory address just below
the target call frame rather than inside the current call
frame.  EH_RETURN_STACKADJ_RTX will have already been assigned,
so it may be used to calculate the location of the target call frame.

Some targets have more complex requirements than storing to an
address calculable during initial code generation.  In that case
the eh_return instruction pattern should be used instead.

If you want to support call frame exception handling, you must
define either this macro or the eh_return instruction pattern.
*/
/*
  We define the eh_return instruction pattern, so this isn't needed.
*/
/* #define EH_RETURN_HANDLER_RTX gen_rtx_REG(Pmode, RET_REGISTER) */

/*
  This macro chooses the encoding of pointers embedded in the
  exception handling sections. If at all possible, this should be
  defined such that the exception handling section will not require
  dynamic relocations, and so may be read-only.

  code is 0 for data, 1 for code labels, 2 for function
  pointers. global is true if the symbol may be affected by dynamic
  relocations. The macro should return a combination of the DW_EH_PE_*
  defines as found in dwarf2.h.

  If this macro is not defined, pointers will not be encoded but
  represented directly.
*/
#define ASM_PREFERRED_EH_DATA_FORMAT(CODE, GLOBAL)	\
  ((flag_pic && (GLOBAL) ? DW_EH_PE_indirect : 0)	\
   | (flag_pic ? DW_EH_PE_pcrel : DW_EH_PE_absptr)	\
   | DW_EH_PE_sdata4)

/* ToDo: The rest of this subsection */

/** Specifying How Stack Checking is Done **/
/* ToDo: All in this subsection */

/** Registers That Address the Stack Frame **/

/*
The register number of the stack pointer register, which must also be a
fixed register according to FIXED_REGISTERS.  On most machines,
the hardware determines which register this is.
*/
/* Using r13 as stack pointer. */
#define STACK_POINTER_REGNUM INTERNAL_REGNUM(13)

/*
The register number of the frame pointer register, which is used to
access automatic variables in the stack frame.  On some machines, the
hardware determines which register this is.  On other machines, you can
choose any register you wish for this purpose.
*/
/* Use r7 */
#define FRAME_POINTER_REGNUM INTERNAL_REGNUM(7)

/*
The register number of the arg pointer register, which is used to access
the function's argument list.  On some machines, this is the same as the
frame pointer register.  On some machines, the hardware determines which
register this is.  On other machines, you can choose any register you
wish for this purpose.  If this is not the same register as the frame
pointer register, then you must mark it as a fixed register according to
FIXED_REGISTERS, or arrange to be able to eliminate it (see Section
10.10.5 [Elimination], page 224).
*/
/* Using r5 */
#define ARG_POINTER_REGNUM INTERNAL_REGNUM(4)


/*
Register numbers used for passing a function's static chain pointer.  If
register windows are used, the register number as seen by the called
function is STATIC_CHAIN_INCOMING_REGNUM, while the register
number as seen by the calling function is STATIC_CHAIN_REGNUM.  If
these registers are the same, STATIC_CHAIN_INCOMING_REGNUM need
not be defined.

The static chain register need not be a fixed register.

If the static chain is passed in memory, these macros should not be
defined; instead, the next two macros should be defined.
*/
/* Using r0 */
#define STATIC_CHAIN_REGNUM INTERNAL_REGNUM(0)

/** Eliminating Frame Pointer and Arg Pointer **/

/*
A C expression which is nonzero if a function must have and use a frame
pointer.  This expression is evaluated  in the reload pass.  If its value is
nonzero the function will have a frame pointer.

The expression can in principle examine the current function and decide
according to the facts, but on most machines the constant 0 or the
constant 1 suffices.  Use 0 when the machine allows code to be generated
with no frame pointer, and doing so saves some time or space.  Use 1
when there is no possible advantage to avoiding a frame pointer.

In certain cases, the compiler does not know how to produce valid code
without a frame pointer.  The compiler recognizes those cases and
automatically gives the function a frame pointer regardless of what
FRAME_POINTER_REQUIRED says.  You don't need to worry about
them.

In a function that does not require a frame pointer, the frame pointer
register can be allocated for ordinary usage, unless you mark it as a
fixed register.  See FIXED_REGISTERS for more information.
*/
/* We need the frame pointer when compiling for profiling */
#define FRAME_POINTER_REQUIRED (crtl->profile)

/*
A C statement to store in the variable DEPTH_VAR the difference
between the frame pointer and the stack pointer values immediately after
the function prologue.  The value would be computed from information
such as the result of get_frame_size () and the tables of
registers regs_ever_live and call_used_regs.

If ELIMINABLE_REGS is defined, this macro will be not be used and
need not be defined.  Otherwise, it must be defined even if
FRAME_POINTER_REQUIRED is defined to always be true; in that
case, you may set DEPTH_VAR to anything.
*/
#define INITIAL_FRAME_POINTER_OFFSET(DEPTH_VAR) ((DEPTH_VAR) = get_frame_size())

/*
If defined, this macro specifies a table of register pairs used to
eliminate unneeded registers that point into the stack frame.  If it is not
defined, the only elimination attempted by the compiler is to replace
references to the frame pointer with references to the stack pointer.

The definition of this macro is a list of structure initializations, each
of which specifies an original and replacement register.

On some machines, the position of the argument pointer is not known until
the compilation is completed.  In such a case, a separate hard register
must be used for the argument pointer.  This register can be eliminated by
replacing it with either the frame pointer or the argument pointer,
depending on whether or not the frame pointer has been eliminated.

In this case, you might specify:
  #define ELIMINABLE_REGS  \
  {{ARG_POINTER_REGNUM, STACK_POINTER_REGNUM}, \
   {ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}, \
   {FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM}}

Note that the elimination of the argument pointer with the stack pointer is
specified first since that is the preferred elimination.
*/
#define ELIMINABLE_REGS					\
{							\
  { FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM },	\
  { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM },		\
  { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM }		\
}

/*
A C expression that returns nonzero if the compiler is allowed to try
to replace register number FROM with register number
TO.  This macro need only be defined if ELIMINABLE_REGS
is defined, and will usually be the constant 1, since most of the cases
preventing register elimination are things that the compiler already
knows about.
*/
#define CAN_ELIMINATE(FROM, TO) \
  (((FROM == ARG_POINTER_REGNUM) && (TO == STACK_POINTER_REGNUM)) ? \
    (frame_pointer_needed) ? 0 : 1 : 1)

/*
This macro is similar to INITIAL_FRAME_POINTER_OFFSET.  It
specifies the initial difference between the specified pair of
registers.  This macro must be defined if ELIMINABLE_REGS is
defined.
*/
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET)		\
  ((OFFSET) = avr32_initial_elimination_offset(FROM, TO))

/** Passing Function Arguments on the Stack **/


/*
A C expression.  If nonzero, push insns will be used to pass
outgoing arguments.
If the target machine does not have a push instruction, set it to zero.
That directs GCC to use an alternate strategy: to
allocate the entire argument block and then store the arguments into
it.  When PUSH_ARGS is nonzero, PUSH_ROUNDING must be defined too.
*/
#define PUSH_ARGS 1

/*
A C expression that is the number of bytes actually pushed onto the
stack when an instruction attempts to push NPUSHED bytes.

On some machines, the definition

  #define PUSH_ROUNDING(BYTES) (BYTES)

will suffice.  But on other machines, instructions that appear
to push one byte actually push two bytes in an attempt to maintain
alignment.  Then the definition should be

  #define PUSH_ROUNDING(BYTES) (((BYTES) + 1) & ~1)
*/
/* Push 4 bytes at the time. */
#define PUSH_ROUNDING(NPUSHED) (((NPUSHED) + 3) & ~3)

/*
A C expression.  If nonzero, the maximum amount of space required for
outgoing arguments will be computed and placed into the variable
current_function_outgoing_args_size.  No space will be pushed
onto the stack for each call; instead, the function prologue should
increase the stack frame size by this amount.

Setting both PUSH_ARGS and ACCUMULATE_OUTGOING_ARGS is not proper.
*/
#define ACCUMULATE_OUTGOING_ARGS 0

/*
A C expression that should indicate the number of bytes of its own
arguments that a function pops on returning, or 0 if the
function pops no arguments and the caller must therefore pop them all
after the function returns.

FUNDECL is a C variable whose value is a tree node that describes
the function in question.  Normally it is a node of type
FUNCTION_DECL that describes the declaration of the function.
From this you can obtain the DECL_ATTRIBUTES of the function.

FUNTYPE is a C variable whose value is a tree node that
describes the function in question.  Normally it is a node of type
FUNCTION_TYPE that describes the data type of the function.
From this it is possible to obtain the data types of the value and
arguments (if known).

When a call to a library function is being considered, FUNDECL
will contain an identifier node for the library function.  Thus, if
you need to distinguish among various library functions, you can do so
by their names.  Note that ``library function'' in this context means
a function used to perform arithmetic, whose name is known specially
in the compiler and was not mentioned in the C code being compiled.

STACK_SIZE is the number of bytes of arguments passed on the
stack.  If a variable number of bytes is passed, it is zero, and
argument popping will always be the responsibility of the calling function.

On the VAX, all functions always pop their arguments, so the definition
of this macro is STACK_SIZE.  On the 68000, using the standard
calling convention, no functions pop their arguments, so the value of
the macro is always 0 in this case.  But an alternative calling
convention is available in which functions that take a fixed number of
arguments pop them but other functions (such as printf) pop
nothing (the caller pops all).  When this convention is in use,
FUNTYPE is examined to determine whether a function takes a fixed
number of arguments.
*/
#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, STACK_SIZE) 0


/*Return true if this function can we use a single return instruction*/
#define USE_RETURN_INSN(ISCOND) avr32_use_return_insn(ISCOND)

/*
A C expression that should indicate the number of bytes a call sequence
pops off the stack.  It is added to the value of RETURN_POPS_ARGS
when compiling a function call.

CUM is the variable in which all arguments to the called function
have been accumulated.

On certain architectures, such as the SH5, a call trampoline is used
that pops certain registers off the stack, depending on the arguments
that have been passed to the function.  Since this is a property of the
call site, not of the called function, RETURN_POPS_ARGS is not
appropriate.
*/
#define CALL_POPS_ARGS(CUM) 0

/* Passing Arguments in Registers */

/*
A C expression that controls whether a function argument is passed
in a register, and which register.

The arguments are CUM, which summarizes all the previous
arguments; MODE, the machine mode of the argument; TYPE,
the data type of the argument as a tree node or 0 if that is not known
(which happens for C support library functions); and NAMED,
which is 1 for an ordinary argument and 0 for nameless arguments that
correspond to '...' in the called function's prototype.
TYPE can be an incomplete type if a syntax error has previously
occurred.

The value of the expression is usually either a reg RTX for the
hard register in which to pass the argument, or zero to pass the
argument on the stack.

For machines like the VAX and 68000, where normally all arguments are
pushed, zero suffices as a definition.

The value of the expression can also be a parallel RTX.  This is
used when an argument is passed in multiple locations.  The mode of the
of the parallel should be the mode of the entire argument.  The
parallel holds any number of expr_list pairs; each one
describes where part of the argument is passed.  In each
expr_list the first operand must be a reg RTX for the hard
register in which to pass this part of the argument, and the mode of the
register RTX indicates how large this part of the argument is.  The
second operand of the expr_list is a const_int which gives
the offset in bytes into the entire argument of where this part starts.
As a special exception the first expr_list in the parallel
RTX may have a first operand of zero.  This indicates that the entire
argument is also stored on the stack.

The last time this macro is called, it is called with MODE == VOIDmode,
and its result is passed to the call or call_value
pattern as operands 2 and 3 respectively.

The usual way to make the ISO library 'stdarg.h' work on a machine
where some arguments are usually passed in registers, is to cause
nameless arguments to be passed on the stack instead.  This is done
by making FUNCTION_ARG return 0 whenever NAMED is 0.

You may use the macro MUST_PASS_IN_STACK (MODE, TYPE)
in the definition of this macro to determine if this argument is of a
type that must be passed in the stack.  If REG_PARM_STACK_SPACE
is not defined and FUNCTION_ARG returns nonzero for such an
argument, the compiler will abort.  If REG_PARM_STACK_SPACE is
defined, the argument will be computed in the stack and then loaded into
a register.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  avr32_function_arg(&(CUM), MODE, TYPE, NAMED)

/*
A C type for declaring a variable that is used as the first argument of
FUNCTION_ARG and other related values.  For some target machines,
the type int suffices and can hold the number of bytes of
argument so far.

There is no need to record in CUMULATIVE_ARGS anything about the
arguments that have been passed on the stack.  The compiler has other
variables to keep track of that.  For target machines on which all
arguments are passed on the stack, there is no need to store anything in
CUMULATIVE_ARGS; however, the data structure must exist and
should not be empty, so use int.
*/
typedef struct avr32_args
{
  /* Index representing the argument register the current function argument
     will occupy */
  int index;
  /* A mask with bits representing the argument registers: if a bit is set
     then this register is used for an argument */
  int used_index;
  /* TRUE if this function has anonymous arguments */
  int uses_anonymous_args;
  /* The size in bytes of the named arguments pushed on the stack */
  int stack_pushed_args_size;
  /* Set to true if this function needs a Return Value Pointer */
  int use_rvp;
  /* Set to true if function is a flashvault function. */
  int flashvault_func;

} CUMULATIVE_ARGS;


#define FIRST_CUM_REG_INDEX 0
#define LAST_CUM_REG_INDEX 4
#define GET_REG_INDEX(CUM) ((CUM)->index)
#define SET_REG_INDEX(CUM, INDEX) ((CUM)->index = (INDEX));
#define GET_USED_INDEX(CUM, INDEX) ((CUM)->used_index & (1 << (INDEX)))
#define SET_USED_INDEX(CUM, INDEX)		\
  do						\
    {						\
      if (INDEX >= 0)				\
        (CUM)->used_index |= (1 << (INDEX));	\
    }						\
  while (0)
#define SET_INDEXES_UNUSED(CUM) ((CUM)->used_index = 0)

/*
   A C statement (sans semicolon) for initializing the variable cum for the
   state at the beginning of the argument list. The variable has type
   CUMULATIVE_ARGS. The value of FNTYPE is the tree node for the data type of
   the function which will receive the args, or 0 if the args are to a compiler
   support library function. For direct calls that are not libcalls, FNDECL
   contain the declaration node of the function. FNDECL is also set when
   INIT_CUMULATIVE_ARGS is used to find arguments for the function being
   compiled.  N_NAMED_ARGS is set to the number of named arguments, including a
   structure return address if it is passed as a parameter, when making a call.
   When processing incoming arguments, N_NAMED_ARGS is set to -1.

   When processing a call to a compiler support library function, LIBNAME
   identifies which one.  It is a symbol_ref rtx which contains the name of the
   function, as a string. LIBNAME is 0 when an ordinary C function call is
   being processed. Thus, each time this macro is called, either LIBNAME or
   FNTYPE is nonzero, but never both of them at once.
*/
#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, FNDECL, N_NAMED_ARGS) \
  avr32_init_cumulative_args(&(CUM), FNTYPE, LIBNAME, FNDECL)

/*
A C statement (sans semicolon) to update the summarizer variable
CUM to advance past an argument in the argument list.  The
values MODE, TYPE and NAMED describe that argument.
Once this is done, the variable CUM is suitable for analyzing
the following argument with FUNCTION_ARG, etc.

This macro need not do anything if the argument in question was passed
on the stack.  The compiler knows how to track the amount of stack space
used for arguments without any special help.
*/
#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED) \
  avr32_function_arg_advance(&(CUM), MODE, TYPE, NAMED)

/*
If defined, a C expression which determines whether, and in which direction,
to pad out an argument with extra space.  The value should be of type
enum direction: either 'upward' to pad above the argument,
'downward' to pad below, or 'none' to inhibit padding.

The amount of padding is always just enough to reach the next
multiple of FUNCTION_ARG_BOUNDARY; this macro does not control
it.

This macro has a default definition which is right for most systems.
For little-endian machines, the default is to pad upward.  For
big-endian machines, the default is to pad downward for an argument of
constant size shorter than an int, and upward otherwise.
*/
#define FUNCTION_ARG_PADDING(MODE, TYPE) \
  avr32_function_arg_padding(MODE, TYPE)

/*
  Specify padding for the last element of a block move between registers
  and memory. First is nonzero if this is the only element. Defining
  this macro allows better control of register function parameters on
  big-endian machines, without using PARALLEL rtl. In particular,
  MUST_PASS_IN_STACK need not test padding and mode of types in registers,
  as there is no longer a "wrong" part of a register; For example, a three
  byte aggregate may be passed in the high part of a register if so required.
*/
#define BLOCK_REG_PADDING(MODE, TYPE, FIRST) \
  avr32_function_arg_padding(MODE, TYPE)

/*
If defined, a C expression which determines whether the default
implementation of va_arg will attempt to pad down before reading the
next argument, if that argument is smaller than its aligned space as
controlled by PARM_BOUNDARY.  If this macro is not defined, all such
arguments are padded down if BYTES_BIG_ENDIAN is true.
*/
#define PAD_VARARGS_DOWN \
  (FUNCTION_ARG_PADDING (TYPE_MODE (type), type) == downward)

/*
A C expression that is nonzero if REGNO is the number of a hard
register in which function arguments are sometimes passed.  This does
not include implicit arguments such as the static chain and
the structure-value address.  On many machines, no registers can be
used for this purpose since all function arguments are pushed on the
stack.
*/
/*
  Use r8 - r12 for function arguments.
*/
#define FUNCTION_ARG_REGNO_P(REGNO) \
  (REGNO >= 3 && REGNO <= 7)

/* Number of registers used for passing function arguments */
#define NUM_ARG_REGS 5

/*
If defined, the order in which arguments are loaded into their
respective argument registers is reversed so that the last
argument is loaded first.  This macro only affects arguments
passed in registers.
*/
/* #define LOAD_ARGS_REVERSED */

/** How Scalar Function Values Are Returned **/

/* AVR32 is using r12 as return register. */
#define RET_REGISTER (15 - 12)

/*
A C expression to create an RTX representing the place where a library
function returns a value of mode MODE.  If the precise function
being called is known, FUNC is a tree node
(FUNCTION_DECL) for it; otherwise, func is a null
pointer.  This makes it possible to use a different value-returning
convention for specific functions when all their calls are
known.

Note that "library function" in this context means a compiler
support routine, used to perform arithmetic, whose name is known
specially by the compiler and was not mentioned in the C code being
compiled.

The definition of LIBRARY_VALUE need not be concerned aggregate
data types, because none of the library functions returns such types.
*/
#define LIBCALL_VALUE(MODE) avr32_libcall_value(MODE)

/*
A C expression that is nonzero if REGNO is the number of a hard
register in which the values of called function may come back.

A register whose use for returning values is limited to serving as the
second of a pair (for a value of type double, say) need not be
recognized by this macro.  So for most machines, this definition
suffices:
  #define FUNCTION_VALUE_REGNO_P(N) ((N) == 0)

If the machine has register windows, so that the caller and the called
function use different registers for the return value, this macro
should recognize only the caller's register numbers.
*/
/*
  When returning a value of mode DImode, r11:r10 is used, else r12 is used.
*/
#define FUNCTION_VALUE_REGNO_P(REGNO) ((REGNO) == RET_REGISTER \
                                       || (REGNO) == INTERNAL_REGNUM(11))


/** How Large Values Are Returned **/


/*
Define this macro to be 1 if all structure and union return values must be
in memory.  Since this results in slower code, this should be defined
only if needed for compatibility with other compilers or with an ABI.
If you define this macro to be 0, then the conventions used for structure
and union return values are decided by the RETURN_IN_MEMORY macro.

If not defined, this defaults to the value 1.
*/
#define DEFAULT_PCC_STRUCT_RETURN 0




/** Generating Code for Profiling **/

/*
A C statement or compound statement to output to FILE some
assembler code to call the profiling subroutine mcount.

The details of how mcount expects to be called are determined by
your operating system environment, not by GCC.  To figure them out,
compile a small program for profiling using the system's installed C
compiler and look at the assembler code that results.

Older implementations of mcount expect the address of a counter
variable to be loaded into some register.  The name of this variable is
'LP' followed by the number LABELNO, so you would generate
the name using 'LP%d' in a fprintf.
*/
/* ToDo: fixme */
#ifndef FUNCTION_PROFILER
#define FUNCTION_PROFILER(FILE, LABELNO) \
  fprintf((FILE), "/* profiler %d */", (LABELNO))
#endif


/*****************************************************************************
 * Trampolines for Nested Functions                                          *
 *****************************************************************************/

/*
A C statement to output, on the stream FILE, assembler code for a
block of data that contains the constant parts of a trampoline.  This
code should not include a label - the label is taken care of
automatically.

If you do not define this macro, it means no template is needed
for the target.  Do not define this macro on systems where the block move
code to copy the trampoline into place would be larger than the code
to generate it on the spot.
*/
/* ToDo: correct? */
#define TRAMPOLINE_TEMPLATE(FILE) avr32_trampoline_template(FILE);


/*
A C expression for the size in bytes of the trampoline, as an integer.
*/
/* ToDo: fixme */
#define TRAMPOLINE_SIZE 0x0C

/*
Alignment required for trampolines, in bits.

If you don't define this macro, the value of BIGGEST_ALIGNMENT
is used for aligning trampolines.
*/
#define TRAMPOLINE_ALIGNMENT 16

/*
A C statement to initialize the variable parts of a trampoline.
ADDR is an RTX for the address of the trampoline; FNADDR is
an RTX for the address of the nested function; STATIC_CHAIN is an
RTX for the static chain value that should be passed to the function
when it is called.
*/
#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, STATIC_CHAIN) \
  avr32_initialize_trampoline(ADDR, FNADDR, STATIC_CHAIN)


/******************************************************************************
 * Implicit Calls to Library Routines
 *****************************************************************************/

/* Tail calling.  */

/* A C expression that evaluates to true if it is ok to perform a sibling
   call to DECL.  */
#define FUNCTION_OK_FOR_SIBCALL(DECL) 0

#define OVERRIDE_OPTIONS  avr32_override_options ()

#define OPTIMIZATION_OPTIONS(LEVEL, SIZE) avr32_optimization_options (LEVEL, SIZE)

/******************************************************************************
 * Addressing Modes
 *****************************************************************************/

/*
A C expression that is nonzero if the machine supports pre-increment,
pre-decrement, post-increment, or post-decrement addressing respectively.
*/
/*
  AVR32 supports Rp++ and --Rp
*/
#define HAVE_PRE_INCREMENT 0
#define HAVE_PRE_DECREMENT 1
#define HAVE_POST_INCREMENT 1
#define HAVE_POST_DECREMENT 0

/*
A C expression that is nonzero if the machine supports pre- or
post-address side-effect generation involving constants other than
the size of the memory operand.
*/
#define HAVE_PRE_MODIFY_DISP 0
#define HAVE_POST_MODIFY_DISP 0

/*
A C expression that is nonzero if the machine supports pre- or
post-address side-effect generation involving a register displacement.
*/
#define HAVE_PRE_MODIFY_REG 0
#define HAVE_POST_MODIFY_REG 0

/*
A C expression that is 1 if the RTX X is a constant which
is a valid address.  On most machines, this can be defined as
CONSTANT_P (X), but a few machines are more restrictive
in which constant addresses are supported.

CONSTANT_P accepts integer-values expressions whose values are
not explicitly known, such as symbol_ref, label_ref, and
high expressions and const arithmetic expressions, in
addition to const_int and const_double expressions.
*/
#define CONSTANT_ADDRESS_P(X) CONSTANT_P(X)

/*
A number, the maximum number of registers that can appear in a valid
memory address.  Note that it is up to you to specify a value equal to
the maximum number that GO_IF_LEGITIMATE_ADDRESS would ever
accept.
*/
#define MAX_REGS_PER_ADDRESS 2

/*
A C compound statement with a conditional goto LABEL;
executed if X (an RTX) is a legitimate memory address on the
target machine for a memory operand of mode MODE.

It usually pays to define several simpler macros to serve as
subroutines for this one.  Otherwise it may be too complicated to
understand.

This macro must exist in two variants: a strict variant and a
non-strict one.  The strict variant is used in the reload pass.  It
must be defined so that any pseudo-register that has not been
allocated a hard register is considered a memory reference.  In
contexts where some kind of register is required, a pseudo-register
with no hard register must be rejected.

The non-strict variant is used in other passes.  It must be defined to
accept all pseudo-registers in every context where some kind of
register is required.

Compiler source files that want to use the strict variant of this
macro define the macro REG_OK_STRICT.  You should use an
#ifdef REG_OK_STRICT conditional to define the strict variant
in that case and the non-strict variant otherwise.

Subroutines to check for acceptable registers for various purposes (one
for base registers, one for index registers, and so on) are typically
among the subroutines used to define GO_IF_LEGITIMATE_ADDRESS.
Then only these subroutine macros need have two variants; the higher
levels of macros may be the same whether strict or not.

Normally, constant addresses which are the sum of a symbol_ref
and an integer are stored inside a const RTX to mark them as
constant.  Therefore, there is no need to recognize such sums
specifically as legitimate addresses.  Normally you would simply
recognize any const as legitimate.

Usually PRINT_OPERAND_ADDRESS is not prepared to handle constant
sums that are not marked with  const.  It assumes that a naked
plus indicates indexing.  If so, then you must reject such
naked constant sums as illegitimate addresses, so that none of them will
be given to PRINT_OPERAND_ADDRESS.

On some machines, whether a symbolic address is legitimate depends on
the section that the address refers to.  On these machines, define the
macro ENCODE_SECTION_INFO to store the information into the
symbol_ref, and then check for it here.  When you see a
const, you will have to look inside it to find the
symbol_ref in order to determine the section.

The best way to modify the name string is by adding text to the
beginning, with suitable punctuation to prevent any ambiguity.  Allocate
the new name in saveable_obstack.  You will have to modify
ASM_OUTPUT_LABELREF to remove and decode the added text and
output the name accordingly, and define STRIP_NAME_ENCODING to
access the original name string.

You can check the information stored here into the symbol_ref in
the definitions of the macros GO_IF_LEGITIMATE_ADDRESS and
PRINT_OPERAND_ADDRESS.
*/
#ifdef REG_OK_STRICT
#  define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)	\
  do							\
    {							\
      if (avr32_legitimate_address(MODE, X, 1))		\
	goto LABEL;					\
    }							\
  while (0)
#else
#  define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL)	\
  do							\
    {							\
      if (avr32_legitimate_address(MODE, X, 0))		\
	goto LABEL;					\
    }							\
  while (0)
#endif



/*
A C compound statement that attempts to replace X with a valid
memory address for an operand of mode MODE.  win will be a
C statement label elsewhere in the code; the macro definition may use

  GO_IF_LEGITIMATE_ADDRESS (MODE, X, WIN);

to avoid further processing if the address has become legitimate.

X will always be the result of a call to break_out_memory_refs,
and OLDX will be the operand that was given to that function to produce
X.

The code generated by this macro should not alter the substructure of
X.  If it transforms X into a more legitimate form, it
should assign X (which will always be a C variable) a new value.

It is not necessary for this macro to come up with a legitimate
address.  The compiler has standard ways of doing so in all cases.  In
fact, it is safe for this macro to do nothing.  But often a
machine-dependent strategy can generate better code.
*/
#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)			\
  do								\
    {								\
      if (GET_CODE(X) == PLUS					\
	  && GET_CODE(XEXP(X, 0)) == REG			\
	  && GET_CODE(XEXP(X, 1)) == CONST_INT			\
	  && !CONST_OK_FOR_CONSTRAINT_P(INTVAL(XEXP(X, 1)),	\
					'K', "Ks16"))		\
	{							\
	  rtx index = force_reg(SImode, XEXP(X, 1));		\
	  X = gen_rtx_PLUS( SImode, XEXP(X, 0), index);		\
	}							\
      GO_IF_LEGITIMATE_ADDRESS(MODE, X, WIN);			\
    }								\
  while(0)


/*
A C statement or compound statement with a conditional
goto LABEL; executed if memory address X (an RTX) can have
different meanings depending on the machine mode of the memory
reference it is used for or if the address is valid for some modes
but not others.

Autoincrement and autodecrement addresses typically have mode-dependent
effects because the amount of the increment or decrement is the size
of the operand being addressed.  Some machines have other mode-dependent
addresses.  Many RISC machines have no mode-dependent addresses.

You may assume that ADDR is a valid address for the machine.
*/
#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)	\
  do							\
    {							\
      if (GET_CODE (ADDR) == POST_INC			\
	  || GET_CODE (ADDR) == PRE_DEC)		\
	goto LABEL;					\
    }							\
  while (0)

/*
A C expression that is nonzero if X is a legitimate constant for
an immediate operand on the target machine.  You can assume that
X satisfies CONSTANT_P, so you need not check this.  In fact,
'1' is a suitable definition for this macro on machines where
anything CONSTANT_P is valid.
*/
#define LEGITIMATE_CONSTANT_P(X) avr32_legitimate_constant_p(X)


/******************************************************************************
 * Condition Code Status
 *****************************************************************************/

/*
C code for a data type which is used for declaring the mdep
component of cc_status.  It defaults to int.

This macro is not used on machines that do not use cc0.
*/

typedef struct
{
  int flags;
  rtx value;
  int cond_exec_cmp_clobbered;
} avr32_status_reg;


#define CC_STATUS_MDEP avr32_status_reg

/*
A C expression to initialize the mdep field to "empty".
The default definition does nothing, since most machines don't use
the field anyway.  If you want to use the field, you should probably
define this macro to initialize it.

This macro is not used on machines that do not use cc0.
*/

#define CC_STATUS_MDEP_INIT  \
   (cc_status.mdep.flags = CC_NONE , cc_status.mdep.cond_exec_cmp_clobbered = 0, cc_status.mdep.value = 0)

/*
A C compound statement to set the components of cc_status
appropriately for an insn INSN whose body is EXP.  It is
this macro's responsibility to recognize insns that set the condition
code as a byproduct of other activity as well as those that explicitly
set (cc0).

This macro is not used on machines that do not use cc0.

If there are insns that do not set the condition code but do alter
other machine registers, this macro must check to see whether they
invalidate the expressions that the condition code is recorded as
reflecting.  For example, on the 68000, insns that store in address
registers do not set the condition code, which means that usually
NOTICE_UPDATE_CC can leave cc_status unaltered for such
insns.  But suppose that the previous insn set the condition code
based on location 'a4@@(102)' and the current insn stores a new
value in 'a4'.  Although the condition code is not changed by
this, it will no longer be true that it reflects the contents of
'a4@@(102)'.  Therefore, NOTICE_UPDATE_CC must alter
cc_status in this case to say that nothing is known about the
condition code value.

The definition of NOTICE_UPDATE_CC must be prepared to deal
with the results of peephole optimization: insns whose patterns are
parallel RTXs containing various reg, mem or
constants which are just the operands.  The RTL structure of these
insns is not sufficient to indicate what the insns actually do.  What
NOTICE_UPDATE_CC should do when it sees one is just to run
CC_STATUS_INIT.

A possible definition of NOTICE_UPDATE_CC is to call a function
that looks at an attribute (see Insn Attributes) named, for example,
'cc'.  This avoids having detailed information about patterns in
two places, the 'md' file and in NOTICE_UPDATE_CC.
*/

#define NOTICE_UPDATE_CC(EXP, INSN) avr32_notice_update_cc(EXP, INSN)




/******************************************************************************
 * Describing Relative Costs of Operations
 *****************************************************************************/



/*
A C expression for the cost of moving data of mode MODE from a
register in class FROM to one in class TO.  The classes are
expressed using the enumeration values such as GENERAL_REGS.  A
value of 2 is the default; other values are interpreted relative to
that.

It is not required that the cost always equal 2 when FROM is the
same as TO; on some machines it is expensive to move between
registers if they are not general registers.

If reload sees an insn consisting of a single set between two
hard registers, and if REGISTER_MOVE_COST applied to their
classes returns a value of 2, reload does not check to ensure that the
constraints of the insn are met.  Setting a cost of other than 2 will
allow reload to verify that the constraints are met.  You should do this
if the movm pattern's constraints do not allow such copying.
*/
#define REGISTER_MOVE_COST(MODE, FROM, TO) \
  ((GET_MODE_SIZE(MODE) <= 4) ? 2:         \
   (GET_MODE_SIZE(MODE) <= 8) ? 3:         \
   4)

/*
A C expression for the cost of moving data of mode MODE between a
register of class CLASS and memory; IN is zero if the value
is to be written to memory, nonzero if it is to be read in.  This cost
is relative to those in REGISTER_MOVE_COST.  If moving between
registers and memory is more expensive than between two registers, you
should define this macro to express the relative cost.

If you do not define this macro, GCC uses a default cost of 4 plus
the cost of copying via a secondary reload register, if one is
needed.  If your machine requires a secondary reload register to copy
between memory and a register of CLASS but the reload mechanism is
more complex than copying via an intermediate, define this macro to
reflect the actual cost of the move.

GCC defines the function memory_move_secondary_cost if
secondary reloads are needed.  It computes the costs due to copying via
a secondary register.  If your machine copies from memory using a
secondary register in the conventional way but the default base value of
4 is not correct for your machine, define this macro to add some other
value to the result of that function.  The arguments to that function
are the same as to this macro.
*/
/*
  Memory moves are costly
*/
#define MEMORY_MOVE_COST(MODE, CLASS, IN)            \
  (((IN) ? ((GET_MODE_SIZE(MODE) < 4) ? 4 :          \
            (GET_MODE_SIZE(MODE) > 8) ? 6 :          \
            3)                                       \
    : ((GET_MODE_SIZE(MODE) > 8) ? 6 : 3)))

/*
A C expression for the cost of a branch instruction.  A value of 1 is
the default; other values are interpreted relative to that.
*/
  /* Try to use conditionals as much as possible */
#define BRANCH_COST(speed_p, predictable_p) (TARGET_BRANCH_PRED ? 3 : 4)

/*A C expression for the maximum number of instructions to execute via conditional
  execution instructions instead of a branch. A value of BRANCH_COST+1 is the default
  if the machine does not use cc0, and 1 if it does use cc0.*/
#define MAX_CONDITIONAL_EXECUTE 4

/*
Define this macro as a C expression which is nonzero if accessing less
than a word of memory (i.e.: a char or a short) is no
faster than accessing a word of memory, i.e., if such access
require more than one instruction or if there is no difference in cost
between byte and (aligned) word loads.

When this macro is not defined, the compiler will access a field by
finding the smallest containing object; when it is defined, a fullword
load will be used if alignment permits.  Unless bytes accesses are
faster than word accesses, using word accesses is preferable since it
may eliminate subsequent memory access if subsequent accesses occur to
other fields in the same word of the structure, but to different bytes.
*/
#define SLOW_BYTE_ACCESS 1


/*
Define this macro if it is as good or better to call a constant
function address than to call an address kept in a register.
*/
#define NO_FUNCTION_CSE


/******************************************************************************
 * Adjusting the Instruction Scheduler
 *****************************************************************************/

/*****************************************************************************
 * Dividing the Output into Sections (Texts, Data, ...)                      *
 *****************************************************************************/

/*
A C expression whose value is a string, including spacing, containing the
assembler operation that should precede instructions and read-only data.
Normally "\t.text" is right.
*/
#define TEXT_SECTION_ASM_OP "\t.text"
/*
A C statement that switches to the default section containing instructions.
Normally this is not needed, as simply defining TEXT_SECTION_ASM_OP
is enough.  The MIPS port uses this to sort all functions after all data
declarations.
*/
/* #define TEXT_SECTION */

/*
A C expression whose value is a string, including spacing, containing the
assembler operation to identify the following data as writable initialized
data.  Normally "\t.data" is right.
*/
#define DATA_SECTION_ASM_OP "\t.data"

/*
If defined, a C expression whose value is a string, including spacing,
containing the assembler operation to identify the following data as
shared data.  If not defined, DATA_SECTION_ASM_OP will be used.
*/

/*
A C expression whose value is a string, including spacing, containing
the assembler operation to identify the following data as read-only
initialized data.
*/
#undef READONLY_DATA_SECTION_ASM_OP
#define READONLY_DATA_SECTION_ASM_OP \
  ((TARGET_USE_RODATA_SECTION) ?  \
   "\t.section\t.rodata" :                \
   TEXT_SECTION_ASM_OP )


/*
If defined, a C expression whose value is a string, including spacing,
containing the assembler operation to identify the following data as
uninitialized global data.  If not defined, and neither
ASM_OUTPUT_BSS nor ASM_OUTPUT_ALIGNED_BSS are defined,
uninitialized global data will be output in the data section if
-fno-common is passed, otherwise ASM_OUTPUT_COMMON will be
used.
*/
#define BSS_SECTION_ASM_OP	"\t.section\t.bss"

/*
If defined, a C expression whose value is a string, including spacing,
containing the assembler operation to identify the following data as
uninitialized global shared data.  If not defined, and
BSS_SECTION_ASM_OP is, the latter will be used.
*/
/*#define SHARED_BSS_SECTION_ASM_OP "\trseg\tshared_bbs_section:data:noroot(0)\n"*/
/*
If defined, a C expression whose value is a string, including spacing,
containing the assembler operation to identify the following data as
initialization code.  If not defined, GCC will assume such a section does
not exist.
*/
#undef  INIT_SECTION_ASM_OP
#define INIT_SECTION_ASM_OP "\t.section\t.init"

/*
If defined, a C expression whose value is a string, including spacing,
containing the assembler operation to identify the following data as
finalization code.  If not defined, GCC will assume such a section does
not exist.
*/
#undef  FINI_SECTION_ASM_OP
#define FINI_SECTION_ASM_OP "\t.section\t.fini"

/*
If defined, an ASM statement that switches to a different section
via SECTION_OP, calls FUNCTION, and switches back to
the text section.  This is used in crtstuff.c if
INIT_SECTION_ASM_OP or FINI_SECTION_ASM_OP to calls
to initialization and finalization functions from the init and fini
sections.  By default, this macro uses a simple function call.  Some
ports need hand-crafted assembly code to avoid dependencies on
registers initialized in the function prologue or to ensure that
constant pools don't end up too far way in the text section.
*/
#define CRT_CALL_STATIC_FUNCTION(SECTION_OP, FUNC)      \
   asm ( SECTION_OP "\n" \
         "mcall   r6[" USER_LABEL_PREFIX #FUNC "@got]\n" \
         TEXT_SECTION_ASM_OP);


/*
Define this macro to be an expression with a nonzero value if jump
tables (for tablejump insns) should be output in the text
section, along with the assembler instructions.  Otherwise, the
readonly data section is used.

This macro is irrelevant if there is no separate readonly data section.
*/
/* Put jump tables in text section if we have caches. Otherwise assume that
   loading data from code memory is slow. */
#define JUMP_TABLES_IN_TEXT_SECTION    \
    (TARGET_CACHES ? 1 : 0)


/******************************************************************************
 * Position Independent Code (PIC)
 *****************************************************************************/

#ifndef AVR32_ALWAYS_PIC
#define AVR32_ALWAYS_PIC 0
#endif

/* GOT is set to r6 */
#define PIC_OFFSET_TABLE_REGNUM INTERNAL_REGNUM(6)

/*
A C expression that is nonzero if X is a legitimate immediate
operand on the target machine when generating position independent code.
You can assume that X satisfies CONSTANT_P, so you need not
check this.  You can also assume flag_pic is true, so you need not
check it either.  You need not define this macro if all constants
(including SYMBOL_REF) can be immediate operands when generating
position independent code.
*/
/* We can't directly access anything that contains a symbol,
   nor can we indirect via the constant pool.  */
#define LEGITIMATE_PIC_OPERAND_P(X) avr32_legitimate_pic_operand_p(X)


/* We need to know when we are making a constant pool; this determines
   whether data needs to be in the GOT or can be referenced via a GOT
   offset.  */
extern int making_const_table;

/******************************************************************************
 * Defining the Output Assembler Language
 *****************************************************************************/


/*
A C string constant describing how to begin a comment in the target
assembler language.  The compiler assumes that the comment will end at
the end of the line.
*/
#define ASM_COMMENT_START "# "

/*
A C string constant for text to be output before each asm
statement or group of consecutive ones.  Normally this is
"#APP", which is a comment that has no effect on most
assemblers but tells the GNU assembler that it must check the lines
that follow for all valid assembler constructs.
*/
#undef ASM_APP_ON
#define ASM_APP_ON "#APP\n"

/*
A C string constant for text to be output after each asm
statement or group of consecutive ones.  Normally this is
"#NO_APP", which tells the GNU assembler to resume making the
time-saving assumptions that are valid for ordinary compiler output.
*/
#undef ASM_APP_OFF
#define ASM_APP_OFF "#NO_APP\n"



#define FILE_ASM_OP 		"\t.file\n"
#define IDENT_ASM_OP 		"\t.ident\t"
#define SET_ASM_OP		"\t.set\t"


/*
 * Output assembly directives to switch to section name. The section
 * should have attributes as specified by flags, which is a bit mask
 * of the SECTION_* flags defined in 'output.h'. If align is nonzero,
 * it contains an alignment in bytes to be used for the section,
 * otherwise some target default should be used. Only targets that
 * must specify an alignment within the section directive need pay
 * attention to align -- we will still use ASM_OUTPUT_ALIGN.
 *
 * NOTE: This one must not be moved to avr32.c
 */
#undef TARGET_ASM_NAMED_SECTION
#define TARGET_ASM_NAMED_SECTION default_elf_asm_named_section


/*
You may define this macro as a C expression.  You should define the
expression to have a nonzero value if GCC should output the constant
pool for a function before the code for the function, or a zero value if
GCC should output the constant pool after the function.  If you do
not define this macro, the usual case, GCC will output the constant
pool before the function.
*/
#define CONSTANT_POOL_BEFORE_FUNCTION 0


/*
Define this macro as a C expression which is nonzero if the constant
EXP, of type tree, should be output after the code for a
function.  The compiler will normally output all constants before the
function; you need not define this macro if this is OK.
*/
#define CONSTANT_AFTER_FUNCTION_P(EXP) 1


/*
Define this macro as a C expression which is nonzero if C is
as a logical line separator by the assembler.  STR points to the
position in the string where C was found; this can be used if a
line separator uses multiple characters.

If you do not define this macro, the default is that only
the character ';' is treated as a logical line separator.
*/
#define IS_ASM_LOGICAL_LINE_SEPARATOR(C,STR) (((C) == '\n') || ((C) == ';'))


/** Output of Uninitialized Variables **/

/*
A C statement (sans semicolon) to output to the stdio stream
STREAM the assembler definition of a common-label named
NAME whose size is SIZE bytes.  The variable ROUNDED
is the size rounded up to whatever alignment the caller wants.

Use the expression assemble_name(STREAM, NAME) to
output the name itself; before and after that, output the additional
assembler syntax for defining the name, and a newline.

This macro controls how the assembler definitions of uninitialized
common global variables are output.
*/
/*
#define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED) \
  avr32_asm_output_common(STREAM, NAME, SIZE, ROUNDED)
*/

#define ASM_OUTPUT_COMMON(FILE, NAME, SIZE, ROUNDED)	\
  do							\
    {							\
      fputs ("\t.comm ", (FILE));			\
      assemble_name ((FILE), (NAME));			\
      fprintf ((FILE), ",%d\n", (SIZE));		\
    }							\
  while (0)

/*
 * Like ASM_OUTPUT_BSS except takes the required alignment as a
 * separate, explicit argument.  If you define this macro, it is used
 * in place of ASM_OUTPUT_BSS, and gives you more flexibility in
 * handling the required alignment of the variable.  The alignment is
 * specified as the number of bits.
 *
 * Try to use function asm_output_aligned_bss defined in file varasm.c
 * when defining this macro.
 */
#define ASM_OUTPUT_ALIGNED_BSS(STREAM, DECL, NAME, SIZE, ALIGNMENT) \
  asm_output_aligned_bss (STREAM, DECL, NAME, SIZE, ALIGNMENT)

/*
A C statement (sans semicolon) to output to the stdio stream
STREAM the assembler definition of a local-common-label named
NAME whose size is SIZE bytes.  The variable ROUNDED
is the size rounded up to whatever alignment the caller wants.

Use the expression assemble_name(STREAM, NAME) to
output the name itself; before and after that, output the additional
assembler syntax for defining the name, and a newline.

This macro controls how the assembler definitions of uninitialized
static variables are output.
*/
#define ASM_OUTPUT_LOCAL(FILE, NAME, SIZE, ROUNDED)	\
  do							\
    {							\
      fputs ("\t.lcomm ", (FILE));			\
      assemble_name ((FILE), (NAME));			\
      fprintf ((FILE), ",%d, %d\n", (SIZE), 2);		\
    }							\
  while (0)


/*
A C statement (sans semicolon) to output to the stdio stream
STREAM the assembler definition of a label named NAME.
Use the expression assemble_name(STREAM, NAME) to
output the name itself; before and after that, output the additional
assembler syntax for defining the name, and a newline.
*/
#define ASM_OUTPUT_LABEL(STREAM, NAME) avr32_asm_output_label(STREAM, NAME)

/* A C string containing the appropriate assembler directive to
 * specify the size of a symbol, without any arguments. On systems
 * that use ELF, the default (in 'config/elfos.h') is '"\t.size\t"';
 * on other systems, the default is not to define this macro.
 *
 * Define this macro only if it is correct to use the default
 * definitions of ASM_ OUTPUT_SIZE_DIRECTIVE and
 * ASM_OUTPUT_MEASURED_SIZE for your system. If you need your own
 * custom definitions of those macros, or if you do not need explicit
 * symbol sizes at all, do not define this macro.
 */
#define SIZE_ASM_OP "\t.size\t"


/*
A C statement (sans semicolon) to output to the stdio stream
STREAM some commands that will make the label NAME global;
that is, available for reference from other files.  Use the expression
assemble_name(STREAM, NAME) to output the name
itself; before and after that, output the additional assembler syntax
for making that name global, and a newline.
*/
#define GLOBAL_ASM_OP "\t.global\t"



/*
A C expression which evaluates to true if the target supports weak symbols.

If you don't define this macro, defaults.h provides a default
definition.  If either ASM_WEAKEN_LABEL or ASM_WEAKEN_DECL
is defined, the default definition is '1'; otherwise, it is
'0'.  Define this macro if you want to control weak symbol support
with a compiler flag such as -melf.
*/
#define SUPPORTS_WEAK 1

/*
A C statement (sans semicolon) to output to the stdio stream
STREAM a reference in assembler syntax to a label named
NAME.  This should add '_' to the front of the name, if that
is customary on your operating system, as it is in most Berkeley Unix
systems.  This macro is used in assemble_name.
*/
#define ASM_OUTPUT_LABELREF(STREAM, NAME) \
  avr32_asm_output_labelref(STREAM, NAME)



/*
A C expression to assign to OUTVAR (which is a variable of type
char *) a newly allocated string made from the string
NAME and the number NUMBER, with some suitable punctuation
added.  Use alloca to get space for the string.

The string will be used as an argument to ASM_OUTPUT_LABELREF to
produce an assembler label for an internal static variable whose name is
NAME.  Therefore, the string must be such as to result in valid
assembler code.  The argument NUMBER is different each time this
macro is executed; it prevents conflicts between similarly-named
internal static variables in different scopes.

Ideally this string should not be a valid C identifier, to prevent any
conflict with the user's own symbols.  Most assemblers allow periods
or percent signs in assembler symbols; putting at least one of these
between the name and the number will suffice.
*/
#define ASM_FORMAT_PRIVATE_NAME(OUTVAR, NAME, NUMBER)		\
  do								\
    {								\
      (OUTVAR) = (char *) alloca (strlen ((NAME)) + 10);	\
      sprintf ((OUTVAR), "%s.%d", (NAME), (NUMBER));		\
    }								\
  while (0)


/** Macros Controlling Initialization Routines **/


/*
If defined, main will not call __main as described above.
This macro should be defined for systems that control start-up code
on a symbol-by-symbol basis, such as OSF/1, and should not
be defined explicitly for systems that support INIT_SECTION_ASM_OP.
*/
/*
  __main is not defined when debugging.
*/
#define HAS_INIT_SECTION


/** Output of Assembler Instructions **/

/*
A C initializer containing the assembler's names for the machine
registers, each one as a C string constant.  This is what translates
register numbers in the compiler into assembler language.
*/

#define REGISTER_NAMES	\
{			\
  "pc",  "lr",		\
  "sp",  "r12",		\
  "r11", "r10",		\
  "r9",  "r8",		\
  "r7",  "r6",		\
  "r5",  "r4",		\
  "r3",  "r2",		\
  "r1",  "r0",		\
}

/*
A C compound statement to output to stdio stream STREAM the
assembler syntax for an instruction operand X.  X is an
RTL expression.

CODE is a value that can be used to specify one of several ways
of printing the operand.  It is used when identical operands must be
printed differently depending on the context.  CODE comes from
the '%' specification that was used to request printing of the
operand.  If the specification was just '%digit' then
CODE is 0; if the specification was '%ltr digit'
then CODE is the ASCII code for ltr.

If X is a register, this macro should print the register's name.
The names can be found in an array reg_names whose type is
char *[].  reg_names is initialized from REGISTER_NAMES.

When the machine description has a specification '%punct'
(a '%' followed by a punctuation character), this macro is called
with a null pointer for X and the punctuation character for
CODE.
*/
#define PRINT_OPERAND(STREAM, X, CODE) avr32_print_operand(STREAM, X, CODE)

/* A C statement to be executed just prior to the output of
   assembler code for INSN, to modify the extracted operands so
   they will be output differently.

   Here the argument OPVEC is the vector containing the operands
   extracted from INSN, and NOPERANDS is the number of elements of
   the vector which contain meaningful data for this insn.
   The contents of this vector are what will be used to convert the insn
   template into assembler code, so you can change the assembler output
   by changing the contents of the vector.  */
#define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) \
  avr32_final_prescan_insn ((INSN), (OPVEC), (NOPERANDS))

/*
A C expression which evaluates to true if CODE is a valid
punctuation character for use in the PRINT_OPERAND macro.  If
PRINT_OPERAND_PUNCT_VALID_P is not defined, it means that no
punctuation characters (except for the standard one, '%') are used
in this way.
*/
#define PRINT_OPERAND_PUNCT_VALID_P(CODE)                          \
  (((CODE) == '?')                                                 \
   || ((CODE) == '!'))

/*
A C compound statement to output to stdio stream STREAM the
assembler syntax for an instruction operand that is a memory reference
whose address is X.  X is an RTL expression.

On some machines, the syntax for a symbolic address depends on the
section that the address refers to.  On these machines, define the macro
ENCODE_SECTION_INFO to store the information into the
symbol_ref, and then check for it here.  (see Assembler Format.)
*/
#define PRINT_OPERAND_ADDRESS(STREAM, X) avr32_print_operand_address(STREAM, X)


/** Output of Dispatch Tables **/

/*
 * A C statement to output to the stdio stream stream an assembler
 * pseudo-instruction to generate a difference between two
 * labels. value and rel are the numbers of two internal labels. The
 * definitions of these labels are output using
 * (*targetm.asm_out.internal_label), and they must be printed in the
 * same way here. For example,
 *
 *         fprintf (stream, "\t.word L%d-L%d\n",
 *                  value, rel)
 *
 * You must provide this macro on machines where the addresses in a
 * dispatch table are relative to the table's own address. If defined,
 * GCC will also use this macro on all machines when producing
 * PIC. body is the body of the ADDR_DIFF_VEC; it is provided so that
 * the mode and flags can be read.
 */
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, BODY, VALUE, REL)	    \
    fprintf(STREAM, "\tbral\t%sL%d\n", LOCAL_LABEL_PREFIX, VALUE)

/*
This macro should be provided on machines where the addresses
in a dispatch table are absolute.

The definition should be a C statement to output to the stdio stream
STREAM an assembler pseudo-instruction to generate a reference to
a label.  VALUE is the number of an internal label whose
definition is output using ASM_OUTPUT_INTERNAL_LABEL.
For example,

fprintf(STREAM, "\t.word L%d\n", VALUE)
*/

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE)  \
  fprintf(STREAM, "\t.long %sL%d\n", LOCAL_LABEL_PREFIX, VALUE)

/** Assembler Commands for Exception Regions */

/* ToDo: All of this subsection */

/** Assembler Commands for Alignment */


/*
A C statement to output to the stdio stream STREAM an assembler
command to advance the location counter to a multiple of 2 to the
POWER bytes.  POWER will be a C expression of type int.
*/
#define ASM_OUTPUT_ALIGN(STREAM, POWER)			\
  do							\
    {							\
      if ((POWER) != 0)					\
	fprintf(STREAM, "\t.align\t%d\n", POWER);	\
    }							\
  while (0)

/*
Like ASM_OUTPUT_ALIGN, except that the \nop" instruction is used for padding, if
necessary.
*/
#define ASM_OUTPUT_ALIGN_WITH_NOP(STREAM, POWER) \
 fprintf(STREAM, "\t.balignw\t%d, 0xd703\n", (1 << POWER))



/******************************************************************************
 * Controlling Debugging Information Format
 *****************************************************************************/

/* How to renumber registers for dbx and gdb.  */
#define DBX_REGISTER_NUMBER(REGNO) ASM_REGNUM (REGNO)

/* The DWARF 2 CFA column which tracks the return address.  */
#define DWARF_FRAME_RETURN_COLUMN DWARF_FRAME_REGNUM(LR_REGNUM)

/*
Define this macro if GCC should produce dwarf version 2 format
debugging output in response to the -g option.

To support optional call frame debugging information, you must also
define INCOMING_RETURN_ADDR_RTX and either set
RTX_FRAME_RELATED_P on the prologue insns if you use RTL for the
prologue, or call dwarf2out_def_cfa and dwarf2out_reg_save
as appropriate from TARGET_ASM_FUNCTION_PROLOGUE if you don't.
*/
#define DWARF2_DEBUGGING_INFO 1


#define DWARF2_ASM_LINE_DEBUG_INFO 1
#define DWARF2_FRAME_INFO 1


/******************************************************************************
 * Miscellaneous Parameters
 *****************************************************************************/

/* ToDo: a lot */

/*
An alias for a machine mode name.  This is the machine mode that
elements of a jump-table should have.
*/
#define CASE_VECTOR_MODE SImode

/*
Define this macro to be a C expression to indicate when jump-tables
should contain relative addresses.  If jump-tables never contain
relative addresses, then you need not define this macro.
*/
#define CASE_VECTOR_PC_RELATIVE 0

/* Increase the threshold for using table jumps on the UC arch. */
#define CASE_VALUES_THRESHOLD  (TARGET_BRANCH_PRED ? 4 : 7)

/*
The maximum number of bytes that a single instruction can move quickly
between memory and registers or between two memory locations.
*/
#define MOVE_MAX (2*UNITS_PER_WORD)


/* A C expression that is nonzero if on this machine the number of bits actually used
   for the count of a shift operation is equal to the number of bits needed to represent
   the size of the object being shifted. When this macro is nonzero, the compiler will
   assume that it is safe to omit a sign-extend, zero-extend, and certain bitwise 'and'
   instructions that truncates the count of a shift operation. On machines that have
   instructions that act on bit-fields at variable positions, which may include 'bit test'
   378 GNU Compiler Collection (GCC) Internals
   instructions, a nonzero SHIFT_COUNT_TRUNCATED also enables deletion of truncations
   of the values that serve as arguments to bit-field instructions.
   If both types of instructions truncate the count (for shifts) and position (for bit-field
   operations), or if no variable-position bit-field instructions exist, you should define
   this macro.
   However, on some machines, such as the 80386 and the 680x0, truncation only applies
   to shift operations and not the (real or pretended) bit-field operations. Define SHIFT_
   COUNT_TRUNCATED to be zero on such machines. Instead, add patterns to the 'md' file
   that include the implied truncation of the shift instructions.
   You need not dene this macro if it would always have the value of zero. */
#define SHIFT_COUNT_TRUNCATED 1

/*
A C expression which is nonzero if on this machine it is safe to
convert an integer of INPREC bits to one of OUTPREC
bits (where OUTPREC is smaller than INPREC) by merely
operating on it as if it had only OUTPREC bits.

On many machines, this expression can be 1.

When TRULY_NOOP_TRUNCATION returns 1 for a pair of sizes for
modes for which MODES_TIEABLE_P is 0, suboptimal code can result.
If this is the case, making TRULY_NOOP_TRUNCATION return 0 in
such cases may improve things.
*/
#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC) 1

/*
An alias for the machine mode for pointers.  On most machines, define
this to be the integer mode corresponding to the width of a hardware
pointer; SImode on 32-bit machine or DImode on 64-bit machines.
On some machines you must define this to be one of the partial integer
modes, such as PSImode.

The width of Pmode must be at least as large as the value of
POINTER_SIZE.  If it is not equal, you must define the macro
POINTERS_EXTEND_UNSIGNED to specify how pointers are extended
to Pmode.
*/
#define Pmode SImode

/*
An alias for the machine mode used for memory references to functions
being called, in call RTL expressions.  On most machines this
should be QImode.
*/
#define FUNCTION_MODE SImode


#define REG_S_P(x) \
 (REG_P (x) || (GET_CODE (x) == SUBREG && REG_P (XEXP (x, 0))))


/* If defined, modifies the length assigned to instruction INSN as a
   function of the context in which it is used.  LENGTH is an lvalue
   that contains the initially computed length of the insn and should
   be updated with the correct length of the insn.  */
#define ADJUST_INSN_LENGTH(INSN, LENGTH) \
  ((LENGTH) = avr32_adjust_insn_length ((INSN), (LENGTH)))


#define CLZ_DEFINED_VALUE_AT_ZERO(mode, value) \
  (value = 32, (mode == SImode))

#define CTZ_DEFINED_VALUE_AT_ZERO(mode, value) \
  (value = 32, (mode == SImode))

#define UNITS_PER_SIMD_WORD(mode) UNITS_PER_WORD

#define STORE_FLAG_VALUE 1


/* IF-conversion macros. */
#define IFCVT_MODIFY_INSN( CE_INFO, PATTERN, INSN )                     \
  {                                                                     \
    (PATTERN) = avr32_ifcvt_modify_insn (CE_INFO, PATTERN, INSN, &num_true_changes); \
  }

#define IFCVT_EXTRA_FIELDS                              \
  int num_cond_clobber_insns;                           \
  int num_extra_move_insns;                             \
  rtx extra_move_insns[MAX_CONDITIONAL_EXECUTE];        \
  rtx moved_insns[MAX_CONDITIONAL_EXECUTE];

#define IFCVT_INIT_EXTRA_FIELDS( CE_INFO )       \
  {                                              \
    (CE_INFO)->num_cond_clobber_insns = 0;       \
    (CE_INFO)->num_extra_move_insns = 0;         \
  }


#define IFCVT_MODIFY_CANCEL( CE_INFO )  avr32_ifcvt_modify_cancel (CE_INFO, &num_true_changes)

#define IFCVT_ALLOW_MODIFY_TEST_IN_INSN 1
#define IFCVT_COND_EXEC_BEFORE_RELOAD (TARGET_COND_EXEC_BEFORE_RELOAD)

enum avr32_builtins
{
  AVR32_BUILTIN_MTSR,
  AVR32_BUILTIN_MFSR,
  AVR32_BUILTIN_MTDR,
  AVR32_BUILTIN_MFDR,
  AVR32_BUILTIN_CACHE,
  AVR32_BUILTIN_SYNC,
  AVR32_BUILTIN_SSRF,
  AVR32_BUILTIN_CSRF,
  AVR32_BUILTIN_TLBR,
  AVR32_BUILTIN_TLBS,
  AVR32_BUILTIN_TLBW,
  AVR32_BUILTIN_BREAKPOINT,
  AVR32_BUILTIN_XCHG,
  AVR32_BUILTIN_LDXI,
  AVR32_BUILTIN_BSWAP16,
  AVR32_BUILTIN_BSWAP32,
  AVR32_BUILTIN_COP,
  AVR32_BUILTIN_MVCR_W,
  AVR32_BUILTIN_MVRC_W,
  AVR32_BUILTIN_MVCR_D,
  AVR32_BUILTIN_MVRC_D,
  AVR32_BUILTIN_MULSATHH_H,
  AVR32_BUILTIN_MULSATHH_W,
  AVR32_BUILTIN_MULSATRNDHH_H,
  AVR32_BUILTIN_MULSATRNDWH_W,
  AVR32_BUILTIN_MULSATWH_W,
  AVR32_BUILTIN_MACSATHH_W,
  AVR32_BUILTIN_SATADD_H,
  AVR32_BUILTIN_SATSUB_H,
  AVR32_BUILTIN_SATADD_W,
  AVR32_BUILTIN_SATSUB_W,
  AVR32_BUILTIN_MULWH_D,
  AVR32_BUILTIN_MULNWH_D,
  AVR32_BUILTIN_MACWH_D,
  AVR32_BUILTIN_MACHH_D,
  AVR32_BUILTIN_MUSFR,
  AVR32_BUILTIN_MUSTR,
  AVR32_BUILTIN_SATS,
  AVR32_BUILTIN_SATU,
  AVR32_BUILTIN_SATRNDS,
  AVR32_BUILTIN_SATRNDU,
  AVR32_BUILTIN_MEMS,
  AVR32_BUILTIN_MEMC,
  AVR32_BUILTIN_MEMT,
  AVR32_BUILTIN_SLEEP,
  AVR32_BUILTIN_DELAY_CYCLES
};


#define FLOAT_LIB_COMPARE_RETURNS_BOOL(MODE, COMPARISON) \
  ((MODE == SFmode) || (MODE == DFmode))

#define RENAME_LIBRARY_SET ".set"

/* Make ABI_NAME an alias for __GCC_NAME.  */
#define RENAME_LIBRARY(GCC_NAME, ABI_NAME)		\
  __asm__ (".globl\t__avr32_" #ABI_NAME "\n"		\
	   ".set\t__avr32_" #ABI_NAME 	\
	     ", __" #GCC_NAME "\n");

/* Give libgcc functions avr32 ABI name.  */
#ifdef L_muldi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (muldi3, mul64)
#endif
#ifdef L_divdi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (divdi3, sdiv64)
#endif
#ifdef L_udivdi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (udivdi3, udiv64)
#endif
#ifdef L_moddi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (moddi3, smod64)
#endif
#ifdef L_umoddi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (umoddi3, umod64)
#endif
#ifdef L_ashldi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (ashldi3, lsl64)
#endif
#ifdef L_lshrdi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (lshrdi3, lsr64)
#endif
#ifdef L_ashrdi3
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (ashrdi3, asr64)
#endif

#ifdef L_fixsfdi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixsfdi, f32_to_s64)
#endif
#ifdef L_fixunssfdi
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (fixunssfdi, f32_to_u64)
#endif
#ifdef L_floatdidf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatdidf, s64_to_f64)
#endif
#ifdef L_floatdisf
#define DECLARE_LIBRARY_RENAMES RENAME_LIBRARY (floatdisf, s64_to_f32)
#endif

#endif
