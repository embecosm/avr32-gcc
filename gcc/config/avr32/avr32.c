/*
   Target hooks and helper functions for AVR32.
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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "rtl.h"
#include "tree.h"
#include "obstack.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "real.h"
#include "insn-config.h"
#include "conditions.h"
#include "output.h"
#include "insn-attr.h"
#include "flags.h"
#include "reload.h"
#include "function.h"
#include "expr.h"
#include "optabs.h"
#include "toplev.h"
#include "recog.h"
#include "ggc.h"
#include "except.h"
#include "c-pragma.h"
#include "integrate.h"
#include "tm_p.h"
#include "langhooks.h"
#include "hooks.h"
#include "df.h"

#include "target.h"
#include "target-def.h"

#include <ctype.h>



/* Global variables.  */
typedef struct minipool_node Mnode;
typedef struct minipool_fixup Mfix;

/* Obstack for minipool constant handling.  */
static struct obstack minipool_obstack;
static char *minipool_startobj;
static rtx minipool_vector_label;

/* True if we are currently building a constant table.  */
int making_const_table;

tree fndecl_attribute_args = NULL_TREE;


/* Function prototypes. */
static unsigned long avr32_isr_value (tree);
static unsigned long avr32_compute_func_type (void);
static tree avr32_handle_isr_attribute (tree *, tree, tree, int, bool *);
static tree avr32_handle_acall_attribute (tree *, tree, tree, int, bool *);
static tree avr32_handle_fndecl_attribute (tree * node, tree name, tree args,
					   int flags, bool * no_add_attrs);
static void avr32_reorg (void);
bool avr32_return_in_msb (tree type);
bool avr32_vector_mode_supported (enum machine_mode mode);
static void avr32_init_libfuncs (void);
static void avr32_file_end (void);
static void flashvault_decl_list_add (unsigned int vector_num, const char *name);



static void
avr32_add_gc_roots (void)
{
  gcc_obstack_init (&minipool_obstack);
  minipool_startobj = (char *) obstack_alloc (&minipool_obstack, 0);
}


/* List of all known AVR32 parts  */
static const struct part_type_s avr32_part_types[] = {
  /* name, part_type, architecture type, macro */
  {"none",         PART_TYPE_AVR32_NONE,         ARCH_TYPE_AVR32_AP,        "__AVR32__"},
  {"ap7000",       PART_TYPE_AVR32_AP7000,       ARCH_TYPE_AVR32_AP,        "__AVR32_AP7000__"},
  {"ap7001",       PART_TYPE_AVR32_AP7001,       ARCH_TYPE_AVR32_AP,        "__AVR32_AP7001__"},
  {"ap7002",       PART_TYPE_AVR32_AP7002,       ARCH_TYPE_AVR32_AP,        "__AVR32_AP7002__"},
  {"ap7200",       PART_TYPE_AVR32_AP7200,       ARCH_TYPE_AVR32_AP,        "__AVR32_AP7200__"},
  {"uc3a0128",     PART_TYPE_AVR32_UC3A0128,     ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3A0128__"},
  {"uc3a0256",     PART_TYPE_AVR32_UC3A0256,     ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3A0256__"},
  {"uc3a0512",     PART_TYPE_AVR32_UC3A0512,     ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3A0512__"},
  {"uc3a0512es",   PART_TYPE_AVR32_UC3A0512ES,   ARCH_TYPE_AVR32_UCR1,      "__AVR32_UC3A0512ES__"},
  {"uc3a1128",     PART_TYPE_AVR32_UC3A1128,     ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3A1128__"},
  {"uc3a1256",     PART_TYPE_AVR32_UC3A1256,     ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3A1256__"},
  {"uc3a1512",     PART_TYPE_AVR32_UC3A1512,     ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3A1512__"},
  {"uc3a1512es",   PART_TYPE_AVR32_UC3A1512ES,   ARCH_TYPE_AVR32_UCR1,      "__AVR32_UC3A1512ES__"},
  {"uc3a3revd",    PART_TYPE_AVR32_UC3A3REVD,    ARCH_TYPE_AVR32_UCR2NOMUL, "__AVR32_UC3A3256S__"},
  {"uc3a364",      PART_TYPE_AVR32_UC3A364,      ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3A364__"},
  {"uc3a364s",     PART_TYPE_AVR32_UC3A364S,     ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3A364S__"},
  {"uc3a3128",     PART_TYPE_AVR32_UC3A3128,     ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3A3128__"},
  {"uc3a3128s",    PART_TYPE_AVR32_UC3A3128S,    ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3A3128S__"},
  {"uc3a3256",     PART_TYPE_AVR32_UC3A3256,     ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3A3256__"},
  {"uc3a3256s",    PART_TYPE_AVR32_UC3A3256S,    ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3A3256S__"},
  {"uc3b064",      PART_TYPE_AVR32_UC3B064,      ARCH_TYPE_AVR32_UCR1,      "__AVR32_UC3B064__"},
  {"uc3b0128",     PART_TYPE_AVR32_UC3B0128,     ARCH_TYPE_AVR32_UCR1,      "__AVR32_UC3B0128__"},
  {"uc3b0256",     PART_TYPE_AVR32_UC3B0256,     ARCH_TYPE_AVR32_UCR1,      "__AVR32_UC3B0256__"},
  {"uc3b0256es",   PART_TYPE_AVR32_UC3B0256ES,   ARCH_TYPE_AVR32_UCR1,      "__AVR32_UC3B0256ES__"},
  {"uc3b0512",     PART_TYPE_AVR32_UC3B0512,     ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3B0512__"},
  {"uc3b0512revc", PART_TYPE_AVR32_UC3B0512REVC, ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3B0512REVC__"},
  {"uc3b164",      PART_TYPE_AVR32_UC3B164,      ARCH_TYPE_AVR32_UCR1,      "__AVR32_UC3B164__"},
  {"uc3b1128",     PART_TYPE_AVR32_UC3B1128,     ARCH_TYPE_AVR32_UCR1,      "__AVR32_UC3B1128__"},
  {"uc3b1256",     PART_TYPE_AVR32_UC3B1256,     ARCH_TYPE_AVR32_UCR1,      "__AVR32_UC3B1256__"},
  {"uc3b1256es",   PART_TYPE_AVR32_UC3B1256ES,   ARCH_TYPE_AVR32_UCR1,      "__AVR32_UC3B1256ES__"},
  {"uc3b1512",     PART_TYPE_AVR32_UC3B1512,     ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3B1512__"},
  {"uc3b1512revc", PART_TYPE_AVR32_UC3B1512REVC, ARCH_TYPE_AVR32_UCR2,      "__AVR32_UC3B1512REVC__"},
  {"uc3c0512crevc",PART_TYPE_AVR32_UC3C0512C,    ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3C0512CREVC__"},
  {"uc3c0256c",    PART_TYPE_AVR32_UC3C0256C,    ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3C0256C__"},
  {"uc3c0128c",    PART_TYPE_AVR32_UC3C0128C,    ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3C0128C__"},
  {"uc3c064c",     PART_TYPE_AVR32_UC3C064C,     ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3C064C__"},
  {"uc3c1512crevc",PART_TYPE_AVR32_UC3C1512C,    ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3C1512CREVC__"},
  {"uc3c1256c",    PART_TYPE_AVR32_UC3C1256C,    ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3C1256C__"},
  {"uc3c1128c",    PART_TYPE_AVR32_UC3C1128C,    ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3C1128C__"},
  {"uc3c164c",     PART_TYPE_AVR32_UC3C164C,     ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3C164C__"},
  {"uc3c2512crevc",PART_TYPE_AVR32_UC3C2512C,    ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3C2512CREVC__"},
  {"uc3c2256c",    PART_TYPE_AVR32_UC3C2256C,    ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3C2256C__"},
  {"uc3c2128c",    PART_TYPE_AVR32_UC3C2128C,    ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3C2128C__"},
  {"uc3c264c",     PART_TYPE_AVR32_UC3C264C,     ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3C264C__"},
  {"uc3l0256",     PART_TYPE_AVR32_UC3L0256,     ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3L0256__"},
  {"uc3l064",      PART_TYPE_AVR32_UC3L064,      ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3L064__"},
  {"uc3l032",      PART_TYPE_AVR32_UC3L032,      ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3L032__"},
  {"uc3l016",      PART_TYPE_AVR32_UC3L016,      ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3L016__"},
  {"uc3l064revb",  PART_TYPE_AVR32_UC3L064,      ARCH_TYPE_AVR32_UCR3,      "__AVR32_UC3L064REVB__"},
  {NULL, 0, 0, NULL}
};

/* List of all known AVR32 architectures  */
static const struct arch_type_s avr32_arch_types[] = {
  /* name, architecture type, microarchitecture type, feature flags, macro */
  {"ap", ARCH_TYPE_AVR32_AP, UARCH_TYPE_AVR32B,
   (FLAG_AVR32_HAS_DSP
    | FLAG_AVR32_HAS_SIMD
    | FLAG_AVR32_HAS_UNALIGNED_WORD
    | FLAG_AVR32_HAS_BRANCH_PRED | FLAG_AVR32_HAS_RETURN_STACK
    | FLAG_AVR32_HAS_CACHES),
   "__AVR32_AP__"},
  {"ucr1", ARCH_TYPE_AVR32_UCR1, UARCH_TYPE_AVR32A,
   (FLAG_AVR32_HAS_DSP | FLAG_AVR32_HAS_RMW),
   "__AVR32_UC__=1"},
  {"ucr2", ARCH_TYPE_AVR32_UCR2, UARCH_TYPE_AVR32A,
   (FLAG_AVR32_HAS_DSP | FLAG_AVR32_HAS_RMW
    | FLAG_AVR32_HAS_V2_INSNS),
   "__AVR32_UC__=2"},
  {"ucr2nomul", ARCH_TYPE_AVR32_UCR2NOMUL, UARCH_TYPE_AVR32A,
   (FLAG_AVR32_HAS_DSP | FLAG_AVR32_HAS_RMW
    | FLAG_AVR32_HAS_V2_INSNS | FLAG_AVR32_HAS_NO_MUL_INSNS),
   "__AVR32_UC__=2"},
  {"ucr3", ARCH_TYPE_AVR32_UCR3, UARCH_TYPE_AVR32A,
   (FLAG_AVR32_HAS_DSP | FLAG_AVR32_HAS_RMW
    | FLAG_AVR32_HAS_V2_INSNS),
   "__AVR32_UC__=3"},
  {"ucr3fp", ARCH_TYPE_AVR32_UCR3FP, UARCH_TYPE_AVR32A,
   (FLAG_AVR32_HAS_DSP | FLAG_AVR32_HAS_RMW | FLAG_AVR32_HAS_FPU
    | FLAG_AVR32_HAS_V2_INSNS),
   "__AVR32_UC__=3"},
  {NULL, 0, 0, 0, NULL}
};

/* Default arch name */
const char *avr32_arch_name = "none";
const char *avr32_part_name = "none";

const struct part_type_s *avr32_part;
const struct arch_type_s *avr32_arch;


/* FIXME: needs to use GC.  */
struct flashvault_decl_list
{
  struct flashvault_decl_list *next;
  unsigned int vector_num;
  const char *name;
};

static struct flashvault_decl_list *flashvault_decl_list_head = NULL;


/* Set default target_flags. */
#undef TARGET_DEFAULT_TARGET_FLAGS
#define TARGET_DEFAULT_TARGET_FLAGS \
  (MASK_HAS_ASM_ADDR_PSEUDOS | MASK_MD_REORG_OPTIMIZATION | MASK_COND_EXEC_BEFORE_RELOAD)

void
avr32_optimization_options (int level, int size)
{
  if (AVR32_ALWAYS_PIC)
    flag_pic = 1;

  /* Enable section anchors if optimization is enabled. */
  if (level > 0 || size)
    flag_section_anchors = 1;
}


/* Override command line options */
void
avr32_override_options (void)
{
  const struct part_type_s *part;
  const struct arch_type_s *arch;

  /*Add backward compability*/
  if (strcmp ("uc", avr32_arch_name)== 0)
    {
      fprintf (stderr, "Warning: Deprecated arch `%s' specified. "
                       "Please use '-march=ucr1' instead. "
                       "Converting to arch 'ucr1'\n",
               avr32_arch_name);
      avr32_arch_name="ucr1";
    }

  /* Check if arch type is set. */
  for (arch = avr32_arch_types; arch->name; arch++)
    {
      if (strcmp (arch->name, avr32_arch_name) == 0)
        break;
    }
  avr32_arch = arch;

  if (!arch->name && strcmp("none", avr32_arch_name) != 0)
    {
      fprintf (stderr, "Unknown arch `%s' specified\n"
                       "Known arch names:\n"
                       "\tuc (deprecated)\n",
               avr32_arch_name);
      for (arch = avr32_arch_types; arch->name; arch++)
        fprintf (stderr, "\t%s\n", arch->name);
      avr32_arch = &avr32_arch_types[ARCH_TYPE_AVR32_AP];
    }

  /* Check if part type is set. */
  for (part = avr32_part_types; part->name; part++)
    if (strcmp (part->name, avr32_part_name) == 0)
      break;

  avr32_part = part;
  if (!part->name)
    {
      fprintf (stderr, "Unknown part `%s' specified\nKnown part names:\n",
               avr32_part_name);
      for (part = avr32_part_types; part->name; part++)
        {
          if (strcmp("none", part->name) != 0)
            fprintf (stderr, "\t%s\n", part->name);
        }
      /* Set default to NONE*/
      avr32_part = &avr32_part_types[PART_TYPE_AVR32_NONE];
    }

  /* NB! option -march= overrides option -mpart
   * if both are used at the same time */
  if (!arch->name)
    avr32_arch = &avr32_arch_types[avr32_part->arch_type];

  /* If optimization level is two or greater, then align start of loops to a
     word boundary since this will allow folding the first insn of the loop.
     Do this only for targets supporting branch prediction. */
  if (optimize >= 2 && TARGET_BRANCH_PRED)
    align_loops = 2;


  /* Enable fast-float library if unsafe math optimizations
     are used. */
  if (flag_unsafe_math_optimizations)
    target_flags |= MASK_FAST_FLOAT;

  /* Check if we should set avr32_imm_in_const_pool
     based on if caches are present or not. */
  if ( avr32_imm_in_const_pool == -1 )
    {
      if ( TARGET_CACHES )
        avr32_imm_in_const_pool = 1;
      else
        avr32_imm_in_const_pool = 0;
    }

  if (TARGET_NO_PIC)
    flag_pic = 0;
  avr32_add_gc_roots ();
}


/*
If defined, a function that outputs the assembler code for entry to a
function.  The prologue is responsible for setting up the stack frame,
initializing the frame pointer register, saving registers that must be
saved, and allocating size additional bytes of storage for the
local variables.  size is an integer.  file is a stdio
stream to which the assembler code should be output.

The label for the beginning of the function need not be output by this
macro.  That has already been done when the macro is run.

To determine which registers to save, the macro can refer to the array
regs_ever_live: element r is nonzero if hard register
r is used anywhere within the function.  This implies the function
prologue should save register r, provided it is not one of the
call-used registers.  (TARGET_ASM_FUNCTION_EPILOGUE must likewise use
regs_ever_live.)

On machines that have ``register windows'', the function entry code does
not save on the stack the registers that are in the windows, even if
they are supposed to be preserved by function calls; instead it takes
appropriate steps to ``push'' the register stack, if any non-call-used
registers are used in the function.

On machines where functions may or may not have frame-pointers, the
function entry code must vary accordingly; it must set up the frame
pointer if one is wanted, and not otherwise.  To determine whether a
frame pointer is in wanted, the macro can refer to the variable
frame_pointer_needed.  The variable's value will be 1 at run
time in a function that needs a frame pointer.  (see Elimination).

The function entry code is responsible for allocating any stack space
required for the function.  This stack space consists of the regions
listed below.  In most cases, these regions are allocated in the
order listed, with the last listed region closest to the top of the
stack (the lowest address if STACK_GROWS_DOWNWARD is defined, and
the highest address if it is not defined).  You can use a different order
for a machine if doing so is more convenient or required for
compatibility reasons.  Except in cases where required by standard
or by a debugger, there is no reason why the stack layout used by GCC
need agree with that used by other compilers for a machine.
*/

#undef TARGET_ASM_FUNCTION_PROLOGUE
#define TARGET_ASM_FUNCTION_PROLOGUE avr32_target_asm_function_prologue

#undef TARGET_ASM_FILE_END
#define TARGET_ASM_FILE_END avr32_file_end

#undef TARGET_DEFAULT_SHORT_ENUMS
#define TARGET_DEFAULT_SHORT_ENUMS hook_bool_void_false

#undef TARGET_PROMOTE_FUNCTION_ARGS
#define TARGET_PROMOTE_FUNCTION_ARGS hook_bool_tree_true

#undef TARGET_PROMOTE_FUNCTION_RETURN
#define TARGET_PROMOTE_FUNCTION_RETURN hook_bool_tree_true

#undef TARGET_PROMOTE_PROTOTYPES
#define TARGET_PROMOTE_PROTOTYPES hook_bool_tree_true

#undef TARGET_MUST_PASS_IN_STACK
#define TARGET_MUST_PASS_IN_STACK avr32_must_pass_in_stack

#undef TARGET_PASS_BY_REFERENCE
#define TARGET_PASS_BY_REFERENCE avr32_pass_by_reference

#undef TARGET_STRICT_ARGUMENT_NAMING
#define TARGET_STRICT_ARGUMENT_NAMING avr32_strict_argument_naming

#undef TARGET_VECTOR_MODE_SUPPORTED_P
#define TARGET_VECTOR_MODE_SUPPORTED_P avr32_vector_mode_supported

#undef TARGET_RETURN_IN_MEMORY
#define TARGET_RETURN_IN_MEMORY avr32_return_in_memory

#undef TARGET_RETURN_IN_MSB
#define TARGET_RETURN_IN_MSB avr32_return_in_msb

#undef TARGET_ENCODE_SECTION_INFO
#define TARGET_ENCODE_SECTION_INFO avr32_encode_section_info

#undef TARGET_ARG_PARTIAL_BYTES
#define TARGET_ARG_PARTIAL_BYTES avr32_arg_partial_bytes

#undef TARGET_STRIP_NAME_ENCODING
#define TARGET_STRIP_NAME_ENCODING avr32_strip_name_encoding

#define streq(string1, string2) (strcmp (string1, string2) == 0)

#undef  TARGET_NARROW_VOLATILE_BITFIELD
#define TARGET_NARROW_VOLATILE_BITFIELD hook_bool_void_false

#undef  TARGET_ATTRIBUTE_TABLE
#define TARGET_ATTRIBUTE_TABLE avr32_attribute_table

#undef  TARGET_COMP_TYPE_ATTRIBUTES
#define TARGET_COMP_TYPE_ATTRIBUTES avr32_comp_type_attributes


#undef  TARGET_RTX_COSTS
#define TARGET_RTX_COSTS avr32_rtx_costs

#undef  TARGET_CANNOT_FORCE_CONST_MEM
#define  TARGET_CANNOT_FORCE_CONST_MEM avr32_cannot_force_const_mem

#undef  TARGET_ASM_INTEGER
#define TARGET_ASM_INTEGER avr32_assemble_integer

#undef  TARGET_FUNCTION_VALUE
#define TARGET_FUNCTION_VALUE avr32_function_value

#undef  TARGET_MIN_ANCHOR_OFFSET
#define TARGET_MIN_ANCHOR_OFFSET (0)

#undef  TARGET_MAX_ANCHOR_OFFSET
#define TARGET_MAX_ANCHOR_OFFSET ((1 << 15) - 1)
#undef TARGET_SECONDARY_RELOAD
#define TARGET_SECONDARY_RELOAD avr32_secondary_reload


/*
 * Defining the option, -mlist-devices to list the devices supported by gcc.
 * This option should be used while printing target-help to list all the 
 * supported devices.
 */
#undef TARGET_HELP
#define TARGET_HELP avr32_target_help

void avr32_target_help ()
{
  if (avr32_list_supported_parts)
    {
      const struct part_type_s *list;
      fprintf (stdout, "List of parts supported by avr32-gcc:\n");
      for (list = avr32_part_types; list->name; list++)
        {
          if (strcmp("none", list->name) != 0)
            fprintf (stdout, "%-20s%s\n", list->name, list->macro);
        }
      fprintf (stdout, "\n\n");
    }
}

enum reg_class
avr32_secondary_reload (bool in_p, rtx x, enum reg_class class,
                        enum machine_mode mode, secondary_reload_info *sri)
{

  if ( avr32_rmw_memory_operand (x, mode) )
    {
      if (!in_p)
        sri->icode = CODE_FOR_reload_out_rmw_memory_operand;
      else
        sri->icode = CODE_FOR_reload_in_rmw_memory_operand;
    }
  return NO_REGS;

}
/*
 * Switches to the appropriate section for output of constant pool
 * entry x in mode. You can assume that x is some kind of constant in
 * RTL. The argument mode is redundant except in the case of a
 * const_int rtx. Select the section by calling readonly_data_ section
 * or one of the alternatives for other sections. align is the
 * constant alignment in bits.
 *
 * The default version of this function takes care of putting symbolic
 * constants in flag_ pic mode in data_section and everything else in
 * readonly_data_section.
 */
//#undef TARGET_ASM_SELECT_RTX_SECTION
//#define TARGET_ASM_SELECT_RTX_SECTION avr32_select_rtx_section


/*
 * If non-null, this hook performs a target-specific pass over the
 * instruction stream. The compiler will run it at all optimization
 * levels, just before the point at which it normally does
 * delayed-branch scheduling.
 *
 * The exact purpose of the hook varies from target to target. Some
 * use it to do transformations that are necessary for correctness,
 * such as laying out in-function constant pools or avoiding hardware
 * hazards. Others use it as an opportunity to do some
 * machine-dependent optimizations.
 *
 * You need not implement the hook if it has nothing to do. The
 * default definition is null.
 */
#undef TARGET_MACHINE_DEPENDENT_REORG
#define TARGET_MACHINE_DEPENDENT_REORG avr32_reorg

/* Target hook for assembling integer objects.
   Need to handle integer vectors */
static bool
avr32_assemble_integer (rtx x, unsigned int size, int aligned_p)
{
  if (avr32_vector_mode_supported (GET_MODE (x)))
    {
      int i, units;

      if (GET_CODE (x) != CONST_VECTOR)
	abort ();

      units = CONST_VECTOR_NUNITS (x);

      switch (GET_MODE (x))
	{
	case V2HImode:
	  size = 2;
	  break;
	case V4QImode:
	  size = 1;
	  break;
	default:
	  abort ();
	}

      for (i = 0; i < units; i++)
	{
	  rtx elt;

	  elt = CONST_VECTOR_ELT (x, i);
	  assemble_integer (elt, size, i == 0 ? 32 : size * BITS_PER_UNIT, 1);
	}

      return true;
    }

  return default_assemble_integer (x, size, aligned_p);
}


/*
 * This target hook describes the relative costs of RTL expressions.
 *
 * The cost may depend on the precise form of the expression, which is
 * available for examination in x, and the rtx code of the expression
 * in which it is contained, found in outer_code. code is the
 * expression code--redundant, since it can be obtained with GET_CODE
 * (x).
 *
 * In implementing this hook, you can use the construct COSTS_N_INSNS
 * (n) to specify a cost equal to n fast instructions.
 *
 * On entry to the hook, *total contains a default estimate for the
 * cost of the expression. The hook should modify this value as
 * necessary. Traditionally, the default costs are COSTS_N_INSNS (5)
 * for multiplications, COSTS_N_INSNS (7) for division and modulus
 * operations, and COSTS_N_INSNS (1) for all other operations.
 *
 * When optimizing for code size, i.e. when optimize_size is non-zero,
 * this target hook should be used to estimate the relative size cost
 * of an expression, again relative to COSTS_N_INSNS.
 *
 * The hook returns true when all subexpressions of x have been
 * processed, and false when rtx_cost should recurse.
 */

/* Worker routine for avr32_rtx_costs.  */
static inline int
avr32_rtx_costs_1 (rtx x, enum rtx_code code ATTRIBUTE_UNUSED,
		   enum rtx_code outer ATTRIBUTE_UNUSED)
{
  enum machine_mode mode = GET_MODE (x);

  switch (GET_CODE (x))
    {
    case MEM:
      /* Using pre decrement / post increment memory operations on the
         avr32_uc architecture means that two writebacks must be performed
         and hence two cycles are needed. */
      if (!optimize_size
	  && GET_MODE_SIZE (mode) <= 2 * UNITS_PER_WORD
	  && TARGET_ARCH_UC
	  && (GET_CODE (XEXP (x, 0)) == PRE_DEC
	      || GET_CODE (XEXP (x, 0)) == POST_INC))
	return COSTS_N_INSNS (5);

      /* Memory costs quite a lot for the first word, but subsequent words
         load at the equivalent of a single insn each.  */
      if (GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	return COSTS_N_INSNS (3 + (GET_MODE_SIZE (mode) / UNITS_PER_WORD));

      return COSTS_N_INSNS (4);
    case SYMBOL_REF:
    case CONST:
      /* These are valid for the pseudo insns: lda.w and call which operates
         on direct addresses. We assume that the cost of a lda.w is the same
         as the cost of a ld.w insn. */
      return (outer == SET) ? COSTS_N_INSNS (4) : COSTS_N_INSNS (1);
    case DIV:
    case MOD:
    case UDIV:
    case UMOD:
      return optimize_size ? COSTS_N_INSNS (1) : COSTS_N_INSNS (16);

    case ROTATE:
    case ROTATERT:
      if (mode == TImode)
	return COSTS_N_INSNS (100);

      if (mode == DImode)
	return COSTS_N_INSNS (10);
      return COSTS_N_INSNS (4);
    case ASHIFT:
    case LSHIFTRT:
    case ASHIFTRT:
    case NOT:
      if (mode == TImode)
	return COSTS_N_INSNS (10);

      if (mode == DImode)
	return COSTS_N_INSNS (4);
      return COSTS_N_INSNS (1);
    case PLUS:
    case MINUS:
    case NEG:
    case COMPARE:
    case ABS:
      if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	return COSTS_N_INSNS (100);

      if (mode == TImode)
	return COSTS_N_INSNS (50);

      if (mode == DImode)
	return COSTS_N_INSNS (2);
      return COSTS_N_INSNS (1);

    case MULT:
      {
	if (GET_MODE_CLASS (mode) == MODE_FLOAT)
	  return COSTS_N_INSNS (300);

	if (mode == TImode)
	  return COSTS_N_INSNS (16);

	if (mode == DImode)
	  return COSTS_N_INSNS (4);

	if (mode == HImode)
	  return COSTS_N_INSNS (2);

	return COSTS_N_INSNS (3);
      }
    case IF_THEN_ELSE:
      if (GET_CODE (XEXP (x, 1)) == PC || GET_CODE (XEXP (x, 2)) == PC)
	return COSTS_N_INSNS (4);
      return COSTS_N_INSNS (1);
    case SIGN_EXTEND:
    case ZERO_EXTEND:
      /* Sign/Zero extensions of registers cost quite much since these
         instrcutions only take one register operand which means that gcc
         often must insert some move instrcutions */
      if (mode == QImode || mode == HImode)
	return (COSTS_N_INSNS (GET_CODE (XEXP (x, 0)) == MEM ? 0 : 1));
      return COSTS_N_INSNS (4);
    case UNSPEC:
      /* divmod operations */
      if (XINT (x, 1) == UNSPEC_UDIVMODSI4_INTERNAL
	  || XINT (x, 1) == UNSPEC_DIVMODSI4_INTERNAL)
	{
	  return optimize_size ? COSTS_N_INSNS (1) : COSTS_N_INSNS (16);
	}
      /* Fallthrough */
    default:
      return COSTS_N_INSNS (1);
    }
}


static bool
avr32_rtx_costs (rtx x, int code, int outer_code, int *total)
{
  *total = avr32_rtx_costs_1 (x, code, outer_code);
  return true;
}


bool
avr32_cannot_force_const_mem (rtx x ATTRIBUTE_UNUSED)
{
  /* Do not want symbols in the constant pool when compiling pic or if using
     address pseudo instructions. */
  return ((flag_pic || TARGET_HAS_ASM_ADDR_PSEUDOS)
	  && avr32_find_symbol (x) != NULL_RTX);
}


/* Table of machine attributes.  */
const struct attribute_spec avr32_attribute_table[] = {
  /* { name, min_len, max_len, decl_req, type_req, fn_type_req, handler } */
  /* Interrupt Service Routines have special prologue and epilogue
     requirements.  */
  {"isr", 0, 1, false, false, false, avr32_handle_isr_attribute},
  {"interrupt", 0, 1, false, false, false, avr32_handle_isr_attribute},
  {"acall", 0, 1, false, true, true, avr32_handle_acall_attribute},
  {"naked", 0, 0, true, false, false, avr32_handle_fndecl_attribute},
  {"rmw_addressable", 0, 0, true, false, false, NULL},
  {"flashvault", 0, 1, true, false, false, avr32_handle_fndecl_attribute},
  {"flashvault_impl", 0, 1, true, false, false, avr32_handle_fndecl_attribute},
  {NULL, 0, 0, false, false, false, NULL}
};


typedef struct
{
  const char *const arg;
  const unsigned long return_value;
}
isr_attribute_arg;


static const isr_attribute_arg isr_attribute_args[] = {
  {"FULL", AVR32_FT_ISR_FULL},
  {"full", AVR32_FT_ISR_FULL},
  {"HALF", AVR32_FT_ISR_HALF},
  {"half", AVR32_FT_ISR_HALF},
  {"NONE", AVR32_FT_ISR_NONE},
  {"none", AVR32_FT_ISR_NONE},
  {"UNDEF", AVR32_FT_ISR_NONE},
  {"undef", AVR32_FT_ISR_NONE},
  {"SWI", AVR32_FT_ISR_NONE},
  {"swi", AVR32_FT_ISR_NONE},
  {NULL, AVR32_FT_ISR_NONE}
};


/* Returns the (interrupt) function type of the current
   function, or AVR32_FT_UNKNOWN if the type cannot be determined.  */
static unsigned long
avr32_isr_value (tree argument)
{
  const isr_attribute_arg *ptr;
  const char *arg;

  /* No argument - default to ISR_NONE.  */
  if (argument == NULL_TREE)
    return AVR32_FT_ISR_NONE;

  /* Get the value of the argument.  */
  if (TREE_VALUE (argument) == NULL_TREE
      || TREE_CODE (TREE_VALUE (argument)) != STRING_CST)
    return AVR32_FT_UNKNOWN;

  arg = TREE_STRING_POINTER (TREE_VALUE (argument));

  /* Check it against the list of known arguments.  */
  for (ptr = isr_attribute_args; ptr->arg != NULL; ptr++)
    if (streq (arg, ptr->arg))
      return ptr->return_value;

  /* An unrecognized interrupt type.  */
  return AVR32_FT_UNKNOWN;
}


/*
These hooks specify assembly directives for creating certain kinds
of integer object.  The TARGET_ASM_BYTE_OP directive creates a
byte-sized object, the TARGET_ASM_ALIGNED_HI_OP one creates an
aligned two-byte object, and so on.  Any of the hooks may be
NULL, indicating that no suitable directive is available.

The compiler will print these strings at the start of a new line,
followed immediately by the object's initial value.  In most cases,
the string should contain a tab, a pseudo-op, and then another tab.
*/
#undef  TARGET_ASM_BYTE_OP
#define TARGET_ASM_BYTE_OP "\t.byte\t"
#undef  TARGET_ASM_ALIGNED_HI_OP
#define TARGET_ASM_ALIGNED_HI_OP "\t.align 1\n\t.short\t"
#undef  TARGET_ASM_ALIGNED_SI_OP
#define TARGET_ASM_ALIGNED_SI_OP "\t.align 2\n\t.int\t"
#undef  TARGET_ASM_ALIGNED_DI_OP
#define TARGET_ASM_ALIGNED_DI_OP NULL
#undef  TARGET_ASM_ALIGNED_TI_OP
#define TARGET_ASM_ALIGNED_TI_OP NULL
#undef  TARGET_ASM_UNALIGNED_HI_OP
#define TARGET_ASM_UNALIGNED_HI_OP "\t.short\t"
#undef  TARGET_ASM_UNALIGNED_SI_OP
#define TARGET_ASM_UNALIGNED_SI_OP "\t.int\t"
#undef  TARGET_ASM_UNALIGNED_DI_OP
#define TARGET_ASM_UNALIGNED_DI_OP NULL
#undef  TARGET_ASM_UNALIGNED_TI_OP
#define TARGET_ASM_UNALIGNED_TI_OP NULL

#undef TARGET_ASM_OUTPUT_MI_THUNK
#define TARGET_ASM_OUTPUT_MI_THUNK avr32_output_mi_thunk

#undef TARGET_ASM_CAN_OUTPUT_MI_THUNK
#define TARGET_ASM_CAN_OUTPUT_MI_THUNK hook_bool_const_tree_hwi_hwi_const_tree_true


static void
avr32_output_mi_thunk (FILE * file,
    tree thunk ATTRIBUTE_UNUSED,
    HOST_WIDE_INT delta,
    HOST_WIDE_INT vcall_offset, tree function)
  {
    int mi_delta = delta;
    int this_regno =
      (aggregate_value_p (TREE_TYPE (TREE_TYPE (function)), function) ?
       INTERNAL_REGNUM (11) : INTERNAL_REGNUM (12));


    if (!avr32_const_ok_for_constraint_p (mi_delta, 'I', "Is21")
        || vcall_offset)
      {
        fputs ("\tpushm\tlr\n", file);
      }


    if (mi_delta != 0)
      {
        if (avr32_const_ok_for_constraint_p (mi_delta, 'I', "Is21"))
          {
            fprintf (file, "\tsub\t%s, %d\n", reg_names[this_regno], -mi_delta);
          }
        else
          {
            /* Immediate is larger than k21 we must make us a temp register by
	     pushing a register to the stack. */
            fprintf (file, "\tmov\tlr, lo(%d)\n", mi_delta);
            fprintf (file, "\torh\tlr, hi(%d)\n", mi_delta);
            fprintf (file, "\tadd\t%s, lr\n", reg_names[this_regno]);
          }
      }


    if (vcall_offset != 0)
      {
        fprintf (file, "\tld.w\tlr, %s[0]\n", reg_names[this_regno]);
        fprintf (file, "\tld.w\tlr, lr[%i]\n", (int) vcall_offset);
        fprintf (file, "\tadd\t%s, lr\n", reg_names[this_regno]);
      }


    if (!avr32_const_ok_for_constraint_p (mi_delta, 'I', "Is21")
        || vcall_offset)
      {
        fputs ("\tpopm\tlr\n", file);
      }

    /* Jump to the function. We assume that we can use an rjmp since the
       function to jump to is local and probably not too far away from
       the thunk. If this assumption proves to be wrong we could implement
       this jump by calculating the offset between the jump source and destination
       and put this in the constant pool and then perform an add to pc.
       This would also be legitimate PIC code. But for now we hope that an rjmp
       will be sufficient...
    */
    fputs ("\trjmp\t", file);
    assemble_name (file, XSTR (XEXP (DECL_RTL (function), 0), 0));
    fputc ('\n', file);
  }


/* Implements target hook vector_mode_supported.  */
bool
avr32_vector_mode_supported (enum machine_mode mode)
{
  if ((mode == V2HImode) || (mode == V4QImode))
    return true;

  return false;
}


#undef TARGET_INIT_LIBFUNCS
#define TARGET_INIT_LIBFUNCS avr32_init_libfuncs

#undef  TARGET_INIT_BUILTINS
#define TARGET_INIT_BUILTINS avr32_init_builtins

#undef  TARGET_EXPAND_BUILTIN
#define TARGET_EXPAND_BUILTIN avr32_expand_builtin

tree int_ftype_int, int_ftype_void, short_ftype_short, void_ftype_int_int,
  void_ftype_ptr_int;
tree void_ftype_int, void_ftype_ulong, void_ftype_void, int_ftype_ptr_int;
tree short_ftype_short, int_ftype_int_short, int_ftype_short_short,
  short_ftype_short_short;
tree int_ftype_int_int, longlong_ftype_int_short, longlong_ftype_short_short;
tree void_ftype_int_int_int_int_int, void_ftype_int_int_int;
tree longlong_ftype_int_int, void_ftype_int_int_longlong;
tree int_ftype_int_int_int, longlong_ftype_longlong_int_short;
tree longlong_ftype_longlong_short_short, int_ftype_int_short_short;

#define def_builtin(NAME, TYPE, CODE)					\
  add_builtin_function ((NAME), (TYPE), (CODE),                          \
                       BUILT_IN_MD, NULL, NULL_TREE)

#define def_mbuiltin(MASK, NAME, TYPE, CODE)				\
  do									\
    {									\
      if ((MASK))							\
	add_builtin_function ((NAME), (TYPE), (CODE),                   \
                              BUILT_IN_MD, NULL, NULL_TREE);            \
    }									\
  while (0)

struct builtin_description
{
  const unsigned int mask;
  const enum insn_code icode;
  const char *const name;
  const int code;
  const enum rtx_code comparison;
  const unsigned int flag;
  const tree *ftype;
};

static const struct builtin_description bdesc_2arg[] = {

#define DSP_BUILTIN(code, builtin, ftype) \
  { 1, CODE_FOR_##code, "__builtin_" #code , \
  AVR32_BUILTIN_##builtin, 0, 0, ftype }

  DSP_BUILTIN (mulsathh_h,    MULSATHH_H,    &short_ftype_short_short),
  DSP_BUILTIN (mulsathh_w,    MULSATHH_W,    &int_ftype_short_short),
  DSP_BUILTIN (mulsatrndhh_h, MULSATRNDHH_H, &short_ftype_short_short),
  DSP_BUILTIN (mulsatrndwh_w, MULSATRNDWH_W, &int_ftype_int_short),
  DSP_BUILTIN (mulsatwh_w,    MULSATWH_W,    &int_ftype_int_short),
  DSP_BUILTIN (satadd_h,      SATADD_H,      &short_ftype_short_short),
  DSP_BUILTIN (satsub_h,      SATSUB_H,      &short_ftype_short_short),
  DSP_BUILTIN (satadd_w,      SATADD_W,      &int_ftype_int_int),
  DSP_BUILTIN (satsub_w,      SATSUB_W,      &int_ftype_int_int),
  DSP_BUILTIN (mulwh_d,       MULWH_D,       &longlong_ftype_int_short),
  DSP_BUILTIN (mulnwh_d,      MULNWH_D,      &longlong_ftype_int_short)
};


void
avr32_init_builtins (void)
{
  unsigned int i;
  const struct builtin_description *d;
  tree endlink = void_list_node;
  tree int_endlink = tree_cons (NULL_TREE, integer_type_node, endlink);
  tree longlong_endlink =
    tree_cons (NULL_TREE, long_long_integer_type_node, endlink);
  tree short_endlink =
    tree_cons (NULL_TREE, short_integer_type_node, endlink);
  tree void_endlink = tree_cons (NULL_TREE, void_type_node, endlink);

  /* int func (int) */
  int_ftype_int = build_function_type (integer_type_node, int_endlink);

  /* short func (short) */
  short_ftype_short
    = build_function_type (short_integer_type_node, short_endlink);

  /* short func (short, short) */
  short_ftype_short_short
    = build_function_type (short_integer_type_node,
			   tree_cons (NULL_TREE, short_integer_type_node,
				      short_endlink));

  /* long long func (long long, short, short) */
  longlong_ftype_longlong_short_short
    = build_function_type (long_long_integer_type_node,
			   tree_cons (NULL_TREE, long_long_integer_type_node,
				      tree_cons (NULL_TREE,
						 short_integer_type_node,
						 short_endlink)));

  /* long long func (short, short) */
  longlong_ftype_short_short
    = build_function_type (long_long_integer_type_node,
			   tree_cons (NULL_TREE, short_integer_type_node,
				      short_endlink));

  /* int func (int, int) */
  int_ftype_int_int
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      int_endlink));

  /* long long func (int, int) */
  longlong_ftype_int_int
    = build_function_type (long_long_integer_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      int_endlink));

  /* long long int func (long long, int, short) */
  longlong_ftype_longlong_int_short
    = build_function_type (long_long_integer_type_node,
			   tree_cons (NULL_TREE, long_long_integer_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 short_endlink)));

  /* long long int func (int, short) */
  longlong_ftype_int_short
    = build_function_type (long_long_integer_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      short_endlink));

  /* int func (int, short, short) */
  int_ftype_int_short_short
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      tree_cons (NULL_TREE,
						 short_integer_type_node,
						 short_endlink)));

  /* int func (short, short) */
  int_ftype_short_short
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, short_integer_type_node,
				      short_endlink));

  /* int func (int, short) */
  int_ftype_int_short
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      short_endlink));

  /* void func (int, int) */
  void_ftype_int_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      int_endlink));

  /* void func (int, int, int) */
  void_ftype_int_int_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 int_endlink)));

  /* void func (int, int, long long) */
  void_ftype_int_int_longlong
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 longlong_endlink)));

  /* void func (int, int, int, int, int) */
  void_ftype_int_int_int_int_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 tree_cons (NULL_TREE,
							    integer_type_node,
							    tree_cons
							    (NULL_TREE,
							     integer_type_node,
							     int_endlink)))));

  /* void func (void *, int) */
  void_ftype_ptr_int
    = build_function_type (void_type_node,
			   tree_cons (NULL_TREE, ptr_type_node, int_endlink));

  /* void func (int) */
  void_ftype_int = build_function_type (void_type_node, int_endlink);

  /* void func (ulong) */
  void_ftype_ulong = build_function_type_list (void_type_node,
                           long_unsigned_type_node, NULL_TREE);

  /* void func (void) */
  void_ftype_void = build_function_type (void_type_node, void_endlink);

  /* int func (void) */
  int_ftype_void = build_function_type (integer_type_node, void_endlink);

  /* int func (void *, int) */
  int_ftype_ptr_int
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, ptr_type_node, int_endlink));

  /* int func (int, int, int) */
  int_ftype_int_int_int
    = build_function_type (integer_type_node,
			   tree_cons (NULL_TREE, integer_type_node,
				      tree_cons (NULL_TREE, integer_type_node,
						 int_endlink)));

  /* Initialize avr32 builtins.  */
  def_builtin ("__builtin_mfsr", int_ftype_int, AVR32_BUILTIN_MFSR);
  def_builtin ("__builtin_mtsr", void_ftype_int_int, AVR32_BUILTIN_MTSR);
  def_builtin ("__builtin_mfdr", int_ftype_int, AVR32_BUILTIN_MFDR);
  def_builtin ("__builtin_mtdr", void_ftype_int_int, AVR32_BUILTIN_MTDR);
  def_builtin ("__builtin_cache", void_ftype_ptr_int, AVR32_BUILTIN_CACHE);
  def_builtin ("__builtin_sync", void_ftype_int, AVR32_BUILTIN_SYNC);
  def_builtin ("__builtin_ssrf", void_ftype_int, AVR32_BUILTIN_SSRF);
  def_builtin ("__builtin_csrf", void_ftype_int, AVR32_BUILTIN_CSRF);
  def_builtin ("__builtin_tlbr", void_ftype_void, AVR32_BUILTIN_TLBR);
  def_builtin ("__builtin_tlbs", void_ftype_void, AVR32_BUILTIN_TLBS);
  def_builtin ("__builtin_tlbw", void_ftype_void, AVR32_BUILTIN_TLBW);
  def_builtin ("__builtin_breakpoint", void_ftype_void,
	       AVR32_BUILTIN_BREAKPOINT);
  def_builtin ("__builtin_xchg", int_ftype_ptr_int, AVR32_BUILTIN_XCHG);
  def_builtin ("__builtin_ldxi", int_ftype_ptr_int, AVR32_BUILTIN_LDXI);
  def_builtin ("__builtin_bswap_16", short_ftype_short,
	       AVR32_BUILTIN_BSWAP16);
  def_builtin ("__builtin_bswap_32", int_ftype_int, AVR32_BUILTIN_BSWAP32);
  def_builtin ("__builtin_cop", void_ftype_int_int_int_int_int,
	       AVR32_BUILTIN_COP);
  def_builtin ("__builtin_mvcr_w", int_ftype_int_int, AVR32_BUILTIN_MVCR_W);
  def_builtin ("__builtin_mvrc_w", void_ftype_int_int_int,
	       AVR32_BUILTIN_MVRC_W);
  def_builtin ("__builtin_mvcr_d", longlong_ftype_int_int,
	       AVR32_BUILTIN_MVCR_D);
  def_builtin ("__builtin_mvrc_d", void_ftype_int_int_longlong,
	       AVR32_BUILTIN_MVRC_D);
  def_builtin ("__builtin_sats", int_ftype_int_int_int, AVR32_BUILTIN_SATS);
  def_builtin ("__builtin_satu", int_ftype_int_int_int, AVR32_BUILTIN_SATU);
  def_builtin ("__builtin_satrnds", int_ftype_int_int_int,
	       AVR32_BUILTIN_SATRNDS);
  def_builtin ("__builtin_satrndu", int_ftype_int_int_int,
	       AVR32_BUILTIN_SATRNDU);
  def_builtin ("__builtin_musfr", void_ftype_int, AVR32_BUILTIN_MUSFR);
  def_builtin ("__builtin_mustr", int_ftype_void, AVR32_BUILTIN_MUSTR);
  def_builtin ("__builtin_macsathh_w", int_ftype_int_short_short,
	       AVR32_BUILTIN_MACSATHH_W);
  def_builtin ("__builtin_macwh_d", longlong_ftype_longlong_int_short,
	       AVR32_BUILTIN_MACWH_D);
  def_builtin ("__builtin_machh_d", longlong_ftype_longlong_short_short,
	       AVR32_BUILTIN_MACHH_D);
  def_builtin ("__builtin_mems", void_ftype_ptr_int, AVR32_BUILTIN_MEMS);
  def_builtin ("__builtin_memt", void_ftype_ptr_int, AVR32_BUILTIN_MEMT);
  def_builtin ("__builtin_memc", void_ftype_ptr_int, AVR32_BUILTIN_MEMC);
  def_builtin ("__builtin_sleep", void_ftype_int, AVR32_BUILTIN_SLEEP);
  def_builtin ("__builtin_avr32_delay_cycles", void_ftype_int, AVR32_BUILTIN_DELAY_CYCLES);

  /* Add all builtins that are more or less simple operations on two
     operands.  */
  for (i = 0, d = bdesc_2arg; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    {
      /* Use one of the operands; the target can have a different mode for
         mask-generating compares.  */

      if (d->name == 0)
	continue;

      def_mbuiltin (d->mask, d->name, *(d->ftype), d->code);
    }
}


/* Subroutine of avr32_expand_builtin to take care of binop insns. */
static rtx
avr32_expand_binop_builtin (enum insn_code icode, tree exp, rtx target)
{
  rtx pat;
  tree arg0 = CALL_EXPR_ARG (exp,0);
  tree arg1 = CALL_EXPR_ARG (exp,1);
  rtx op0 = expand_normal (arg0);
  rtx op1 = expand_normal (arg1);
  enum machine_mode tmode = insn_data[icode].operand[0].mode;
  enum machine_mode mode0 = insn_data[icode].operand[1].mode;
  enum machine_mode mode1 = insn_data[icode].operand[2].mode;

  if (!target
      || GET_MODE (target) != tmode
      || !(*insn_data[icode].operand[0].predicate) (target, tmode))
    target = gen_reg_rtx (tmode);

  /* In case the insn wants input operands in modes different from the
     result, abort.  */
  if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
    {
      /* If op0 is already a reg we must cast it to the correct mode. */
      if (REG_P (op0))
	op0 = convert_to_mode (mode0, op0, 1);
      else
	op0 = copy_to_mode_reg (mode0, op0);
    }
  if (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
    {
      /* If op1 is already a reg we must cast it to the correct mode. */
      if (REG_P (op1))
	op1 = convert_to_mode (mode1, op1, 1);
      else
	op1 = copy_to_mode_reg (mode1, op1);
    }
  pat = GEN_FCN (icode) (target, op0, op1);
  if (!pat)
    return 0;
  emit_insn (pat);
  return target;
}


/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */
rtx
avr32_expand_builtin (tree exp,
		      rtx target,
		      rtx subtarget ATTRIBUTE_UNUSED,
		      enum machine_mode mode ATTRIBUTE_UNUSED,
		      int ignore ATTRIBUTE_UNUSED)
{
  const struct builtin_description *d;
  unsigned int i;
  enum insn_code icode = 0;
  tree fndecl = TREE_OPERAND (CALL_EXPR_FN (exp), 0);
  tree arg0, arg1, arg2;
  rtx op0, op1, op2, pat;
  enum machine_mode tmode, mode0, mode1;
  enum machine_mode arg0_mode;
  int fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
    default:
      break;

    case AVR32_BUILTIN_SATS:
    case AVR32_BUILTIN_SATU:
    case AVR32_BUILTIN_SATRNDS:
    case AVR32_BUILTIN_SATRNDU:
      {
	const char *fname;
	switch (fcode)
	  {
	  default:
	  case AVR32_BUILTIN_SATS:
	    icode = CODE_FOR_sats;
	    fname = "sats";
	    break;
	  case AVR32_BUILTIN_SATU:
	    icode = CODE_FOR_satu;
	    fname = "satu";
	    break;
	  case AVR32_BUILTIN_SATRNDS:
	    icode = CODE_FOR_satrnds;
	    fname = "satrnds";
	    break;
	  case AVR32_BUILTIN_SATRNDU:
	    icode = CODE_FOR_satrndu;
	    fname = "satrndu";
	    break;
	  }

	arg0 = CALL_EXPR_ARG (exp,0);
	arg1 = CALL_EXPR_ARG (exp,1);
	arg2 = CALL_EXPR_ARG (exp,2);
	op0 = expand_normal (arg0);
	op1 = expand_normal (arg1);
	op2 = expand_normal (arg2);

	tmode = insn_data[icode].operand[0].mode;


	if (target == 0
	    || GET_MODE (target) != tmode
	    || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	  target = gen_reg_rtx (tmode);


	if (!(*insn_data[icode].operand[0].predicate) (op0, GET_MODE (op0)))
	  {
	    op0 = copy_to_mode_reg (insn_data[icode].operand[0].mode, op0);
	  }

	if (!(*insn_data[icode].operand[1].predicate) (op1, SImode))
	  {
	    error ("Parameter 2 to __builtin_%s should be a constant number.",
		   fname);
	    return NULL_RTX;
	  }

	if (!(*insn_data[icode].operand[1].predicate) (op2, SImode))
	  {
	    error ("Parameter 3 to __builtin_%s should be a constant number.",
		   fname);
	    return NULL_RTX;
	  }

	emit_move_insn (target, op0);
	pat = GEN_FCN (icode) (target, op1, op2);
	if (!pat)
	  return 0;
	emit_insn (pat);

	return target;
      }
    case AVR32_BUILTIN_MUSTR:
      icode = CODE_FOR_mustr;
      tmode = insn_data[icode].operand[0].mode;

      if (target == 0
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target);
      if (!pat)
	return 0;
      emit_insn (pat);
      return target;

    case AVR32_BUILTIN_MFSR:
      icode = CODE_FOR_mfsr;
      arg0 = CALL_EXPR_ARG (exp,0);
      op0 = expand_normal (arg0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;

      if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
	{
	  error ("Parameter 1 to __builtin_mfsr must be a constant number");
	}

      if (target == 0
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0);
      if (!pat)
	return 0;
      emit_insn (pat);
      return target;
    case AVR32_BUILTIN_MTSR:
      icode = CODE_FOR_mtsr;
      arg0 = CALL_EXPR_ARG (exp,0);
      arg1 = CALL_EXPR_ARG (exp,1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;

      if (!(*insn_data[icode].operand[0].predicate) (op0, mode0))
	{
	  error ("Parameter 1 to __builtin_mtsr must be a constant number");
	  return gen_reg_rtx (mode0);
	}
      if (!(*insn_data[icode].operand[1].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);
      pat = GEN_FCN (icode) (op0, op1);
      if (!pat)
	return 0;
      emit_insn (pat);
      return NULL_RTX;
    case AVR32_BUILTIN_MFDR:
      icode = CODE_FOR_mfdr;
      arg0 = CALL_EXPR_ARG (exp,0);
      op0 = expand_normal (arg0);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;

      if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
	{
	  error ("Parameter 1 to __builtin_mfdr must be a constant number");
	}

      if (target == 0
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0);
      if (!pat)
	return 0;
      emit_insn (pat);
      return target;
    case AVR32_BUILTIN_MTDR:
      icode = CODE_FOR_mtdr;
      arg0 = CALL_EXPR_ARG (exp,0);
      arg1 = CALL_EXPR_ARG (exp,1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;

      if (!(*insn_data[icode].operand[0].predicate) (op0, mode0))
	{
	  error ("Parameter 1 to __builtin_mtdr must be a constant number");
	  return gen_reg_rtx (mode0);
	}
      if (!(*insn_data[icode].operand[1].predicate) (op1, mode1))
	op1 = copy_to_mode_reg (mode1, op1);
      pat = GEN_FCN (icode) (op0, op1);
      if (!pat)
	return 0;
      emit_insn (pat);
      return NULL_RTX;
    case AVR32_BUILTIN_CACHE:
      icode = CODE_FOR_cache;
      arg0 = CALL_EXPR_ARG (exp,0);
      arg1 = CALL_EXPR_ARG (exp,1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      mode0 = insn_data[icode].operand[0].mode;
      mode1 = insn_data[icode].operand[1].mode;

      if (!(*insn_data[icode].operand[1].predicate) (op1, mode1))
	{
	  error ("Parameter 2 to __builtin_cache must be a constant number");
	  return gen_reg_rtx (mode1);
	}

      if (!(*insn_data[icode].operand[0].predicate) (op0, mode0))
	op0 = copy_to_mode_reg (mode0, op0);

      pat = GEN_FCN (icode) (op0, op1);
      if (!pat)
	return 0;
      emit_insn (pat);
      return NULL_RTX;
    case AVR32_BUILTIN_SYNC:
    case AVR32_BUILTIN_MUSFR:
    case AVR32_BUILTIN_SSRF:
    case AVR32_BUILTIN_CSRF:
      {
	const char *fname;
	switch (fcode)
	  {
	  default:
	  case AVR32_BUILTIN_SYNC:
	    icode = CODE_FOR_sync;
	    fname = "sync";
	    break;
	  case AVR32_BUILTIN_MUSFR:
	    icode = CODE_FOR_musfr;
	    fname = "musfr";
	    break;
	  case AVR32_BUILTIN_SSRF:
	    icode = CODE_FOR_ssrf;
	    fname = "ssrf";
	    break;
	  case AVR32_BUILTIN_CSRF:
	    icode = CODE_FOR_csrf;
	    fname = "csrf";
	    break;
	  }

	arg0 = CALL_EXPR_ARG (exp,0);
	op0 = expand_normal (arg0);
	mode0 = insn_data[icode].operand[0].mode;

	if (!(*insn_data[icode].operand[0].predicate) (op0, mode0))
	  {
	    if (icode == CODE_FOR_musfr)
	      op0 = copy_to_mode_reg (mode0, op0);
	    else
	      {
		error ("Parameter to __builtin_%s is illegal.", fname);
		return gen_reg_rtx (mode0);
	      }
	  }
	pat = GEN_FCN (icode) (op0);
	if (!pat)
	  return 0;
	emit_insn (pat);
	return NULL_RTX;
      }
    case AVR32_BUILTIN_TLBR:
      icode = CODE_FOR_tlbr;
      pat = GEN_FCN (icode) (NULL_RTX);
      if (!pat)
	return 0;
      emit_insn (pat);
      return NULL_RTX;
    case AVR32_BUILTIN_TLBS:
      icode = CODE_FOR_tlbs;
      pat = GEN_FCN (icode) (NULL_RTX);
      if (!pat)
	return 0;
      emit_insn (pat);
      return NULL_RTX;
    case AVR32_BUILTIN_TLBW:
      icode = CODE_FOR_tlbw;
      pat = GEN_FCN (icode) (NULL_RTX);
      if (!pat)
	return 0;
      emit_insn (pat);
      return NULL_RTX;
    case AVR32_BUILTIN_BREAKPOINT:
      icode = CODE_FOR_breakpoint;
      pat = GEN_FCN (icode) (NULL_RTX);
      if (!pat)
	return 0;
      emit_insn (pat);
      return NULL_RTX;
    case AVR32_BUILTIN_XCHG:
      icode = CODE_FOR_sync_lock_test_and_setsi;
      arg0 = CALL_EXPR_ARG (exp,0);
      arg1 = CALL_EXPR_ARG (exp,1);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;
      mode1 = insn_data[icode].operand[2].mode;

      if (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
	{
	  op1 = copy_to_mode_reg (mode1, op1);
	}

      op0 = force_reg (GET_MODE (op0), op0);
      op0 = gen_rtx_MEM (GET_MODE (op0), op0);
      if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
	{
	  error
	    ("Parameter 1 to __builtin_xchg must be a pointer to an integer.");
	}

      if (target == 0
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1);
      if (!pat)
	return 0;
      emit_insn (pat);
      return target;
    case AVR32_BUILTIN_LDXI:
      icode = CODE_FOR_ldxi;
      arg0 = CALL_EXPR_ARG (exp,0);
      arg1 = CALL_EXPR_ARG (exp,1);
      arg2 = CALL_EXPR_ARG (exp,2);
      op0 = expand_normal (arg0);
      op1 = expand_normal (arg1);
      op2 = expand_normal (arg2);
      tmode = insn_data[icode].operand[0].mode;
      mode0 = insn_data[icode].operand[1].mode;
      mode1 = insn_data[icode].operand[2].mode;

      if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
	{
	  op0 = copy_to_mode_reg (mode0, op0);
	}

      if (!(*insn_data[icode].operand[2].predicate) (op1, mode1))
	{
	  op1 = copy_to_mode_reg (mode1, op1);
	}

      if (!(*insn_data[icode].operand[3].predicate) (op2, SImode))
	{
	  error
	    ("Parameter 3 to __builtin_ldxi must be a valid extract shift operand: (0|8|16|24)");
	  return gen_reg_rtx (mode0);
	}

      if (target == 0
	  || GET_MODE (target) != tmode
	  || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	target = gen_reg_rtx (tmode);
      pat = GEN_FCN (icode) (target, op0, op1, op2);
      if (!pat)
	return 0;
      emit_insn (pat);
      return target;
    case AVR32_BUILTIN_BSWAP16:
      {
	icode = CODE_FOR_bswap_16;
	arg0 = CALL_EXPR_ARG (exp,0);
	arg0_mode = TYPE_MODE (TREE_TYPE (arg0));
	mode0 = insn_data[icode].operand[1].mode;
	if (arg0_mode != mode0)
	  arg0 = build1 (NOP_EXPR,
			 (*lang_hooks.types.type_for_mode) (mode0, 0), arg0);

	op0 = expand_expr (arg0, NULL_RTX, HImode, 0);
	tmode = insn_data[icode].operand[0].mode;


	if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
	  {
            if ( CONST_INT_P (op0) )
              {
                HOST_WIDE_INT val = ( ((INTVAL (op0)&0x00ff) << 8) |
                                      ((INTVAL (op0)&0xff00) >> 8) );
                /* Sign extend 16-bit value to host wide int */
                val <<= (HOST_BITS_PER_WIDE_INT - 16);
                val >>= (HOST_BITS_PER_WIDE_INT - 16);
                op0 = GEN_INT(val);
                if (target == 0
                    || GET_MODE (target) != tmode
                    || !(*insn_data[icode].operand[0].predicate) (target, tmode))
                  target = gen_reg_rtx (tmode);
                emit_move_insn(target, op0);
                return target;
              }
            else
              op0 = copy_to_mode_reg (mode0, op0);
	  }

	if (target == 0
	    || GET_MODE (target) != tmode
	    || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	  {
	    target = gen_reg_rtx (tmode);
	  }


	pat = GEN_FCN (icode) (target, op0);
	if (!pat)
	  return 0;
	emit_insn (pat);

	return target;
      }
    case AVR32_BUILTIN_BSWAP32:
      {
	icode = CODE_FOR_bswap_32;
	arg0 = CALL_EXPR_ARG (exp,0);
	op0 = expand_normal (arg0);
	tmode = insn_data[icode].operand[0].mode;
	mode0 = insn_data[icode].operand[1].mode;

	if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
	  {
            if ( CONST_INT_P (op0) )
              {
                HOST_WIDE_INT val = ( ((INTVAL (op0)&0x000000ff) << 24) |
                                      ((INTVAL (op0)&0x0000ff00) << 8) |
                                      ((INTVAL (op0)&0x00ff0000) >> 8) |
                                      ((INTVAL (op0)&0xff000000) >> 24) );
                /* Sign extend 32-bit value to host wide int */
                val <<= (HOST_BITS_PER_WIDE_INT - 32);
                val >>= (HOST_BITS_PER_WIDE_INT - 32);
                op0 = GEN_INT(val);
                if (target == 0
                    || GET_MODE (target) != tmode
                    || !(*insn_data[icode].operand[0].predicate) (target, tmode))
                  target = gen_reg_rtx (tmode);
                emit_move_insn(target, op0);
                return target;
              }
            else
              op0 = copy_to_mode_reg (mode0, op0);
	  }

	if (target == 0
	    || GET_MODE (target) != tmode
	    || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	  target = gen_reg_rtx (tmode);


	pat = GEN_FCN (icode) (target, op0);
	if (!pat)
	  return 0;
	emit_insn (pat);

	return target;
      }
    case AVR32_BUILTIN_MVCR_W:
    case AVR32_BUILTIN_MVCR_D:
      {
	arg0 = CALL_EXPR_ARG (exp,0);
	arg1 = CALL_EXPR_ARG (exp,1);
	op0 = expand_normal (arg0);
	op1 = expand_normal (arg1);

	if (fcode == AVR32_BUILTIN_MVCR_W)
	  icode = CODE_FOR_mvcrsi;
	else
	  icode = CODE_FOR_mvcrdi;

	tmode = insn_data[icode].operand[0].mode;

	if (target == 0
	    || GET_MODE (target) != tmode
	    || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	  target = gen_reg_rtx (tmode);

	if (!(*insn_data[icode].operand[1].predicate) (op0, SImode))
	  {
	    error
	      ("Parameter 1 to __builtin_cop is not a valid coprocessor number.");
	    error ("Number should be between 0 and 7.");
	    return NULL_RTX;
	  }

	if (!(*insn_data[icode].operand[2].predicate) (op1, SImode))
	  {
	    error
	      ("Parameter 2 to __builtin_cop is not a valid coprocessor register number.");
	    error ("Number should be between 0 and 15.");
	    return NULL_RTX;
	  }

	pat = GEN_FCN (icode) (target, op0, op1);
	if (!pat)
	  return 0;
	emit_insn (pat);

	return target;
      }
    case AVR32_BUILTIN_MACSATHH_W:
    case AVR32_BUILTIN_MACWH_D:
    case AVR32_BUILTIN_MACHH_D:
      {
	arg0 = CALL_EXPR_ARG (exp,0);
	arg1 = CALL_EXPR_ARG (exp,1);
	arg2 = CALL_EXPR_ARG (exp,2);
	op0 = expand_normal (arg0);
	op1 = expand_normal (arg1);
	op2 = expand_normal (arg2);

	icode = ((fcode == AVR32_BUILTIN_MACSATHH_W) ? CODE_FOR_macsathh_w :
		 (fcode == AVR32_BUILTIN_MACWH_D) ? CODE_FOR_macwh_d :
		 CODE_FOR_machh_d);

	tmode = insn_data[icode].operand[0].mode;
	mode0 = insn_data[icode].operand[1].mode;
	mode1 = insn_data[icode].operand[2].mode;


	if (!target
	    || GET_MODE (target) != tmode
	    || !(*insn_data[icode].operand[0].predicate) (target, tmode))
	  target = gen_reg_rtx (tmode);

	if (!(*insn_data[icode].operand[0].predicate) (op0, tmode))
	  {
	    /* If op0 is already a reg we must cast it to the correct mode. */
	    if (REG_P (op0))
	      op0 = convert_to_mode (tmode, op0, 1);
	    else
	      op0 = copy_to_mode_reg (tmode, op0);
	  }

	if (!(*insn_data[icode].operand[1].predicate) (op1, mode0))
	  {
	    /* If op1 is already a reg we must cast it to the correct mode. */
	    if (REG_P (op1))
	      op1 = convert_to_mode (mode0, op1, 1);
	    else
	      op1 = copy_to_mode_reg (mode0, op1);
	  }

	if (!(*insn_data[icode].operand[2].predicate) (op2, mode1))
	  {
	    /* If op1 is already a reg we must cast it to the correct mode. */
	    if (REG_P (op2))
	      op2 = convert_to_mode (mode1, op2, 1);
	    else
	      op2 = copy_to_mode_reg (mode1, op2);
	  }

	emit_move_insn (target, op0);

	pat = GEN_FCN (icode) (target, op1, op2);
	if (!pat)
	  return 0;
	emit_insn (pat);
	return target;
      }
    case AVR32_BUILTIN_MVRC_W:
    case AVR32_BUILTIN_MVRC_D:
      {
	arg0 = CALL_EXPR_ARG (exp,0);
	arg1 = CALL_EXPR_ARG (exp,1);
	arg2 = CALL_EXPR_ARG (exp,2);
	op0 = expand_normal (arg0);
	op1 = expand_normal (arg1);
	op2 = expand_normal (arg2);

	if (fcode == AVR32_BUILTIN_MVRC_W)
	  icode = CODE_FOR_mvrcsi;
	else
	  icode = CODE_FOR_mvrcdi;

	if (!(*insn_data[icode].operand[0].predicate) (op0, SImode))
	  {
	    error ("Parameter 1 is not a valid coprocessor number.");
	    error ("Number should be between 0 and 7.");
	    return NULL_RTX;
	  }

	if (!(*insn_data[icode].operand[1].predicate) (op1, SImode))
	  {
	    error ("Parameter 2 is not a valid coprocessor register number.");
	    error ("Number should be between 0 and 15.");
	    return NULL_RTX;
	  }

	if (GET_CODE (op2) == CONST_INT
	    || GET_CODE (op2) == CONST
	    || GET_CODE (op2) == SYMBOL_REF || GET_CODE (op2) == LABEL_REF)
	  {
	    op2 = force_const_mem (insn_data[icode].operand[2].mode, op2);
	  }

	if (!(*insn_data[icode].operand[2].predicate) (op2, GET_MODE (op2)))
	  op2 = copy_to_mode_reg (insn_data[icode].operand[2].mode, op2);


	pat = GEN_FCN (icode) (op0, op1, op2);
	if (!pat)
	  return 0;
	emit_insn (pat);

	return NULL_RTX;
      }
    case AVR32_BUILTIN_COP:
      {
	rtx op3, op4;
	tree arg3, arg4;
	icode = CODE_FOR_cop;
	arg0 = CALL_EXPR_ARG (exp,0);
	arg1 = CALL_EXPR_ARG (exp,1);
	arg2 = CALL_EXPR_ARG (exp,2);
	arg3 = CALL_EXPR_ARG (exp,3);
	arg4 = CALL_EXPR_ARG (exp,4);
	op0 = expand_normal (arg0);
	op1 = expand_normal (arg1);
	op2 = expand_normal (arg2);
	op3 = expand_normal (arg3);
	op4 = expand_normal (arg4);

	if (!(*insn_data[icode].operand[0].predicate) (op0, SImode))
	  {
	    error
	      ("Parameter 1 to __builtin_cop is not a valid coprocessor number.");
	    error ("Number should be between 0 and 7.");
	    return NULL_RTX;
	  }

	if (!(*insn_data[icode].operand[1].predicate) (op1, SImode))
	  {
	    error
	      ("Parameter 2 to __builtin_cop is not a valid coprocessor register number.");
	    error ("Number should be between 0 and 15.");
	    return NULL_RTX;
	  }

	if (!(*insn_data[icode].operand[2].predicate) (op2, SImode))
	  {
	    error
	      ("Parameter 3 to __builtin_cop is not a valid coprocessor register number.");
	    error ("Number should be between 0 and 15.");
	    return NULL_RTX;
	  }

	if (!(*insn_data[icode].operand[3].predicate) (op3, SImode))
	  {
	    error
	      ("Parameter 4 to __builtin_cop is not a valid coprocessor register number.");
	    error ("Number should be between 0 and 15.");
	    return NULL_RTX;
	  }

	if (!(*insn_data[icode].operand[4].predicate) (op4, SImode))
	  {
	    error
	      ("Parameter 5 to __builtin_cop is not a valid coprocessor operation.");
	    error ("Number should be between 0 and 127.");
	    return NULL_RTX;
	  }

	pat = GEN_FCN (icode) (op0, op1, op2, op3, op4);
	if (!pat)
	  return 0;
	emit_insn (pat);

	return target;
      }

     case AVR32_BUILTIN_MEMS:
     case AVR32_BUILTIN_MEMC:
     case AVR32_BUILTIN_MEMT:
       {
         if (!TARGET_RMW)
           error ("Trying to use __builtin_mem(s/c/t) when target does not support RMW insns.");
         
         switch (fcode) {
         case AVR32_BUILTIN_MEMS:
           icode = CODE_FOR_iorsi3;
           break;
         case AVR32_BUILTIN_MEMC:
           icode = CODE_FOR_andsi3;
           break;
         case AVR32_BUILTIN_MEMT:
           icode = CODE_FOR_xorsi3;
           break;
         }
			arg0 = CALL_EXPR_ARG (exp,0);
			arg1 = CALL_EXPR_ARG (exp,1);
         op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
         if ( GET_CODE (op0) == SYMBOL_REF )
           // This symbol must be RMW addressable
           SYMBOL_REF_FLAGS (op0) |= (1 << SYMBOL_FLAG_RMW_ADDR_SHIFT);
         op0 = gen_rtx_MEM(SImode, op0);
         op1 = expand_expr (arg1, NULL_RTX, VOIDmode, 0);
         mode0 = insn_data[icode].operand[1].mode;
         
         
         if (!(*insn_data[icode].operand[1].predicate) (op0, mode0))
           {
             error ("Parameter 1 to __builtin_mem(s/c/t) must be a Ks15<<2 address or a rmw addressable symbol.");
           }
         
         if ( !CONST_INT_P (op1)
              || INTVAL (op1) > 31
              || INTVAL (op1) < 0 )
           error ("Parameter 2 to __builtin_mem(s/c/t) must be a constant between 0 and 31.");
 
         if ( fcode == AVR32_BUILTIN_MEMC )
           op1 = GEN_INT((~(1 << INTVAL(op1)))&0xffffffff);
         else
           op1 = GEN_INT((1 << INTVAL(op1))&0xffffffff);
         pat = GEN_FCN (icode) (op0, op0, op1);
         if (!pat)
           return 0;
         emit_insn (pat);
         return op0;
       }
       
     case AVR32_BUILTIN_SLEEP:
       {
 	arg0 = CALL_EXPR_ARG (exp, 0);
 	op0  = expand_normal (arg0);
 	int intval = INTVAL(op0);
 
 	/* Check if the argument if integer and if the value of integer
 	   is greater than 0. */ 
 	 
 	if (!CONSTANT_P (op0))
         error ("Parameter 1 to __builtin_sleep() is not a valid integer.");
 	if (intval < 0 )
 	     error ("Parameter 1 to __builtin_sleep() should be an integer greater than 0.");
 
         int strncmpval = strncmp (avr32_part_name,"uc3l", 4);
  
 	/* Check if op0 is less than 7 for uc3l* and less than 6 for other
 	   devices. By this check we are avoiding if operand is less than  
 	   256. For more devices, add more such checks. */
 	 
 	if ( strncmpval == 0 && intval >= 7)  
        error ("Parameter 1 to __builtin_sleep() should be less than or equal to 7.");
 	else if ( strncmp != 0 && intval >= 6)
 	    error ("Parameter 1 to __builtin_sleep() should be less than or equal to 6.");
 
 	emit_insn (gen_sleep(op0));
 	return target;
 
       }	
     case AVR32_BUILTIN_DELAY_CYCLES: 
       {
       arg0 = CALL_EXPR_ARG (exp, 0);
       op0 = expand_expr (arg0, NULL_RTX, VOIDmode, 0);
 
       if (TARGET_ARCH_AP)
         error (" __builtin_avr32_delay_cycles() not supported for \'%s\' architecture.", avr32_arch_name);
       if (!CONSTANT_P (op0))
        error ("Parameter 1 to __builtin_avr32_delay_cycles() should be an integer.");
       emit_insn (gen_delay_cycles (op0));
       return 0;
 
       }       

    }

  for (i = 0, d = bdesc_2arg; i < ARRAY_SIZE (bdesc_2arg); i++, d++)
    if (d->code == fcode)
      return avr32_expand_binop_builtin (d->icode, exp, target);


  /* @@@ Should really do something sensible here.  */
  return NULL_RTX;
}


/* Handle an "interrupt" or "isr" attribute;
   arguments as in struct attribute_spec.handler.  */
static tree
avr32_handle_isr_attribute (tree * node, tree name, tree args,
			    int flags, bool * no_add_attrs)
{
  if (DECL_P (*node))
    {
      if (TREE_CODE (*node) != FUNCTION_DECL)
	{
	  warning (OPT_Wattributes,"`%s' attribute only applies to functions",
		   IDENTIFIER_POINTER (name));
	  *no_add_attrs = true;
	}
      /* FIXME: the argument if any is checked for type attributes; should it
         be checked for decl ones? */
    }
  else
    {
      if (TREE_CODE (*node) == FUNCTION_TYPE
	  || TREE_CODE (*node) == METHOD_TYPE)
	{
	  if (avr32_isr_value (args) == AVR32_FT_UNKNOWN)
	    {
	      warning (OPT_Wattributes,"`%s' attribute ignored", IDENTIFIER_POINTER (name));
	      *no_add_attrs = true;
	    }
	}
      else if (TREE_CODE (*node) == POINTER_TYPE
	       && (TREE_CODE (TREE_TYPE (*node)) == FUNCTION_TYPE
		   || TREE_CODE (TREE_TYPE (*node)) == METHOD_TYPE)
	       && avr32_isr_value (args) != AVR32_FT_UNKNOWN)
	{
	  *node = build_variant_type_copy (*node);
	  TREE_TYPE (*node) = build_type_attribute_variant
	    (TREE_TYPE (*node),
	     tree_cons (name, args, TYPE_ATTRIBUTES (TREE_TYPE (*node))));
	  *no_add_attrs = true;
	}
      else
	{
	  /* Possibly pass this attribute on from the type to a decl.  */
	  if (flags & ((int) ATTR_FLAG_DECL_NEXT
		       | (int) ATTR_FLAG_FUNCTION_NEXT
		       | (int) ATTR_FLAG_ARRAY_NEXT))
	    {
	      *no_add_attrs = true;
	      return tree_cons (name, args, NULL_TREE);
	    }
	  else
	    {
	      warning (OPT_Wattributes,"`%s' attribute ignored", IDENTIFIER_POINTER (name));
	    }
	}
    }

  return NULL_TREE;
}


/* Handle an attribute requiring a FUNCTION_DECL;
   arguments as in struct attribute_spec.handler.  */
static tree
avr32_handle_fndecl_attribute (tree * node, tree name,
			       tree args,
			       int flags ATTRIBUTE_UNUSED,
			       bool * no_add_attrs)
{
  if (TREE_CODE (*node) != FUNCTION_DECL)
    {
      warning (OPT_Wattributes,"%qs attribute only applies to functions",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
      return NULL_TREE;
    }

  fndecl_attribute_args = args;
  if (args == NULL_TREE)
	  return NULL_TREE;

  tree value = TREE_VALUE (args);
  if (TREE_CODE (value) != INTEGER_CST)
    {
      warning (OPT_Wattributes,
	       "argument of %qs attribute is not an integer constant",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
    }

  return NULL_TREE;
}


/* Handle an acall attribute;
   arguments as in struct attribute_spec.handler.  */

static tree
avr32_handle_acall_attribute (tree * node, tree name,
			      tree args ATTRIBUTE_UNUSED,
			      int flags ATTRIBUTE_UNUSED, bool * no_add_attrs)
{
  if (TREE_CODE (*node) == FUNCTION_TYPE || TREE_CODE (*node) == METHOD_TYPE)
    {
      warning (OPT_Wattributes,"`%s' attribute not yet supported...",
	       IDENTIFIER_POINTER (name));
      *no_add_attrs = true;
      return NULL_TREE;
    }

  warning (OPT_Wattributes,"`%s' attribute only applies to functions",
	   IDENTIFIER_POINTER (name));
  *no_add_attrs = true;
  return NULL_TREE;
}


bool
avr32_flashvault_call(tree decl)
{
  tree attributes;
  tree fv_attribute;
  tree vector_tree;
  unsigned int vector;

  if (decl && TREE_CODE (decl) == FUNCTION_DECL) 
    {
      attributes = DECL_ATTRIBUTES(decl);
      fv_attribute = lookup_attribute ("flashvault", attributes);
      if (fv_attribute != NULL_TREE)
        {
          /* Get attribute parameter, for the function vector number. */
          /* 
          There is probably an easier, standard way to retrieve the
          attribute parameter which needs to be done here.
          */
          vector_tree = TREE_VALUE(fv_attribute);
          if (vector_tree != NULL_TREE)
            {
              vector = (unsigned int)TREE_INT_CST_LOW(TREE_VALUE(vector_tree));
              fprintf (asm_out_file,
                       "\tmov\tr8, lo(%i)\t# Load vector number for sscall.\n",
                       vector);
            }

          fprintf (asm_out_file,
                   "\tsscall\t# Secure system call.\n");

          return true;
        }
    }
  
  return false;
}


static bool has_attribute_p (tree decl, const char *name)
{
  if (decl && TREE_CODE (decl) == FUNCTION_DECL) 
    {
      return (lookup_attribute (name, DECL_ATTRIBUTES(decl)) != NULL_TREE);
    }
  return NULL_TREE;    
}


/* Return 0 if the attributes for two types are incompatible, 1 if they
   are compatible, and 2 if they are nearly compatible (which causes a
   warning to be generated).  */
static int
avr32_comp_type_attributes (tree type1, tree type2)
{
  bool acall1, acall2, isr1, isr2, naked1, naked2, fv1, fv2, fvimpl1, fvimpl2;

  /* Check for mismatch of non-default calling convention.  */
  if (TREE_CODE (type1) != FUNCTION_TYPE)
    return 1;

  /* Check for mismatched call attributes.  */
  acall1 = lookup_attribute ("acall", TYPE_ATTRIBUTES (type1)) != NULL;
  acall2 = lookup_attribute ("acall", TYPE_ATTRIBUTES (type2)) != NULL;
  naked1 = lookup_attribute ("naked", TYPE_ATTRIBUTES (type1)) != NULL;
  naked2 = lookup_attribute ("naked", TYPE_ATTRIBUTES (type2)) != NULL;
  fv1 = lookup_attribute ("flashvault", TYPE_ATTRIBUTES (type1)) != NULL;
  fv2 = lookup_attribute ("flashvault", TYPE_ATTRIBUTES (type2)) != NULL;
  fvimpl1 = lookup_attribute ("flashvault_impl", TYPE_ATTRIBUTES (type1)) != NULL;
  fvimpl2 = lookup_attribute ("flashvault_impl", TYPE_ATTRIBUTES (type2)) != NULL;
  isr1 = lookup_attribute ("isr", TYPE_ATTRIBUTES (type1)) != NULL;
  if (!isr1)
    isr1 = lookup_attribute ("interrupt", TYPE_ATTRIBUTES (type1)) != NULL;

  isr2 = lookup_attribute ("isr", TYPE_ATTRIBUTES (type2)) != NULL;
  if (!isr2)
    isr2 = lookup_attribute ("interrupt", TYPE_ATTRIBUTES (type2)) != NULL;

  if ((acall1 && isr2)
      || (acall2 && isr1)
      || (naked1 && isr2)
      || (naked2 && isr1)
      || (fv1 && isr2)
      || (fv2 && isr1)
      || (fvimpl1 && isr2)
      || (fvimpl2 && isr1)
      || (fv1 && fvimpl2)
      || (fv2 && fvimpl1)
      )
    return 0;

  return 1;
}


/* Computes the type of the current function.  */
static unsigned long
avr32_compute_func_type (void)
{
  unsigned long type = AVR32_FT_UNKNOWN;
  tree a;
  tree attr;

  if (TREE_CODE (current_function_decl) != FUNCTION_DECL)
    abort ();

  /* Decide if the current function is volatile.  Such functions never
     return, and many memory cycles can be saved by not storing register
     values that will never be needed again.  This optimization was added to
     speed up context switching in a kernel application.  */
  if (optimize > 0
      && TREE_NOTHROW (current_function_decl)
      && TREE_THIS_VOLATILE (current_function_decl))
    type |= AVR32_FT_VOLATILE;

  if (cfun->static_chain_decl != NULL)
    type |= AVR32_FT_NESTED;

  attr = DECL_ATTRIBUTES (current_function_decl);

  a = lookup_attribute ("isr", attr);
  if (a == NULL_TREE)
    a = lookup_attribute ("interrupt", attr);

  if (a == NULL_TREE)
    type |= AVR32_FT_NORMAL;
  else
    type |= avr32_isr_value (TREE_VALUE (a));


  a = lookup_attribute ("acall", attr);
  if (a != NULL_TREE)
    type |= AVR32_FT_ACALL;

  a = lookup_attribute ("naked", attr);
  if (a != NULL_TREE)
    type |= AVR32_FT_NAKED;

  a = lookup_attribute ("flashvault", attr);
  if (a != NULL_TREE)
    type |= AVR32_FT_FLASHVAULT;

  a = lookup_attribute ("flashvault_impl", attr);
  if (a != NULL_TREE)
    type |= AVR32_FT_FLASHVAULT_IMPL;

  return type;
}


/* Returns the type of the current function.  */
static unsigned long
avr32_current_func_type (void)
{
  if (AVR32_FUNC_TYPE (cfun->machine->func_type) == AVR32_FT_UNKNOWN)
    cfun->machine->func_type = avr32_compute_func_type ();

  return cfun->machine->func_type;
}


/*
This target hook should return true if we should not pass type solely
in registers. The file expr.h defines a definition that is usually appropriate,
refer to expr.h for additional documentation.
*/
bool
avr32_must_pass_in_stack (enum machine_mode mode ATTRIBUTE_UNUSED, tree type)
{
  if (type && AGGREGATE_TYPE_P (type)
      /* If the alignment is less than the size then pass in the struct on
         the stack. */
      && ((unsigned int) TYPE_ALIGN_UNIT (type) <
	  (unsigned int) int_size_in_bytes (type))
      /* If we support unaligned word accesses then structs of size 4 and 8
         can have any alignment and still be passed in registers. */
      && !(TARGET_UNALIGNED_WORD
	   && (int_size_in_bytes (type) == 4
	       || int_size_in_bytes (type) == 8))
      /* Double word structs need only a word alignment. */
      && !(int_size_in_bytes (type) == 8 && TYPE_ALIGN_UNIT (type) >= 4))
    return true;

  if (type && AGGREGATE_TYPE_P (type)
      /* Structs of size 3,5,6,7 are always passed in registers. */
      && (int_size_in_bytes (type) == 3
	  || int_size_in_bytes (type) == 5
	  || int_size_in_bytes (type) == 6 || int_size_in_bytes (type) == 7))
    return true;


  return (type && TREE_ADDRESSABLE (type));
}


bool
avr32_strict_argument_naming (CUMULATIVE_ARGS * ca ATTRIBUTE_UNUSED)
{
  return true;
}


/*
   This target hook should return true if an argument at the position indicated
   by cum should be passed by reference. This predicate is queried after target
   independent reasons for being passed by reference, such as TREE_ADDRESSABLE (type).

   If the hook returns true, a copy of that argument is made in memory and a
   pointer to the argument is passed instead of the argument itself. The pointer
   is passed in whatever way is appropriate for passing a pointer to that type.
*/
bool
avr32_pass_by_reference (CUMULATIVE_ARGS * cum ATTRIBUTE_UNUSED,
			 enum machine_mode mode ATTRIBUTE_UNUSED,
			 tree type, bool named ATTRIBUTE_UNUSED)
{
  return (type && (TREE_CODE (TYPE_SIZE (type)) != INTEGER_CST));
}


static int
avr32_arg_partial_bytes (CUMULATIVE_ARGS * pcum ATTRIBUTE_UNUSED,
			 enum machine_mode mode ATTRIBUTE_UNUSED,
			 tree type ATTRIBUTE_UNUSED,
			 bool named ATTRIBUTE_UNUSED)
{
  return 0;
}


struct gcc_target targetm = TARGET_INITIALIZER;

/*
  Table used to convert from register number in the assembler instructions and
  the register numbers used in gcc.
*/
const int avr32_function_arg_reglist[] = {
  INTERNAL_REGNUM (12),
  INTERNAL_REGNUM (11),
  INTERNAL_REGNUM (10),
  INTERNAL_REGNUM (9),
  INTERNAL_REGNUM (8)
};


rtx avr32_compare_op0 = NULL_RTX;
rtx avr32_compare_op1 = NULL_RTX;
rtx avr32_compare_operator = NULL_RTX;
rtx avr32_acc_cache = NULL_RTX;


/*
  Returns nonzero if it is allowed to store a value of mode mode in hard
  register number regno.
*/
int
avr32_hard_regno_mode_ok (int regnr, enum machine_mode mode)
{
  switch (mode)
    {
      case DImode:		/* long long */
      case DFmode:		/* double */
      case SCmode:		/* __complex__ float */
      case CSImode:		/* __complex__ int */
        if (regnr < 4)
	  {		/* long long int not supported in r12, sp, lr or pc. */
	    return 0;
	  }
        else
	  {
	    /* long long int has to be referred in even registers. */
            if (regnr % 2)
	      return 0;
	    else
	      return 1;
	  }
      case CDImode:		/* __complex__ long long */
      case DCmode:		/* __complex__ double */
      case TImode:		/* 16 bytes */
        if (regnr < 7)
	  return 0;
        else if (regnr % 2)
	  return 0;
        else
	  return 1;
      default:
        return 1;
    }
}


int
avr32_rnd_operands (rtx add, rtx shift)
{
  if (GET_CODE (shift) == CONST_INT &&
      GET_CODE (add) == CONST_INT && INTVAL (shift) > 0)
    {
      if ((1 << (INTVAL (shift) - 1)) == INTVAL (add))
	return TRUE;
    }

  return FALSE;
}


int
avr32_const_ok_for_constraint_p (HOST_WIDE_INT value, char c, const char *str)
{
  switch (c)
    {
    case 'K':
    case 'I':
      {
	HOST_WIDE_INT min_value = 0, max_value = 0;
	char size_str[3];
	int const_size;

	size_str[0] = str[2];
	size_str[1] = str[3];
	size_str[2] = '\0';
	const_size = atoi (size_str);

	if (toupper (str[1]) == 'U')
	  {
	    min_value = 0;
	    max_value = (1 << const_size) - 1;
	  }
	else if (toupper (str[1]) == 'S')
	  {
	    min_value = -(1 << (const_size - 1));
	    max_value = (1 << (const_size - 1)) - 1;
	  }

	if (c == 'I')
	  {
	    value = -value;
	  }

	if (value >= min_value && value <= max_value)
	  {
	    return 1;
	  }
	break;
      }
    case 'M':
      return avr32_mask_upper_bits_operand (GEN_INT (value), VOIDmode);
    case 'J':
      return avr32_hi16_immediate_operand (GEN_INT (value), VOIDmode);
    case 'O':
      return one_bit_set_operand (GEN_INT (value), VOIDmode);
    case 'N':
      return one_bit_cleared_operand (GEN_INT (value), VOIDmode);
    case 'L':
      /* The lower 16-bits are set. */
      return ((value & 0xffff) == 0xffff) ;
    }

  return 0;
}


/* Compute mask of registers which needs saving upon function entry. */
static unsigned long
avr32_compute_save_reg_mask (int push)
{
  unsigned long func_type;
  unsigned int save_reg_mask = 0;
  unsigned int reg;

  func_type = avr32_current_func_type ();

  if (IS_INTERRUPT (func_type))
    {
      unsigned int max_reg = 12;

      /* Get the banking scheme for the interrupt */
      switch (func_type)
	{
	case AVR32_FT_ISR_FULL:
	  max_reg = 0;
	  break;
	case AVR32_FT_ISR_HALF:
	  max_reg = 7;
	  break;
	case AVR32_FT_ISR_NONE:
	  max_reg = 12;
	  break;
	}

      /* Interrupt functions must not corrupt any registers, even call
         clobbered ones.  If this is a leaf function we can just examine the
         registers used by the RTL, but otherwise we have to assume that
         whatever function is called might clobber anything, and so we have
         to save all the call-clobbered registers as well.  */

      /* Need not push the registers r8-r12 for AVR32A architectures, as this
         is automatially done in hardware. We also do not have any shadow
         registers. */
      if (TARGET_UARCH_AVR32A)
	{
	  max_reg = 7;
	  func_type = AVR32_FT_ISR_NONE;
	}

      /* All registers which are used and are not shadowed must be saved. */
      for (reg = 0; reg <= max_reg; reg++)
	if (df_regs_ever_live_p (INTERNAL_REGNUM (reg))
	    || (!current_function_is_leaf
		&& call_used_regs[INTERNAL_REGNUM (reg)]))
	  save_reg_mask |= (1 << reg);

      /* Check LR */
      if ((df_regs_ever_live_p (LR_REGNUM)
	   || !current_function_is_leaf || frame_pointer_needed)
	  /* Only non-shadowed register models */
	  && (func_type == AVR32_FT_ISR_NONE))
	save_reg_mask |= (1 << ASM_REGNUM (LR_REGNUM));

      /* Make sure that the GOT register is pushed. */
      if (max_reg >= ASM_REGNUM (PIC_OFFSET_TABLE_REGNUM)
	  && current_function_uses_pic_offset_table)
	save_reg_mask |= (1 << ASM_REGNUM (PIC_OFFSET_TABLE_REGNUM));

    }
  else
    {
      int use_pushm = optimize_size;

      /* In the normal case we only need to save those registers which are
         call saved and which are used by this function.  */
      for (reg = 0; reg <= 7; reg++)
	if (df_regs_ever_live_p (INTERNAL_REGNUM (reg))
	    && !call_used_regs[INTERNAL_REGNUM (reg)])
	  save_reg_mask |= (1 << reg);

      /* Make sure that the GOT register is pushed. */
      if (current_function_uses_pic_offset_table)
	save_reg_mask |= (1 << ASM_REGNUM (PIC_OFFSET_TABLE_REGNUM));


      /* If we optimize for size and do not have anonymous arguments: use
         pushm/popm always. */
      if (use_pushm)
	{
	  if ((save_reg_mask & (1 << 0))
	      || (save_reg_mask & (1 << 1))
	      || (save_reg_mask & (1 << 2)) || (save_reg_mask & (1 << 3)))
	    save_reg_mask |= 0xf;

	  if ((save_reg_mask & (1 << 4))
	      || (save_reg_mask & (1 << 5))
	      || (save_reg_mask & (1 << 6)) || (save_reg_mask & (1 << 7)))
	    save_reg_mask |= 0xf0;

	  if ((save_reg_mask & (1 << 8)) || (save_reg_mask & (1 << 9)))
	    save_reg_mask |= 0x300;
	}


        /* Check LR */
        if ((df_regs_ever_live_p (LR_REGNUM)
        || !current_function_is_leaf
        || (optimize_size
        && save_reg_mask
        && !current_function_calls_eh_return)
          || frame_pointer_needed)
          && !IS_FLASHVAULT (func_type))
	{
	  if (push
	      /* Never pop LR into PC for functions which
	         calls __builtin_eh_return, since we need to
	         fix the SP after the restoring of the registers
	         and before returning. */
	      || current_function_calls_eh_return)
	    {
	      /* Push/Pop LR */
	      save_reg_mask |= (1 << ASM_REGNUM (LR_REGNUM));
	    }
	  else
	    {
	      /* Pop PC */
	      save_reg_mask |= (1 << ASM_REGNUM (PC_REGNUM));
	    }
	}
    }


  /* Save registers so the exception handler can modify them.  */
  if (current_function_calls_eh_return)
    {
      unsigned int i;

      for (i = 0;; i++)
	{
	  reg = EH_RETURN_DATA_REGNO (i);
	  if (reg == INVALID_REGNUM)
	    break;
	  save_reg_mask |= 1 << ASM_REGNUM (reg);
	}
    }

  return save_reg_mask;
}


/* Compute total size in bytes of all saved registers. */
static int
avr32_get_reg_mask_size (int reg_mask)
{
  int reg, size;
  size = 0;

  for (reg = 0; reg <= 15; reg++)
    if (reg_mask & (1 << reg))
      size += 4;

  return size;
}


/* Get a register from one of the registers which are saved onto the stack
  upon function entry. */
static int
avr32_get_saved_reg (int save_reg_mask)
{
  unsigned int reg;

  /* Find the first register which is saved in the saved_reg_mask */
  for (reg = 0; reg <= 15; reg++)
    if (save_reg_mask & (1 << reg))
      return reg;

  return -1;
}


/* Return 1 if it is possible to return using a single instruction. */
int
avr32_use_return_insn (int iscond)
{
  unsigned int func_type = avr32_current_func_type ();
  unsigned long saved_int_regs;

  /* Never use a return instruction before reload has run. */
  if (!reload_completed)
    return 0;

  /* Must adjust the stack for vararg functions. */
  if (current_function_args_info.uses_anonymous_args)
    return 0;

  /* If there a stack adjstment.  */
  if (get_frame_size ())
    return 0;

  saved_int_regs = avr32_compute_save_reg_mask (TRUE);

  /* Conditional returns can not be performed in one instruction if we need
     to restore registers from the stack */
  if (iscond && saved_int_regs)
    return 0;

  /* Conditional return can not be used for interrupt handlers. */
  if (iscond && IS_INTERRUPT (func_type))
    return 0;

  /* For interrupt handlers which needs to pop registers */
  if (saved_int_regs && IS_INTERRUPT (func_type))
    return 0;


  /* If there are saved registers but the LR isn't saved, then we need two
     instructions for the return.  */
  if (saved_int_regs && !(saved_int_regs & (1 << ASM_REGNUM (LR_REGNUM))))
    return 0;


  return 1;
}


/* Generate some function prologue info in the assembly file. */
void
avr32_target_asm_function_prologue (FILE * f, HOST_WIDE_INT frame_size)
{
  unsigned long func_type = avr32_current_func_type ();

  if (IS_NAKED (func_type))
    fprintf (f,
      "\t# Function is naked: Prologue and epilogue provided by programmer\n");

  if (IS_FLASHVAULT (func_type))
  {
    fprintf(f, 
      "\t.ident \"flashvault\"\n\t# Function is defined with flashvault attribute.\n");
  }

  if (IS_FLASHVAULT_IMPL (func_type))
  {
    fprintf(f, 
      "\t.ident \"flashvault\"\n\t# Function is defined with flashvault_impl attribute.\n");

    /* Save information on flashvault function declaration. */
    tree fv_attribute = lookup_attribute ("flashvault_impl", DECL_ATTRIBUTES(current_function_decl));
    if (fv_attribute != NULL_TREE)
      {
        tree vector_tree = TREE_VALUE(fv_attribute);
        if (vector_tree != NULL_TREE)
          {
            unsigned int vector_num;
            const char * name;

            vector_num = (unsigned int) TREE_INT_CST_LOW (TREE_VALUE (vector_tree));

            name = XSTR  (XEXP (DECL_RTL (current_function_decl), 0), 0);

            flashvault_decl_list_add (vector_num, name);
          }
      }
  }

  if (IS_INTERRUPT (func_type))
    {
      switch (func_type)
        {
          case AVR32_FT_ISR_FULL:
            fprintf (f,
                     "\t# Interrupt Function: Fully shadowed register file\n");
            break;
          case AVR32_FT_ISR_HALF:
            fprintf (f,
                     "\t# Interrupt Function: Half shadowed register file\n");
            break;
          default:
          case AVR32_FT_ISR_NONE:
            fprintf (f, "\t# Interrupt Function: No shadowed register file\n");
            break;
        }
    }


  fprintf (f, "\t# args = %i, frame = %li, pretend = %i\n",
           current_function_args_size, frame_size,
           current_function_pretend_args_size);

  fprintf (f, "\t# frame_needed = %i, leaf_function = %i\n",
           frame_pointer_needed, current_function_is_leaf);

  fprintf (f, "\t# uses_anonymous_args = %i\n",
           current_function_args_info.uses_anonymous_args);

  if (current_function_calls_eh_return)
    fprintf (f, "\t# Calls __builtin_eh_return.\n");

}


/* Generate and emit an insn that we will recognize as a pushm or stm.
   Unfortunately, since this insn does not reflect very well the actual
   semantics of the operation, we need to annotate the insn for the benefit
   of DWARF2 frame unwind information.  */

int avr32_convert_to_reglist16 (int reglist8_vect);

static rtx
emit_multi_reg_push (int reglist, int usePUSHM)
{
  rtx insn;
  rtx dwarf;
  rtx tmp;
  rtx reg;
  int i;
  int nr_regs;
  int index = 0;

  if (usePUSHM)
    {
      insn = emit_insn (gen_pushm (gen_rtx_CONST_INT (SImode, reglist)));
      reglist = avr32_convert_to_reglist16 (reglist);
    }
  else
    {
      insn = emit_insn (gen_stm (stack_pointer_rtx,
				 gen_rtx_CONST_INT (SImode, reglist),
				 gen_rtx_CONST_INT (SImode, 1)));
    }

  nr_regs = avr32_get_reg_mask_size (reglist) / 4;
  dwarf = gen_rtx_SEQUENCE (VOIDmode, rtvec_alloc (nr_regs + 1));

  for (i = 15; i >= 0; i--)
    {
      if (reglist & (1 << i))
	{
	  reg = gen_rtx_REG (SImode, INTERNAL_REGNUM (i));
	  tmp = gen_rtx_SET (VOIDmode,
			     gen_rtx_MEM (SImode,
					  plus_constant (stack_pointer_rtx,
							 4 * index)), reg);
	  RTX_FRAME_RELATED_P (tmp) = 1;
	  XVECEXP (dwarf, 0, 1 + index++) = tmp;
	}
    }

  tmp = gen_rtx_SET (SImode,
		     stack_pointer_rtx,
		     gen_rtx_PLUS (SImode,
				   stack_pointer_rtx,
				   GEN_INT (-4 * nr_regs)));
  RTX_FRAME_RELATED_P (tmp) = 1;
  XVECEXP (dwarf, 0, 0) = tmp;
  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR, dwarf,
					REG_NOTES (insn));
  return insn;
}

rtx
avr32_gen_load_multiple (rtx * regs, int count, rtx from,
			 int write_back, int in_struct_p, int scalar_p)
{

  rtx result;
  int i = 0, j;

  result =
    gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count + (write_back ? 1 : 0)));

  if (write_back)
    {
      XVECEXP (result, 0, 0)
	= gen_rtx_SET (GET_MODE (from), from,
		       plus_constant (from, count * 4));
      i = 1;
      count++;
    }


  for (j = 0; i < count; i++, j++)
    {
      rtx unspec;
      rtx mem = gen_rtx_MEM (SImode, plus_constant (from, j * 4));
      MEM_IN_STRUCT_P (mem) = in_struct_p;
      MEM_SCALAR_P (mem) = scalar_p;
      unspec = gen_rtx_UNSPEC (VOIDmode, gen_rtvec (1, mem), UNSPEC_LDM);
      XVECEXP (result, 0, i) = gen_rtx_SET (VOIDmode, regs[j], unspec);
    }

  return result;
}


rtx
avr32_gen_store_multiple (rtx * regs, int count, rtx to,
			  int in_struct_p, int scalar_p)
{
  rtx result;
  int i = 0, j;

  result = gen_rtx_PARALLEL (VOIDmode, rtvec_alloc (count));

  for (j = 0; i < count; i++, j++)
    {
      rtx mem = gen_rtx_MEM (SImode, plus_constant (to, j * 4));
      MEM_IN_STRUCT_P (mem) = in_struct_p;
      MEM_SCALAR_P (mem) = scalar_p;
      XVECEXP (result, 0, i)
	= gen_rtx_SET (VOIDmode, mem,
		       gen_rtx_UNSPEC (VOIDmode,
				       gen_rtvec (1, regs[j]),
				       UNSPEC_STORE_MULTIPLE));
    }

  return result;
}


/* Move a block of memory if it is word aligned or we support unaligned
   word memory accesses. The size must be maximum 64 bytes. */
int
avr32_gen_movmemsi (rtx * operands)
{
  HOST_WIDE_INT bytes_to_go;
  rtx src, dst;
  rtx st_src, st_dst;
  int src_offset = 0, dst_offset = 0;
  int block_size;
  int dst_in_struct_p, src_in_struct_p;
  int dst_scalar_p, src_scalar_p;
  int unaligned;

  if (GET_CODE (operands[2]) != CONST_INT
      || GET_CODE (operands[3]) != CONST_INT
      || INTVAL (operands[2]) > 64
      || ((INTVAL (operands[3]) & 3) && !TARGET_UNALIGNED_WORD))
    return 0;

  unaligned = (INTVAL (operands[3]) & 3) != 0;

  block_size = 4;

  st_dst = XEXP (operands[0], 0);
  st_src = XEXP (operands[1], 0);

  dst_in_struct_p = MEM_IN_STRUCT_P (operands[0]);
  dst_scalar_p = MEM_SCALAR_P (operands[0]);
  src_in_struct_p = MEM_IN_STRUCT_P (operands[1]);
  src_scalar_p = MEM_SCALAR_P (operands[1]);

  dst = copy_to_mode_reg (SImode, st_dst);
  src = copy_to_mode_reg (SImode, st_src);

  bytes_to_go = INTVAL (operands[2]);

  while (bytes_to_go)
    {
      enum machine_mode move_mode;
      /* (Seems to be a problem with reloads for the movti pattern so this is
         disabled until that problem is resolved)
         UPDATE: Problem seems to be solved now.... */
      if (bytes_to_go >= GET_MODE_SIZE (TImode) && !unaligned
	  /* Do not emit ldm/stm for UC3 as ld.d/st.d is more optimal. */
	  && !TARGET_ARCH_UC)
	move_mode = TImode;
      else if ((bytes_to_go >= GET_MODE_SIZE (DImode)) && !unaligned)
	move_mode = DImode;
      else if (bytes_to_go >= GET_MODE_SIZE (SImode))
	move_mode = SImode;
      else
	move_mode = QImode;

      {
        rtx src_mem;
	rtx dst_mem = gen_rtx_MEM (move_mode,
				   gen_rtx_PLUS (SImode, dst,
						 GEN_INT (dst_offset)));
        dst_offset += GET_MODE_SIZE (move_mode);
        if ( 0 /* This causes an error in GCC. Think there is
                  something wrong in the gcse pass which causes REQ_EQUIV notes
                  to be wrong so disabling it for now. */
             && move_mode == TImode
             && INTVAL (operands[2]) > GET_MODE_SIZE (TImode) )
          {
            src_mem = gen_rtx_MEM (move_mode,
				   gen_rtx_POST_INC (SImode, src));
          }
        else
          {
            src_mem = gen_rtx_MEM (move_mode,
				   gen_rtx_PLUS (SImode, src,
						 GEN_INT (src_offset)));
            src_offset += GET_MODE_SIZE (move_mode);
          }

	bytes_to_go -= GET_MODE_SIZE (move_mode);

	MEM_IN_STRUCT_P (dst_mem) = dst_in_struct_p;
	MEM_SCALAR_P (dst_mem) = dst_scalar_p;

	MEM_IN_STRUCT_P (src_mem) = src_in_struct_p;
	MEM_SCALAR_P (src_mem) = src_scalar_p;
	emit_move_insn (dst_mem, src_mem);

      }
    }

  return 1;
}


/* Expand the prologue instruction. */
void
avr32_expand_prologue (void)
{
  rtx insn, dwarf;
  unsigned long saved_reg_mask;
  int reglist8 = 0;

  /* Naked functions do not have a prologue. */
  if (IS_NAKED (avr32_current_func_type ()))
    return;

  saved_reg_mask = avr32_compute_save_reg_mask (TRUE);

  if (saved_reg_mask)
    {
      /* Must push used registers. */

      /* Should we use POPM or LDM? */
      int usePUSHM = TRUE;
      reglist8 = 0;
      if (((saved_reg_mask & (1 << 0)) ||
	   (saved_reg_mask & (1 << 1)) ||
	   (saved_reg_mask & (1 << 2)) || (saved_reg_mask & (1 << 3))))
	{
	  /* One of R0-R3 should at least be pushed. */
	  if (((saved_reg_mask & (1 << 0)) &&
	       (saved_reg_mask & (1 << 1)) &&
	       (saved_reg_mask & (1 << 2)) && (saved_reg_mask & (1 << 3))))
	    {
	      /* All should be pushed. */
	      reglist8 |= 0x01;
	    }
	  else
	    {
	      usePUSHM = FALSE;
	    }
	}

      if (((saved_reg_mask & (1 << 4)) ||
	   (saved_reg_mask & (1 << 5)) ||
	   (saved_reg_mask & (1 << 6)) || (saved_reg_mask & (1 << 7))))
	{
	  /* One of R4-R7 should at least be pushed */
	  if (((saved_reg_mask & (1 << 4)) &&
	       (saved_reg_mask & (1 << 5)) &&
	       (saved_reg_mask & (1 << 6)) && (saved_reg_mask & (1 << 7))))
	    {
	      if (usePUSHM)
		/* All should be pushed */
		reglist8 |= 0x02;
	    }
	  else
	    {
	      usePUSHM = FALSE;
	    }
	}

      if (((saved_reg_mask & (1 << 8)) || (saved_reg_mask & (1 << 9))))
	{
	  /* One of R8-R9 should at least be pushed. */
	  if (((saved_reg_mask & (1 << 8)) && (saved_reg_mask & (1 << 9))))
	    {
	      if (usePUSHM)
		/* All should be pushed. */
		reglist8 |= 0x04;
	    }
	  else
	    {
	      usePUSHM = FALSE;
	    }
	}

      if (saved_reg_mask & (1 << 10))
	reglist8 |= 0x08;

      if (saved_reg_mask & (1 << 11))
	reglist8 |= 0x10;

      if (saved_reg_mask & (1 << 12))
	reglist8 |= 0x20;

      if ((saved_reg_mask & (1 << ASM_REGNUM (LR_REGNUM)))
           && !IS_FLASHVAULT (avr32_current_func_type ()))
	{
	  /* Push LR */
	  reglist8 |= 0x40;
	}

      if (usePUSHM)
	{
	  insn = emit_multi_reg_push (reglist8, TRUE);
	}
      else
	{
	  insn = emit_multi_reg_push (saved_reg_mask, FALSE);
	}
      RTX_FRAME_RELATED_P (insn) = 1;

      /* Prevent this instruction from being scheduled after any other
         instructions.  */
      emit_insn (gen_blockage ());
    }

  /* Set frame pointer */
  if (frame_pointer_needed)
    {
      insn = emit_move_insn (frame_pointer_rtx, stack_pointer_rtx);
      RTX_FRAME_RELATED_P (insn) = 1;
    }

  if (get_frame_size () > 0)
    {
      if (avr32_const_ok_for_constraint_p (get_frame_size (), 'K', "Ks21"))
	{
	  insn = emit_insn (gen_rtx_SET (SImode,
					 stack_pointer_rtx,
					 gen_rtx_PLUS (SImode,
						       stack_pointer_rtx,
						       gen_rtx_CONST_INT
						       (SImode,
							-get_frame_size
							()))));
	  RTX_FRAME_RELATED_P (insn) = 1;
	}
      else
	{
	  /* Immediate is larger than k21 We must either check if we can use
	     one of the pushed reegisters as temporary storage or we must
	     make us a temp register by pushing a register to the stack. */
	  rtx temp_reg, const_pool_entry, insn;
	  if (saved_reg_mask)
	    {
	      temp_reg =
		gen_rtx_REG (SImode,
			     INTERNAL_REGNUM (avr32_get_saved_reg
					      (saved_reg_mask)));
	    }
	  else
	    {
	      temp_reg = gen_rtx_REG (SImode, INTERNAL_REGNUM (7));
	      emit_move_insn (gen_rtx_MEM
			      (SImode,
			       gen_rtx_PRE_DEC (SImode, stack_pointer_rtx)),
			      temp_reg);
	    }

	  const_pool_entry =
	    force_const_mem (SImode,
			     gen_rtx_CONST_INT (SImode, get_frame_size ()));
	  emit_move_insn (temp_reg, const_pool_entry);

	  insn = emit_insn (gen_rtx_SET (SImode,
					 stack_pointer_rtx,
					 gen_rtx_MINUS (SImode,
							stack_pointer_rtx,
							temp_reg)));

	  dwarf = gen_rtx_SET (VOIDmode, stack_pointer_rtx,
			       gen_rtx_PLUS (SImode, stack_pointer_rtx,
					     GEN_INT (-get_frame_size ())));
	  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_FRAME_RELATED_EXPR,
						dwarf, REG_NOTES (insn));
	  RTX_FRAME_RELATED_P (insn) = 1;

	  if (!saved_reg_mask)
	    {
	      insn =
		emit_move_insn (temp_reg,
				gen_rtx_MEM (SImode,
					     gen_rtx_POST_INC (SImode,
							       gen_rtx_REG
							       (SImode,
								13))));
	    }

	  /* Mark the temp register as dead */
	  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_DEAD, temp_reg,
						REG_NOTES (insn));


	}

      /* Prevent the the stack adjustment to be scheduled after any
         instructions using the frame pointer.  */
      emit_insn (gen_blockage ());
    }

  /* Load GOT */
  if (flag_pic)
    {
      avr32_load_pic_register ();

      /* gcc does not know that load or call instructions might use the pic
         register so it might schedule these instructions before the loading
         of the pic register. To avoid this emit a barrier for now. TODO!
         Find out a better way to let gcc know which instructions might use
         the pic register. */
      emit_insn (gen_blockage ());
    }
  return;
}


void
avr32_set_return_address (rtx source, rtx scratch)
{
  rtx addr;
  unsigned long saved_regs;

  saved_regs = avr32_compute_save_reg_mask (TRUE);

  if (!(saved_regs & (1 << ASM_REGNUM (LR_REGNUM))))
    emit_move_insn (gen_rtx_REG (Pmode, LR_REGNUM), source);
  else
    {
      if (frame_pointer_needed)
	addr = gen_rtx_REG (Pmode, FRAME_POINTER_REGNUM);
      else
	if (avr32_const_ok_for_constraint_p (get_frame_size (), 'K', "Ks16"))
	{
	  addr = plus_constant (stack_pointer_rtx, get_frame_size ());
	}
      else
	{
	  emit_insn (gen_movsi (scratch, GEN_INT (get_frame_size ())));
	  addr = scratch;
	}
      emit_move_insn (gen_rtx_MEM (Pmode, addr), source);
    }
}


/* Return the length of INSN.  LENGTH is the initial length computed by
   attributes in the machine-description file.  */
int
avr32_adjust_insn_length (rtx insn ATTRIBUTE_UNUSED,
			  int length ATTRIBUTE_UNUSED)
{
  return length;
}


void
avr32_output_return_instruction (int single_ret_inst ATTRIBUTE_UNUSED,
				 int iscond ATTRIBUTE_UNUSED,
				 rtx cond ATTRIBUTE_UNUSED, rtx r12_imm)
{

  unsigned long saved_reg_mask;
  int insert_ret = TRUE;
  int reglist8 = 0;
  int stack_adjustment = get_frame_size ();
  unsigned int func_type = avr32_current_func_type ();
  FILE *f = asm_out_file;

  /* Naked functions does not have an epilogue */
  if (IS_NAKED (func_type))
    return;

  saved_reg_mask = avr32_compute_save_reg_mask (FALSE);

  /* Reset frame pointer */
  if (stack_adjustment > 0)
    {
      if (avr32_const_ok_for_constraint_p (stack_adjustment, 'I', "Is21"))
	{
	  fprintf (f, "\tsub\tsp, %i # Reset Frame Pointer\n",
		   -stack_adjustment);
	}
      else
	{
	  /* TODO! Is it safe to use r8 as scratch?? */
	  fprintf (f, "\tmov\tr8, lo(%i) # Reset Frame Pointer\n",
		   -stack_adjustment);
	  fprintf (f, "\torh\tr8, hi(%i) # Reset Frame Pointer\n",
		   -stack_adjustment);
	  fprintf (f, "\tadd\tsp, r8  # Reset Frame Pointer\n");
	}
    }

  if (saved_reg_mask)
    {
      /* Must pop used registers */

      /* Should we use POPM or LDM? */
      int usePOPM = TRUE;
      if (((saved_reg_mask & (1 << 0)) ||
	   (saved_reg_mask & (1 << 1)) ||
	   (saved_reg_mask & (1 << 2)) || (saved_reg_mask & (1 << 3))))
	{
	  /* One of R0-R3 should at least be popped */
	  if (((saved_reg_mask & (1 << 0)) &&
	       (saved_reg_mask & (1 << 1)) &&
	       (saved_reg_mask & (1 << 2)) && (saved_reg_mask & (1 << 3))))
	    {
	      /* All should be popped */
	      reglist8 |= 0x01;
	    }
	  else
	    {
	      usePOPM = FALSE;
	    }
	}

      if (((saved_reg_mask & (1 << 4)) ||
	   (saved_reg_mask & (1 << 5)) ||
	   (saved_reg_mask & (1 << 6)) || (saved_reg_mask & (1 << 7))))
	{
	  /* One of R0-R3 should at least be popped */
	  if (((saved_reg_mask & (1 << 4)) &&
	       (saved_reg_mask & (1 << 5)) &&
	       (saved_reg_mask & (1 << 6)) && (saved_reg_mask & (1 << 7))))
	    {
	      if (usePOPM)
		/* All should be popped */
		reglist8 |= 0x02;
	    }
	  else
	    {
	      usePOPM = FALSE;
	    }
	}

      if (((saved_reg_mask & (1 << 8)) || (saved_reg_mask & (1 << 9))))
	{
	  /* One of R8-R9 should at least be pushed */
	  if (((saved_reg_mask & (1 << 8)) && (saved_reg_mask & (1 << 9))))
	    {
	      if (usePOPM)
		/* All should be pushed */
		reglist8 |= 0x04;
	    }
	  else
	    {
	      usePOPM = FALSE;
	    }
	}

      if (saved_reg_mask & (1 << 10))
	reglist8 |= 0x08;

      if (saved_reg_mask & (1 << 11))
	reglist8 |= 0x10;

      if (saved_reg_mask & (1 << 12))
	reglist8 |= 0x20;

      if (saved_reg_mask & (1 << ASM_REGNUM (LR_REGNUM)))
	/* Pop LR */
	reglist8 |= 0x40;

      if ((saved_reg_mask & (1 << ASM_REGNUM (PC_REGNUM))) 
           && !IS_FLASHVAULT_IMPL (func_type))
	/* Pop LR into PC. */
	reglist8 |= 0x80;

      if (usePOPM)
	{
	  char reglist[64];	/* 64 bytes should be enough... */
	  avr32_make_reglist8 (reglist8, (char *) reglist);

	  if (reglist8 & 0x80)
	    /* This instruction is also a return */
	    insert_ret = FALSE;

	  if (r12_imm && !insert_ret)
	    fprintf (f, "\tpopm\t%s, r12=%li\n", reglist, INTVAL (r12_imm));
	  else
	    fprintf (f, "\tpopm\t%s\n", reglist);

	}
      else
	{
	  char reglist[64];	/* 64 bytes should be enough... */
	  avr32_make_reglist16 (saved_reg_mask, (char *) reglist);
	  if (saved_reg_mask & (1 << ASM_REGNUM (PC_REGNUM)))
	    /* This instruction is also a return */
	    insert_ret = FALSE;

	  if (r12_imm && !insert_ret)
	    fprintf (f, "\tldm\tsp++, %s, r12=%li\n", reglist,
		     INTVAL (r12_imm));
	  else
	    fprintf (f, "\tldm\tsp++, %s\n", reglist);

	}

    }

  /* Stack adjustment for exception handler.  */
  if (current_function_calls_eh_return)
    fprintf (f, "\tadd\tsp, r%d\n", ASM_REGNUM (EH_RETURN_STACKADJ_REGNO));


  if (IS_INTERRUPT (func_type))
    {
      fprintf (f, "\trete\n");
    }
  else if (IS_FLASHVAULT (func_type))
    {
      /* Normal return from Secure System call, increment SS_RAR before
      returning. Use R8 as scratch. */
      fprintf (f,
               "\t# Normal return from sscall.\n"
               "\t# Increment SS_RAR before returning.\n"
               "\t# Use R8 as scratch.\n"
               "\tmfsr\tr8,  440\n"
               "\tsub\tr8,  -2\n"
               "\tmtsr\t440, r8\n"
               "\tretss\n");
    }
  else if (insert_ret)
    {
      if (r12_imm)
	fprintf (f, "\tretal\t%li\n", INTVAL (r12_imm));
      else
	fprintf (f, "\tretal\tr12\n");
    }
}

void
avr32_make_reglist16 (int reglist16_vect, char *reglist16_string)
{
  int i;
  bool first_reg = true;
  /* Make sure reglist16_string is empty. */
  reglist16_string[0] = '\0';

  for (i = 0; i < 16; ++i)
    {
      if (reglist16_vect & (1 << i))
	{
          first_reg == true ?  first_reg = false : strcat(reglist16_string,", ");
	  strcat (reglist16_string, reg_names[INTERNAL_REGNUM (i)]);
	}
    }
}

int
avr32_convert_to_reglist16 (int reglist8_vect)
{
  int reglist16_vect = 0;
  if (reglist8_vect & 0x1)
    reglist16_vect |= 0xF;
  if (reglist8_vect & 0x2)
    reglist16_vect |= 0xF0;
  if (reglist8_vect & 0x4)
    reglist16_vect |= 0x300;
  if (reglist8_vect & 0x8)
    reglist16_vect |= 0x400;
  if (reglist8_vect & 0x10)
    reglist16_vect |= 0x800;
  if (reglist8_vect & 0x20)
    reglist16_vect |= 0x1000;
  if (reglist8_vect & 0x40)
    reglist16_vect |= 0x4000;
  if (reglist8_vect & 0x80)
    reglist16_vect |= 0x8000;

  return reglist16_vect;
}

void
avr32_make_reglist8 (int reglist8_vect, char *reglist8_string)
{
  /* Make sure reglist8_string is empty. */
  reglist8_string[0] = '\0';

  if (reglist8_vect & 0x1)
    strcpy (reglist8_string, "r0-r3");
  if (reglist8_vect & 0x2)
    strlen (reglist8_string) ? strcat (reglist8_string, ", r4-r7") :
      strcpy (reglist8_string, "r4-r7");
  if (reglist8_vect & 0x4)
    strlen (reglist8_string) ? strcat (reglist8_string, ", r8-r9") :
      strcpy (reglist8_string, "r8-r9");
  if (reglist8_vect & 0x8)
    strlen (reglist8_string) ? strcat (reglist8_string, ", r10") :
      strcpy (reglist8_string, "r10");
  if (reglist8_vect & 0x10)
    strlen (reglist8_string) ? strcat (reglist8_string, ", r11") :
      strcpy (reglist8_string, "r11");
  if (reglist8_vect & 0x20)
    strlen (reglist8_string) ? strcat (reglist8_string, ", r12") :
      strcpy (reglist8_string, "r12");
  if (reglist8_vect & 0x40)
    strlen (reglist8_string) ? strcat (reglist8_string, ", lr") :
      strcpy (reglist8_string, "lr");
  if (reglist8_vect & 0x80)
    strlen (reglist8_string) ? strcat (reglist8_string, ", pc") :
      strcpy (reglist8_string, "pc");
}


int
avr32_eh_return_data_regno (int n)
{
  if (n >= 0 && n <= 3)
    return 8 + n;
  else
    return INVALID_REGNUM;
}


/* Compute the distance from register FROM to register TO.
   These can be the arg pointer, the frame pointer or
   the stack pointer.
   Typical stack layout looks like this:

       old stack pointer -> |    |
			     ----
			    |    | \
			    |    |   saved arguments for
			    |    |   vararg functions
 arg_pointer	->	    |    | /
			      --
			    |    | \
			    |    |   call saved
			    |    |   registers
			    |    | /
  frame ptr	 ->	--
			    |    | \
			    |    |   local
			    |    |   variables
  stack ptr -->	     |    | /
			      --
			    |    | \
			    |    |   outgoing
			    |    |   arguments
			    |    | /
			      --

  For a given funciton some or all of these stack compomnents
  may not be needed, giving rise to the possibility of
  eliminating some of the registers.

  The values returned by this function must reflect the behaviour
  of avr32_expand_prologue() and avr32_compute_save_reg_mask().

  The sign of the number returned reflects the direction of stack
  growth, so the values are positive for all eliminations except
  from the soft frame pointer to the hard frame pointer.  */
int
avr32_initial_elimination_offset (int from, int to)
{
  int i;
  int call_saved_regs = 0;
  unsigned long saved_reg_mask;
  unsigned int local_vars = get_frame_size ();

  saved_reg_mask = avr32_compute_save_reg_mask (TRUE);

  for (i = 0; i < 16; ++i)
    {
      if (saved_reg_mask & (1 << i))
	call_saved_regs += 4;
    }

  switch (from)
    {
    case ARG_POINTER_REGNUM:
      switch (to)
	{
	case STACK_POINTER_REGNUM:
	  return call_saved_regs + local_vars;
	case FRAME_POINTER_REGNUM:
	  return call_saved_regs;
	default:
	  abort ();
	}
    case FRAME_POINTER_REGNUM:
      switch (to)
	{
	case STACK_POINTER_REGNUM:
	  return local_vars;
	default:
	  abort ();
	}
    default:
      abort ();
    }
}


/*
  Returns a rtx used when passing the next argument to a function.
  avr32_init_cumulative_args() and avr32_function_arg_advance() sets which
  register to use.
*/
rtx
avr32_function_arg (CUMULATIVE_ARGS * cum, enum machine_mode mode,
		    tree type, int named)
{
  int index = -1;
  //unsigned long func_type = avr32_current_func_type ();
  //int last_reg_index = (IS_FLASHVAULT(func_type) || IS_FLASHVAULT_IMPL(func_type) || cum->flashvault_func ? LAST_CUM_REG_INDEX - 1 : LAST_CUM_REG_INDEX);
  int last_reg_index = (cum->flashvault_func ? LAST_CUM_REG_INDEX - 1 : LAST_CUM_REG_INDEX);

  HOST_WIDE_INT arg_size, arg_rsize;
  if (type)
    {
      arg_size = int_size_in_bytes (type);
    }
  else
    {
      arg_size = GET_MODE_SIZE (mode);
    }
  arg_rsize = PUSH_ROUNDING (arg_size);

  /*
     The last time this macro is called, it is called with mode == VOIDmode,
     and its result is passed to the call or call_value pattern as operands 2
     and 3 respectively. */
  if (mode == VOIDmode)
    {
      return gen_rtx_CONST_INT (SImode, 22);	/* ToDo: fixme. */
    }

  if ((*targetm.calls.must_pass_in_stack) (mode, type) || !named)
    {
      return NULL_RTX;
    }

  if (arg_rsize == 8)
    {
      /* use r11:r10 or r9:r8. */
      if (!(GET_USED_INDEX (cum, 1) || GET_USED_INDEX (cum, 2)))
	index = 1;
      else if ((last_reg_index == 4) && 
               !(GET_USED_INDEX (cum, 3) || GET_USED_INDEX (cum, 4)))
	index = 3;
      else
	index = -1;
    }
  else if (arg_rsize == 4)
    {				/* Use first available register */
      index = 0;
      while (index <= last_reg_index && GET_USED_INDEX (cum, index))
	index++;
      if (index > last_reg_index)
	index = -1;
    }

  SET_REG_INDEX (cum, index);

  if (GET_REG_INDEX (cum) >= 0)
    return gen_rtx_REG (mode, avr32_function_arg_reglist[GET_REG_INDEX (cum)]);

  return NULL_RTX;
}


/* Set the register used for passing the first argument to a function. */
void
avr32_init_cumulative_args (CUMULATIVE_ARGS * cum,
                            tree fntype ATTRIBUTE_UNUSED,
                            rtx libname ATTRIBUTE_UNUSED,
                            tree fndecl)
{
  /* Set all registers as unused. */
  SET_INDEXES_UNUSED (cum);

  /* Reset uses_anonymous_args */
  cum->uses_anonymous_args = 0;

  /* Reset size of stack pushed arguments */
  cum->stack_pushed_args_size = 0;
  
  cum->flashvault_func = (fndecl && (has_attribute_p (fndecl,"flashvault") || has_attribute_p (fndecl,"flashvault_impl")));
}


/*
  Set register used for passing the next argument to a function. Only the
  Scratch Registers are used.

		number  name
		   15   r15  PC
		   14   r14  LR
		   13   r13 _SP_________
     FIRST_CUM_REG 12   r12 _||_
		   10   r11  ||
		   11   r10 _||_  Scratch Registers
		    8   r9   ||
  LAST_SCRATCH_REG  9   r8  _\/_________
		    6   r7   /\
		    7   r6   ||
		    4   r5   ||
		    5   r4   ||
		    2   r3   ||
		    3   r2   ||
		    0   r1   ||
		    1   r0  _||_________

*/
void
avr32_function_arg_advance (CUMULATIVE_ARGS * cum, enum machine_mode mode,
			    tree type, int named ATTRIBUTE_UNUSED)
{
  HOST_WIDE_INT arg_size, arg_rsize;

  if (type)
    {
      arg_size = int_size_in_bytes (type);
    }
  else
    {
      arg_size = GET_MODE_SIZE (mode);
    }
  arg_rsize = PUSH_ROUNDING (arg_size);

  /* If the argument had to be passed in stack, no register is used. */
  if ((*targetm.calls.must_pass_in_stack) (mode, type))
    {
      cum->stack_pushed_args_size += PUSH_ROUNDING (int_size_in_bytes (type));
      return;
    }

  /* Mark the used registers as "used". */
  if (GET_REG_INDEX (cum) >= 0)
    {
      SET_USED_INDEX (cum, GET_REG_INDEX (cum));
      if (arg_rsize == 8)
	{
	  SET_USED_INDEX (cum, (GET_REG_INDEX (cum) + 1));
	}
    }
  else
    {
      /* Had to use stack */
      cum->stack_pushed_args_size += arg_rsize;
    }
}


/*
  Defines witch direction to go to find the next register to use if the
  argument is larger then one register or for arguments shorter than an
  int which is not promoted, such as the last part of structures with
  size not a multiple of 4. */
enum direction
avr32_function_arg_padding (enum machine_mode mode ATTRIBUTE_UNUSED,
			    tree type)
{
  /* Pad upward for all aggregates except byte and halfword sized aggregates
     which can be passed in registers. */
  if (type
      && AGGREGATE_TYPE_P (type)
      && (int_size_in_bytes (type) != 1)
      && !((int_size_in_bytes (type) == 2)
	   && TYPE_ALIGN_UNIT (type) >= 2)
      && (int_size_in_bytes (type) & 0x3))
    {
      return upward;
    }

  return downward;
}


/* Return a rtx used for the return value from a function call. */
rtx
avr32_function_value (tree type, tree func, bool outgoing ATTRIBUTE_UNUSED)
{
  if (avr32_return_in_memory (type, func))
    return NULL_RTX;

  if (int_size_in_bytes (type) <= 4)
    {
      enum machine_mode mode = TYPE_MODE (type);
      int unsignedp = 0;
      PROMOTE_FUNCTION_MODE (mode, unsignedp, type);
      return gen_rtx_REG (mode, RET_REGISTER);
    }
  else if (int_size_in_bytes (type) <= 8)
    return gen_rtx_REG (TYPE_MODE (type), INTERNAL_REGNUM (11));

  return NULL_RTX;
}


/* Return a rtx used for the return value from a library function call. */
rtx
avr32_libcall_value (enum machine_mode mode)
{

  if (GET_MODE_SIZE (mode) <= 4)
    return gen_rtx_REG (mode, RET_REGISTER);
  else if (GET_MODE_SIZE (mode) <= 8)
    return gen_rtx_REG (mode, INTERNAL_REGNUM (11));
  else
    return NULL_RTX;
}


/* Return TRUE if X references a SYMBOL_REF.  */
int
symbol_mentioned_p (rtx x)
{
  const char *fmt;
  int i;

  if (GET_CODE (x) == SYMBOL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (x));

  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (symbol_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && symbol_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}


/* Return TRUE if X references a LABEL_REF.  */
int
label_mentioned_p (rtx x)
{
  const char *fmt;
  int i;

  if (GET_CODE (x) == LABEL_REF)
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (label_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && label_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}


/* Return TRUE if X contains a MEM expression.  */
int
mem_mentioned_p (rtx x)
{
  const char *fmt;
  int i;

  if (MEM_P (x))
    return 1;

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
	{
	  int j;

	  for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	    if (mem_mentioned_p (XVECEXP (x, i, j)))
	      return 1;
	}
      else if (fmt[i] == 'e' && mem_mentioned_p (XEXP (x, i)))
	return 1;
    }

  return 0;
}


int
avr32_legitimate_pic_operand_p (rtx x)
{

  /* We can't have const, this must be broken down to a symbol. */
  if (GET_CODE (x) == CONST)
    return FALSE;

  /* Can't access symbols or labels via the constant pool either */
  if ((GET_CODE (x) == SYMBOL_REF
       && CONSTANT_POOL_ADDRESS_P (x)
       && (symbol_mentioned_p (get_pool_constant (x))
	   || label_mentioned_p (get_pool_constant (x)))))
    return FALSE;

  return TRUE;
}


rtx
legitimize_pic_address (rtx orig, enum machine_mode mode ATTRIBUTE_UNUSED,
			rtx reg)
{

  if (GET_CODE (orig) == SYMBOL_REF || GET_CODE (orig) == LABEL_REF)
    {
      int subregs = 0;

      if (reg == 0)
	{
	  if (!can_create_pseudo_p ())
	    abort ();
	  else
	    reg = gen_reg_rtx (Pmode);

	  subregs = 1;
	}

      emit_move_insn (reg, orig);

      /* Only set current function as using pic offset table if flag_pic is
         set. This is because this function is also used if
         TARGET_HAS_ASM_ADDR_PSEUDOS is set. */
      if (flag_pic)
	current_function_uses_pic_offset_table = 1;

      /* Put a REG_EQUAL note on this insn, so that it can be optimized by
         loop.  */
      return reg;
    }
  else if (GET_CODE (orig) == CONST)
    {
      rtx base, offset;

      if (flag_pic
	  && GET_CODE (XEXP (orig, 0)) == PLUS
	  && XEXP (XEXP (orig, 0), 0) == pic_offset_table_rtx)
	return orig;

      if (reg == 0)
	{
	  if (!can_create_pseudo_p ())
	    abort ();
	  else
	    reg = gen_reg_rtx (Pmode);
	}

      if (GET_CODE (XEXP (orig, 0)) == PLUS)
	{
	  base =
	    legitimize_pic_address (XEXP (XEXP (orig, 0), 0), Pmode, reg);
	  offset =
	    legitimize_pic_address (XEXP (XEXP (orig, 0), 1), Pmode,
				    base == reg ? 0 : reg);
	}
      else
	abort ();

      if (GET_CODE (offset) == CONST_INT)
	{
	  /* The base register doesn't really matter, we only want to test
	     the index for the appropriate mode.  */
	  if (!avr32_const_ok_for_constraint_p (INTVAL (offset), 'I', "Is21"))
	    {
	      if (can_create_pseudo_p ())
		offset = force_reg (Pmode, offset);
	      else
		abort ();
	    }

	  if (GET_CODE (offset) == CONST_INT)
	    return plus_constant (base, INTVAL (offset));
	}

      return gen_rtx_PLUS (Pmode, base, offset);
    }

  return orig;
}


/* Generate code to load the PIC register.  */
void
avr32_load_pic_register (void)
{
  rtx l1, pic_tmp;
  rtx global_offset_table;

  if ((current_function_uses_pic_offset_table == 0) || TARGET_NO_INIT_GOT)
    return;

  if (!flag_pic)
    abort ();

  l1 = gen_label_rtx ();

  global_offset_table = gen_rtx_SYMBOL_REF (Pmode, "_GLOBAL_OFFSET_TABLE_");
  pic_tmp =
    gen_rtx_CONST (Pmode,
		   gen_rtx_MINUS (SImode, gen_rtx_LABEL_REF (Pmode, l1),
				  global_offset_table));
  emit_insn (gen_pic_load_addr
	     (pic_offset_table_rtx, force_const_mem (SImode, pic_tmp)));
  emit_insn (gen_pic_compute_got_from_pc (pic_offset_table_rtx, l1));

  /* Need to emit this whether or not we obey regdecls, since setjmp/longjmp
     can cause life info to screw up.  */
  emit_insn (gen_rtx_USE (VOIDmode, pic_offset_table_rtx));
}


/* This hook should return true if values of type type are returned at the most
   significant end of a register (in other words, if they are padded at the
   least significant end). You can assume that type is returned in a register;
   the caller is required to check this.  Note that the register provided by
   FUNCTION_VALUE must be able to hold the complete return value. For example,
   if a 1-, 2- or 3-byte structure is returned at the most significant end of a
   4-byte register, FUNCTION_VALUE should provide an SImode rtx. */
bool
avr32_return_in_msb (tree type ATTRIBUTE_UNUSED)
{
  /* if ( AGGREGATE_TYPE_P (type) ) if ((int_size_in_bytes(type) == 1) ||
     ((int_size_in_bytes(type) == 2) && TYPE_ALIGN_UNIT(type) >= 2)) return
     false; else return true; */

  return false;
}


/*
  Returns one if a certain function value is going to be returned in memory
  and zero if it is going to be returned in a register.

  BLKmode and all other modes that is larger than 64 bits are returned in
  memory.
*/
bool
avr32_return_in_memory (tree type, tree fntype ATTRIBUTE_UNUSED)
{
  if (TYPE_MODE (type) == VOIDmode)
    return false;

  if (int_size_in_bytes (type) > (2 * UNITS_PER_WORD)
      || int_size_in_bytes (type) == -1)
    {
      return true;
    }

  /* If we have an aggregate then use the same mechanism as when checking if
     it should be passed on the stack. */
  if (type
      && AGGREGATE_TYPE_P (type)
      && (*targetm.calls.must_pass_in_stack) (TYPE_MODE (type), type))
    return true;

  return false;
}


/* Output the constant part of the trampoline.
   lddpc    r0, pc[0x8:e] ; load static chain register
   lddpc    pc, pc[0x8:e] ; jump to subrutine
   .long    0		 ; Address to static chain,
			 ; filled in by avr32_initialize_trampoline()
   .long    0		 ; Address to subrutine,
			 ; filled in by avr32_initialize_trampoline()
*/
void
avr32_trampoline_template (FILE * file)
{
  fprintf (file, "\tlddpc    r0, pc[8]\n");
  fprintf (file, "\tlddpc    pc, pc[8]\n");
  /* make room for the address of the static chain. */
  fprintf (file, "\t.long\t0\n");
  /* make room for the address to the subrutine. */
  fprintf (file, "\t.long\t0\n");
}


/* Initialize the variable parts of a trampoline. */
void
avr32_initialize_trampoline (rtx addr, rtx fnaddr, rtx static_chain)
{
  /* Store the address to the static chain. */
  emit_move_insn (gen_rtx_MEM
		  (SImode, plus_constant (addr, TRAMPOLINE_SIZE - 4)),
		  static_chain);

  /* Store the address to the function. */
  emit_move_insn (gen_rtx_MEM (SImode, plus_constant (addr, TRAMPOLINE_SIZE)),
		  fnaddr);

  emit_insn (gen_cache (gen_rtx_REG (SImode, 13),
			gen_rtx_CONST_INT (SImode,
					   AVR32_CACHE_INVALIDATE_ICACHE)));
}


/* Return nonzero if X is valid as an addressing register.  */
int
avr32_address_register_rtx_p (rtx x, int strict_p)
{
  int regno;

  if (!register_operand(x, GET_MODE(x)))
    return 0;

  /* If strict we require the register to be a hard register. */
  if (strict_p
      && !REG_P(x))
    return 0;

  regno = REGNO (x);

  if (strict_p)
    return REGNO_OK_FOR_BASE_P (regno);

  return (regno <= LAST_REGNUM || regno >= FIRST_PSEUDO_REGISTER);
}


/* Return nonzero if INDEX is valid for an address index operand.  */
int
avr32_legitimate_index_p (enum machine_mode mode, rtx index, int strict_p)
{
  enum rtx_code code = GET_CODE (index);

  if (GET_MODE_SIZE (mode) > 8)
    return 0;

  /* Standard coprocessor addressing modes.  */
  if (code == CONST_INT)
    {
	return CONST_OK_FOR_CONSTRAINT_P (INTVAL (index), 'K', "Ks16");
    }

  if (avr32_address_register_rtx_p (index, strict_p))
    return 1;

  if (code == MULT)
    {
      rtx xiop0 = XEXP (index, 0);
      rtx xiop1 = XEXP (index, 1);
      return ((avr32_address_register_rtx_p (xiop0, strict_p)
	       && power_of_two_operand (xiop1, SImode)
	       && (INTVAL (xiop1) <= 8))
	      || (avr32_address_register_rtx_p (xiop1, strict_p)
		  && power_of_two_operand (xiop0, SImode)
		  && (INTVAL (xiop0) <= 8)));
    }
  else if (code == ASHIFT)
    {
      rtx op = XEXP (index, 1);

      return (avr32_address_register_rtx_p (XEXP (index, 0), strict_p)
	      && GET_CODE (op) == CONST_INT
	      && INTVAL (op) > 0 && INTVAL (op) <= 3);
    }

  return 0;
}


/*
  Used in the GO_IF_LEGITIMATE_ADDRESS macro. Returns a nonzero value if
  the RTX x is a legitimate memory address.

  Returns NO_REGS if the address is not legatime, GENERAL_REGS or ALL_REGS
  if it is.
*/


/* Forward declaration */
int is_minipool_label (rtx label);

int
avr32_legitimate_address (enum machine_mode mode, rtx x, int strict)
{

  switch (GET_CODE (x))
    {
    case REG:
      return avr32_address_register_rtx_p (x, strict);
    case CONST_INT:
      return ((mode==SImode) && TARGET_RMW_ADDRESSABLE_DATA
              && CONST_OK_FOR_CONSTRAINT_P(INTVAL(x), 'K', "Ks17"));
    case CONST:
      {
	rtx label = avr32_find_symbol (x);
	if (label
	    &&
	    (/*
               If we enable (const (plus (symbol_ref ...))) type constant
               pool entries we must add support for it in the predicates and
               in the minipool generation in avr32_reorg().
               (CONSTANT_POOL_ADDRESS_P (label)
               && !(flag_pic
               && (symbol_mentioned_p (get_pool_constant (label))
               || label_mentioned_p (get_pool_constant (label)))))
               ||*/
             ((GET_CODE (label) == LABEL_REF)
              && GET_CODE (XEXP (label, 0)) == CODE_LABEL
    		  && is_minipool_label (XEXP (label, 0)))
              /*|| ((GET_CODE (label) == SYMBOL_REF) 
                  && mode == SImode
                  && SYMBOL_REF_RMW_ADDR(label))*/))
	  {
	    return TRUE;
	  }
      }
      break;
    case LABEL_REF:
      if (GET_CODE (XEXP (x, 0)) == CODE_LABEL
	  && is_minipool_label (XEXP (x, 0)))
	{
	  return TRUE;
	}
      break;
    case SYMBOL_REF:
      {
	if (CONSTANT_POOL_ADDRESS_P (x)
	    && !(flag_pic
		 && (symbol_mentioned_p (get_pool_constant (x))
		     || label_mentioned_p (get_pool_constant (x)))))
	  return TRUE;
	else if (SYMBOL_REF_RCALL_FUNCTION_P (x)
                 || (mode == SImode
                     && SYMBOL_REF_RMW_ADDR (x)))
	  return TRUE;
	break;
      }
    case PRE_DEC:		/* (pre_dec (...)) */
    case POST_INC:		/* (post_inc (...)) */
      return avr32_address_register_rtx_p (XEXP (x, 0), strict);
    case PLUS:			/* (plus (...) (...)) */
      {
	rtx xop0 = XEXP (x, 0);
	rtx xop1 = XEXP (x, 1);

	return ((avr32_address_register_rtx_p (xop0, strict)
		 && avr32_legitimate_index_p (mode, xop1, strict))
		|| (avr32_address_register_rtx_p (xop1, strict)
		    && avr32_legitimate_index_p (mode, xop0, strict)));
      }
    default:
      break;
    }

  return FALSE;
}


int
avr32_const_ok_for_move (HOST_WIDE_INT c)
{
  if ( TARGET_V2_INSNS )
    return ( avr32_const_ok_for_constraint_p (c, 'K', "Ks21")
             /* movh instruction */
             || avr32_hi16_immediate_operand (GEN_INT(c), VOIDmode) );
  else
    return avr32_const_ok_for_constraint_p (c, 'K', "Ks21");
}


int
avr32_const_double_immediate (rtx value)
{
  HOST_WIDE_INT hi, lo;

  if (GET_CODE (value) != CONST_DOUBLE)
    return FALSE;

  if (SCALAR_FLOAT_MODE_P (GET_MODE (value)))
    {
      HOST_WIDE_INT target_float[2];
      hi = lo = 0;
      real_to_target (target_float, CONST_DOUBLE_REAL_VALUE (value),
		      GET_MODE (value));
      lo = target_float[0];
      hi = target_float[1];
    }
  else
    {
      hi = CONST_DOUBLE_HIGH (value);
      lo = CONST_DOUBLE_LOW (value);
    }

  if (avr32_const_ok_for_constraint_p (lo, 'K', "Ks21")
      && (GET_MODE (value) == SFmode
	  || avr32_const_ok_for_constraint_p (hi, 'K', "Ks21")))
    {
      return TRUE;
    }

  return FALSE;
}


int
avr32_legitimate_constant_p (rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
      /* Check if we should put large immediate into constant pool
       or load them directly with mov/orh.*/
      if (!avr32_imm_in_const_pool)
	return 1;

      return avr32_const_ok_for_move (INTVAL (x));
    case CONST_DOUBLE:
      /* Check if we should put large immediate into constant pool
         or load them directly with mov/orh.*/
      if (!avr32_imm_in_const_pool)
	return 1;

      if (GET_MODE (x) == SFmode
	  || GET_MODE (x) == DFmode || GET_MODE (x) == DImode)
	return avr32_const_double_immediate (x);
      else
	return 0;
    case LABEL_REF:
    case SYMBOL_REF:
      return avr32_find_symbol (x) && (flag_pic || TARGET_HAS_ASM_ADDR_PSEUDOS);
    case CONST:
    case HIGH:
    case CONST_VECTOR:
      return 0;
    default:
      printf ("%s():\n", __FUNCTION__);
      debug_rtx (x);
      return 1;
    }
}


/* Strip any special encoding from labels */
const char *
avr32_strip_name_encoding (const char *name)
{
  const char *stripped = name;

  while (1)
    {
      switch (stripped[0])
	{
	case '#':
	  stripped = strchr (name + 1, '#') + 1;
	  break;
	case '*':
	  stripped = &stripped[1];
	  break;
	default:
	  return stripped;
	}
    }
}



/* Do anything needed before RTL is emitted for each function.  */
static struct machine_function *
avr32_init_machine_status (void)
{
  struct machine_function *machine;
  machine =
    (machine_function *) ggc_alloc_cleared (sizeof (machine_function));

#if AVR32_FT_UNKNOWN != 0
  machine->func_type = AVR32_FT_UNKNOWN;
#endif

  machine->minipool_label_head = 0;
  machine->minipool_label_tail = 0;
  machine->ifcvt_after_reload = 0;
  return machine;
}


void
avr32_init_expanders (void)
{
  /* Arrange to initialize and mark the machine per-function status.  */
  init_machine_status = avr32_init_machine_status;
}


/* Return an RTX indicating where the return address to the
   calling function can be found.  */
rtx
avr32_return_addr (int count, rtx frame ATTRIBUTE_UNUSED)
{
  if (count != 0)
    return NULL_RTX;

  return get_hard_reg_initial_val (Pmode, LR_REGNUM);
}


void
avr32_encode_section_info (tree decl, rtx rtl, int first)
{
   default_encode_section_info(decl, rtl, first);

   if ( TREE_CODE (decl) == VAR_DECL
        && (GET_CODE (XEXP (rtl, 0)) == SYMBOL_REF)
        && (lookup_attribute ("rmw_addressable", DECL_ATTRIBUTES (decl))
            || TARGET_RMW_ADDRESSABLE_DATA) ){
     if ( !TARGET_RMW || flag_pic )
       return;
     //  {
     //    warning ("Using RMW addressable data with an arch that does not support RMW instructions.");
     //    return;
     //  } 
     //
     //if ( flag_pic )
     //  {
     //    warning ("Using RMW addressable data with together with -fpic switch. Can not use RMW instruction when compiling with -fpic.");
     //    return;
     //  } 
     SYMBOL_REF_FLAGS (XEXP (rtl, 0)) |= (1 << SYMBOL_FLAG_RMW_ADDR_SHIFT);
  }
}


void
avr32_asm_output_label (FILE * stream, const char *name)
{
  name = avr32_strip_name_encoding (name);

  /* Print the label. */
  assemble_name (stream, name);
  fprintf (stream, ":\n");
}


void
avr32_asm_weaken_label (FILE * stream, const char *name)
{
  fprintf (stream, "\t.weak ");
  assemble_name (stream, name);
  fprintf (stream, "\n");
}


/*
  Checks if a labelref is equal to a reserved word in the assembler. If it is,
  insert a '_' before the label name.
*/
void
avr32_asm_output_labelref (FILE * stream, const char *name)
{
  int verbatim = FALSE;
  const char *stripped = name;
  int strip_finished = FALSE;

  while (!strip_finished)
    {
      switch (stripped[0])
	{
	case '#':
	  stripped = strchr (name + 1, '#') + 1;
	  break;
	case '*':
	  stripped = &stripped[1];
	  verbatim = TRUE;
	  break;
	default:
	  strip_finished = TRUE;
	  break;
	}
    }

  if (verbatim)
    fputs (stripped, stream);
  else
    asm_fprintf (stream, "%U%s", stripped);
}


/*
   Check if the comparison in compare_exp is redundant
   for the condition given in next_cond given that the
   needed flags are already set by an earlier instruction.
   Uses cc_prev_status to check this.

   Returns NULL_RTX if the compare is not redundant
   or the new condition to use in the conditional
   instruction if the compare is redundant.
*/
static rtx
is_compare_redundant (rtx compare_exp, rtx next_cond)
{
  int z_flag_valid = FALSE;
  int n_flag_valid = FALSE;
  rtx new_cond;

  if (GET_CODE (compare_exp) != COMPARE
      && GET_CODE (compare_exp) != AND)
    return NULL_RTX;


  if (rtx_equal_p (cc_prev_status.mdep.value, compare_exp))
    {
      /* cc0 already contains the correct comparison -> delete cmp insn */
      return next_cond;
    }

  if (GET_MODE (compare_exp) != SImode)
    return NULL_RTX;

  switch (cc_prev_status.mdep.flags)
    {
    case CC_SET_VNCZ:
    case CC_SET_NCZ:
      n_flag_valid = TRUE;
    case CC_SET_CZ:
    case CC_SET_Z:
      z_flag_valid = TRUE;
    }

  if (cc_prev_status.mdep.value
      && GET_CODE (compare_exp) == COMPARE
      && REG_P (XEXP (compare_exp, 0))
      && REGNO (XEXP (compare_exp, 0)) == REGNO (cc_prev_status.mdep.value)
      && GET_CODE (XEXP (compare_exp, 1)) == CONST_INT
      && next_cond != NULL_RTX)
    {
      if (INTVAL (XEXP (compare_exp, 1)) == 0
	  && z_flag_valid
	  && (GET_CODE (next_cond) == EQ || GET_CODE (next_cond) == NE))
	/* We can skip comparison Z flag is already reflecting ops[0] */
	return next_cond;
      else if (n_flag_valid
	       && ((INTVAL (XEXP (compare_exp, 1)) == 0
		    && (GET_CODE (next_cond) == GE
			|| GET_CODE (next_cond) == LT))
		   || (INTVAL (XEXP (compare_exp, 1)) == -1
		       && (GET_CODE (next_cond) == GT
			   || GET_CODE (next_cond) == LE))))
	{
	  /* We can skip comparison N flag is already reflecting ops[0],
	     which means that we can use the mi/pl conditions to check if
	     ops[0] is GE or LT 0. */
	  if ((GET_CODE (next_cond) == GE) || (GET_CODE (next_cond) == GT))
	    new_cond =
	      gen_rtx_UNSPEC (GET_MODE (next_cond), gen_rtvec (2, cc0_rtx, const0_rtx),
			      UNSPEC_COND_PL);
	  else
	    new_cond =
	      gen_rtx_UNSPEC (GET_MODE (next_cond), gen_rtvec (2, cc0_rtx, const0_rtx),
			      UNSPEC_COND_MI);
	  return new_cond;
	}
    }
  return NULL_RTX;
}


/* Updates cc_status.  */
void
avr32_notice_update_cc (rtx exp, rtx insn)
{
  enum attr_cc attr_cc = get_attr_cc (insn);

  if ( attr_cc == CC_SET_Z_IF_NOT_V2 )
    {
      if (TARGET_V2_INSNS)
        attr_cc = CC_NONE;
      else
        attr_cc = CC_SET_Z;
    }

  switch (attr_cc)
    {
    case CC_CALL_SET:
      CC_STATUS_INIT;
      /* Check if the function call returns a value in r12 */
      if (REG_P (recog_data.operand[0])
	  && REGNO (recog_data.operand[0]) == RETVAL_REGNUM)
	{
	  cc_status.flags = 0;
	  cc_status.mdep.value =
	    gen_rtx_COMPARE (SImode, recog_data.operand[0], const0_rtx);
	  cc_status.mdep.flags = CC_SET_VNCZ;
          cc_status.mdep.cond_exec_cmp_clobbered = 0;

	}
      break;
    case CC_COMPARE:
      {
        /* Check that compare will not be optimized away if so nothing should
           be done */
        rtx compare_exp = SET_SRC (exp);
        /* Check if we have a tst expression. If so convert it to a
           compare with 0. */
        if ( REG_P (SET_SRC (exp)) )
          compare_exp = gen_rtx_COMPARE (GET_MODE (SET_SRC (exp)),
                                         SET_SRC (exp),
                                         const0_rtx);

        if (!next_insn_emits_cmp (insn)
            && (is_compare_redundant (compare_exp, get_next_insn_cond (insn)) == NULL_RTX))
          {

            /* Reset the nonstandard flag */
            CC_STATUS_INIT;
            cc_status.flags = 0;
            cc_status.mdep.value = compare_exp;
            cc_status.mdep.flags = CC_SET_VNCZ;
            cc_status.mdep.cond_exec_cmp_clobbered = 0;
         }
      }
      break;
    case CC_CMP_COND_INSN:
      {
	/* Conditional insn that emit the compare itself. */
        rtx cmp;
        rtx cmp_op0, cmp_op1;
        rtx cond;
        rtx dest;
        rtx next_insn = next_nonnote_insn (insn);

        if ( GET_CODE (exp) == COND_EXEC )
          {
            cmp_op0 = XEXP (COND_EXEC_TEST (exp), 0);
            cmp_op1 = XEXP (COND_EXEC_TEST (exp), 1);
            cond = COND_EXEC_TEST (exp);
            dest = SET_DEST (COND_EXEC_CODE (exp));
          }
        else
          {
            /* If then else conditional. compare operands are in operands
               4 and 5. */
            cmp_op0 = recog_data.operand[4];
            cmp_op1 = recog_data.operand[5];
            cond = recog_data.operand[1];
            dest = SET_DEST (exp);
          }

        if ( GET_CODE (cmp_op0) == AND )
          cmp = cmp_op0;
        else
          cmp = gen_rtx_COMPARE (GET_MODE (cmp_op0),
                                 cmp_op0,
                                 cmp_op1);

        /* Check if the conditional insns updates a register present
           in the comparison, if so then we must reset the cc_status. */
        if (REG_P (dest)
            && (reg_mentioned_p (dest, cmp_op0)
                || reg_mentioned_p (dest, cmp_op1))
            && GET_CODE (exp) != COND_EXEC )
          {
            CC_STATUS_INIT;
          }
	else if (is_compare_redundant (cmp, cond) == NULL_RTX)
	  {
	    /* Reset the nonstandard flag */
	    CC_STATUS_INIT;
            if ( GET_CODE (cmp_op0) == AND )
              {
                cc_status.flags = CC_INVERTED;
                cc_status.mdep.flags = CC_SET_Z;
              }
            else
              {
                cc_status.flags = 0;
                cc_status.mdep.flags = CC_SET_VNCZ;
              }
	    cc_status.mdep.value = cmp;
            cc_status.mdep.cond_exec_cmp_clobbered = 0;
	  }


        /* Check if we have a COND_EXEC insn which updates one
           of the registers in the compare status. */
        if (REG_P (dest)
            && (reg_mentioned_p (dest, cmp_op0)
                || reg_mentioned_p (dest, cmp_op1))
            && GET_CODE (exp) == COND_EXEC )
          cc_status.mdep.cond_exec_cmp_clobbered = 1;

        if ( cc_status.mdep.cond_exec_cmp_clobbered
             && GET_CODE (exp) == COND_EXEC
             && next_insn != NULL
             && INSN_P (next_insn)
             && !(GET_CODE (PATTERN (next_insn)) == COND_EXEC
                  && rtx_equal_p (XEXP (COND_EXEC_TEST (PATTERN (next_insn)), 0), cmp_op0)
                  && rtx_equal_p (XEXP (COND_EXEC_TEST (PATTERN (next_insn)), 1), cmp_op1)
                  && (GET_CODE (COND_EXEC_TEST (PATTERN (next_insn))) == GET_CODE (cond)
                      || GET_CODE (COND_EXEC_TEST (PATTERN (next_insn))) == reverse_condition (GET_CODE (cond)))) )
          {
            /* We have a sequence of conditional insns where the compare status has been clobbered
               since the compare no longer reflects the content of the values to compare. */
            CC_STATUS_INIT;
            cc_status.mdep.cond_exec_cmp_clobbered = 1;
          }

      }
      break;
    case CC_BLD:
      /* Bit load is kind of like an inverted testsi, because the Z flag is
         inverted */
      CC_STATUS_INIT;
      cc_status.flags = CC_INVERTED;
      cc_status.mdep.value = SET_SRC (exp);
      cc_status.mdep.flags = CC_SET_Z;
      cc_status.mdep.cond_exec_cmp_clobbered = 0;
      break;
    case CC_NONE:
      /* Insn does not affect CC at all. Check if the instruction updates
         some of the register currently reflected in cc0 */

      if (GET_CODE (exp) == SET)
        {
          if (GET_MODE (SET_DEST (exp)) == DImode)
            {
              rtx second = gen_rtx_REG (SImode, REGNO (SET_DEST (exp)) + 1);
              if ((cc_status.value1 || cc_status.value2 || cc_status.mdep.value)
	        && (reg_mentioned_p (SET_DEST (exp), cc_status.value1)
	           || reg_mentioned_p (SET_DEST (exp), cc_status.value2)
	           || reg_mentioned_p (SET_DEST (exp), cc_status.mdep.value)
	           || reg_mentioned_p (second, cc_status.value1)
	           || reg_mentioned_p (second, cc_status.value2)
	           || reg_mentioned_p (second, cc_status.mdep.value)))
	        {
	          CC_STATUS_INIT;
	        }
            }
          else 
            {
	      if ((cc_status.value1 || cc_status.value2 || cc_status.mdep.value)
	        && (reg_mentioned_p (SET_DEST (exp), cc_status.value1)
	          || reg_mentioned_p (SET_DEST (exp), cc_status.value2)
	          || reg_mentioned_p (SET_DEST (exp), cc_status.mdep.value)))
	        {
	          CC_STATUS_INIT;
	        }
            }
        }
      /* If this is a parallel we must step through each of the parallel
         expressions */
      if (GET_CODE (exp) == PARALLEL)
	{
	  int i;
	  for (i = 0; i < XVECLEN (exp, 0); ++i)
	    {
	      rtx vec_exp = XVECEXP (exp, 0, i);
	      if ((GET_CODE (vec_exp) == SET)
		  && (cc_status.value1 || cc_status.value2
		      || cc_status.mdep.value)
		  && (reg_mentioned_p (SET_DEST (vec_exp), cc_status.value1)
		      || reg_mentioned_p (SET_DEST (vec_exp),
					  cc_status.value2)
		      || reg_mentioned_p (SET_DEST (vec_exp),
					  cc_status.mdep.value)))
		{
		  CC_STATUS_INIT;
		}
	    }
	}

      /* Check if we have memory opartions with post_inc or pre_dec on the
         register currently reflected in cc0 */
      if (GET_CODE (exp) == SET
	  && GET_CODE (SET_SRC (exp)) == MEM
	  && (GET_CODE (XEXP (SET_SRC (exp), 0)) == POST_INC
	      || GET_CODE (XEXP (SET_SRC (exp), 0)) == PRE_DEC)
	  &&
	  (reg_mentioned_p
	   (XEXP (XEXP (SET_SRC (exp), 0), 0), cc_status.value1)
	   || reg_mentioned_p (XEXP (XEXP (SET_SRC (exp), 0), 0),
			       cc_status.value2)
	   || reg_mentioned_p (XEXP (XEXP (SET_SRC (exp), 0), 0),
			       cc_status.mdep.value)))
	CC_STATUS_INIT;

      if (GET_CODE (exp) == SET
	  && GET_CODE (SET_DEST (exp)) == MEM
	  && (GET_CODE (XEXP (SET_DEST (exp), 0)) == POST_INC
	      || GET_CODE (XEXP (SET_DEST (exp), 0)) == PRE_DEC)
	  &&
	  (reg_mentioned_p
	   (XEXP (XEXP (SET_DEST (exp), 0), 0), cc_status.value1)
	   || reg_mentioned_p (XEXP (XEXP (SET_DEST (exp), 0), 0),
			       cc_status.value2)
	   || reg_mentioned_p (XEXP (XEXP (SET_DEST (exp), 0), 0),
			       cc_status.mdep.value)))
	CC_STATUS_INIT;
      break;

    case CC_SET_VNCZ:
      CC_STATUS_INIT;
      cc_status.mdep.value = recog_data.operand[0];
      cc_status.mdep.flags = CC_SET_VNCZ;
      cc_status.mdep.cond_exec_cmp_clobbered = 0;
      break;

    case CC_SET_NCZ:
      CC_STATUS_INIT;
      cc_status.mdep.value = recog_data.operand[0];
      cc_status.mdep.flags = CC_SET_NCZ;
      cc_status.mdep.cond_exec_cmp_clobbered = 0;
      break;

    case CC_SET_CZ:
      CC_STATUS_INIT;
      cc_status.mdep.value = recog_data.operand[0];
      cc_status.mdep.flags = CC_SET_CZ;
      cc_status.mdep.cond_exec_cmp_clobbered = 0;
      break;

    case CC_SET_Z:
      CC_STATUS_INIT;
      cc_status.mdep.value = recog_data.operand[0];
      cc_status.mdep.flags = CC_SET_Z;
      cc_status.mdep.cond_exec_cmp_clobbered = 0;
      break;

    case CC_CLOBBER:
      CC_STATUS_INIT;
      break;

    default:
      CC_STATUS_INIT;
    }
}


/*
  Outputs to stdio stream stream the assembler syntax for an instruction
  operand x. x is an RTL expression.
*/
void
avr32_print_operand (FILE * stream, rtx x, int code)
{
  int error = 0;

  if ( code == '?' )
    {
      /* Predicable instruction, print condition code */

      /* If the insn should not be conditional then do nothing. */
      if ( current_insn_predicate == NULL_RTX )
        return;

      /* Set x to the predicate to force printing
         the condition later on. */
      x = current_insn_predicate;

      /* Reverse condition if useing bld insn. */
      if ( GET_CODE (XEXP(current_insn_predicate,0)) == AND )
        x = reversed_condition (current_insn_predicate);
    }
  else if ( code == '!' )
    {
      /* Output compare for conditional insn if needed. */
      rtx new_cond;
      gcc_assert ( current_insn_predicate != NULL_RTX );
      new_cond = avr32_output_cmp(current_insn_predicate,
                                  GET_MODE(XEXP(current_insn_predicate,0)),
                                  XEXP(current_insn_predicate,0),
                                  XEXP(current_insn_predicate,1));

      /* Check if the new condition is a special avr32 condition
         specified using UNSPECs. If so we must handle it differently. */
      if ( GET_CODE (new_cond) == UNSPEC )
        {
          current_insn_predicate =
            gen_rtx_UNSPEC (CCmode,
                            gen_rtvec (2,
                                       XEXP(current_insn_predicate,0),
                                       XEXP(current_insn_predicate,1)),
                            XINT (new_cond, 1));
        }
      else
        {
          PUT_CODE(current_insn_predicate, GET_CODE(new_cond));
        }
      return;
    }

  switch (GET_CODE (x))
    {
    case UNSPEC:
      switch (XINT (x, 1))
	{
	case UNSPEC_COND_PL:
	  if (code == 'i')
	    fputs ("mi", stream);
	  else
	    fputs ("pl", stream);
	  break;
	case UNSPEC_COND_MI:
	  if (code == 'i')
	    fputs ("pl", stream);
	  else
	    fputs ("mi", stream);
	  break;
	default:
	  error = 1;
	}
      break;
    case EQ:
      if (code == 'i')
	fputs ("ne", stream);
      else
	fputs ("eq", stream);
      break;
    case NE:
      if (code == 'i')
	fputs ("eq", stream);
      else
	fputs ("ne", stream);
      break;
    case GT:
      if (code == 'i')
	fputs ("le", stream);
      else
	fputs ("gt", stream);
      break;
    case GTU:
      if (code == 'i')
	fputs ("ls", stream);
      else
	fputs ("hi", stream);
      break;
    case LT:
      if (code == 'i')
	fputs ("ge", stream);
      else
	fputs ("lt", stream);
      break;
    case LTU:
      if (code == 'i')
	fputs ("hs", stream);
      else
	fputs ("lo", stream);
      break;
    case GE:
      if (code == 'i')
	fputs ("lt", stream);
      else
	fputs ("ge", stream);
      break;
    case GEU:
      if (code == 'i')
	fputs ("lo", stream);
      else
	fputs ("hs", stream);
      break;
    case LE:
      if (code == 'i')
	fputs ("gt", stream);
      else
	fputs ("le", stream);
      break;
    case LEU:
      if (code == 'i')
	fputs ("hi", stream);
      else
	fputs ("ls", stream);
      break;
    case CONST_INT:
      {
        HOST_WIDE_INT value = INTVAL (x);

        switch (code)
          {
          case 'm':
            if ( HOST_BITS_PER_WIDE_INT > BITS_PER_WORD )
              {
                /* A const_int can be used to represent DImode constants. */
                value >>= BITS_PER_WORD;
              }
            /* We might get a const_int immediate for setting a DI register,
               we then must then return the correct sign extended DI. The most
               significant word is just a sign extension. */
            else if (value < 0)
              value = -1;
            else
              value = 0;
            break;
          case 'i':
            value++;
            break;
          case 'p':
            {
              /* Set to bit position of first bit set in immediate */
              int i, bitpos = 32;
              for (i = 0; i < 32; i++)
                if (value & (1 << i))
                  {
                    bitpos = i;
                    break;
                  }
              value = bitpos;
            }
            break;
          case 'z':
            {
              /* Set to bit position of first bit cleared in immediate */
              int i, bitpos = 32;
              for (i = 0; i < 32; i++)
                if (!(value & (1 << i)))
                  {
                    bitpos = i;
                    break;
                  }
              value = bitpos;
            }
            break;
          case 'r':
            {
              /* Reglist 8 */
              char op[50];
              op[0] = '\0';

              if (value & 0x01)
                strcpy (op, "r0-r3");
              if (value & 0x02)
                strlen (op) ? strcat (op, ", r4-r7") : strcpy (op,"r4-r7");
              if (value & 0x04)
                strlen (op) ? strcat (op, ", r8-r9") : strcpy (op,"r8-r9");
              if (value & 0x08)
                strlen (op) ? strcat (op, ", r10") : strcpy (op,"r10");
              if (value & 0x10)
                strlen (op) ? strcat (op, ", r11") : strcpy (op,"r11");
              if (value & 0x20)
                strlen (op) ? strcat (op, ", r12") : strcpy (op,"r12");
              if (value & 0x40)
                strlen (op) ? strcat (op, ", lr") : strcpy (op, "lr");
              if (value & 0x80)
                strlen (op) ? strcat (op, ", pc") : strcpy (op, "pc");

              fputs (op, stream);
              return;
            }
          case 's':
            {
              /* Reglist 16 */
              char reglist16_string[100];
              int i;
	      bool first_reg = true;
              reglist16_string[0] = '\0';

	      for (i = 0; i < 16; ++i)
		{
		  if (value & (1 << i))
		    {
			first_reg == true ?  first_reg = false : strcat(reglist16_string,", ");
			strcat(reglist16_string,reg_names[INTERNAL_REGNUM(i)]);		      
		    }
		}
	      fputs (reglist16_string, stream);
	      return;
	    }
	  case 'h':
	    /* Print halfword part of word */
	    fputs (value ? "b" : "t", stream);
	    return;
	  }

	/* Print Value */
	fprintf (stream, "%d", value);
	break;
      }
    case CONST_DOUBLE:
      {
	HOST_WIDE_INT hi, lo;
	if (SCALAR_FLOAT_MODE_P (GET_MODE (x)))
	  {
	    HOST_WIDE_INT target_float[2];
	    hi = lo = 0;
	    real_to_target (target_float, CONST_DOUBLE_REAL_VALUE (x),
			    GET_MODE (x));
	    /* For doubles the most significant part starts at index 0. */
	    if (GET_MODE_SIZE (GET_MODE (x)) > UNITS_PER_WORD)
	      {
		hi = target_float[0];
		lo = target_float[1];
	      }
	    else
	      {
		lo = target_float[0];
	      }
	  }
	else
	  {
	    hi = CONST_DOUBLE_HIGH (x);
	    lo = CONST_DOUBLE_LOW (x);
	  }

	if (code == 'm')
	  fprintf (stream, "%ld", hi);
	else
	  fprintf (stream, "%ld", lo);

	break;
      }
    case CONST:
      output_addr_const (stream, XEXP (XEXP (x, 0), 0));
      fprintf (stream, "+%ld", INTVAL (XEXP (XEXP (x, 0), 1)));
      break;
    case REG:
      /* Swap register name if the register is DImode or DFmode. */
      if (GET_MODE (x) == DImode || GET_MODE (x) == DFmode)
	{
	  /* Double register must have an even numbered address */
	  gcc_assert (!(REGNO (x) % 2));
	  if (code == 'm')
	    fputs (reg_names[true_regnum (x)], stream);
	  else
	    fputs (reg_names[true_regnum (x) + 1], stream);
	}
      else if (GET_MODE (x) == TImode)
	{
	  switch (code)
	    {
	    case 'T':
	      fputs (reg_names[true_regnum (x)], stream);
	      break;
	    case 'U':
	      fputs (reg_names[true_regnum (x) + 1], stream);
	      break;
	    case 'L':
	      fputs (reg_names[true_regnum (x) + 2], stream);
	      break;
	    case 'B':
	      fputs (reg_names[true_regnum (x) + 3], stream);
	      break;
	    default:
	      fprintf (stream, "%s, %s, %s, %s",
		       reg_names[true_regnum (x) + 3],
		       reg_names[true_regnum (x) + 2],
		       reg_names[true_regnum (x) + 1],
		       reg_names[true_regnum (x)]);
	      break;
	    }
	}
      else
	{
	  fputs (reg_names[true_regnum (x)], stream);
	}
      break;
    case CODE_LABEL:
    case LABEL_REF:
    case SYMBOL_REF:
      output_addr_const (stream, x);
      break;
    case MEM:
      switch (GET_CODE (XEXP (x, 0)))
	{
	case LABEL_REF:
	case SYMBOL_REF:
	  output_addr_const (stream, XEXP (x, 0));
	  break;
	case MEM:
	  switch (GET_CODE (XEXP (XEXP (x, 0), 0)))
	    {
	    case SYMBOL_REF:
	      output_addr_const (stream, XEXP (XEXP (x, 0), 0));
	      break;
	    default:
	      error = 1;
	      break;
	    }
	  break;
	case REG:
	  avr32_print_operand (stream, XEXP (x, 0), 0);
	  if (code != 'p')
	    fputs ("[0]", stream);
	  break;
	case PRE_DEC:
	  fputs ("--", stream);
	  avr32_print_operand (stream, XEXP (XEXP (x, 0), 0), 0);
	  break;
	case POST_INC:
	  avr32_print_operand (stream, XEXP (XEXP (x, 0), 0), 0);
	  fputs ("++", stream);
	  break;
	case PLUS:
	  {
	    rtx op0 = XEXP (XEXP (x, 0), 0);
	    rtx op1 = XEXP (XEXP (x, 0), 1);
	    rtx base = NULL_RTX, offset = NULL_RTX;

	    if (avr32_address_register_rtx_p (op0, 1))
	      {
		base = op0;
		offset = op1;
	      }
	    else if (avr32_address_register_rtx_p (op1, 1))
	      {
		/* Operands are switched. */
		base = op1;
		offset = op0;
	      }

	    gcc_assert (base && offset
			&& avr32_address_register_rtx_p (base, 1)
			&& avr32_legitimate_index_p (GET_MODE (x), offset,
						     1));

	    avr32_print_operand (stream, base, 0);
	    fputs ("[", stream);
	    avr32_print_operand (stream, offset, 0);
	    fputs ("]", stream);
	    break;
	  }
	case CONST:
	  output_addr_const (stream, XEXP (XEXP (XEXP (x, 0), 0), 0));
	  fprintf (stream, " + %ld",
		   INTVAL (XEXP (XEXP (XEXP (x, 0), 0), 1)));
	  break;
        case CONST_INT:
	  avr32_print_operand (stream, XEXP (x, 0), 0);
          break;
	default:
	  error = 1;
	}
      break;
    case MULT:
      {
	int value = INTVAL (XEXP (x, 1));

	/* Convert immediate in multiplication into a shift immediate */
	switch (value)
	  {
	  case 2:
	    value = 1;
	    break;
	  case 4:
	    value = 2;
	    break;
	  case 8:
	    value = 3;
	    break;
	  default:
	    value = 0;
	  }
	fprintf (stream, "%s << %i", reg_names[true_regnum (XEXP (x, 0))],
		 value);
	break;
      }
    case ASHIFT:
      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	fprintf (stream, "%s << %i", reg_names[true_regnum (XEXP (x, 0))],
		 (int) INTVAL (XEXP (x, 1)));
      else if (REG_P (XEXP (x, 1)))
	fprintf (stream, "%s << %s", reg_names[true_regnum (XEXP (x, 0))],
		 reg_names[true_regnum (XEXP (x, 1))]);
      else
	{
	  error = 1;
	}
      break;
    case LSHIFTRT:
      if (GET_CODE (XEXP (x, 1)) == CONST_INT)
	fprintf (stream, "%s >> %i", reg_names[true_regnum (XEXP (x, 0))],
		 (int) INTVAL (XEXP (x, 1)));
      else if (REG_P (XEXP (x, 1)))
	fprintf (stream, "%s >> %s", reg_names[true_regnum (XEXP (x, 0))],
		 reg_names[true_regnum (XEXP (x, 1))]);
      else
	{
	  error = 1;
	}
      fprintf (stream, ">>");
      break;
    case PARALLEL:
      {
	/* Load store multiple */
	int i;
	int count = XVECLEN (x, 0);
	int reglist16 = 0;
	char reglist16_string[100];

	for (i = 0; i < count; ++i)
	  {
	    rtx vec_elm = XVECEXP (x, 0, i);
	    if (GET_MODE (vec_elm) != SET)
	      {
		debug_rtx (vec_elm);
		internal_error ("Unknown element in parallel expression!");
	      }
	    if (GET_MODE (XEXP (vec_elm, 0)) == REG)
	      {
		/* Load multiple */
		reglist16 |= 1 << ASM_REGNUM (REGNO (XEXP (vec_elm, 0)));
	      }
	    else
	      {
		/* Store multiple */
		reglist16 |= 1 << ASM_REGNUM (REGNO (XEXP (vec_elm, 1)));
	      }
	  }

	avr32_make_reglist16 (reglist16, reglist16_string);
	fputs (reglist16_string, stream);

	break;
      }

    case PLUS:
      {
        rtx op0 = XEXP (x, 0);
        rtx op1 = XEXP (x, 1);
        rtx base = NULL_RTX, offset = NULL_RTX;

        if (avr32_address_register_rtx_p (op0, 1))
          {
            base = op0;
            offset = op1;
          }
        else if (avr32_address_register_rtx_p (op1, 1))
          {
            /* Operands are switched. */
            base = op1;
            offset = op0;
          }

        gcc_assert (base && offset
                    && avr32_address_register_rtx_p (base, 1)
                    && avr32_legitimate_index_p (GET_MODE (x), offset, 1));

        avr32_print_operand (stream, base, 0);
        fputs ("[", stream);
        avr32_print_operand (stream, offset, 0);
        fputs ("]", stream);
        break;
      }

    default:
      error = 1;
    }

  if (error)
    {
      debug_rtx (x);
      internal_error ("Illegal expression for avr32_print_operand");
    }
}

rtx
avr32_get_note_reg_equiv (rtx insn)
{
  rtx note;

  note = find_reg_note (insn, REG_EQUIV, NULL_RTX);

  if (note != NULL_RTX)
    return XEXP (note, 0);
  else
    return NULL_RTX;
}


/*
  Outputs to stdio stream stream the assembler syntax for an instruction
  operand that is a memory reference whose address is x. x is an RTL
  expression.

  ToDo: fixme.
*/
void
avr32_print_operand_address (FILE * stream, rtx x)
{
  fprintf (stream, "(%d) /* address */", REGNO (x));
}


/* Return true if _GLOBAL_OFFSET_TABLE_ symbol is mentioned.  */
bool
avr32_got_mentioned_p (rtx addr)
{
  if (GET_CODE (addr) == MEM)
    addr = XEXP (addr, 0);
  while (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);
  if (GET_CODE (addr) == SYMBOL_REF)
    {
      return streq (XSTR (addr, 0), "_GLOBAL_OFFSET_TABLE_");
    }
  if (GET_CODE (addr) == PLUS || GET_CODE (addr) == MINUS)
    {
      bool l1, l2;

      l1 = avr32_got_mentioned_p (XEXP (addr, 0));
      l2 = avr32_got_mentioned_p (XEXP (addr, 1));
      return l1 || l2;
    }
  return false;
}


/* Find the symbol in an address expression.  */
rtx
avr32_find_symbol (rtx addr)
{
  if (GET_CODE (addr) == MEM)
    addr = XEXP (addr, 0);

  while (GET_CODE (addr) == CONST)
    addr = XEXP (addr, 0);

  if (GET_CODE (addr) == SYMBOL_REF || GET_CODE (addr) == LABEL_REF)
    return addr;
  if (GET_CODE (addr) == PLUS)
    {
      rtx l1, l2;

      l1 = avr32_find_symbol (XEXP (addr, 0));
      l2 = avr32_find_symbol (XEXP (addr, 1));
      if (l1 != NULL_RTX && l2 == NULL_RTX)
	return l1;
      else if (l1 == NULL_RTX && l2 != NULL_RTX)
	return l2;
    }

  return NULL_RTX;
}


/* Routines for manipulation of the constant pool.  */

/* AVR32 instructions cannot load a large constant directly into a
   register; they have to come from a pc relative load.  The constant
   must therefore be placed in the addressable range of the pc
   relative load.  Depending on the precise pc relative load
   instruction the range is somewhere between 256 bytes and 4k.  This
   means that we often have to dump a constant inside a function, and
   generate code to branch around it.

   It is important to minimize this, since the branches will slow
   things down and make the code larger.

   Normally we can hide the table after an existing unconditional
   branch so that there is no interruption of the flow, but in the
   worst case the code looks like this:

	lddpc	rn, L1
	...
	rjmp	L2
	align
	L1:	.long value
	L2:
	...

	lddpc	rn, L3
	...
	rjmp	L4
	align
	L3:	.long value
	L4:
	...

   We fix this by performing a scan after scheduling, which notices
   which instructions need to have their operands fetched from the
   constant table and builds the table.

   The algorithm starts by building a table of all the constants that
   need fixing up and all the natural barriers in the function (places
   where a constant table can be dropped without breaking the flow).
   For each fixup we note how far the pc-relative replacement will be
   able to reach and the offset of the instruction into the function.

   Having built the table we then group the fixes together to form
   tables that are as large as possible (subject to addressing
   constraints) and emit each table of constants after the last
   barrier that is within range of all the instructions in the group.
   If a group does not contain a barrier, then we forcibly create one
   by inserting a jump instruction into the flow.  Once the table has
   been inserted, the insns are then modified to reference the
   relevant entry in the pool.

   Possible enhancements to the algorithm (not implemented) are:

   1) For some processors and object formats, there may be benefit in
   aligning the pools to the start of cache lines; this alignment
   would need to be taken into account when calculating addressability
   of a pool.  */

/* These typedefs are located at the start of this file, so that
   they can be used in the prototypes there.  This comment is to
   remind readers of that fact so that the following structures
   can be understood more easily.

     typedef struct minipool_node    Mnode;
     typedef struct minipool_fixup   Mfix;  */

struct minipool_node
{
  /* Doubly linked chain of entries.  */
  Mnode *next;
  Mnode *prev;
  /* The maximum offset into the code that this entry can be placed.  While
     pushing fixes for forward references, all entries are sorted in order of
     increasing max_address.  */
  HOST_WIDE_INT max_address;
  /* Similarly for an entry inserted for a backwards ref.  */
  HOST_WIDE_INT min_address;
  /* The number of fixes referencing this entry.  This can become zero if we
     "unpush" an entry.  In this case we ignore the entry when we come to
     emit the code.  */
  int refcount;
  /* The offset from the start of the minipool.  */
  HOST_WIDE_INT offset;
  /* The value in table.  */
  rtx value;
  /* The mode of value.  */
  enum machine_mode mode;
  /* The size of the value.  */
  int fix_size;
};


struct minipool_fixup
{
  Mfix *next;
  rtx insn;
  HOST_WIDE_INT address;
  rtx *loc;
  enum machine_mode mode;
  int fix_size;
  rtx value;
  Mnode *minipool;
  HOST_WIDE_INT forwards;
  HOST_WIDE_INT backwards;
};


/* Fixes less than a word need padding out to a word boundary.  */
#define MINIPOOL_FIX_SIZE(mode, value)                          \
  (IS_FORCE_MINIPOOL(value) ? 0 :                               \
   (GET_MODE_SIZE ((mode)) >= 4 ? GET_MODE_SIZE ((mode)) : 4))

#define IS_FORCE_MINIPOOL(x)                    \
  (GET_CODE(x) == UNSPEC &&                     \
   XINT(x, 1) == UNSPEC_FORCE_MINIPOOL)

static Mnode *minipool_vector_head;
static Mnode *minipool_vector_tail;

/* The linked list of all minipool fixes required for this function.  */
Mfix *minipool_fix_head;
Mfix *minipool_fix_tail;
/* The fix entry for the current minipool, once it has been placed.  */
Mfix *minipool_barrier;


/* Determines if INSN is the start of a jump table.  Returns the end
   of the TABLE or NULL_RTX.  */
static rtx
is_jump_table (rtx insn)
{
  rtx table;

  if (GET_CODE (insn) == JUMP_INSN
      && JUMP_LABEL (insn) != NULL
      && ((table = next_real_insn (JUMP_LABEL (insn)))
	  == next_real_insn (insn))
      && table != NULL
      && GET_CODE (table) == JUMP_INSN
      && (GET_CODE (PATTERN (table)) == ADDR_VEC
	  || GET_CODE (PATTERN (table)) == ADDR_DIFF_VEC))
    return table;

  return NULL_RTX;
}


static HOST_WIDE_INT
get_jump_table_size (rtx insn)
{
  /* ADDR_VECs only take room if read-only data does into the text section.  */
  if (JUMP_TABLES_IN_TEXT_SECTION
#if !defined(READONLY_DATA_SECTION_ASM_OP)
      || 1
#endif
    )
    {
      rtx body = PATTERN (insn);
      int elt = GET_CODE (body) == ADDR_DIFF_VEC ? 1 : 0;

      return GET_MODE_SIZE (GET_MODE (body)) * XVECLEN (body, elt);
    }

  return 0;
}


/* Move a minipool fix MP from its current location to before MAX_MP.
   If MAX_MP is NULL, then MP doesn't need moving, but the addressing
   constraints may need updating.  */
static Mnode *
move_minipool_fix_forward_ref (Mnode * mp, Mnode * max_mp,
			       HOST_WIDE_INT max_address)
{
  /* This should never be true and the code below assumes these are
     different.  */
  if (mp == max_mp)
    abort ();

  if (max_mp == NULL)
    {
      if (max_address < mp->max_address)
	mp->max_address = max_address;
    }
  else
    {
      if (max_address > max_mp->max_address - mp->fix_size)
	mp->max_address = max_mp->max_address - mp->fix_size;
      else
	mp->max_address = max_address;

      /* Unlink MP from its current position.  Since max_mp is non-null,
         mp->prev must be non-null.  */
      mp->prev->next = mp->next;
      if (mp->next != NULL)
	mp->next->prev = mp->prev;
      else
	minipool_vector_tail = mp->prev;

      /* Re-insert it before MAX_MP.  */
      mp->next = max_mp;
      mp->prev = max_mp->prev;
      max_mp->prev = mp;

      if (mp->prev != NULL)
	mp->prev->next = mp;
      else
	minipool_vector_head = mp;
    }

  /* Save the new entry.  */
  max_mp = mp;

  /* Scan over the preceding entries and adjust their addresses as required.
   */
  while (mp->prev != NULL
	 && mp->prev->max_address > mp->max_address - mp->prev->fix_size)
    {
      mp->prev->max_address = mp->max_address - mp->prev->fix_size;
      mp = mp->prev;
    }

  return max_mp;
}


/* Add a constant to the minipool for a forward reference.  Returns the
   node added or NULL if the constant will not fit in this pool.  */
static Mnode *
add_minipool_forward_ref (Mfix * fix)
{
  /* If set, max_mp is the first pool_entry that has a lower constraint than
     the one we are trying to add.  */
  Mnode *max_mp = NULL;
  HOST_WIDE_INT max_address = fix->address + fix->forwards;
  Mnode *mp;

  /* If this fix's address is greater than the address of the first entry,
     then we can't put the fix in this pool.  We subtract the size of the
     current fix to ensure that if the table is fully packed we still have
     enough room to insert this value by suffling the other fixes forwards.  */
  if (minipool_vector_head &&
      fix->address >= minipool_vector_head->max_address - fix->fix_size)
    return NULL;

  /* Scan the pool to see if a constant with the same value has already been
     added.  While we are doing this, also note the location where we must
     insert the constant if it doesn't already exist.  */
  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      if (GET_CODE (fix->value) == GET_CODE (mp->value)
	  && fix->mode == mp->mode
	  && (GET_CODE (fix->value) != CODE_LABEL
	      || (CODE_LABEL_NUMBER (fix->value)
		  == CODE_LABEL_NUMBER (mp->value)))
	  && rtx_equal_p (fix->value, mp->value))
	{
	  /* More than one fix references this entry.  */
	  mp->refcount++;
	  return move_minipool_fix_forward_ref (mp, max_mp, max_address);
	}

      /* Note the insertion point if necessary.  */
      if (max_mp == NULL && mp->max_address > max_address)
	max_mp = mp;

    }

  /* The value is not currently in the minipool, so we need to create a new
     entry for it.  If MAX_MP is NULL, the entry will be put on the end of
     the list since the placement is less constrained than any existing
     entry.  Otherwise, we insert the new fix before MAX_MP and, if
     necessary, adjust the constraints on the other entries.  */
  mp = xmalloc (sizeof (*mp));
  mp->fix_size = fix->fix_size;
  mp->mode = fix->mode;
  mp->value = fix->value;
  mp->refcount = 1;
  /* Not yet required for a backwards ref.  */
  mp->min_address = -65536;

  if (max_mp == NULL)
    {
      mp->max_address = max_address;
      mp->next = NULL;
      mp->prev = minipool_vector_tail;

      if (mp->prev == NULL)
	{
	  minipool_vector_head = mp;
	  minipool_vector_label = gen_label_rtx ();
	}
      else
	mp->prev->next = mp;

      minipool_vector_tail = mp;
    }
  else
    {
      if (max_address > max_mp->max_address - mp->fix_size)
	mp->max_address = max_mp->max_address - mp->fix_size;
      else
	mp->max_address = max_address;

      mp->next = max_mp;
      mp->prev = max_mp->prev;
      max_mp->prev = mp;
      if (mp->prev != NULL)
	mp->prev->next = mp;
      else
	minipool_vector_head = mp;
    }

  /* Save the new entry.  */
  max_mp = mp;

  /* Scan over the preceding entries and adjust their addresses as required.
   */
  while (mp->prev != NULL
	 && mp->prev->max_address > mp->max_address - mp->prev->fix_size)
    {
      mp->prev->max_address = mp->max_address - mp->prev->fix_size;
      mp = mp->prev;
    }

  return max_mp;
}


static Mnode *
move_minipool_fix_backward_ref (Mnode * mp, Mnode * min_mp,
				HOST_WIDE_INT min_address)
{
  HOST_WIDE_INT offset;

  /* This should never be true, and the code below assumes these are
     different.  */
  if (mp == min_mp)
    abort ();

  if (min_mp == NULL)
    {
      if (min_address > mp->min_address)
	mp->min_address = min_address;
    }
  else
    {
      /* We will adjust this below if it is too loose.  */
      mp->min_address = min_address;

      /* Unlink MP from its current position.  Since min_mp is non-null,
         mp->next must be non-null.  */
      mp->next->prev = mp->prev;
      if (mp->prev != NULL)
	mp->prev->next = mp->next;
      else
	minipool_vector_head = mp->next;

      /* Reinsert it after MIN_MP.  */
      mp->prev = min_mp;
      mp->next = min_mp->next;
      min_mp->next = mp;
      if (mp->next != NULL)
	mp->next->prev = mp;
      else
	minipool_vector_tail = mp;
    }

  min_mp = mp;

  offset = 0;
  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      mp->offset = offset;
      if (mp->refcount > 0)
	offset += mp->fix_size;

      if (mp->next && mp->next->min_address < mp->min_address + mp->fix_size)
	mp->next->min_address = mp->min_address + mp->fix_size;
    }

  return min_mp;
}


/* Add a constant to the minipool for a backward reference.  Returns the
   node added or NULL if the constant will not fit in this pool.

   Note that the code for insertion for a backwards reference can be
   somewhat confusing because the calculated offsets for each fix do
   not take into account the size of the pool (which is still under
   construction.  */
static Mnode *
add_minipool_backward_ref (Mfix * fix)
{
  /* If set, min_mp is the last pool_entry that has a lower constraint than
     the one we are trying to add.  */
  Mnode *min_mp = NULL;
  /* This can be negative, since it is only a constraint.  */
  HOST_WIDE_INT min_address = fix->address - fix->backwards;
  Mnode *mp;

  /* If we can't reach the current pool from this insn, or if we can't insert
     this entry at the end of the pool without pushing other fixes out of
     range, then we don't try.  This ensures that we can't fail later on.  */
  if (min_address >= minipool_barrier->address
      || (minipool_vector_tail->min_address + fix->fix_size
	  >= minipool_barrier->address))
    return NULL;

  /* Scan the pool to see if a constant with the same value has already been
     added.  While we are doing this, also note the location where we must
     insert the constant if it doesn't already exist.  */
  for (mp = minipool_vector_tail; mp != NULL; mp = mp->prev)
    {
      if (GET_CODE (fix->value) == GET_CODE (mp->value)
	  && fix->mode == mp->mode
	  && (GET_CODE (fix->value) != CODE_LABEL
	      || (CODE_LABEL_NUMBER (fix->value)
		  == CODE_LABEL_NUMBER (mp->value)))
	  && rtx_equal_p (fix->value, mp->value)
	  /* Check that there is enough slack to move this entry to the end
	     of the table (this is conservative).  */
	  && (mp->max_address
	      > (minipool_barrier->address
		 + minipool_vector_tail->offset
		 + minipool_vector_tail->fix_size)))
	{
	  mp->refcount++;
	  return move_minipool_fix_backward_ref (mp, min_mp, min_address);
	}

      if (min_mp != NULL)
	mp->min_address += fix->fix_size;
      else
	{
	  /* Note the insertion point if necessary.  */
	  if (mp->min_address < min_address)
	    {
	      min_mp = mp;
	    }
	  else if (mp->max_address
		   < minipool_barrier->address + mp->offset + fix->fix_size)
	    {
	      /* Inserting before this entry would push the fix beyond its
	         maximum address (which can happen if we have re-located a
	         forwards fix); force the new fix to come after it.  */
	      min_mp = mp;
	      min_address = mp->min_address + fix->fix_size;
	    }
	}
    }

  /* We need to create a new entry.  */
  mp = xmalloc (sizeof (*mp));
  mp->fix_size = fix->fix_size;
  mp->mode = fix->mode;
  mp->value = fix->value;
  mp->refcount = 1;
  mp->max_address = minipool_barrier->address + 65536;

  mp->min_address = min_address;

  if (min_mp == NULL)
    {
      mp->prev = NULL;
      mp->next = minipool_vector_head;

      if (mp->next == NULL)
	{
	  minipool_vector_tail = mp;
	  minipool_vector_label = gen_label_rtx ();
	}
      else
	mp->next->prev = mp;

      minipool_vector_head = mp;
    }
  else
    {
      mp->next = min_mp->next;
      mp->prev = min_mp;
      min_mp->next = mp;

      if (mp->next != NULL)
	mp->next->prev = mp;
      else
	minipool_vector_tail = mp;
    }

  /* Save the new entry.  */
  min_mp = mp;

  if (mp->prev)
    mp = mp->prev;
  else
    mp->offset = 0;

  /* Scan over the following entries and adjust their offsets.  */
  while (mp->next != NULL)
    {
      if (mp->next->min_address < mp->min_address + mp->fix_size)
	mp->next->min_address = mp->min_address + mp->fix_size;

      if (mp->refcount)
	mp->next->offset = mp->offset + mp->fix_size;
      else
	mp->next->offset = mp->offset;

      mp = mp->next;
    }

  return min_mp;
}


static void
assign_minipool_offsets (Mfix * barrier)
{
  HOST_WIDE_INT offset = 0;
  Mnode *mp;

  minipool_barrier = barrier;

  for (mp = minipool_vector_head; mp != NULL; mp = mp->next)
    {
      mp->offset = offset;

      if (mp->refcount > 0)
	offset += mp->fix_size;
    }
}


/* Print a symbolic form of X to the debug file, F.  */
static void
avr32_print_value (FILE * f, rtx x)
{
  switch (GET_CODE (x))
    {
    case CONST_INT:
      fprintf (f, "0x%x", (int) INTVAL (x));
      return;

    case CONST_DOUBLE:
      fprintf (f, "<0x%lx,0x%lx>", (long) XWINT (x, 2), (long) XWINT (x, 3));
      return;

    case CONST_VECTOR:
      {
	int i;

	fprintf (f, "<");
	for (i = 0; i < CONST_VECTOR_NUNITS (x); i++)
	  {
	    fprintf (f, "0x%x", (int) INTVAL (CONST_VECTOR_ELT (x, i)));
	    if (i < (CONST_VECTOR_NUNITS (x) - 1))
	      fputc (',', f);
	  }
	fprintf (f, ">");
      }
      return;

    case CONST_STRING:
      fprintf (f, "\"%s\"", XSTR (x, 0));
      return;

    case SYMBOL_REF:
      fprintf (f, "`%s'", XSTR (x, 0));
      return;

    case LABEL_REF:
      fprintf (f, "L%d", INSN_UID (XEXP (x, 0)));
      return;

    case CONST:
      avr32_print_value (f, XEXP (x, 0));
      return;

    case PLUS:
      avr32_print_value (f, XEXP (x, 0));
      fprintf (f, "+");
      avr32_print_value (f, XEXP (x, 1));
      return;

    case PC:
      fprintf (f, "pc");
      return;

    default:
      fprintf (f, "????");
      return;
    }
}


int
is_minipool_label (rtx label)
{
  minipool_labels *cur_mp_label = cfun->machine->minipool_label_head;

  if (GET_CODE (label) != CODE_LABEL)
    return FALSE;

  while (cur_mp_label)
    {
      if (CODE_LABEL_NUMBER (label)
	  == CODE_LABEL_NUMBER (cur_mp_label->label))
	return TRUE;
      cur_mp_label = cur_mp_label->next;
    }
  return FALSE;
}


static void
new_minipool_label (rtx label)
{
  if (!cfun->machine->minipool_label_head)
    {
      cfun->machine->minipool_label_head =
	ggc_alloc (sizeof (minipool_labels));
      cfun->machine->minipool_label_tail = cfun->machine->minipool_label_head;
      cfun->machine->minipool_label_head->label = label;
      cfun->machine->minipool_label_head->next = 0;
      cfun->machine->minipool_label_head->prev = 0;
    }
  else
    {
      cfun->machine->minipool_label_tail->next =
	ggc_alloc (sizeof (minipool_labels));
      cfun->machine->minipool_label_tail->next->label = label;
      cfun->machine->minipool_label_tail->next->next = 0;
      cfun->machine->minipool_label_tail->next->prev =
	cfun->machine->minipool_label_tail;
      cfun->machine->minipool_label_tail =
	cfun->machine->minipool_label_tail->next;
    }
}


/* Output the literal table */
static void
dump_minipool (rtx scan)
{
  Mnode *mp;
  Mnode *nmp;

  if (dump_file)
    fprintf (dump_file,
	     ";; Emitting minipool after insn %u; address %ld; align %d (bytes)\n",
	     INSN_UID (scan), (unsigned long) minipool_barrier->address, 4);

  scan = emit_insn_after (gen_consttable_start (), scan);
  scan = emit_insn_after (gen_align_4 (), scan);
  scan = emit_label_after (minipool_vector_label, scan);
  new_minipool_label (minipool_vector_label);

  for (mp = minipool_vector_head; mp != NULL; mp = nmp)
    {
      if (mp->refcount > 0)
	{
	  if (dump_file)
	    {
	      fprintf (dump_file,
		       ";;  Offset %u, min %ld, max %ld ",
		       (unsigned) mp->offset, (unsigned long) mp->min_address,
		       (unsigned long) mp->max_address);
	      avr32_print_value (dump_file, mp->value);
	      fputc ('\n', dump_file);
	    }

	  switch (mp->fix_size)
	    {
#ifdef HAVE_consttable_4
	    case 4:
	      scan = emit_insn_after (gen_consttable_4 (mp->value), scan);
	      break;

#endif
#ifdef HAVE_consttable_8
	    case 8:
	      scan = emit_insn_after (gen_consttable_8 (mp->value), scan);
	      break;

#endif
#ifdef HAVE_consttable_16
            case 16:
              scan = emit_insn_after (gen_consttable_16 (mp->value), scan);
              break;

#endif
            case 0:
              /* This can happen for force-minipool entries which just are
	         there to force the minipool to be generate. */
	      break;
	    default:
	      abort ();
	      break;
	    }
	}

      nmp = mp->next;
      free (mp);
    }

  minipool_vector_head = minipool_vector_tail = NULL;
  scan = emit_insn_after (gen_consttable_end (), scan);
  scan = emit_barrier_after (scan);
}


/* Return the cost of forcibly inserting a barrier after INSN.  */
static int
avr32_barrier_cost (rtx insn)
{
  /* Basing the location of the pool on the loop depth is preferable, but at
     the moment, the basic block information seems to be corrupt by this
     stage of the compilation.  */
  int base_cost = 50;
  rtx next = next_nonnote_insn (insn);

  if (next != NULL && GET_CODE (next) == CODE_LABEL)
    base_cost -= 20;

  switch (GET_CODE (insn))
    {
    case CODE_LABEL:
      /* It will always be better to place the table before the label, rather
         than after it.  */
      return 50;

    case INSN:
    case CALL_INSN:
      return base_cost;

    case JUMP_INSN:
      return base_cost - 10;

    default:
      return base_cost + 10;
    }
}


/* Find the best place in the insn stream in the range
   (FIX->address,MAX_ADDRESS) to forcibly insert a minipool barrier.
   Create the barrier by inserting a jump and add a new fix entry for
   it.  */
static Mfix *
create_fix_barrier (Mfix * fix, HOST_WIDE_INT max_address)
{
  HOST_WIDE_INT count = 0;
  rtx barrier;
  rtx from = fix->insn;
  rtx selected = from;
  int selected_cost;
  HOST_WIDE_INT selected_address;
  Mfix *new_fix;
  HOST_WIDE_INT max_count = max_address - fix->address;
  rtx label = gen_label_rtx ();

  selected_cost = avr32_barrier_cost (from);
  selected_address = fix->address;

  while (from && count < max_count)
    {
      rtx tmp;
      int new_cost;

      /* This code shouldn't have been called if there was a natural barrier
         within range.  */
      if (GET_CODE (from) == BARRIER)
	abort ();

      /* Count the length of this insn.  */
      count += get_attr_length (from);

      /* If there is a jump table, add its length.  */
      tmp = is_jump_table (from);
      if (tmp != NULL)
	{
	  count += get_jump_table_size (tmp);

	  /* Jump tables aren't in a basic block, so base the cost on the
	     dispatch insn.  If we select this location, we will still put
	     the pool after the table.  */
	  new_cost = avr32_barrier_cost (from);

	  if (count < max_count && new_cost <= selected_cost)
	    {
	      selected = tmp;
	      selected_cost = new_cost;
	      selected_address = fix->address + count;
	    }

	  /* Continue after the dispatch table.  */
	  from = NEXT_INSN (tmp);
	  continue;
	}

      new_cost = avr32_barrier_cost (from);

      if (count < max_count && new_cost <= selected_cost)
	{
	  selected = from;
	  selected_cost = new_cost;
	  selected_address = fix->address + count;
	}

      from = NEXT_INSN (from);
    }

  /* Create a new JUMP_INSN that branches around a barrier.  */
  from = emit_jump_insn_after (gen_jump (label), selected);
  JUMP_LABEL (from) = label;
  barrier = emit_barrier_after (from);
  emit_label_after (label, barrier);

  /* Create a minipool barrier entry for the new barrier.  */
  new_fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (*new_fix));
  new_fix->insn = barrier;
  new_fix->address = selected_address;
  new_fix->next = fix->next;
  fix->next = new_fix;

  return new_fix;
}


/* Record that there is a natural barrier in the insn stream at
   ADDRESS.  */
static void
push_minipool_barrier (rtx insn, HOST_WIDE_INT address)
{
  Mfix *fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (*fix));

  fix->insn = insn;
  fix->address = address;

  fix->next = NULL;
  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}


/* Record INSN, which will need fixing up to load a value from the
   minipool.  ADDRESS is the offset of the insn since the start of the
   function; LOC is a pointer to the part of the insn which requires
   fixing; VALUE is the constant that must be loaded, which is of type
   MODE.  */
static void
push_minipool_fix (rtx insn, HOST_WIDE_INT address, rtx * loc,
		   enum machine_mode mode, rtx value)
{
  Mfix *fix = (Mfix *) obstack_alloc (&minipool_obstack, sizeof (*fix));
  rtx body = PATTERN (insn);

  fix->insn = insn;
  fix->address = address;
  fix->loc = loc;
  fix->mode = mode;
  fix->fix_size = MINIPOOL_FIX_SIZE (mode, value);
  fix->value = value;

  if (GET_CODE (body) == PARALLEL)
    {
      /* Mcall : Ks16 << 2 */
      fix->forwards = ((1 << 15) - 1) << 2;
      fix->backwards = (1 << 15) << 2;
    }
  else if (GET_CODE (body) == SET
           && GET_MODE_SIZE (GET_MODE (SET_DEST (body))) == 4)
    {
          if (optimize_size)
            {
              /* Lddpc : Ku7 << 2 */
              fix->forwards = ((1 << 7) - 1) << 2;
              fix->backwards = 0;
            }
          else
            {
              /* Ld.w : Ks16 */
              fix->forwards = ((1 << 15) - 4);
              fix->backwards = (1 << 15);
            }
        }
  else if (GET_CODE (body) == SET
           && GET_MODE_SIZE (GET_MODE (SET_DEST (body))) == 8)
    {
          /* Ld.d : Ks16 */
          fix->forwards = ((1 << 15) - 4);
          fix->backwards = (1 << 15);
        }
  else if (GET_CODE (body) == UNSPEC_VOLATILE
           && XINT (body, 1) == VUNSPEC_MVRC)
    {
      /* Coprocessor load */
      /* Ldc : Ku8 << 2 */
      fix->forwards = ((1 << 8) - 1) << 2;
      fix->backwards = 0;
    }
  else
    {
      /* Assume worst case which is lddpc insn. */
      fix->forwards = ((1 << 7) - 1) << 2;
      fix->backwards = 0;
    }

  fix->minipool = NULL;

  /* If an insn doesn't have a range defined for it, then it isn't expecting
     to be reworked by this code.  Better to abort now than to generate duff
     assembly code.  */
  if (fix->forwards == 0 && fix->backwards == 0)
    abort ();

  if (dump_file)
    {
      fprintf (dump_file,
	       ";; %smode fixup for i%d; addr %lu, range (%ld,%ld): ",
	       GET_MODE_NAME (mode),
	       INSN_UID (insn), (unsigned long) address,
	       -1 * (long) fix->backwards, (long) fix->forwards);
      avr32_print_value (dump_file, fix->value);
      fprintf (dump_file, "\n");
    }

  /* Add it to the chain of fixes.  */
  fix->next = NULL;

  if (minipool_fix_head != NULL)
    minipool_fix_tail->next = fix;
  else
    minipool_fix_head = fix;

  minipool_fix_tail = fix;
}


/* Scan INSN and note any of its operands that need fixing.
   If DO_PUSHES is false we do not actually push any of the fixups
   needed.  The function returns TRUE is any fixups were needed/pushed.
   This is used by avr32_memory_load_p() which needs to know about loads
   of constants that will be converted into minipool loads.  */
static bool
note_invalid_constants (rtx insn, HOST_WIDE_INT address, int do_pushes)
{
  bool result = false;
  int opno;

  extract_insn (insn);

  if (!constrain_operands (1))
    fatal_insn_not_found (insn);

  if (recog_data.n_alternatives == 0)
    return false;

  /* Fill in recog_op_alt with information about the constraints of this
     insn.  */
  preprocess_constraints ();

  for (opno = 0; opno < recog_data.n_operands; opno++)
    {
      rtx op;

      /* Things we need to fix can only occur in inputs.  */
      if (recog_data.operand_type[opno] != OP_IN)
	continue;

      op = recog_data.operand[opno];

      if (avr32_const_pool_ref_operand (op, GET_MODE (op)))
	{
	  if (do_pushes)
	    {
	      rtx cop = avoid_constant_pool_reference (op);

	      /* Casting the address of something to a mode narrower than a
	         word can cause avoid_constant_pool_reference() to return the
	         pool reference itself.  That's no good to us here.  Lets
	         just hope that we can use the constant pool value directly.
	       */
	      if (op == cop)
		cop = get_pool_constant (XEXP (op, 0));

	      push_minipool_fix (insn, address,
				 recog_data.operand_loc[opno],
				 recog_data.operand_mode[opno], cop);
	    }

	  result = true;
	}
      else if (TARGET_HAS_ASM_ADDR_PSEUDOS
	       && avr32_address_operand (op, GET_MODE (op)))
	{
	  /* Handle pseudo instructions using a direct address. These pseudo
	     instructions might need entries in the constant pool and we must
	     therefor create a constant pool for them, in case the
	     assembler/linker needs to insert entries. */
	  if (do_pushes)
	    {
	      /* Push a dummy constant pool entry so that the .cpool
	         directive should be inserted on the appropriate place in the
	         code even if there are no real constant pool entries. This
	         is used by the assembler and linker to know where to put
	         generated constant pool entries. */
	      push_minipool_fix (insn, address,
				 recog_data.operand_loc[opno],
				 recog_data.operand_mode[opno],
				 gen_rtx_UNSPEC (VOIDmode,
						 gen_rtvec (1, const0_rtx),
						 UNSPEC_FORCE_MINIPOOL));
	      result = true;
	    }
	}
    }
  return result;
}


static int
avr32_insn_is_cast (rtx insn)
{

  if (NONJUMP_INSN_P (insn)
      && GET_CODE (PATTERN (insn)) == SET
      && (GET_CODE (SET_SRC (PATTERN (insn))) == ZERO_EXTEND
	  || GET_CODE (SET_SRC (PATTERN (insn))) == SIGN_EXTEND)
      && REG_P (XEXP (SET_SRC (PATTERN (insn)), 0))
      && REG_P (SET_DEST (PATTERN (insn))))
    return true;
  return false;
}


/* Replace all occurances of reg FROM with reg TO in X. */
rtx
avr32_replace_reg (rtx x, rtx from, rtx to)
{
  int i, j;
  const char *fmt;

  gcc_assert ( REG_P (from) && REG_P (to) );

  /* Allow this function to make replacements in EXPR_LISTs.  */
  if (x == 0)
    return 0;

  if (rtx_equal_p (x, from))
    return to;

  if (GET_CODE (x) == SUBREG)
    {
      rtx new = avr32_replace_reg (SUBREG_REG (x), from, to);

      if (GET_CODE (new) == CONST_INT)
	{
	  x = simplify_subreg (GET_MODE (x), new,
			       GET_MODE (SUBREG_REG (x)),
			       SUBREG_BYTE (x));
	  gcc_assert (x);
	}
      else
	SUBREG_REG (x) = new;

      return x;
    }
  else if (GET_CODE (x) == ZERO_EXTEND)
    {
      rtx new = avr32_replace_reg (XEXP (x, 0), from, to);

      if (GET_CODE (new) == CONST_INT)
	{
	  x = simplify_unary_operation (ZERO_EXTEND, GET_MODE (x),
					new, GET_MODE (XEXP (x, 0)));
	  gcc_assert (x);
	}
      else
	XEXP (x, 0) = new;

      return x;
    }

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	XEXP (x, i) = avr32_replace_reg (XEXP (x, i), from, to);
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  XVECEXP (x, i, j) = avr32_replace_reg (XVECEXP (x, i, j), from, to);
    }

  return x;
}


/* FIXME: The level of nesting in this function is way too deep. It needs to be
   torn apart.  */
static void
avr32_reorg_optimization (void)
{
  rtx first = get_first_nonnote_insn ();
  rtx insn;

  if (TARGET_MD_REORG_OPTIMIZATION && (optimize_size || (optimize > 0)))
    {

      /* Scan through all insns looking for cast operations. */
      if (dump_file)
	{
	  fprintf (dump_file, ";; Deleting redundant cast operations:\n");
	}
      for (insn = first; insn; insn = NEXT_INSN (insn))
	{
	  rtx reg, src_reg, scan;
	  enum machine_mode mode;
	  int unused_cast;
	  rtx label_ref;

	  if (avr32_insn_is_cast (insn)
	      && (GET_MODE (XEXP (SET_SRC (PATTERN (insn)), 0)) == QImode
		  || GET_MODE (XEXP (SET_SRC (PATTERN (insn)), 0)) == HImode))
	    {
	      mode = GET_MODE (XEXP (SET_SRC (PATTERN (insn)), 0));
	      reg = SET_DEST (PATTERN (insn));
	      src_reg = XEXP (SET_SRC (PATTERN (insn)), 0);
	    }
	  else
	    {
	      continue;
	    }

	  unused_cast = false;
	  label_ref = NULL_RTX;
	  for (scan = NEXT_INSN (insn); scan; scan = NEXT_INSN (scan))
	    {
	      /* Check if we have reached the destination of a simple
	         conditional jump which we have already scanned past. If so,
	         we can safely continue scanning. */
	      if (LABEL_P (scan) && label_ref != NULL_RTX)
		{
		  if (CODE_LABEL_NUMBER (scan) ==
		      CODE_LABEL_NUMBER (XEXP (label_ref, 0)))
		    label_ref = NULL_RTX;
		  else
		    break;
		}

	      if (!INSN_P (scan))
		continue;

	      /* For conditional jumps we can manage to keep on scanning if
	         we meet the destination label later on before any new jump
	         insns occure. */
	      if (GET_CODE (scan) == JUMP_INSN)
		{
		  if (any_condjump_p (scan) && label_ref == NULL_RTX)
		    label_ref = condjump_label (scan);
		  else
		    break;
		}

              /* Check if we have a call and the register is used as an argument. */
              if (CALL_P (scan)
                  && find_reg_fusage (scan, USE, reg) )
                break;

	      if (!reg_mentioned_p (reg, PATTERN (scan)))
		continue;

	      /* Check if casted register is used in this insn */
	      if ((regno_use_in (REGNO (reg), PATTERN (scan)) != NULL_RTX)
		  && (GET_MODE (regno_use_in (REGNO (reg), PATTERN (scan))) ==
		      GET_MODE (reg)))
		{
		  /* If not used in the source to the set or in a memory
		     expression in the destiantion then the register is used
		     as a destination and is really dead. */
		  if (single_set (scan)
		      && GET_CODE (PATTERN (scan)) == SET
		      && REG_P (SET_DEST (PATTERN (scan)))
		      && !regno_use_in (REGNO (reg), SET_SRC (PATTERN (scan)))
		      && label_ref == NULL_RTX)
		    {
		      unused_cast = true;
		    }
		  break;
		}

	      /* Check if register is dead or set in this insn */
	      if (dead_or_set_p (scan, reg))
		{
		  unused_cast = true;
		  break;
		}
	    }

	  /* Check if we have unresolved conditional jumps */
	  if (label_ref != NULL_RTX)
	    continue;

	  if (unused_cast)
	    {
	      if (REGNO (reg) == REGNO (XEXP (SET_SRC (PATTERN (insn)), 0)))
		{
		  /* One operand cast, safe to delete */
		  if (dump_file)
		    {
		      fprintf (dump_file,
			       ";;  INSN %i removed, casted register %i value not used.\n",
			       INSN_UID (insn), REGNO (reg));
		    }
		  SET_INSN_DELETED (insn);
		  /* Force the instruction to be recognized again */
		  INSN_CODE (insn) = -1;
		}
	      else
		{
		  /* Two operand cast, which really could be substituted with
		     a move, if the source register is dead after the cast
		     insn and then the insn which sets the source register
		     could instead directly set the destination register for
		     the cast. As long as there are no insns in between which
		     uses the register. */
		  rtx link = NULL_RTX;
		  rtx set;
		  rtx src_reg = XEXP (SET_SRC (PATTERN (insn)), 0);
		  unused_cast = false;

		  if (!find_reg_note (insn, REG_DEAD, src_reg))
		    continue;

		  /* Search for the insn which sets the source register */
                  for (scan = PREV_INSN (insn);
                       scan && GET_CODE (scan) != CODE_LABEL;
                       scan = PREV_INSN (scan))
                    {
                      if (! INSN_P (scan))
                        continue;

		      set = single_set (scan);
                      // Fix for bug #11763 : the following if condition
                      // has been modified and else part is included to 
                      // set the link to NULL_RTX. 
                      // if (set && rtx_equal_p (src_reg, SET_DEST (set)))
                      if (set && (REGNO(src_reg) == REGNO(SET_DEST(set))))
                       {
                         if (rtx_equal_p (src_reg, SET_DEST (set)))
			  {
			    link = scan;
			    break;
                          }
                         else
                          {
                            link = NULL_RTX;
                            break;
                          }
                       }
                    }


		  /* Found no link or link is a call insn where we can not
		     change the destination register */
		  if (link == NULL_RTX || CALL_P (link))
		    continue;

		  /* Scan through all insn between link and insn */
		  for (scan = NEXT_INSN (link); scan; scan = NEXT_INSN (scan))
		    {
		      /* Don't try to trace forward past a CODE_LABEL if we
		         haven't seen INSN yet.  Ordinarily, we will only
		         find the setting insn in LOG_LINKS if it is in the
		         same basic block.  However, cross-jumping can insert
		         code labels in between the load and the call, and
		         can result in situations where a single call insn
		         may have two targets depending on where we came
		         from.  */

		      if (GET_CODE (scan) == CODE_LABEL)
			break;

		      if (!INSN_P (scan))
			continue;

		      /* Don't try to trace forward past a JUMP.  To optimize
		         safely, we would have to check that all the
		         instructions at the jump destination did not use REG.
		       */

		      if (GET_CODE (scan) == JUMP_INSN)
			{
			  break;
			}

		      if (!reg_mentioned_p (src_reg, PATTERN (scan)))
			continue;

		      /* We have reached the cast insn */
		      if (scan == insn)
			{
			  /* We can remove cast and replace the destination
			     register of the link insn with the destination
			     of the cast */
			  if (dump_file)
			    {
			      fprintf (dump_file,
				       ";;  INSN %i removed, casted value unused. "
				       "Destination of removed cast operation: register %i,  folded into INSN %i.\n",
				       INSN_UID (insn), REGNO (reg),
				       INSN_UID (link));
			    }
			  /* Update link insn */
			  SET_DEST (PATTERN (link)) =
			    gen_rtx_REG (mode, REGNO (reg));
			  /* Force the instruction to be recognized again */
			  INSN_CODE (link) = -1;

			  /* Delete insn */
			  SET_INSN_DELETED (insn);
			  /* Force the instruction to be recognized again */
			  INSN_CODE (insn) = -1;
			  break;
			}
		    }
		}
	    }
	}
    }

  if (TARGET_MD_REORG_OPTIMIZATION && (optimize_size || (optimize > 0)))
    {

      /* Scan through all insns looking for shifted add operations */
      if (dump_file)
	{
	  fprintf (dump_file,
		   ";; Deleting redundant shifted add operations:\n");
	}
      for (insn = first; insn; insn = NEXT_INSN (insn))
	{
	  rtx reg, mem_expr, scan, op0, op1;
	  int add_only_used_as_pointer;

	  if (INSN_P (insn)
	      && GET_CODE (PATTERN (insn)) == SET
	      && GET_CODE (SET_SRC (PATTERN (insn))) == PLUS
	      && (GET_CODE (XEXP (SET_SRC (PATTERN (insn)), 0)) == MULT
		  || GET_CODE (XEXP (SET_SRC (PATTERN (insn)), 0)) == ASHIFT)
	      && GET_CODE (XEXP (XEXP (SET_SRC (PATTERN (insn)), 0), 1)) ==
	      CONST_INT && REG_P (SET_DEST (PATTERN (insn)))
	      && REG_P (XEXP (SET_SRC (PATTERN (insn)), 1))
	      && REG_P (XEXP (XEXP (SET_SRC (PATTERN (insn)), 0), 0)))
	    {
	      reg = SET_DEST (PATTERN (insn));
	      mem_expr = SET_SRC (PATTERN (insn));
	      op0 = XEXP (XEXP (mem_expr, 0), 0);
	      op1 = XEXP (mem_expr, 1);
	    }
	  else
	    {
	      continue;
	    }

	  /* Scan forward the check if the result of the shifted add
	     operation is only used as an address in memory operations and
	     that the operands to the shifted add are not clobbered. */
	  add_only_used_as_pointer = false;
	  for (scan = NEXT_INSN (insn); scan; scan = NEXT_INSN (scan))
	    {
	      if (!INSN_P (scan))
		continue;

	      /* Don't try to trace forward past a JUMP or CALL.  To optimize
	         safely, we would have to check that all the instructions at
	         the jump destination did not use REG.  */

	      if (GET_CODE (scan) == JUMP_INSN)
		{
		  break;
		}

	      /* If used in a call insn then we cannot optimize it away */
	      if (CALL_P (scan) && find_regno_fusage (scan, USE, REGNO (reg)))
		break;

	      /* If any of the operands of the shifted add are clobbered we
	         cannot optimize the shifted adda away */
	      if ((reg_set_p (op0, scan) && (REGNO (op0) != REGNO (reg)))
		  || (reg_set_p (op1, scan) && (REGNO (op1) != REGNO (reg))))
		break;

	      if (!reg_mentioned_p (reg, PATTERN (scan)))
		continue;

	      /* If used any other place than as a pointer or as the
	         destination register we failed */
              if (!(single_set (scan)
                    && GET_CODE (PATTERN (scan)) == SET
                    && ((MEM_P (SET_DEST (PATTERN (scan)))
                         && REG_P (XEXP (SET_DEST (PATTERN (scan)), 0))
                         && REGNO (XEXP (SET_DEST (PATTERN (scan)), 0)) == REGNO (reg))
                        || (MEM_P (SET_SRC (PATTERN (scan)))
                            && REG_P (XEXP (SET_SRC (PATTERN (scan)), 0))
                            && REGNO (XEXP
                                      (SET_SRC (PATTERN (scan)), 0)) == REGNO (reg))))
                  && !(GET_CODE (PATTERN (scan)) == SET
                       && REG_P (SET_DEST (PATTERN (scan)))
                       && !regno_use_in (REGNO (reg),
                                         SET_SRC (PATTERN (scan)))))
                break;

              /* We cannot replace the pointer in TImode insns
                 as these has a differene addressing mode than the other
                 memory insns. */
              if ( GET_MODE (SET_DEST (PATTERN (scan))) == TImode )
                break;

	      /* Check if register is dead or set in this insn */
	      if (dead_or_set_p (scan, reg))
		{
		  add_only_used_as_pointer = true;
		  break;
		}
	    }

	  if (add_only_used_as_pointer)
	    {
	      /* Lets delete the add insn and replace all memory references
	         which uses the pointer with the full expression. */
	      if (dump_file)
		{
		  fprintf (dump_file,
			   ";; Deleting INSN %i since address expression can be folded into all "
			   "memory references using this expression\n",
			   INSN_UID (insn));
		}
	      SET_INSN_DELETED (insn);
	      /* Force the instruction to be recognized again */
	      INSN_CODE (insn) = -1;

	      for (scan = NEXT_INSN (insn); scan; scan = NEXT_INSN (scan))
		{
		  if (!INSN_P (scan))
		    continue;

		  if (!reg_mentioned_p (reg, PATTERN (scan)))
		    continue;

		  /* If used any other place than as a pointer or as the
		     destination register we failed */
		  if ((single_set (scan)
		       && GET_CODE (PATTERN (scan)) == SET
		       && ((MEM_P (SET_DEST (PATTERN (scan)))
			    && REG_P (XEXP (SET_DEST (PATTERN (scan)), 0))
			    && REGNO (XEXP (SET_DEST (PATTERN (scan)), 0)) ==
			    REGNO (reg)) || (MEM_P (SET_SRC (PATTERN (scan)))
					     &&
					     REG_P (XEXP
						    (SET_SRC (PATTERN (scan)),
						     0))
					     &&
					     REGNO (XEXP
						    (SET_SRC (PATTERN (scan)),
						     0)) == REGNO (reg)))))
		    {
		      if (dump_file)
			{
			  fprintf (dump_file,
				   ";; Register %i replaced by indexed address in INSN %i\n",
				   REGNO (reg), INSN_UID (scan));
			}
		      if (MEM_P (SET_DEST (PATTERN (scan))))
			XEXP (SET_DEST (PATTERN (scan)), 0) = mem_expr;
		      else
			XEXP (SET_SRC (PATTERN (scan)), 0) = mem_expr;
		    }

		  /* Check if register is dead or set in this insn */
		  if (dead_or_set_p (scan, reg))
		    {
		      break;
		    }

		}
	    }
	}
    }


  if (TARGET_MD_REORG_OPTIMIZATION && (optimize_size || (optimize > 0)))
    {

      /* Scan through all insns looking for conditional register to
         register move operations */
      if (dump_file)
	{
	  fprintf (dump_file,
		   ";; Folding redundant conditional move operations:\n");
	}
      for (insn = first; insn; insn = next_nonnote_insn (insn))
	{
	  rtx src_reg, dst_reg, scan, test;

	  if (INSN_P (insn)
              && GET_CODE (PATTERN (insn)) == COND_EXEC
	      && GET_CODE (COND_EXEC_CODE (PATTERN (insn))) == SET
	      && REG_P (SET_SRC (COND_EXEC_CODE (PATTERN (insn))))
	      && REG_P (SET_DEST (COND_EXEC_CODE (PATTERN (insn))))
              && find_reg_note (insn, REG_DEAD, SET_SRC (COND_EXEC_CODE (PATTERN (insn)))))
	    {
	      src_reg = SET_SRC (COND_EXEC_CODE (PATTERN (insn)));
	      dst_reg = SET_DEST (COND_EXEC_CODE (PATTERN (insn)));
              test = COND_EXEC_TEST (PATTERN (insn));
	    }
	  else
	    {
	      continue;
	    }

          /* Scan backward through the rest of insns in this if-then or if-else
             block and check if we can fold the move into another of the conditional
             insns in the same block. */
          scan = prev_nonnote_insn (insn);
          while (scan
                 && INSN_P (scan)
                 && GET_CODE (PATTERN (scan)) == COND_EXEC
                 && rtx_equal_p (COND_EXEC_TEST (PATTERN (scan)), test))
            {
              rtx pattern = COND_EXEC_CODE (PATTERN (scan));
              if ( GET_CODE (pattern) == PARALLEL )
                pattern = XVECEXP (pattern, 0, 0);

              if ( reg_set_p (src_reg, pattern) )
                {
                  /* Fold in the destination register for the cond. move
                     into this insn. */
                  SET_DEST (pattern) = dst_reg;
                  if (dump_file)
                    {
                      fprintf (dump_file,
                               ";; Deleting INSN %i since this operation can be folded into INSN %i\n",
                               INSN_UID (insn), INSN_UID (scan));
                    }

                  /* Scan and check if any of the insns in between uses the src_reg. We
                     must then replace it with the dst_reg. */
                  while ( (scan = next_nonnote_insn (scan)) != insn ){
                    avr32_replace_reg (scan, src_reg, dst_reg);
                  }
                  /* Delete the insn. */
                  SET_INSN_DELETED (insn);

                  /* Force the instruction to be recognized again */
                  INSN_CODE (insn) = -1;
                  break;
                }

              /* If the destination register is used but not set in this insn
                 we cannot fold. */
              if ( reg_mentioned_p (dst_reg, pattern) )
                break;

              scan = prev_nonnote_insn (scan);
            }
        }
    }

}


/* Exported to toplev.c.

   Do a final pass over the function, just before delayed branch
   scheduling.  */
static void
avr32_reorg (void)
{
  rtx insn;
  HOST_WIDE_INT address = 0;
  Mfix *fix;

  minipool_fix_head = minipool_fix_tail = NULL;

  /* The first insn must always be a note, or the code below won't scan it
     properly.  */
  insn = get_insns ();
  if (GET_CODE (insn) != NOTE)
    abort ();

  /* Scan all the insns and record the operands that will need fixing.  */
  for (insn = next_nonnote_insn (insn); insn; insn = next_nonnote_insn (insn))
    {
      if (GET_CODE (insn) == BARRIER)
	push_minipool_barrier (insn, address);
      else if (INSN_P (insn))
	{
	  rtx table;

	  note_invalid_constants (insn, address, true);
	  address += get_attr_length (insn);

	  /* If the insn is a vector jump, add the size of the table and skip
	     the table.  */
	  if ((table = is_jump_table (insn)) != NULL)
	    {
	      address += get_jump_table_size (table);
	      insn = table;
	    }
	}
    }

  fix = minipool_fix_head;

  /* Now scan the fixups and perform the required changes.  */
  while (fix)
    {
      Mfix *ftmp;
      Mfix *fdel;
      Mfix *last_added_fix;
      Mfix *last_barrier = NULL;
      Mfix *this_fix;

      /* Skip any further barriers before the next fix.  */
      while (fix && GET_CODE (fix->insn) == BARRIER)
	fix = fix->next;

      /* No more fixes.  */
      if (fix == NULL)
	break;

      last_added_fix = NULL;

      for (ftmp = fix; ftmp; ftmp = ftmp->next)
	{
	  if (GET_CODE (ftmp->insn) == BARRIER)
	    {
	      if (ftmp->address >= minipool_vector_head->max_address)
		break;

	      last_barrier = ftmp;
	    }
	  else if ((ftmp->minipool = add_minipool_forward_ref (ftmp)) == NULL)
	    break;

	  last_added_fix = ftmp;	/* Keep track of the last fix added.
					 */
	}

      /* If we found a barrier, drop back to that; any fixes that we could
         have reached but come after the barrier will now go in the next
         mini-pool.  */
      if (last_barrier != NULL)
	{
	  /* Reduce the refcount for those fixes that won't go into this pool
	     after all.  */
	  for (fdel = last_barrier->next;
	       fdel && fdel != ftmp; fdel = fdel->next)
	    {
	      fdel->minipool->refcount--;
	      fdel->minipool = NULL;
	    }

	  ftmp = last_barrier;
	}
      else
	{
	  /* ftmp is first fix that we can't fit into this pool and there no
	     natural barriers that we could use.  Insert a new barrier in the
	     code somewhere between the previous fix and this one, and
	     arrange to jump around it.  */
	  HOST_WIDE_INT max_address;

	  /* The last item on the list of fixes must be a barrier, so we can
	     never run off the end of the list of fixes without last_barrier
	     being set.  */
	  if (ftmp == NULL)
	    abort ();

	  max_address = minipool_vector_head->max_address;
	  /* Check that there isn't another fix that is in range that we
	     couldn't fit into this pool because the pool was already too
	     large: we need to put the pool before such an instruction.  */
	  if (ftmp->address < max_address)
	    max_address = ftmp->address;

	  last_barrier = create_fix_barrier (last_added_fix, max_address);
	}

      assign_minipool_offsets (last_barrier);

      while (ftmp)
	{
	  if (GET_CODE (ftmp->insn) != BARRIER
	      && ((ftmp->minipool = add_minipool_backward_ref (ftmp))
		  == NULL))
	    break;

	  ftmp = ftmp->next;
	}

      /* Scan over the fixes we have identified for this pool, fixing them up
         and adding the constants to the pool itself.  */
        for (this_fix = fix; this_fix && ftmp != this_fix;
             this_fix = this_fix->next)
          if (GET_CODE (this_fix->insn) != BARRIER
              /* Do nothing for entries present just to force the insertion of
	       a minipool. */
	    && !IS_FORCE_MINIPOOL (this_fix->value))
	  {
	    rtx addr = plus_constant (gen_rtx_LABEL_REF (VOIDmode,
							 minipool_vector_label),
				      this_fix->minipool->offset);
	    *this_fix->loc = gen_rtx_MEM (this_fix->mode, addr);
	  }

      dump_minipool (last_barrier->insn);
      fix = ftmp;
    }

  /* Free the minipool memory.  */
  obstack_free (&minipool_obstack, minipool_startobj);

  avr32_reorg_optimization ();
}


/* Hook for doing some final scanning of instructions. Does nothing yet...*/
void
avr32_final_prescan_insn (rtx insn ATTRIBUTE_UNUSED,
			  rtx * opvec ATTRIBUTE_UNUSED,
			  int noperands ATTRIBUTE_UNUSED)
{
  return;
}


/* Function for changing the condition on the next instruction,
   should be used when emmiting compare instructions and
   the condition of the next instruction needs to change.
*/
int
set_next_insn_cond (rtx cur_insn, rtx new_cond)
{
  rtx next_insn = next_nonnote_insn (cur_insn);
   if ((next_insn != NULL_RTX)
       && (INSN_P (next_insn)))
     {
       if ((GET_CODE (PATTERN (next_insn)) == SET)
           && (GET_CODE (SET_SRC (PATTERN (next_insn))) == IF_THEN_ELSE))
         {
           /* Branch instructions */
           XEXP (SET_SRC (PATTERN (next_insn)), 0) = new_cond;
           /* Force the instruction to be recognized again */
           INSN_CODE (next_insn) = -1;
           return TRUE;
         }
       else if ((GET_CODE (PATTERN (next_insn)) == SET)
                && avr32_comparison_operator (SET_SRC (PATTERN (next_insn)),
                                              GET_MODE (SET_SRC (PATTERN (next_insn)))))
         {
           /* scc with no compare */
           SET_SRC (PATTERN (next_insn)) = new_cond;
           /* Force the instruction to be recognized again */
           INSN_CODE (next_insn) = -1;
           return TRUE;
         }
       else if (GET_CODE (PATTERN (next_insn)) == COND_EXEC)
         {
           if ( GET_CODE (new_cond) == UNSPEC )
             {
               COND_EXEC_TEST (PATTERN (next_insn)) =
                 gen_rtx_UNSPEC (CCmode,
                                 gen_rtvec (2,
                                            XEXP (COND_EXEC_TEST (PATTERN (next_insn)), 0),
                                            XEXP (COND_EXEC_TEST (PATTERN (next_insn)), 1)),
                                 XINT (new_cond, 1));
             }
           else
             {
               PUT_CODE(COND_EXEC_TEST (PATTERN (next_insn)), GET_CODE(new_cond));
             }
         }
     }

  return FALSE;
}


/* Function for obtaining the condition for the next instruction after cur_insn.
*/
rtx
get_next_insn_cond (rtx cur_insn)
{
  rtx next_insn = next_nonnote_insn (cur_insn);
  rtx cond = NULL_RTX;
  if (next_insn != NULL_RTX
      && INSN_P (next_insn))
    {
      if ((GET_CODE (PATTERN (next_insn)) == SET)
          && (GET_CODE (SET_SRC (PATTERN (next_insn))) == IF_THEN_ELSE))
        {
          /* Branch and cond if then else instructions */
          cond = XEXP (SET_SRC (PATTERN (next_insn)), 0);
        }
      else if ((GET_CODE (PATTERN (next_insn)) == SET)
               && avr32_comparison_operator (SET_SRC (PATTERN (next_insn)),
                                             GET_MODE (SET_SRC (PATTERN (next_insn)))))
        {
          /* scc with no compare */
          cond = SET_SRC (PATTERN (next_insn));
        }
      else if (GET_CODE (PATTERN (next_insn)) == COND_EXEC)
        {
          cond = COND_EXEC_TEST (PATTERN (next_insn));
        }
    }
  return cond;
}


/* Check if the next insn is a conditional insn that will emit a compare
   for itself.
*/
rtx
next_insn_emits_cmp (rtx cur_insn)
{
  rtx next_insn = next_nonnote_insn (cur_insn);
  rtx cond = NULL_RTX;
  if (next_insn != NULL_RTX
      && INSN_P (next_insn))
    {
      if ( ((GET_CODE (PATTERN (next_insn)) == SET)
            && (GET_CODE (SET_SRC (PATTERN (next_insn))) == IF_THEN_ELSE)
            && (XEXP (XEXP (SET_SRC (PATTERN (next_insn)), 0),0) != cc0_rtx))
           || GET_CODE (PATTERN (next_insn)) == COND_EXEC )
        return TRUE;
    }
  return FALSE;
}


rtx
avr32_output_cmp (rtx cond, enum machine_mode mode, rtx op0, rtx op1)
{

  rtx new_cond = NULL_RTX;
  rtx ops[2];
  rtx compare_pattern;
  ops[0] = op0;
  ops[1] = op1;

  if ( GET_CODE (op0) == AND )
    compare_pattern = op0;
  else
    compare_pattern = gen_rtx_COMPARE (mode, op0, op1);

  new_cond = is_compare_redundant (compare_pattern, cond);

  if (new_cond != NULL_RTX)
    return new_cond;

  /* Check if we are inserting a bit-load instead of a compare. */
  if ( GET_CODE (op0) == AND )
    {
      ops[0] = XEXP (op0, 0);
      ops[1] = XEXP (op0, 1);
      output_asm_insn ("bld\t%0, %p1", ops);
      return cond;
    }

  /* Insert compare */
  switch (mode)
    {
    case QImode:
      output_asm_insn ("cp.b\t%0, %1", ops);
      break;
    case HImode:
      output_asm_insn ("cp.h\t%0, %1", ops);
      break;
    case SImode:
      output_asm_insn ("cp.w\t%0, %1", ops);
      break;
    case DImode:
      if (GET_CODE (op1) != REG)
	output_asm_insn ("cp.w\t%0, %1\ncpc\t%m0", ops);
      else
	output_asm_insn ("cp.w\t%0, %1\ncpc\t%m0, %m1", ops);
      break;
    default:
      internal_error ("Unknown comparison mode");
      break;
    }

  return cond;
}


int
avr32_load_multiple_operation (rtx op,
			       enum machine_mode mode ATTRIBUTE_UNUSED)
{
  int count = XVECLEN (op, 0);
  unsigned int dest_regno;
  rtx src_addr;
  rtx elt;
  int i = 1, base = 0;

  if (count <= 1 || GET_CODE (XVECEXP (op, 0, 0)) != SET)
    return 0;

  /* Check to see if this might be a write-back.  */
  if (GET_CODE (SET_SRC (elt = XVECEXP (op, 0, 0))) == PLUS)
    {
      i++;
      base = 1;

      /* Now check it more carefully.  */
      if (GET_CODE (SET_DEST (elt)) != REG
	  || GET_CODE (XEXP (SET_SRC (elt), 0)) != REG
	  || GET_CODE (XEXP (SET_SRC (elt), 1)) != CONST_INT
	  || INTVAL (XEXP (SET_SRC (elt), 1)) != (count - 1) * 4)
	return 0;
    }

  /* Perform a quick check so we don't blow up below.  */
  if (count <= 1
      || GET_CODE (XVECEXP (op, 0, i - 1)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, i - 1))) != REG
      || GET_CODE (SET_SRC (XVECEXP (op, 0, i - 1))) != UNSPEC)
    return 0;

  dest_regno = REGNO (SET_DEST (XVECEXP (op, 0, i - 1)));
  src_addr = XEXP (SET_SRC (XVECEXP (op, 0, i - 1)), 0);

  for (; i < count; i++)
    {
      elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != REG
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || GET_CODE (SET_SRC (elt)) != UNSPEC)
	return 0;
    }

  return 1;
}


int
avr32_store_multiple_operation (rtx op,
				enum machine_mode mode ATTRIBUTE_UNUSED)
{
  int count = XVECLEN (op, 0);
  int src_regno;
  rtx dest_addr;
  rtx elt;
  int i = 1;

  if (count <= 1 || GET_CODE (XVECEXP (op, 0, 0)) != SET)
    return 0;

  /* Perform a quick check so we don't blow up below.  */
  if (count <= i
      || GET_CODE (XVECEXP (op, 0, i - 1)) != SET
      || GET_CODE (SET_DEST (XVECEXP (op, 0, i - 1))) != MEM
      || GET_CODE (SET_SRC (XVECEXP (op, 0, i - 1))) != UNSPEC)
    return 0;

  src_regno = REGNO (SET_SRC (XVECEXP (op, 0, i - 1)));
  dest_addr = XEXP (SET_DEST (XVECEXP (op, 0, i - 1)), 0);

  for (; i < count; i++)
    {
      elt = XVECEXP (op, 0, i);

      if (GET_CODE (elt) != SET
	  || GET_CODE (SET_DEST (elt)) != MEM
	  || GET_MODE (SET_DEST (elt)) != SImode
	  || GET_CODE (SET_SRC (elt)) != UNSPEC)
	return 0;
    }

  return 1;
}


int
avr32_valid_macmac_bypass (rtx insn_out, rtx insn_in)
{
  /* Check if they use the same accumulator */
  if (rtx_equal_p
      (SET_DEST (PATTERN (insn_out)), SET_DEST (PATTERN (insn_in))))
    {
      return TRUE;
    }

  return FALSE;
}


int
avr32_valid_mulmac_bypass (rtx insn_out, rtx insn_in)
{
  /*
     Check if the mul instruction produces the accumulator for the mac
     instruction. */
  if (rtx_equal_p
      (SET_DEST (PATTERN (insn_out)), SET_DEST (PATTERN (insn_in))))
    {
      return TRUE;
    }
  return FALSE;
}


int
avr32_store_bypass (rtx insn_out, rtx insn_in)
{
  /* Only valid bypass if the output result is used as an src in the store
     instruction, NOT if used as a pointer or base. */
  if (rtx_equal_p
      (SET_DEST (PATTERN (insn_out)), SET_SRC (PATTERN (insn_in))))
    {
      return TRUE;
    }

  return FALSE;
}


int
avr32_mul_waw_bypass (rtx insn_out, rtx insn_in)
{
  /* Check if the register holding the result from the mul instruction is
     used as a result register in the input instruction. */
  if (rtx_equal_p
      (SET_DEST (PATTERN (insn_out)), SET_DEST (PATTERN (insn_in))))
    {
      return TRUE;
    }

  return FALSE;
}


int
avr32_valid_load_double_bypass (rtx insn_out, rtx insn_in)
{
  /* Check if the first loaded word in insn_out is used in insn_in. */
  rtx dst_reg;
  rtx second_loaded_reg;

  /* If this is a double alu operation then the bypass is not valid */
  if ((get_attr_type (insn_in) == TYPE_ALU
       || get_attr_type (insn_in) == TYPE_ALU2)
      && (GET_MODE_SIZE (GET_MODE (SET_DEST (PATTERN (insn_out)))) > 4))
    return FALSE;

  /* Get the destination register in the load */
  if (!REG_P (SET_DEST (PATTERN (insn_out))))
    return FALSE;

  dst_reg = SET_DEST (PATTERN (insn_out));
  second_loaded_reg = gen_rtx_REG (SImode, REGNO (dst_reg) + 1);

  if (!reg_mentioned_p (second_loaded_reg, PATTERN (insn_in)))
    return TRUE;

  return FALSE;
}


int
avr32_valid_load_quad_bypass (rtx insn_out, rtx insn_in)
{
  /*
     Check if the two first loaded word in insn_out are used in insn_in. */
  rtx dst_reg;
  rtx third_loaded_reg, fourth_loaded_reg;

  /* Get the destination register in the load */
  if (!REG_P (SET_DEST (PATTERN (insn_out))))
    return FALSE;

  dst_reg = SET_DEST (PATTERN (insn_out));
  third_loaded_reg = gen_rtx_REG (SImode, REGNO (dst_reg) + 2);
  fourth_loaded_reg = gen_rtx_REG (SImode, REGNO (dst_reg) + 3);

  if (!reg_mentioned_p (third_loaded_reg, PATTERN (insn_in))
      && !reg_mentioned_p (fourth_loaded_reg, PATTERN (insn_in)))
    {
      return TRUE;
    }

  return FALSE;
}


rtx
avr32_ifcvt_modify_test (ce_if_block_t *ce_info, rtx test )
{
  rtx branch_insn;
  rtx cmp_test;
  rtx compare_op0;
  rtx compare_op1;


  if ( !ce_info
       || test == NULL_RTX
       || !reg_mentioned_p (cc0_rtx, test))
    return test;

  branch_insn = BB_END (ce_info->test_bb);
  cmp_test = PATTERN(prev_nonnote_insn (branch_insn));

  if (GET_CODE(cmp_test) != SET
      || !CC0_P(XEXP(cmp_test, 0)) )
    return cmp_test;

  if ( GET_CODE(SET_SRC(cmp_test)) == COMPARE ){
    compare_op0 = XEXP(SET_SRC(cmp_test), 0);
    compare_op1 = XEXP(SET_SRC(cmp_test), 1);
  } else {
    compare_op0 = SET_SRC(cmp_test);
    compare_op1 = const0_rtx;
  }

  return gen_rtx_fmt_ee (GET_CODE(test), GET_MODE (compare_op0),
                         compare_op0, compare_op1);
}


rtx
avr32_ifcvt_modify_insn (ce_if_block_t *ce_info, rtx pattern, rtx insn,
                         int *num_true_changes)
{
  rtx test = COND_EXEC_TEST(pattern);
  rtx op = COND_EXEC_CODE(pattern);
  rtx cmp_insn;
  rtx cond_exec_insn;
  int inputs_set_outside_ifblock = 1;
  basic_block current_bb = BLOCK_FOR_INSN (insn);
  rtx bb_insn ;
  enum machine_mode mode = GET_MODE (XEXP (op, 0));

  if (CC0_P(XEXP(test, 0)))
    test = avr32_ifcvt_modify_test (ce_info,
                                    test );

  /* We do not support multiple tests. */
  if ( ce_info
       && ce_info->num_multiple_test_blocks > 0 )
    return NULL_RTX;

  pattern = gen_rtx_COND_EXEC (VOIDmode, test, op);

  if ( !reload_completed )
    {
      rtx start;
      int num_insns;
      int max_insns = MAX_CONDITIONAL_EXECUTE;

      if ( !ce_info )
        return op;

      /* Check if the insn is not suitable for conditional
         execution. */
      start_sequence ();
      cond_exec_insn = emit_insn (pattern);
      if ( recog_memoized (cond_exec_insn) < 0
           && can_create_pseudo_p () )
        {
          /* Insn is not suitable for conditional execution, try
             to fix it up by using an extra scratch register or
             by pulling the operation outside the if-then-else
             and then emiting a conditional move inside the if-then-else. */
          end_sequence ();
          if ( GET_CODE (op) != SET
               || !REG_P (SET_DEST (op))
               || GET_CODE (SET_SRC (op)) == IF_THEN_ELSE
               || GET_MODE_SIZE (mode) > UNITS_PER_WORD )
            return NULL_RTX;

          /* Check if any of the input operands to the insn is set inside the
             current block. */
          if ( current_bb->index == ce_info->then_bb->index )
            start = PREV_INSN (BB_HEAD (ce_info->then_bb));
          else
            start = PREV_INSN (BB_HEAD (ce_info->else_bb));


          for ( bb_insn = next_nonnote_insn (start); bb_insn != insn; bb_insn = next_nonnote_insn (bb_insn) )
            {
              rtx set = single_set (bb_insn);

              if ( set && reg_mentioned_p (SET_DEST (set), SET_SRC (op)))
                {
                  inputs_set_outside_ifblock = 0;
                  break;
                }
            }

          cmp_insn = prev_nonnote_insn (BB_END (ce_info->test_bb));


          /* Check if we can insert more insns. */
          num_insns = ( ce_info->num_then_insns +
                        ce_info->num_else_insns +
                        ce_info->num_cond_clobber_insns +
                        ce_info->num_extra_move_insns );

          if ( ce_info->num_else_insns != 0 )
            max_insns *=2;

          if ( num_insns >= max_insns )
            return NULL_RTX;

          /* PK 10 Oct 2012 AVRTC-285: Commenting out this block to stop generating pattern 
           * for <predicable_insn3><mode>_imm_clobber with immediate operand bigger than 8 bits.
          */
          #if (0)
          /* Check if we have an instruction which might be converted to
             conditional form if we give it a scratch register to clobber. */
          {
            rtx clobber_insn;
            rtx scratch_reg = gen_reg_rtx (mode);
            rtx new_pattern = copy_rtx (pattern);
            rtx set_src = SET_SRC (COND_EXEC_CODE (new_pattern));

            rtx clobber = gen_rtx_CLOBBER (mode, scratch_reg);
            rtx vec[2] = { COND_EXEC_CODE (new_pattern), clobber };
            COND_EXEC_CODE (new_pattern) = gen_rtx_PARALLEL (mode, gen_rtvec_v (2, vec));

            start_sequence ();
            clobber_insn = emit_insn (new_pattern);

            if ( recog_memoized (clobber_insn) >= 0
                 && ( ( GET_RTX_LENGTH (GET_CODE (set_src)) == 2
                        && CONST_INT_P (XEXP (set_src, 1))
                        && avr32_const_ok_for_constraint_p (INTVAL (XEXP (set_src, 1)), 'K', "Ks08") )
                      || !ce_info->else_bb
                      || current_bb->index == ce_info->else_bb->index ))
              {
                end_sequence ();
                /* Force the insn to be recognized again. */
                INSN_CODE (insn) = -1;

                /* If this is the first change in this IF-block then
                   signal that we have made a change. */
                if ( ce_info->num_cond_clobber_insns == 0
                     && ce_info->num_extra_move_insns == 0 )
                  *num_true_changes += 1;

                ce_info->num_cond_clobber_insns++;

                if (dump_file)
                  fprintf (dump_file,
                           "\nReplacing INSN %d with an insn using a scratch register for later ifcvt passes...\n",
                           INSN_UID (insn));

                return COND_EXEC_CODE (new_pattern);
              }
            end_sequence ();
          }
          #endif

          if ( inputs_set_outside_ifblock )
            {
              /* Check if the insn before the cmp is an and which used
                 together with the cmp can be optimized into a bld. If
                 so then we should try to put the insn before the and
                 so that we can catch the bld peephole. */
              rtx set;
              rtx insn_before_cmp_insn = prev_nonnote_insn (cmp_insn);
              if (insn_before_cmp_insn
                  && (set = single_set (insn_before_cmp_insn))
                  && GET_CODE (SET_SRC (set)) == AND
                  && one_bit_set_operand (XEXP (SET_SRC (set), 1), SImode)
                  /* Also make sure that the insn does not set any
                     of the input operands to the insn we are pulling out. */
                  && !reg_mentioned_p (SET_DEST (set), SET_SRC (op)) )
                cmp_insn = prev_nonnote_insn (cmp_insn);

              /* We can try to put the operation outside the if-then-else
                 blocks and insert a move. */
              if ( !insn_invalid_p (insn)
                   /* Do not allow conditional insns to be moved outside the
                      if-then-else. */
                   && !reg_mentioned_p (cc0_rtx, insn)
                   /* We cannot move memory loads outside of the if-then-else
                      since the memory access should not be perfomed if the
                      condition is not met. */
                   && !mem_mentioned_p (SET_SRC (op)) )
                {
                  rtx scratch_reg = gen_reg_rtx (mode);
                  rtx op_pattern = copy_rtx (op);
                  rtx new_insn, seq;
                  rtx link, prev_link;
                  op = copy_rtx (op);
                  /* Emit the operation to a temp reg before the compare,
                     and emit a move inside the if-then-else, hoping that the
                     whole if-then-else can be converted to conditional
                     execution. */
                  SET_DEST (op_pattern) = scratch_reg;
                  start_sequence ();
                  new_insn = emit_insn (op_pattern);
                  seq = get_insns();
                  end_sequence ();

                  /* Check again that the insn is valid. For some insns the insn might
                     become invalid if the destination register is changed. Ie. for mulacc
                     operations. */
                  if ( insn_invalid_p (new_insn) )
                    return NULL_RTX;

                  emit_insn_before_setloc (seq, cmp_insn, INSN_LOCATOR (insn));

                  if (dump_file)
                    fprintf (dump_file,
                             "\nMoving INSN %d out of IF-block by adding INSN %d...\n",
                             INSN_UID (insn), INSN_UID (new_insn));

                  ce_info->extra_move_insns[ce_info->num_extra_move_insns] = insn;
                  ce_info->moved_insns[ce_info->num_extra_move_insns] = new_insn;
                  XEXP (op, 1) = scratch_reg;
                  /* Force the insn to be recognized again. */
                  INSN_CODE (insn) = -1;

                  /* Move REG_DEAD notes to the moved insn. */
                  prev_link = NULL_RTX;
                  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
                    {
                      if (REG_NOTE_KIND (link) == REG_DEAD)
                        {
                          /* Add the REG_DEAD note to the new insn. */
                          rtx dead_reg = XEXP (link, 0);
                          REG_NOTES (new_insn) = gen_rtx_EXPR_LIST (REG_DEAD, dead_reg, REG_NOTES (new_insn));
                          /* Remove the REG_DEAD note from the insn we convert to a move. */
                          if ( prev_link )
                            XEXP (prev_link, 1) = XEXP (link, 1);
                          else
                            REG_NOTES (insn) = XEXP (link, 1);
                        }
                      else
                        {
                          prev_link = link;
                        }
                    }
                  /* Add a REG_DEAD note to signal that the scratch register is dead. */
                  REG_NOTES (insn) = gen_rtx_EXPR_LIST (REG_DEAD, scratch_reg, REG_NOTES (insn));

                  /* If this is the first change in this IF-block then
                     signal that we have made a change. */
                  if ( ce_info->num_cond_clobber_insns == 0
                       && ce_info->num_extra_move_insns == 0 )
                    *num_true_changes += 1;

                  ce_info->num_extra_move_insns++;
                  return op;
                }
            }

          /* We failed to fixup the insns, so this if-then-else can not be made
             conditional. Just return NULL_RTX so that the if-then-else conversion
             for this if-then-else will be cancelled. */
          return NULL_RTX;
        }
      end_sequence ();
      return op;
    }

  /* Signal that we have started if conversion after reload, which means
     that it should be safe to split all the predicable clobber insns which
     did not become cond_exec back into a simpler form if possible. */
  cfun->machine->ifcvt_after_reload = 1;

  return pattern;
}


void
avr32_ifcvt_modify_cancel ( ce_if_block_t *ce_info, int *num_true_changes)
{
  int n;

  if ( ce_info->num_extra_move_insns > 0
       && ce_info->num_cond_clobber_insns == 0)
    /* Signal that we did not do any changes after all. */
    *num_true_changes -= 1;

  /* Remove any inserted move insns. */
  for ( n = 0; n < ce_info->num_extra_move_insns; n++ )
    {
      rtx link, prev_link;

      /* Remove REG_DEAD note since we are not needing the scratch register anyway. */
      prev_link = NULL_RTX;
      for (link = REG_NOTES (ce_info->extra_move_insns[n]); link; link = XEXP (link, 1))
        {
          if (REG_NOTE_KIND (link) == REG_DEAD)
            {
              if ( prev_link )
                XEXP (prev_link, 1) = XEXP (link, 1);
              else
                REG_NOTES (ce_info->extra_move_insns[n]) = XEXP (link, 1);
            }
          else
            {
              prev_link = link;
            }
        }

      /* Revert all reg_notes for the moved insn. */
      for (link = REG_NOTES (ce_info->moved_insns[n]); link; link = XEXP (link, 1))
        {
          REG_NOTES (ce_info->extra_move_insns[n]) = gen_rtx_EXPR_LIST (REG_NOTE_KIND (link),
                                                                        XEXP (link, 0),
                                                                        REG_NOTES (ce_info->extra_move_insns[n]));
        }

      /* Remove the moved insn. */
      remove_insn ( ce_info->moved_insns[n] );
    }
}


/* Function returning TRUE if INSN with OPERANDS is a splittable
   conditional immediate clobber insn. We assume that the insn is
   already a conditional immediate clobber insns and do not check
   for that. */
int
avr32_cond_imm_clobber_splittable (rtx insn, rtx operands[])
{
  if ( REGNO (operands[0]) == REGNO (operands[1]) )
    {
      if ( (GET_CODE (SET_SRC (XVECEXP (PATTERN (insn),0,0))) == PLUS
            && !avr32_const_ok_for_constraint_p (INTVAL (operands[2]), 'I', "Is21"))
           || (GET_CODE (SET_SRC (XVECEXP (PATTERN (insn),0,0))) == MINUS
               && !avr32_const_ok_for_constraint_p (INTVAL (operands[2]), 'K', "Ks21")))
        return FALSE;
    }
  else if ( (logical_binary_operator (SET_SRC (XVECEXP (PATTERN (insn),0,0)), VOIDmode)
             || (GET_CODE (SET_SRC (XVECEXP (PATTERN (insn),0,0))) == PLUS
                 && !avr32_const_ok_for_constraint_p (INTVAL (operands[2]), 'I', "Is16"))
             || (GET_CODE (SET_SRC (XVECEXP (PATTERN (insn),0,0))) == MINUS
                 && !avr32_const_ok_for_constraint_p (INTVAL (operands[2]), 'K', "Ks16"))) )
    return FALSE;

  return TRUE;
}


/* Function for getting an integer value from a const_int or const_double
   expression regardless of the HOST_WIDE_INT size. Each target cpu word
   will be put into the val array where the LSW will be stored at the lowest
   address and so forth. Assumes that const_expr is either a const_int or
   const_double. Only valid for modes which have sizes that are a multiple
   of the word size.
*/
void
avr32_get_intval (enum machine_mode mode, rtx const_expr, HOST_WIDE_INT *val)
{
  int words_in_mode = GET_MODE_SIZE (mode)/UNITS_PER_WORD;
  const int words_in_const_int = HOST_BITS_PER_WIDE_INT / BITS_PER_WORD;

  if ( GET_CODE(const_expr) == CONST_DOUBLE ){
    HOST_WIDE_INT hi = CONST_DOUBLE_HIGH(const_expr);
    HOST_WIDE_INT lo = CONST_DOUBLE_LOW(const_expr);
    /* Evaluate hi and lo values of const_double. */
    avr32_get_intval (mode_for_size (HOST_BITS_PER_WIDE_INT, MODE_INT, 0),
                      GEN_INT (lo),
                      &val[0]);
    avr32_get_intval (mode_for_size (HOST_BITS_PER_WIDE_INT, MODE_INT, 0),
                      GEN_INT (hi),
                      &val[words_in_const_int]);
  } else if ( GET_CODE(const_expr) == CONST_INT ){
    HOST_WIDE_INT value = INTVAL(const_expr);
    int word;
    for ( word = 0; (word < words_in_mode) && (word < words_in_const_int); word++ ){
      /* Shift word up to the MSW and shift down again to extract the
         word and sign-extend. */
      int lshift = (words_in_const_int - word - 1) * BITS_PER_WORD;
      int rshift = (words_in_const_int-1) * BITS_PER_WORD;
      val[word] = (value << lshift) >> rshift;
    }

    for ( ; word < words_in_mode; word++ ){
      /* Just put the sign bits in the remaining words. */
      val[word] = value < 0 ? -1 : 0;
    }
  }
}


void
avr32_split_const_expr (enum machine_mode mode, enum machine_mode new_mode,
                        rtx expr, rtx *split_expr)
{
  int i, word;
  int words_in_intval = GET_MODE_SIZE (mode)/UNITS_PER_WORD;
  int words_in_split_values = GET_MODE_SIZE (new_mode)/UNITS_PER_WORD;
  const int words_in_const_int = HOST_BITS_PER_WIDE_INT / BITS_PER_WORD;
  HOST_WIDE_INT *val = alloca (words_in_intval * UNITS_PER_WORD);

  avr32_get_intval (mode, expr, val);

  for ( i=0; i < (words_in_intval/words_in_split_values); i++ )
    {
      HOST_WIDE_INT value_lo = 0, value_hi = 0;
      for ( word = 0; word < words_in_split_values; word++ )
        {
          if ( word >= words_in_const_int )
            value_hi |= ((val[i * words_in_split_values + word] &
                          (((HOST_WIDE_INT)1 << BITS_PER_WORD)-1))
                         << (BITS_PER_WORD * (word - words_in_const_int)));
          else
            value_lo |= ((val[i * words_in_split_values + word] &
                          (((HOST_WIDE_INT)1 << BITS_PER_WORD)-1))
                         << (BITS_PER_WORD * word));
        }
      split_expr[i] = immed_double_const(value_lo, value_hi, new_mode);
    }
}


/* Set up library functions to comply to AVR32 ABI  */
static void
avr32_init_libfuncs (void)
{
  /* Convert gcc run-time function names to AVR32 ABI names */

  /* Double-precision floating-point arithmetic. */
  set_optab_libfunc (neg_optab, DFmode, NULL);

  /* Double-precision comparisons.  */
  set_optab_libfunc (eq_optab, DFmode, "__avr32_f64_cmp_eq");
  set_optab_libfunc (ne_optab, DFmode, NULL);
  set_optab_libfunc (lt_optab, DFmode, "__avr32_f64_cmp_lt");
  set_optab_libfunc (le_optab, DFmode, NULL);
  set_optab_libfunc (ge_optab, DFmode, "__avr32_f64_cmp_ge");
  set_optab_libfunc (gt_optab, DFmode, NULL);

  /* Single-precision floating-point arithmetic. */
  set_optab_libfunc (smul_optab, SFmode, "__avr32_f32_mul");
  set_optab_libfunc (neg_optab, SFmode, NULL);

  /* Single-precision comparisons.  */
  set_optab_libfunc (eq_optab, SFmode, "__avr32_f32_cmp_eq");
  set_optab_libfunc (ne_optab, SFmode, NULL);
  set_optab_libfunc (lt_optab, SFmode, "__avr32_f32_cmp_lt");
  set_optab_libfunc (le_optab, SFmode, NULL);
  set_optab_libfunc (ge_optab, SFmode, "__avr32_f32_cmp_ge");
  set_optab_libfunc (gt_optab, SFmode, NULL);

  /* Floating-point to integer conversions. */
  set_conv_libfunc (sfix_optab, SImode, DFmode, "__avr32_f64_to_s32");
  set_conv_libfunc (ufix_optab, SImode, DFmode, "__avr32_f64_to_u32");
  set_conv_libfunc (sfix_optab, DImode, DFmode, "__avr32_f64_to_s64");
  set_conv_libfunc (ufix_optab, DImode, DFmode, "__avr32_f64_to_u64");
  set_conv_libfunc (sfix_optab, SImode, SFmode, "__avr32_f32_to_s32");
  set_conv_libfunc (ufix_optab, SImode, SFmode, "__avr32_f32_to_u32");
  set_conv_libfunc (sfix_optab, DImode, SFmode, "__avr32_f32_to_s64");
  set_conv_libfunc (ufix_optab, DImode, SFmode, "__avr32_f32_to_u64");

  /* Conversions between floating types.  */
  set_conv_libfunc (trunc_optab, SFmode, DFmode, "__avr32_f64_to_f32");
  set_conv_libfunc (sext_optab, DFmode, SFmode, "__avr32_f32_to_f64");

  /* Integer to floating-point conversions.  Table 8.  */
  set_conv_libfunc (sfloat_optab, DFmode, SImode, "__avr32_s32_to_f64");
  set_conv_libfunc (sfloat_optab, DFmode, DImode, "__avr32_s64_to_f64");
  set_conv_libfunc (sfloat_optab, SFmode, SImode, "__avr32_s32_to_f32");
  set_conv_libfunc (sfloat_optab, SFmode, DImode, "__avr32_s64_to_f32");
  set_conv_libfunc (ufloat_optab, DFmode, SImode, "__avr32_u32_to_f64");
  set_conv_libfunc (ufloat_optab, SFmode, SImode, "__avr32_u32_to_f32");
  /* TODO: Add these to gcc library functions */
  //set_conv_libfunc (ufloat_optab, DFmode, DImode, NULL);
  //set_conv_libfunc (ufloat_optab, SFmode, DImode, NULL);

  /* Long long.  Table 9.  */
  set_optab_libfunc (smul_optab, DImode, "__avr32_mul64");
  set_optab_libfunc (sdiv_optab, DImode, "__avr32_sdiv64");
  set_optab_libfunc (udiv_optab, DImode, "__avr32_udiv64");
  set_optab_libfunc (smod_optab, DImode, "__avr32_smod64");
  set_optab_libfunc (umod_optab, DImode, "__avr32_umod64");
  set_optab_libfunc (ashl_optab, DImode, "__avr32_lsl64");
  set_optab_libfunc (lshr_optab, DImode, "__avr32_lsr64");
  set_optab_libfunc (ashr_optab, DImode, "__avr32_asr64");

  /* Floating point library functions which have fast versions. */
  if ( TARGET_FAST_FLOAT )
    {
      set_optab_libfunc (sdiv_optab, DFmode, "__avr32_f64_div_fast");
      set_optab_libfunc (smul_optab, DFmode, "__avr32_f64_mul_fast");
      set_optab_libfunc (add_optab, DFmode, "__avr32_f64_add_fast");
      set_optab_libfunc (sub_optab, DFmode, "__avr32_f64_sub_fast");
      set_optab_libfunc (add_optab, SFmode, "__avr32_f32_add_fast");
      set_optab_libfunc (sub_optab, SFmode, "__avr32_f32_sub_fast");
      set_optab_libfunc (sdiv_optab, SFmode, "__avr32_f32_div_fast");
    }
  else
    {
      set_optab_libfunc (sdiv_optab, DFmode, "__avr32_f64_div");
      set_optab_libfunc (smul_optab, DFmode, "__avr32_f64_mul");
      set_optab_libfunc (add_optab, DFmode, "__avr32_f64_add");
      set_optab_libfunc (sub_optab, DFmode, "__avr32_f64_sub");
      set_optab_libfunc (add_optab, SFmode, "__avr32_f32_add");
      set_optab_libfunc (sub_optab, SFmode, "__avr32_f32_sub");
      set_optab_libfunc (sdiv_optab, SFmode, "__avr32_f32_div");
    }
}


/* Record a flashvault declaration.  */
static void
flashvault_decl_list_add (unsigned int vector_num, const char *name)
{
  struct flashvault_decl_list *p;

  p = (struct flashvault_decl_list *)
       xmalloc (sizeof (struct flashvault_decl_list));
  p->next = flashvault_decl_list_head;
  p->name = name;
  p->vector_num = vector_num;
  flashvault_decl_list_head = p;
}


static void
avr32_file_end (void)
{
  struct flashvault_decl_list *p;
  unsigned int num_entries = 0;

  /* Check if a list of flashvault declarations exists. */
  if (flashvault_decl_list_head != NULL)
    {
      /* Calculate the number of entries in the table. */
      for (p = flashvault_decl_list_head; p != NULL; p = p->next)
        {
           num_entries++;
        }

      /* Generate the beginning of the flashvault data table. */
      fputs ("\t.global     __fv_table\n"
             "\t.data\n"
             "\t.align 2\n"
             "\t.set .LFVTABLE, . + 0\n"
             "\t.type __fv_table, @object\n", asm_out_file);
      /* Each table entry is 8 bytes. */
      fprintf (asm_out_file, "\t.size __fv_table, %u\n", (num_entries * 8));

      fputs("__fv_table:\n", asm_out_file);

      for (p = flashvault_decl_list_head; p != NULL; p = p->next)
        {
          /* Output table entry. */
          fprintf (asm_out_file, 
                   "\t.align 2\n"
                   "\t.int %u\n", p->vector_num);
          fprintf (asm_out_file, 
                   "\t.align 2\n"
                   "\t.int %s\n", p->name);
        }
    }
}
