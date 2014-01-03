/*
   Prototypes for exported functions defined in avr32.c
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


#ifndef AVR32_PROTOS_H
#define AVR32_PROTOS_H

extern const int swap_reg[];

extern int avr32_valid_macmac_bypass (rtx, rtx);
extern int avr32_valid_mulmac_bypass (rtx, rtx);

extern int avr32_decode_lcomm_symbol_offset (rtx, int *);
extern void avr32_encode_lcomm_symbol_offset (tree, char *, int);

extern const char *avr32_strip_name_encoding (const char *);

extern rtx avr32_get_note_reg_equiv (rtx insn);

extern int avr32_use_return_insn (int iscond);

extern void avr32_make_reglist16 (int reglist16_vect, char *reglist16_string);

extern void avr32_make_reglist8 (int reglist8_vect, char *reglist8_string);
extern void avr32_make_fp_reglist_w (int reglist_mask, char *reglist_string);
extern void avr32_make_fp_reglist_d (int reglist_mask, char *reglist_string);

extern void avr32_output_return_instruction (int single_ret_inst,
					     int iscond, rtx cond,
					     rtx r12_imm);
extern void avr32_expand_prologue (void);
extern void avr32_set_return_address (rtx source, rtx scratch);

extern int avr32_hard_regno_mode_ok (int regno, enum machine_mode mode);
extern int avr32_extra_constraint_s (rtx value, const int strict);
extern int avr32_eh_return_data_regno (const int n);
extern int avr32_initial_elimination_offset (const int from, const int to);
extern rtx avr32_function_arg (CUMULATIVE_ARGS * cum, enum machine_mode mode,
			       tree type, int named);
extern void avr32_init_cumulative_args (CUMULATIVE_ARGS * cum, tree fntype,
					rtx libname, tree fndecl);
extern void avr32_function_arg_advance (CUMULATIVE_ARGS * cum,
					enum machine_mode mode,
					tree type, int named);
#ifdef ARGS_SIZE_RTX
/* expr.h defines ARGS_SIZE_RTX and `enum direction'.  */
extern enum direction avr32_function_arg_padding (enum machine_mode mode,
						  tree type);
#endif /* ARGS_SIZE_RTX */
extern rtx avr32_function_value (tree valtype, tree func, bool outgoing);
extern rtx avr32_libcall_value (enum machine_mode mode);
extern int avr32_sched_use_dfa_pipeline_interface (void);
extern bool avr32_return_in_memory (tree type, tree fntype);
extern void avr32_regs_to_save (char *operand);
extern void avr32_target_asm_function_prologue (FILE * file,
						HOST_WIDE_INT size);
extern void avr32_target_asm_function_epilogue (FILE * file,
						HOST_WIDE_INT size);
extern void avr32_trampoline_template (FILE * file);
extern void avr32_initialize_trampoline (rtx addr, rtx fnaddr,
					 rtx static_chain);
extern int avr32_legitimate_address (enum machine_mode mode, rtx x,
				     int strict);
extern int avr32_legitimate_constant_p (rtx x);

extern int avr32_legitimate_pic_operand_p (rtx x);

extern rtx avr32_find_symbol (rtx x);
extern void avr32_select_section (rtx exp, int reloc, int align);
extern void avr32_encode_section_info (tree decl, rtx rtl, int first);
extern void avr32_asm_file_end (FILE * stream);
extern void avr32_asm_output_ascii (FILE * stream, char *ptr, int len);
extern void avr32_asm_output_common (FILE * stream, const char *name,
				     int size, int rounded);
extern void avr32_asm_output_label (FILE * stream, const char *name);
extern void avr32_asm_declare_object_name (FILE * stream, char *name,
					   tree decl);
extern void avr32_asm_globalize_label (FILE * stream, const char *name);
extern void avr32_asm_weaken_label (FILE * stream, const char *name);
extern void avr32_asm_output_external (FILE * stream, tree decl,
				       const char *name);
extern void avr32_asm_output_external_libcall (FILE * stream, rtx symref);
extern void avr32_asm_output_labelref (FILE * stream, const char *name);
extern void avr32_notice_update_cc (rtx exp, rtx insn);
extern void avr32_print_operand (FILE * stream, rtx x, int code);
extern void avr32_print_operand_address (FILE * stream, rtx x);

extern int avr32_symbol (rtx x);

extern void avr32_select_rtx_section (enum machine_mode mode, rtx x,
				      unsigned HOST_WIDE_INT align);

extern int avr32_load_multiple_operation (rtx op, enum machine_mode mode);
extern int avr32_store_multiple_operation (rtx op, enum machine_mode mode);

extern int avr32_const_ok_for_constraint_p (HOST_WIDE_INT value, char c,
					    const char *str);

extern bool avr32_cannot_force_const_mem (rtx x);

extern void avr32_init_builtins (void);

extern rtx avr32_expand_builtin (tree exp, rtx target, rtx subtarget,
				 enum machine_mode mode, int ignore);

extern bool avr32_must_pass_in_stack (enum machine_mode mode, tree type);

extern bool avr32_strict_argument_naming (CUMULATIVE_ARGS * ca);

extern bool avr32_pass_by_reference (CUMULATIVE_ARGS * cum,
				     enum machine_mode mode,
				     tree type, bool named);

extern rtx avr32_gen_load_multiple (rtx * regs, int count, rtx from,
				    int write_back, int in_struct_p,
				    int scalar_p);
extern rtx avr32_gen_store_multiple (rtx * regs, int count, rtx to,
				     int in_struct_p, int scalar_p);
extern int avr32_gen_movmemsi (rtx * operands);

extern int avr32_rnd_operands (rtx add, rtx shift);
extern int avr32_adjust_insn_length (rtx insn, int length);

extern int symbol_mentioned_p (rtx x);
extern int label_mentioned_p (rtx x);
extern rtx legitimize_pic_address (rtx orig, enum machine_mode mode, rtx reg);
extern int avr32_address_register_rtx_p (rtx x, int strict_p);
extern int avr32_legitimate_index_p (enum machine_mode mode, rtx index,
				     int strict_p);

extern int avr32_const_double_immediate (rtx value);
extern void avr32_init_expanders (void);
extern rtx avr32_return_addr (int count, rtx frame);
extern bool avr32_got_mentioned_p (rtx addr);

extern void avr32_final_prescan_insn (rtx insn, rtx * opvec, int noperands);

extern int avr32_expand_movcc (enum machine_mode mode, rtx operands[]);
extern int avr32_expand_addcc (enum machine_mode mode, rtx operands[]);
#ifdef RTX_CODE
extern int avr32_expand_scc (RTX_CODE cond, rtx * operands);
#endif

extern int avr32_store_bypass (rtx insn_out, rtx insn_in);
extern int avr32_mul_waw_bypass (rtx insn_out, rtx insn_in);
extern int avr32_valid_load_double_bypass (rtx insn_out, rtx insn_in);
extern int avr32_valid_load_quad_bypass (rtx insn_out, rtx insn_in);
extern rtx avr32_output_cmp (rtx cond, enum machine_mode mode,
			     rtx op0, rtx op1);

rtx get_next_insn_cond (rtx cur_insn);
int set_next_insn_cond (rtx cur_insn, rtx cond);
rtx next_insn_emits_cmp (rtx cur_insn);
void avr32_override_options (void);
void avr32_load_pic_register (void);
#ifdef GCC_BASIC_BLOCK_H
rtx avr32_ifcvt_modify_insn (ce_if_block_t *ce_info, rtx pattern, rtx insn, 
                             int *num_true_changes);
rtx avr32_ifcvt_modify_test (ce_if_block_t *ce_info, rtx test );
void avr32_ifcvt_modify_cancel ( ce_if_block_t *ce_info, int *num_true_changes);
#endif
void avr32_optimization_options (int level, int size);
int avr32_const_ok_for_move (HOST_WIDE_INT c);

void avr32_split_const_expr (enum machine_mode mode,
                             enum machine_mode new_mode,
                             rtx expr, 
                             rtx *split_expr);
void avr32_get_intval (enum machine_mode mode,
                       rtx const_expr, 
                       HOST_WIDE_INT *val);

int avr32_cond_imm_clobber_splittable (rtx insn, 
                                       rtx operands[]);

bool avr32_flashvault_call(tree decl);
extern void avr32_emit_swdivsf (rtx, rtx, rtx);

#endif /* AVR32_PROTOS_H */
