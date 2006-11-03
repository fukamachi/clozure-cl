/*
   Copyright (C) 1994-2001 Digitool, Inc
   This file is part of OpenMCL.  

   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
   License , known as the LLGPL and distributed with OpenMCL as the
   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
   which is distributed with OpenMCL as the file "LGPL".  Where these
   conflict, the preamble takes precedence.  

   OpenMCL is referenced in the preamble as the "LIBRARY."

   The LLGPL is also available online at
   http://opensource.franz.com/preamble.html
*/

#ifndef __ERRORS_X
#define __ERRORS_X 1

/*
10/22/96 bill error_too_many_values
--- 4.0 ---
05/12/96  gb  conditionalize on __ERRORS_X to avoid conflict with <errors.h>
--- 3.9 ---
04/10/96  gb  error_memory_full
04/09/96  gb  error_excised_function_call
03/01/96  gb  FPU exceptions
01/22/96  gb  add/rename error_alloc_failed, error_stack_overflow
12/13/95  gb  add error_alloc_fail, error_throw_tag_missing.
11/09/95  gb  in synch with %type-error-types%.
*/

#define error_reg_regnum 0
#define error_udf 1
#define error_udf_call 2
#define error_throw_tag_missing 3
#define error_alloc_failed 4
#define error_stack_overflow 5
#define error_excised_function_call 6
#define error_too_many_values 7
#define error_cant_call 17

#define error_type_error 128

typedef enum {
  error_object_not_array = error_type_error,
  error_object_not_bignum,
  error_object_not_fixnum,
  error_object_not_character,
  error_object_not_integer,
  error_object_not_list,
  error_object_not_number,
  error_object_not_sequence,
  error_object_not_simple_string,
  error_object_not_simple_vector,
  error_object_not_string,
  error_object_not_symbol,
  error_object_not_macptr,
  error_object_not_real,
  error_object_not_cons,
  error_object_not_unsigned_byte,
  error_object_not_radix,
  error_object_not_float,
  error_object_not_rational,
  error_object_not_ratio,
  error_object_not_short_float,
  error_object_not_double_float,
  error_object_not_complex,
  error_object_not_vector,
  error_object_not_simple_base_string,
  error_object_not_function,
  error_object_not_unsigned_byte_16,
  error_object_not_unsigned_byte_8,
  error_object_not_unsigned_byte_32,
  error_object_not_signed_byte_32,
  error_object_not_signed_byte_16,
  error_object_not_signed_byte_8,	
  error_object_not_base_character,
  error_object_not_bit,
  error_object_not_unsigned_byte_24,
  error_object_not_u64,
  error_object_not_s64,
  error_object_not_unsigned_byte_56,
  error_object_not_simple_array_double_float_2d,
  error_object_not_simple_array_single_float_2d,
  error_object_not_mod_char_code_limit,
  error_object_not_array_2d,
  error_object_not_array_3d,
  error_object_not_array_t,
  error_object_not_array_bit,
  error_object_not_array_s8,
  error_object_not_array_u8,
  error_object_not_array_s16,
  error_object_not_array_u16,
  error_object_not_array_s32,
  error_object_not_array_u32,
  error_object_not_array_s64,
  error_object_not_array_u64,
  error_object_not_array_fixnum,
  error_object_not_array_single_float,
  error_object_not_array_double_float,
  error_object_not_array_char,
  error_object_not_array_t_2d,
  error_object_not_array_bit_2d,
  error_object_not_array_s8_2d,
  error_object_not_array_u8_2d,
  error_object_not_array_s16_2d,
  error_object_not_array_u16_2d,
  error_object_not_array_s32_2d,
  error_object_not_array_u32_2d,
  error_object_not_array_s64_2d,
  error_object_not_array_u64_2d,
  error_object_not_array_fixnum_2d,
  error_object_not_array_single_float_2d,
  error_object_not_array_double_float_2d,
  error_object_not_array_char_2d,
  error_object_not_simple_array_t_2d,
  error_object_not_simple_array_bit_2d,
  error_object_not_simple_array_s8_2d,
  error_object_not_simple_array_u8_2d,
  error_object_not_simple_array_s16_2d,
  error_object_not_simple_array_u16_2d,
  error_object_not_simple_array_s32_2d,
  error_object_not_simple_array_u32_2d,
  error_object_not_simple_array_s64_2d,
  error_object_not_simple_array_u64_2d,
  error_object_not_simple_array_fixnum_2d,
  error_object_not_simple_array_char_2d
} type_error;

#define error_FPU_exception_double 1024
#define error_FPU_exception_short 1025

#define error_memory_full 2048



#endif /* __ERRORS_X */
