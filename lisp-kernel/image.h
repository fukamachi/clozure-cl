/*
   Copyright (C) 2002 Clozure Associates
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


#define IMAGE_SIG0 (('O'<<24) | ('p'<<16) | ('e'<<8) | 'n')
#define IMAGE_SIG1 (('M'<<24) | ('C'<<16) | ('L'<<8) | 'I')
#define IMAGE_SIG2 (('m'<<24) | ('a'<<16) | ('g'<<8) | 'e')
#define IMAGE_SIG3 (('F'<<24) | ('i'<<16) | ('l'<<8) | 'e')

/* 
   An image file contains a header (which describes the type, size,
   and nominal memory address of one or more sections) and data for
   each section; each section's data is page-aligned within the image
   file, so its disk address is implicit.  The header must reside
   entirely within a page; the first section's data starts on the page
   after the image header, and subsequent sections start on the pages
   after the page which contains the last byte of their predecessor's
   data.

   The image header's position relative to the start of the file is
   arbitrary.  The image header's position relative to the end of the
   file is indicated by the last word in the file (which is preceded
   by the first three signature words above.)  The last word contains
   the distance from the end-of-file to the start of the header.

   As long as these alignment constraints are met, the image file can
   have arbitrary data (or executable programs, or shell scripts)
   prepended to it.  This is supposed to simplify distribution.
*/

typedef struct {
  area_code code;
  area *area;
  unsigned memory_size;
  unsigned disk_size;
} openmcl_image_section_header;

typedef struct {
  unsigned sig0, sig1, sig2, sig3;
  unsigned timestamp;
  unsigned canonical_image_base; /* IMAGE_BASE_ADDRESS */
  unsigned actual_image_base;	/* Hopefully the same */
  unsigned nsections;
  unsigned abi_version;
  unsigned pad[7];
} openmcl_image_file_header;

typedef struct {
  unsigned sig0, sig1, sig2;
  int delta;
} openmcl_image_file_trailer;

LispObj
load_openmcl_image(int, openmcl_image_file_header*);



/* Compression bytecodes.  Some of these can appear whenever the
   loader is expecting a header or a node; some can only appear
   in one context or another.
*/

#define comp_val  fulltag_nil
#define comp_header_val (comp_val | (1 << ntagbits))
#define comp_mask fulltagmask
#define comp_header_mask (comp_mask | (1 << ntagbits))

#define comp_node(n)  (comp_val | ((n) << (ntagbits+1)))
#define comp_header(n) (comp_header_val | ((n) << (ntagbits+1)))

/* is x a compression bytecode ? */
#define comp_p(x)  (((x) & comp_mask) == comp_val)

/* is x a compressed node bytecode ? */
#define comp_node_p(x) (((x) & comp_header_mask) == comp_val)

/* is x a compressed header bytecode ? */
#define comp_header_p(x) (((x) & comp_header_mask) == comp_header_val)



/* reference to NIL */
#define compressed_nil                       comp_node(0)

/* (untagged) signed byte follows */ 
#define compressed_byte_fixnum               comp_node(1)

 /* 8-bit (doubleword) relative heap reference to misc-tagged object */
#define compressed_byte_relative_misc        comp_node(2)

/* 16-bit (doubleword) relative heap reference to misc-tagged object */
#define compressed_halfword_relative_misc    comp_node(3)

/* reference to #<undefined> */
#define compressed_undefined                 comp_node(4)

/* reference to undefined_function object */
#define compressed_undefined_function        comp_node(5)

/* reference to 0 */
#define compressed_zero                      comp_node(6)

/* reference to cons cell at previous doubleword */
#define compressed_prev_cons                 comp_node(7)

/* reference to cons cell at next doubleword */
#define compressed_next_cons                 comp_node(8)

/* 8-bit (doubleword) relative heap reference to cons cell */
#define compressed_byte_relative_cons        comp_node(9)

/* 16-bit (doubleword) relative heap reference to cons cell */
#define compressed_halfword_relative_cons        comp_node(10)



/* "simple" keyword: no function def or plist.  pname follows */
#define compressed_keyword                   comp_header(0)

/* function: element count fits in 1 byte */
#define compressed_8bit_function             comp_header(1)

/* function: element count fits in 2 bytes */
#define compressed_16bit_function            comp_header(2)

/* simple vector (array t (*)): 1 byte element count */
#define compressed_8bit_simple_vector        comp_header(3)

/* simple vector: 2 byte element count */
#define compressed_16bit_simple_vector       comp_header(4)

/* simple base string: 1 byte element count */
#define compressed_8bit_simple_base_string   comp_header(5)

/* simple base string: 2 byte element count */
#define compressed_16bit_simple_base_string  comp_header(6)

/* double float: 8 bytes of data follow.  Note alignment/padding. */
#define compressed_double_float              comp_header(7)
 
/* "random" miscobj header: subtag, 1 byte element count follow */
#define compressed_8bit_vector               comp_header(8)

/* "random" miscobj header: subtag, 2 byte element count follow */
#define compressed_16bit_vector              comp_header(9)

/* symbol, all cells follow (should also encode unbound/unfbound symbols) */
#define compressed_symbol                    comp_header(10)


typedef struct compressed_node {
  union {
    unsigned char bytes[4];
    LispObj node;
  } u;
} compressed_node;

