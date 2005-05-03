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

#include "lisp.h"
#include "lisp_globals.h"
#include "area.h"
#include "image.h"
#include "gc.h"
#include <errno.h>
#include <unistd.h>
#include <sys/mman.h>
#include <stdio.h>



#ifdef PPC64
#define RELOCATABLE_FULLTAG_MASK \
  ((1<<fulltag_cons)|(1<<fulltag_misc))
#undef COMPRESSED_IMAGE_SUPPORT
#define COMPRESSED_IMAGE_SUPPORT 0
#else
#define RELOCATABLE_FULLTAG_MASK \
  ((1<<fulltag_cons)|(1<<fulltag_nil)|(1<<fulltag_misc))
#undef COMPRESSED_IMAGE_SUPPORT
#define COMPRESSED_IMAGE_SUPPORT 1
#endif

/* 
  Phase out compression support; don't support compressed images on PPC64.
*/

#if COMPRESSED_IMAGE_SUPPORT

unsigned char *compressed_output_ptr;

void
out_byte(unsigned char c)
{
  *compressed_output_ptr++=c;
}

void
out_n_bytes(unsigned char *from, unsigned n)
{
  while (n--) {
    out_byte(*from++);
  }
}


/* nodes & headers are written tag-byte first. */
void
output_reversed_object(LispObj node)
{
  compressed_node n;

  n.u.node = node;
  out_byte(n.u.bytes[3]);
  out_byte(n.u.bytes[2]);
  out_byte(n.u.bytes[1]);
  out_byte(n.u.bytes[0]);
}

unsigned char *
relocate_compressed_node(unsigned char *bufP, LispObj *dest, LispObj bias)
{
  LispObj val;
  unsigned char b, b0;
  signed char c;
  signed short w;
  int i;
  compressed_node n;

  b0 = *bufP++;
  if (comp_p(b0)) {
    if (comp_header_p(b0)) {
      Bug(NULL, "Header not expected in this context!");
    }
    switch (b0) {
    case compressed_nil:
      val = lisp_nil;
      break;
      
    case compressed_undefined:
      val = undefined;
      break;

    case compressed_undefined_function:
      val = nrs_UDF.vcell;
      break;

    case compressed_zero:
      val = 0;
      break;

    case compressed_byte_fixnum:
      b = *bufP++;
      c = b;
      val = (LispObj) (((int)c) << fixnumshift);
      break;

    case compressed_byte_relative_cons:
    case compressed_byte_relative_misc:
      b = *bufP++;
      c = b;
      i = (int) c;
      val = (untag((LispObj)dest) - (i << 3)) | 
        ((b0 == compressed_byte_relative_cons) ? fulltag_cons : fulltag_misc);
      break;

    case compressed_halfword_relative_cons:
    case compressed_halfword_relative_misc:
      b = *bufP++;              /* low byte */
      c = b;
      b = *bufP++;              /* high byte */
      w = (b << 8) | (c & 0xff);
      i = (int) w;
      val = (untag((LispObj)dest) - (i << 3)) |
        ((b0 == compressed_halfword_relative_cons) ? fulltag_cons : fulltag_misc);
      break;

    case compressed_next_cons:
      val = (untag((LispObj)dest))+8+fulltag_cons;
      break;

    case compressed_prev_cons:
      val = (untag((LispObj)dest))-(8-fulltag_cons);
      break;
    }
  } else {                      /* not compressed */
    n.u.bytes[3] = b0;
    n.u.bytes[2] = *bufP++;
    n.u.bytes[1] = *bufP++;
    n.u.bytes[0] = *bufP++;
    val = n.u.node + bias;
  }
  *dest = val;
  return bufP;
}
      
unsigned char *
byte_blt(unsigned char *src, unsigned char *dest, unsigned n)
{
  while (n--) {
    *dest++ = *src++;
  }
  return src;
}

OSErr
relocate_compressed_heap(unsigned char *bufP, 
			 LispObj *start, 
			 LispObj *end,
			 LispObj bias)
{
  unsigned char b0, *work;
  lispsymbol *symp;
  double_float *dfloat;
  int tag, subtag, elements, nbytes;
  LispObj header, *next;
  compressed_node n;

  while (start < end) {
    /* 
    Read a byte from the compression buffer.  It may or may not be a
    compression opcode.  If it isn't, it may or may not be a header.
    If the first byte's a compressed node opcode, uncompress it and
    the next word.  If the first byte's a compressed header opcode,
    there are a few special cases.  Treat the special cases specially,
    otherwise behave as if a (node,imm)header was read.  If the first
    byte's not a compression opcode, read the compressed node. If it's
    a node or immheader, handle those cases, otherwise relocate the
    node and read another.

    If it sounds like this is fencepost-ridden and hard to structure,
    it is.
    */

    b0 = *bufP++;
    tag = fulltag_of(b0);

    if (comp_node_p(b0) || 
        ((! comp_header_p(b0)) && 
         (tag != fulltag_nodeheader) && 
         (tag != fulltag_immheader))) {
      bufP = relocate_compressed_node(bufP-1, start++, bias);
      bufP = relocate_compressed_node(bufP, start++, bias);
    } else {
      /* A few -very- special cases come first. */
      if (b0 == compressed_double_float) {
        dfloat = (double_float *) start;
        dfloat->header = make_header(subtag_double_float, 3);
        dfloat->pad = 0;
        bufP = byte_blt(bufP, (unsigned char *)(&(dfloat->value_high)), 8);
        start = (LispObj *) (dfloat+1);
      } else {
        /* Some other symbol cases are worth treating specially.
           Keywords that have no plists or function bindings need only
           encode the keyword's pname.  We could also note those symbols
           that have no function and/or value bindings and save a byte
           or two that way. */
        if (b0 == compressed_keyword) {
          symp = (lispsymbol *)start;
          symp->header = make_header(subtag_symbol, 5);
          bufP = relocate_compressed_node(bufP, &(symp->pname), bias);
          symp->vcell = ((LispObj)symp) + fulltag_misc;
          symp->fcell = nrs_UDF.vcell;
          symp->package_plist = nrs_KEYWORD_PACKAGE.vcell;
          symp->flags = (18 << fixnumshift);
          start = (LispObj *) (symp+1);
        } else {
          if (! comp_header_p(b0)) {
            n.u.bytes[3] = b0;
            n.u.bytes[2] = *bufP++;
            n.u.bytes[1] = *bufP++;
            n.u.bytes[0] = *bufP++;
          } else {
            n.u.node = 0;
            switch (b0) {
            case compressed_8bit_function:
            case compressed_16bit_function:
              n.u.bytes[3] = subtag_function;
              n.u.bytes[2] = *bufP++;
              if (b0 == compressed_16bit_function) {
                n.u.bytes[1] = *bufP++;
              }
              break;

            case compressed_8bit_simple_vector:
            case compressed_16bit_simple_vector:
              n.u.bytes[3] = subtag_simple_vector;
              n.u.bytes[2] = *bufP++;
              if (b0 == compressed_16bit_simple_vector) {
                n.u.bytes[1] = *bufP++;
              }
              break;

            case compressed_8bit_simple_base_string:
            case compressed_16bit_simple_base_string:
              n.u.bytes[3] = subtag_simple_base_string;
              n.u.bytes[2] = *bufP++;
              if (b0 == compressed_16bit_simple_base_string) {
                n.u.bytes[1] = *bufP++;
              }
              break;
              
            case compressed_8bit_vector:
            case compressed_16bit_vector:
              n.u.bytes[3] = *bufP++;
              n.u.bytes[2] = *bufP++;
              if (b0 == compressed_16bit_vector) {
                n.u.bytes[1] = *bufP++;
              }
              break;

            case compressed_symbol:
              n.u.bytes[3] = subtag_symbol;
              n.u.bytes[2] = 5;
              break;
            }
            tag = fulltag_of(n.u.bytes[3]);
          }
          header = n.u.node;
          *start++ = header;
          subtag = header_subtag(header);
          elements = header_element_count(header);

          if (tag == fulltag_nodeheader) {
            next = (start + (elements|1));
            while (elements--) {
              bufP = relocate_compressed_node(bufP, start++, bias);
            }
            if (((int)start & 7)) {
              *start++ = 0;
            }
          } else {
            if (subtag <= max_32_bit_ivector_subtag) {
              nbytes = elements << 2;
            } else if (subtag <= max_8_bit_ivector_subtag) {
              nbytes = elements;
            } else if (subtag <= max_16_bit_ivector_subtag) {
              nbytes = elements << 1;
            } else if (subtag == subtag_double_float_vector) {
              nbytes = 4 + (elements << 3);
            } else {
              nbytes = (elements+7) >> 3;
            }
            
            work = (unsigned char *) start;
            bufP = byte_blt(bufP, work, nbytes);
            work += nbytes;
            while ((int)work & 7) {
              *work++ = 0;
            }
            next = (LispObj *) work;
          }
          start = next;
        }
      }
    }
  }
  return noErr;
}

void
output_compressed_node(LispObj *where, LispObj low, LispObj high)
{
  LispObj node = *where;
  int diff, tag = fulltag_of(node);

  if (node == lisp_nil) {
    out_byte(compressed_nil);
  } else if (node == undefined) {
    out_byte(compressed_undefined);
  } else if (node == 0) {
    out_byte(compressed_zero);
  } else if (node == nrs_UDF.vcell) {
    out_byte(compressed_undefined_function);
  } else {    
    switch (tag) {
    case fulltag_even_fixnum:
    case fulltag_odd_fixnum:
      diff = ((int)node) >> fixnumshift;
      if (diff == (signed char)diff) {
        out_byte(compressed_byte_fixnum);
        out_byte(diff & 0xff);
        return;
      }
      break;

    case fulltag_cons:
      /* The cons cell in the previous/next doubleword can be encoded in 1 byte */
      if ((untag((LispObj)where)+8+fulltag_cons) == node) {
        out_byte(compressed_next_cons);
        return;
      }
      if ((untag((LispObj)where)-8+fulltag_cons) == node) {
        out_byte(compressed_prev_cons);
        return;
      }
      /* Else fall thru: */

    case fulltag_misc:
      if ((node > low) && (node < high)) {
        diff = (((int)(untag((LispObj)where))) - (int)(untag(node)))>>3;
        if (diff == (signed short) diff) {
          if (diff == (signed char) diff) {
            out_byte((tag == fulltag_cons) ? 
                     compressed_byte_relative_cons : compressed_byte_relative_misc);
            out_byte(diff & 0xff);
          } else {
            out_byte((tag == fulltag_cons) ? 
                     compressed_halfword_relative_cons : compressed_halfword_relative_misc);
            out_byte(diff & 0xff);
            out_byte((diff >> 8) & 0xff);
          }
          return;
        }
      }
      break;
    }
    output_reversed_object(node);
  }
}

void
output_compressed_range(LispObj *start, LispObj *end)
{
  LispObj header, low = (LispObj)start, high = (LispObj)end;
  int tag, subtag, elements, nbytes;
  lispsymbol *symp;
  unsigned char *work;

  while (start < end) {
    header = *start;
    tag = fulltag_of(header);
    if ((tag != fulltag_nodeheader) &&
        (tag != fulltag_immheader)) {
      output_compressed_node(start++, low, high);
      output_compressed_node(start++, low, high);
    } else {
      subtag = header_subtag(header);
      if (subtag == subtag_macptr) {
        subtag = subtag_dead_macptr;
      }
      elements = header_element_count(header);
      if (subtag == subtag_double_float) {
        out_byte(compressed_double_float);
        work = (unsigned char *) (start+2);
        out_n_bytes(work, 8);
        start += 4;
      } else if (subtag == subtag_symbol) {
        symp = (lispsymbol *) start;
        if ((symp->flags == (18 << fixnumshift)) &&
            (symp->package_plist == nrs_KEYWORD_PACKAGE.vcell) &&
            (symp->fcell == nrs_UDF.vcell) &&
            (symp->vcell == (((LispObj)symp) + fulltag_misc))) {
          out_byte(compressed_keyword);
          output_compressed_node(&(symp->pname), low, high);
        } else {
          out_byte(compressed_symbol);
          output_compressed_node(&(symp->pname), low, high);
          output_compressed_node(&(symp->vcell), low, high);
          output_compressed_node(&(symp->fcell), low, high);
          output_compressed_node(&(symp->package_plist), low, high);
          output_compressed_node(&(symp->flags), low, high);
        }
        start = (LispObj *) (symp+1);
      } else {
        /* Neither a double float or a symbol.  The header might be
           something that could be compactly represented.  Write it,
           then write the node/imm object that follows.
           */
        if (elements <= 0xff) {
          switch (subtag) {
          case subtag_simple_vector:
            out_byte(compressed_8bit_simple_vector);
            break;
            
          case subtag_simple_base_string:
            out_byte(compressed_8bit_simple_base_string);
            break;
            
          case subtag_function:
            out_byte(compressed_8bit_function);
            break;

          default:
            out_byte(compressed_8bit_vector);
            out_byte(subtag);
            break;
          }
          out_byte(elements);
        } else if (elements <= 0xffff) {
          switch (subtag) {
          case subtag_simple_vector:
            out_byte(compressed_16bit_simple_vector);
            break;

          case subtag_simple_base_string:
            out_byte(compressed_16bit_simple_base_string);
            break;
            
          case subtag_function:
            out_byte(compressed_16bit_function);
            break;
            

          default:
            out_byte(compressed_16bit_vector);
            out_byte(subtag);
            break;
          }
          out_byte(elements & 0xff);
          out_byte((elements >> 8) & 0xff);
        } else {
          output_reversed_object(header);
        }
        if (subtag == subtag_hash_vector) {
          hash_table_vector_header *hashp = (hash_table_vector_header *) start;
          if (hashp->flags & nhash_track_keys_mask) {
            hashp->flags |= nhash_key_moved_mask;
          }
        }
        start++;
        if (tag == fulltag_nodeheader) {
          while (elements--) {
            output_compressed_node(start++, low, high);
          }
          if (((int)start) & 7) {
            start++;
          }
        } else {
          if (subtag <= max_32_bit_ivector_subtag) {
            nbytes = elements << 2;
          } else if (subtag <= max_8_bit_ivector_subtag) {
            nbytes = elements;
          } else if (subtag <= max_16_bit_ivector_subtag) {
            nbytes = elements << 1;
          } else if (subtag == subtag_double_float_vector) {
            nbytes = 4 + (elements << 3);
          } else {
            nbytes = (elements+7) >> 3;
          }

          work = (unsigned char *) start;
          out_n_bytes(work, nbytes);
          work += nbytes;
          start = (LispObj *) ((((int) work) + 7) & ~7);
        }
      }
    }
  }
}

unsigned
compress_area_data(area *a)
{
  BytePtr start = a->low;
  compressed_output_ptr = (unsigned char *)start;
  output_compressed_range((LispObj *)start, (LispObj *)a->active);
  return (compressed_output_ptr - (unsigned char *)start);
}

#endif

void
relocate_area_contents(area *a, LispObj bias)
{
  LispObj 
    *start = (LispObj *)(a->low), 
    *end = (LispObj *)(a->active),
    low = (LispObj)image_base - bias,
    high = ptr_to_lispobj(active_dynamic_area->active) - bias,
    w0;
  int fulltag;

  while (start < end) {
    w0 = *start;
    fulltag = fulltag_of(w0);
    if (immheader_tag_p(fulltag)) {
      start = (LispObj *)skip_over_ivector((natural)start, w0);
    } else {
      if ((w0 >= low) && (w0 < high) &&
	  ((1<<fulltag) & RELOCATABLE_FULLTAG_MASK)) {
	*start = (w0+bias);
      }
      w0 = *++start;
      fulltag = fulltag_of(w0);
      if ((w0 >= low) && (w0 < high) &&
	  ((1<<fulltag) & RELOCATABLE_FULLTAG_MASK)) {
	*start = (w0+bias);
      }
      ++start;
    }
  }
}
      



off_t
seek_to_next_page(int fd)
{
  off_t pos = lseek(fd, 0, SEEK_CUR);
  pos = align_to_power_of_2(pos, 12);
  return lseek(fd, pos, SEEK_SET);
}
  
/*
  fd is positioned to EOF; header has been allocated by caller.
  If we find a trailer (and that leads us to the header), read
  the header & return true else return false.
*/
Boolean
find_openmcl_image_file_header(int fd, openmcl_image_file_header *header)
{
  openmcl_image_file_trailer trailer;
  int disp;
  off_t pos;
  unsigned version;

  pos = lseek(fd, 0, SEEK_END);
  if (pos < 0) {
    return false;
  }
  pos -= sizeof(trailer);

  if (lseek(fd, pos, SEEK_SET) < 0) {
    return false;
  }
  if (read(fd, &trailer, sizeof(trailer)) != sizeof(trailer)) {
    return false;
  }
  if ((trailer.sig0 != IMAGE_SIG0) ||
      (trailer.sig1 != IMAGE_SIG1) ||
      (trailer.sig2 != IMAGE_SIG2)) {
    return false;
  }
  disp = trailer.delta;
  
  if (disp >= 0) {
    return false;
  }
  if (lseek(fd, disp, SEEK_CUR) < 0) {
    return false;
  }
  if (read(fd, header, sizeof(openmcl_image_file_header)) !=
      sizeof(openmcl_image_file_header)) {
    return false;
  }
  if ((header->sig0 != IMAGE_SIG0) ||
      (header->sig1 != IMAGE_SIG1) ||
      (header->sig2 != IMAGE_SIG2) ||
      (header->sig3 != IMAGE_SIG3)) {
    return false;
  }
  version = header->abi_version;
  if (version) {
    unsigned 
      major = version >> 24, 
      minor = (version >> 16) & 0xff;

    if ((major != OPENMCL_MAJOR_VERSION) ||
	(minor != OPENMCL_MINOR_VERSION)) {
      fprintf(stderr, "Image is for version %d.%d; kernel is for version %d.%d\n", major, minor, OPENMCL_MAJOR_VERSION, OPENMCL_MINOR_VERSION);
      return false;
    }
  } else {
    Bug(NULL, "No version stamp on image file");
  }
  return true;
}

void
load_image_section(int fd, openmcl_image_section_header *sect)
{
  extern area* allocate_dynamic_area(unsigned);
  off_t
    pos = seek_to_next_page(fd);
  int 
    disk_size = sect->disk_size, 
    mem_size = sect->memory_size;
  void *addr;
  area *a;

  switch(sect->code) {
  case AREA_READONLY:
    addr = mmap(pure_space_active,
		disk_size,
		PROT_READ | PROT_EXEC,
		MAP_PRIVATE | MAP_FIXED,
		fd,
		pos);
    if (addr != pure_space_active) {
      return;
    }
    a = new_area(pure_space_active, pure_space_limit, AREA_READONLY);
    pure_space_active += mem_size;
    a->active = pure_space_active;
    sect->area = a;
    break;

  case AREA_STATIC:
    addr = mmap(static_space_active,
		disk_size,
		PROT_READ | PROT_WRITE | PROT_EXEC,
		MAP_PRIVATE | MAP_FIXED,
		fd,
		pos);
    if (addr != static_space_active) {
      return;
    }
    a = new_area(static_space_active, static_space_limit, AREA_STATIC);
    static_space_active += mem_size;
    a->active = static_space_active;
    sect->area = a;
    break;

  case AREA_DYNAMIC:
    a = allocate_dynamic_area(mem_size);
    if (disk_size == mem_size) {
      addr = mmap(a->low,
		  disk_size,
		  PROT_READ | PROT_WRITE | PROT_EXEC,
		  MAP_PRIVATE | MAP_FIXED,
		  fd,
		  pos);
      if (addr != a->low) {
	return;
      }
    } else {
      if (read (fd, a->low+(mem_size-disk_size), disk_size) != disk_size) {
	return;
      }
    }
    sect->area = a;
    
    break;

  default:
    return;
    
  }
  lseek(fd, pos+disk_size, SEEK_SET);
}
    
LispObj
load_openmcl_image(int fd, openmcl_image_file_header *h)
{
  LispObj image_nil = 0;
  area *a;
  if (find_openmcl_image_file_header(fd, h)) {
    int i, nsections = h->nsections;
    openmcl_image_section_header sections[nsections], *sect=sections;
    LispObj bias = image_base - h->actual_image_base;

    if (read (fd, sections, nsections*sizeof(openmcl_image_section_header)) !=
	nsections * sizeof(openmcl_image_section_header)) {
      return 0;
    }
    for (i = 0; i < nsections; i++, sect++) {
      load_image_section(fd, sect);
      a = sect->area;
      if (a == NULL) {
	return 0;
      }
    }

    for (i = 0, sect = sections; i < nsections; i++, sect++) {
      a = sect->area;
      switch(sect->code) {
      case AREA_STATIC:
	nilreg_area = a;
#ifdef PPC64
        image_nil = ptr_to_lispobj(a->low + sizeof(lispsymbol) + fulltag_misc);
#else
	image_nil = (LispObj)(a->low + 8 + 8 + (1024*4) + fulltag_nil);
#endif
	set_nil(image_nil);
	if (bias) {
	  relocate_area_contents(a, bias);
	}
	make_dynamic_heap_executable(a->low, a->active);
      case AREA_READONLY:
	add_area_holding_area_lock(a);
	break;
      }
    }
    for (i = 0, sect = sections; i < nsections; i++, sect++) {
      a = sect->area;
      switch(sect->code) {
      case AREA_DYNAMIC:
	lisp_global(HEAP_START) = ptr_to_lispobj(a->low);
	lisp_global(HEAP_END) = ptr_to_lispobj(a->high);
#if COMPRESSED_IMAGE_SUPPORT
	if (sect->memory_size != sect->disk_size) {
	  relocate_compressed_heap((unsigned char *)(a->low+(sect->memory_size-sect->disk_size)),
				   (LispObj*)a->low,
				   (LispObj*)a->active,
				   bias);
	} else if (bias) {
	  relocate_area_contents(a, bias);
	}
#else
        if (bias) {
          relocate_area_contents(a, bias);
        }
#endif
	resize_dynamic_heap(a->active, lisp_heap_gc_threshold);
	xMakeDataExecutable(a->low, a->active - a->low);
	break;
      }
    }
  }
  return image_nil;
}
      
void
prepare_to_write_dynamic_space()
{
  area *a = active_dynamic_area;
  LispObj 
    *start = (LispObj *)(a->low),
    *end = (LispObj *) (a->active),
    x1;
  int tag, subtag, element_count;

  while (start < end) {
    x1 = *start;
    tag = fulltag_of(x1);
    if (immheader_tag_p(tag)) {
      subtag = header_subtag(x1);
      if (subtag == subtag_macptr) {
	*start = make_header(subtag_dead_macptr,header_element_count(x1));
      }
      start = (LispObj *)skip_over_ivector((natural)start, x1);
    } else if (nodeheader_tag_p(tag)) {
      element_count = header_element_count(x1) | 1;
      start += (element_count+1);
    } else {
      start += 2;
    }
  }
}

OSErr
write_area_pages(int fd, area *a)
{
  int total = a->active - a->low, count, n, done=0;
  char buffer[32768];

  while (total) {
    if (total > 32768) {
      count = 32768;
    } else {
      count = total;
    }
    bcopy(a->low+done,buffer,count);
    n = write(fd, buffer, count);
    if (n < 0) {
      return n;
    }
    total -= n;
    done += n;
  }
  return 0;
}
  
    
OSErr
save_application(unsigned fd, Boolean compress)
{
  openmcl_image_file_header fh;
  openmcl_image_section_header sections[3];
  openmcl_image_file_trailer trailer;
  area *areas[3], *a;
  int i;
  off_t header_pos, eof_pos;;

  areas[2] = active_dynamic_area;
  for (a = active_dynamic_area->succ; a != all_areas; a = a->succ) {
    if (a->code == AREA_STATIC) {
      areas[1] = a;
    } else if (a->code == AREA_READONLY) {
      areas[0] = a;
    }
  }
  for (i = 0; i < 3; i++) {
    a = areas[i];
    sections[i].code = a->code;
    sections[i].area = NULL;
    sections[i].memory_size  = a->active - a->low;

#if COMPRESSED_IMAGE_SUPPORT
    if ((a->code == AREA_DYNAMIC) && compress) {
      sections[i].disk_size = compress_area_data(a);
    } else {
#endif
      sections[i].disk_size = sections[i].memory_size;
#if COMPRESSED_IMAGE_SUPPORT
    }
#endif
  }
  fh.sig0 = IMAGE_SIG0;
  fh.sig1 = IMAGE_SIG1;
  fh.sig2 = IMAGE_SIG2;
  fh.sig3 = IMAGE_SIG3;
  fh.timestamp = time(NULL);
  fh.canonical_image_base = IMAGE_BASE_ADDRESS;
  fh.actual_image_base = image_base;
  fh.nsections = 3;
  fh.abi_version=(OPENMCL_MAJOR_VERSION<<24)|(OPENMCL_MINOR_VERSION<<16);
  for (i = 0; i < sizeof(fh.pad)/sizeof(fh.pad[0]); i++) {
    fh.pad[i] = 0;
  }
  header_pos = seek_to_next_page(fd);

  if (lseek (fd, header_pos, SEEK_SET) < 0) {
    return errno;
  }
  if (write(fd, &fh, sizeof(fh)) != sizeof(fh)) {
    return errno;
  }
  if (write(fd, &sections, sizeof(sections)) != sizeof(sections)) {
    return errno;
  }

  /*
    Coerce macptrs to dead_macptrs.
  */
  
  prepare_to_write_dynamic_space(active_dynamic_area);

  /*
    lisp_global(GC_NUM) and lisp_global(FWDNUM) are persistent.
    Nothing else is even meaningful at this point.
  */
  for (i = -1019; i < 0; i++) {
    switch (i) {
    case FWDNUM:
    case GC_NUM:
      break;
    default:
      lisp_global(i) = 0;
    }
  }

  for (i = 0; i < 3; i++) {
    int n;
    a = areas[i];
    seek_to_next_page(fd);
    n = sections[i].disk_size;
    if (a->code == AREA_READONLY) {
      /* 
	 Darwin seems to have problems writing the readonly area for
	 some reason.  It seems to work better to write a page at a
	 time.
      */
      if (write_area_pages(fd, a) != 0) {
	return errno;
      }
    } else {
      if (write(fd, a->low, n) != n) {
	return errno;
      }
    }
  }
  trailer.sig0 = IMAGE_SIG0;
  trailer.sig1 = IMAGE_SIG1;
  trailer.sig2 = IMAGE_SIG2;
  eof_pos = lseek(fd, 0, SEEK_CUR) + sizeof(trailer);
  trailer.delta = (int) (header_pos-eof_pos);
  if (write(fd, &trailer, sizeof(trailer)) == sizeof(trailer)) {
    fsync(fd);
    close(fd);
    return 0;
  } 
  i = errno;
  close(fd);
  return i;
}
      



