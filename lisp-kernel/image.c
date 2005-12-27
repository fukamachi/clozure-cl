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
#else
#define RELOCATABLE_FULLTAG_MASK \
  ((1<<fulltag_cons)|(1<<fulltag_nil)|(1<<fulltag_misc))
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
  pos = align_to_power_of_2(pos, log2_page_size);
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
  version = (header->abi_version) & 0xffff;
  if (version < ABI_VERSION_MIN) {
    fprintf(stderr, "Heap image is too old for this kernel.\n");
    return false;
  }
  if (version > ABI_VERSION_MAX) {
    fprintf(stderr, "Heap image is too new for this kernel.\n");
    return false;
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
    mem_size = sect->memory_size;
  void *addr;
  area *a;

  switch(sect->code) {
  case AREA_READONLY:
    addr = mmap(pure_space_active,
		align_to_power_of_2(mem_size,log2_page_size),
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
		align_to_power_of_2(mem_size,log2_page_size),
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
    addr = mmap(a->low,
		align_to_power_of_2(mem_size,log2_page_size),
                PROT_READ | PROT_WRITE | PROT_EXEC,
                MAP_PRIVATE | MAP_FIXED,
                fd,
                pos);
    if (addr != a->low) {
      return;
    }

    sect->area = a;
    
    break;

  default:
    return;
    
  }
  lseek(fd, pos+mem_size, SEEK_SET);
}
    
LispObj
load_openmcl_image(int fd, openmcl_image_file_header *h)
{
  LispObj image_nil = 0;
  area *a;
  if (find_openmcl_image_file_header(fd, h)) {
    int i, nsections = h->nsections;
    openmcl_image_section_header sections[nsections], *sect=sections;
    LispObj bias = image_base - ACTUAL_IMAGE_BASE(h);
#ifdef PPC64
    signed_natural section_data_delta = 
      ((signed_natural)(h->section_data_offset_high) << 32L) | h->section_data_offset_low;
#endif

    if (read (fd, sections, nsections*sizeof(openmcl_image_section_header)) !=
	nsections * sizeof(openmcl_image_section_header)) {
      return 0;
    }
#ifdef PPC64
    lseek(fd, section_data_delta, SEEK_CUR);
#endif
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
        image_nil = ptr_to_lispobj(a->low + (1024*4) + sizeof(lispsymbol) + fulltag_misc);
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
        if (bias) {
          relocate_area_contents(a, bias);
        }
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
  natural total = a->active - a->low, count, done=0;
  signed_natural n;
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
  

int
write_file_and_section_headers(int fd, 
                               openmcl_image_file_header *file_header,
                               openmcl_image_section_header* section_headers,
                               int nsections,
                               off_t *header_pos)
{
  *header_pos = seek_to_next_page(fd);

  if (lseek (fd, *header_pos, SEEK_SET) < 0) {
    return errno;
  }
  if (write(fd, file_header, sizeof(*file_header)) != sizeof(*file_header)) {
    return errno;
  }
  if (write(fd, section_headers, sizeof(section_headers[0])*nsections)
      != (sizeof(section_headers[0]) *nsections)) {
    return errno;
  }
  return 0;
}
  
  
OSErr
save_application(unsigned fd)
{
  openmcl_image_file_header fh;
  openmcl_image_section_header sections[3];
  openmcl_image_file_trailer trailer;
  area *areas[3], *a;
  int i, err;
  off_t header_pos, eof_pos;
#ifdef PPC64
  off_t image_data_pos;
  signed_natural section_data_delta;
#endif

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
  }
  fh.sig0 = IMAGE_SIG0;
  fh.sig1 = IMAGE_SIG1;
  fh.sig2 = IMAGE_SIG2;
  fh.sig3 = IMAGE_SIG3;
  fh.timestamp = time(NULL);
  CANONICAL_IMAGE_BASE(&fh) = IMAGE_BASE_ADDRESS;
  ACTUAL_IMAGE_BASE(&fh) = image_base;
  fh.nsections = 3;
  fh.abi_version=ABI_VERSION_CURRENT;
#if WORD_SIZE == 64
  fh.section_data_offset_high = 0;
  fh.section_data_offset_low = 0;
#else
  for (i = 0; i < sizeof(fh.pad)/sizeof(fh.pad[0]); i++) {
    fh.pad[i] = 0;
  }
#endif
#if WORD_SIZE == 64
  fh.flags = 1;
#endif

#ifdef PPC64
  image_data_pos = seek_to_next_page(fd);
#else
  err = write_file_and_section_headers(fd, &fh, sections, 3, &header_pos);
  if (err) {
    return err;
  }
#endif

  /*
    Coerce macptrs to dead_macptrs.
  */
  
  prepare_to_write_dynamic_space(active_dynamic_area);

  /*
    lisp_global(GC_NUM) and lisp_global(FWDNUM) are persistent.
    Nothing else is even meaningful at this point.
  */
  for (i = MIN_KERNEL_GLOBAL; i < 0; i++) {
    switch (i) {
    case FWDNUM:
    case GC_NUM:
      break;
    default:
      lisp_global(i) = 0;
    }
  }

  for (i = 0; i < 3; i++) {
    natural n;
    a = areas[i];
    seek_to_next_page(fd);
    n = sections[i].memory_size;
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

#ifdef PPC64
  seek_to_next_page(fd);
  section_data_delta = -((lseek(fd,0,SEEK_CUR)+sizeof(fh)+sizeof(sections)) -
                         image_data_pos);
  fh.section_data_offset_high = (int)(section_data_delta>>32L);
  fh.section_data_offset_low = (unsigned)section_data_delta;
  err =  write_file_and_section_headers(fd, &fh, sections, 3, &header_pos);
  if (err) {
    return err;
  }  
#endif

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
      



