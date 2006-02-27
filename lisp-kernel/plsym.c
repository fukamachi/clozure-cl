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

#include "lispdcmd.h"

void
describe_symbol(LispObj sym)
{
  lispsymbol *rawsym = (lispsymbol *)ptr_from_lispobj(untag(sym));
  LispObj function = rawsym->fcell;

  Dprintf("Symbol %s at #x%08X", print_lisp_object(sym), sym);
  Dprintf("  value    : %s", print_lisp_object(rawsym->vcell));
  if (function != nrs_UDF.vcell) {
    Dprintf("  function : %s", print_lisp_object(function));
  }
}
  
  

/*
  Walk the heap until we find a symbol
  whose pname matches "name".  Return the 
  tagged symbol or NULL.
*/

LispObj
find_symbol_in_range(LispObj *start, LispObj *end, char *name)
{
  LispObj header, tag;
  int n = strlen(name);
  char *s = name, *p;
  while (start < end) {
    header = *start;
    tag = fulltag_of(header);
    if (header_subtag(header) == subtag_symbol) {
      LispObj 
        pname = deref(ptr_to_lispobj(start), 1),
        pname_header = header_of(pname);
      if ((header_subtag(pname_header) == subtag_simple_base_string) &&
          (header_element_count(pname_header) == n)) {
        p = (char *) ptr_from_lispobj(pname + misc_data_offset);
        if (strncmp(p, s, n) == 0) {
          return (ptr_to_lispobj(start))+fulltag_misc;
        }
      }
    }
    if (nodeheader_tag_p(tag)) {
      start += (~1 & (2 + header_element_count(header)));
    } else if (immheader_tag_p(tag)) {
      start = (LispObj *) skip_over_ivector((natural)start, header);
    } else {
      start += 2;
    }
  }
  return (LispObj)NULL;
}

LispObj 
find_symbol(char *name)
{
  area *a =  ((area *) (ptr_from_lispobj(lisp_global(ALL_AREAS))))->succ;
  area_code code;
  LispObj sym;

  while ((code = a->code) != AREA_VOID) {
    if ((code == AREA_STATIC) ||
        (code == AREA_DYNAMIC)) {
      sym = find_symbol_in_range((LispObj *)(a->low), (LispObj *)(a->active), name);
      if (sym) {
        break;
      }
    }
    a = a->succ;
  }
  return sym;
}

    
void 
plsym(ExceptionInformation *xp, char *pname) 
{
  natural address = 0;

  address = find_symbol(pname);
  if (address == 0) {
    Dprintf("Can't find symbol.");
    return;
  }
  
  if ((fulltag_of(address) == fulltag_misc) &&
      (header_subtag(header_of(address)) == subtag_symbol)){
    describe_symbol(address);
  } else {
    fprintf(stderr, "Not a symbol.\n");
  }
  return;
}

