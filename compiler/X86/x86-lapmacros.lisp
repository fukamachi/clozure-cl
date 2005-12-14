;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;;   Copyright (C) 2005, Clozure Associates and contributors.
;;;   This file is part of OpenMCL.  
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License , known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL,
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict, the preamble takes precedence.  
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(defx86lapmacro extract-lisptag (node dest)
  `(progn
    (movb ($ x8664::tagmask) (%b ,dest))
    (andb (%b ,node) (%b ,dest))))

(defx86lapmacro extract-fulltag (node dest)
  `(progn
    (movb ($ x8664::fulltagmask) (%b ,dest))
    (andb (%b ,node) (%b ,dest))))

(defx86lapmacro extract-subtag (node dest)
  `(movb (@ x8664::misc-subtag-offset (%q ,node)) (%b ,dest)))

(defx86lapmacro extract-typecode (node dest)
  ;;; In general, these things are only defined to affect the low
  ;;; byte of the destination register.  This can also affect
  ;;; the #xff00 byte.
  `(progn
    (extract-lisptag ,node ,dest)
    (cmpb ($ x8664::tag-misc) (%b ,dest))
    (cmovew (@  x8664::misc-subtag-offset (%q ,node)) (%w ,dest))))
  
