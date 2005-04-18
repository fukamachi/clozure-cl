;;;
;;;   Copyright (C) 1994-2001 Digitool, Inc
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
(defparameter *error-format-strings* 
  '((1 . "Unbound variable: ~S .")
    (2 . "Can't take CDR of ~S.")
    (3 . "Too many arguments.")
    (4 . "Too few arguments.")
    (5 . "Argument ~S is not of the required type.")
    (6 . "Undefined function: ~S .")
    (7 . "Can't take CAR of ~S.")
    (8 . "Can't coerce ~S to ~S")
    (9 . "System version 6.04 or later, MacPlus ROMs or later required.")
    (10 . "Out of memory.")
    (11 . "Default image file not found.")
    (12 . "No translation for ~S")
    (13 . "~S can't be FUNCALLed or APPLYed.")
    (14 . "~S is not a symbol or lambda expression")
    (15 . "Declaration ~S in unexpected position")
    (16 . "Can't setq constant ~S")
    (17 . "Odd number of forms to setq in ~S")
    (18 . "Illegal arg to setq ~S")
    (19 . "~S is not a symbol.")
    (20 . "~S is a constant.")
    (21 . "Bad initialization form: ~S")
    (22 . "Symbol macro ~S is declared or proclaimed special")
    (23 . "Too many arguments in ~S")
    (24 . "Local macro cannot reference lexically defined variable ~S")
    (25 . "Local macro cannot reference lexically defined function ~S")
    (26 . "Local macro cannot reference lexically defined tag ~S")
    (27 . "Local macro cannot reference lexically defined block ~S")
    (28 . "Cant find tag ~S")
    (29 . "Duplicate tag ~S")
    (30 . "Cant find block ~S")
    (31 . "Bad lambda list  ~S.")
    (32 . "~S is not a valid lambda expression.")
    (33 . "Can't throw to tag ~S .")
    (34 . "Object ~S is not of type ~S.")
    (35 . "FUNCTION can't reference lexically defined macro ~S")
    (36 . "Unimplemented FPU instruction ~^~S.")
    (41 . "Unmatched ')'.")
    (42 . "~S and ~S must be on the same volume.")
    (43 . "Filename ~S contains illegal character ~S")
    (44 . "Illegal use of wildcarded filename ~S")
    (45 . "~S is not a FASL or TEXT file.")
    (46 . "Cannot rename directory to file ~S")
    (47 . "Found a directory instead of a file or vice versa ~S")
    (48 . "Cannot copy directories: ~S")
    (49 . "String too long for pascal record")
    (50 . "Cannot create ~S")
    (64 . "Floating point overflow")
    (66 . "Can't divide by zero.")
    (75 . "Stack overflow. Bytes requested: ~d")
    (76 . "Memory allocation request failed.")
    (77 . "~S exceeds array size limit of ~S bytes.")
    (94. "Printer error.")
    (95. "Can't load printer.")
    (96. "Can't get printer parameters.")
    (97. "Can't start up printer job.")
    (98. "Floating point exception.")
    (111 . "Unexpected end of file encountered.")
    (112 . "Array index ~S out of bounds for ~S .")
    (113 . "Reader error: ~S encountered.")
    (114 . "Reader error: Unknown reader macro character ~S .")
    (115 . "Can't redefine constant ~S .")
    (116 . "Reader error: Illegal character ~S .")
    (117 . "Reader error: Illegal symbol syntax.")
    (118 . "Reader error: Dot context error.")
    (119 . "Reader error: Bad value ~S for *READ-BASE*.")
    (120 . "Can't construct argument list from ~S.")
    (121 . "Wrong FASL version.")
    (122 . "Not a FASL file.")
    (123 . "Undefined function ~s called with arguments ~s.")
    (124 . "Image file incompatible with current version of Lisp.")
    (127 .   "Using ~S in ~S ~%would cause name conflicts with symbols inherited by that package: ~%~:{~S  ~S~%~}")
    (128 .   "Importing ~S to ~S would conflict with inherited symbol ~S ." )
    (129 .   "Reader error: Malformed number in a #b/#o/#x/#r macro." )
    (130 .   "There is no package named ~S ." )
    (131 .   "Reader error: No external symbol named ~S in package ~S ." )
    (132 .   "Bad FASL file: internal inconsistency detected." )
    (133 .   "Importing ~S to ~S would conflict with symbol ~S ." )
    (134 .   "Uninterning ~S from ~S would cause conflicts among ~S ." )
    (135 .   "~S is not accessible in ~S ." )
    (136 .   "Exporting ~S in ~S would cause a name conflict with ~S in ~S ." )
    (137 .   "Using ~S in ~S ~%would cause name conflicts with symbols already present in that package: ~%~:{~S  ~S~%~}")
    (139 .   "Reader macro function ~S called outside of reader." )
    (140 .   "Reader error: undefined character ~S in a ~S dispatch macro." )
    (141 .   "Reader dispatch macro character ~S doesn't take an argument." )
    (142 .   "Reader dispatch macro character ~S requires an argument." )
    (143 .   "Reader error: Bad radix in #R macro." )
    (144 .   "Reader error: Duplicate #~S= label." )
    (145 .   "Reader error: Missing #~S# label." )
    (146 .   "Reader error: Illegal font number in #\\ macro." )
    (147 .   "Unknown character name ~S in #\\ macro." )
    (148 .   "~S cannot be accessed with ~S subscripts." )
    (149 .   "Requested size is too large to displace to ~S ." )
    (150 .   "Too many elements in argument list ~S ." )
    (151 .    "Arrays are not of the same size" )
    (152 . "Conflicting keyword arguments : ~S ~S, ~S ~S .")
    (153 . "Incorrect keyword arguments in ~S .")
    (154 . "Two few arguments in form ~S .")
    (155 . "Too many arguments in form ~S .")
    (157 . "value ~S is not of the expected type ~S.")
    (158 . "~S is not a structure.")
    (159 . "Access to slot ~S of structure ~S is out of bounds.")
    (160 . "Form ~S does not match lambda list ~A .")
    (161 . "Temporary number space exhausted.")
    (163 . "Illegal #+/- expression ~S.")
    (164 . "File ~S does not exist.")
    (165 . "~S argument ~S is not of the required type.")
    (166 . "~S argument ~S is not of type ~S.")
    (167 . "Too many arguments in ~S.")
    (168 . "Too few arguments in ~S.")
    (169 . "Arguments don't match lambda list in ~S.")
    (170 . "~S is not a proper list.")
    (171 . "~S is not an array with a fill pointer.")
    (172 . "~S is not an adjustable array.")
    (173 . "Can't access component ~D of ~S.")
    (174 . "~S doesn't match array element type of ~S.")
    (175 . "Stack group ~S is exhausted.")
    (176 . "Stack group ~S called with arguments ~:S; exactly 1 argument accepted.")
    (177 . "Attempt to return too many values.")
    (178 . "Can't dynamically bind ~S. ")))
  

