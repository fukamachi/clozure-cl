;;;-*-Mode: LISP; Package: CCL -*-
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



; module-name      binary                          (source . files-depends-on)
; -----------      ------                          ---------------------------
(defparameter *ccl-system* '(
 (level-1          "ccl:ccl;level-1"          ("ccl:l1;level-1.lisp"))
 (runtime          "ccl:ccl;runtime"          ("ccl:l1;runtime.lisp"))
 (level-1-test     "ccl:level-1-test"         ("ccl:l1;level-1-test.lisp"))
 (l1-cl-package    "ccl:l1f;l1-cl-package"    ("ccl:l1;l1-cl-package.lisp"))
 (l1-utils         "ccl:l1f;l1-utils"         ("ccl:l1;l1-utils.lisp"))
 (l1-numbers       "ccl:l1f;l1-numbers"       ("ccl:l1;l1-numbers.lisp"))
 (l1-init          "ccl:l1f;l1-init"          ("ccl:l1;l1-init.lisp"))
 (version          "ccl:l1f;version"          ("ccl:l1;version.lisp"))
 (l1-boot-1        "ccl:l1f;l1-boot-1"        ("ccl:l1;l1-boot-1.lisp"))
 (l1-boot-2        "ccl:l1f;l1-boot-2"        ("ccl:l1;l1-boot-2.lisp"))
 (l1-boot-3        "ccl:l1f;l1-boot-3"        ("ccl:l1;l1-boot-3.lisp"))
 (l1-boot-lds      "ccl:l1f;l1-boot-lds"      ("ccl:l1;l1-boot-lds.lisp"))
 (l1-files         "ccl:l1f;l1-files"         ("ccl:l1;l1-files.lisp"))
 (l1-sort          "ccl:l1f;l1-sort"          ("ccl:l1;l1-sort.lisp"))
 (l1-dcode         "ccl:l1f;l1-dcode"         ("ccl:l1;l1-dcode.lisp"))
 (l1-clos-boot     "ccl:l1f;l1-clos-boot"    ("ccl:l1;l1-clos-boot.lisp"))
 (l1-clos          "ccl:l1f;l1-clos"          ("ccl:l1;l1-clos.lisp"))
 (l1-io            "ccl:l1f;l1-io"            ("ccl:l1;l1-io.lisp"))
 (l1-menus         "ccl:l1f;l1-menus"         ("ccl:l1;l1-menus.lisp"))
 (l1-streams       "ccl:l1f;l1-streams"       ("ccl:l1;l1-streams.lisp"))
 (l1-windows       "ccl:l1f;l1-windows"       ("ccl:l1;l1-windows.lisp"))
 (fredenv          "ccl:bin;fredenv"          ("ccl:lib;fredenv.lisp"))
 (l1-edwin         "ccl:l1f;l1-edwin"         ("ccl:l1;l1-edwin.lisp"
                                                    "ccl:lib;fredenv.lisp"))
 (l1-ed-lds        "ccl:l1f;l1-ed-lds"        ("ccl:l1;l1-ed-lds.lisp"
                                                "ccl:lib;fredenv.lisp"))
 (l1-edcmd         "ccl:l1f;l1-edcmd"         ("ccl:l1;l1-edcmd.lisp"
                                                "ccl:lib;fredenv.lisp"))
 (script-manager   "ccl:l1f;script-manager"   ("ccl:l1;script-manager.lisp"))
 (l1-edfrec        "ccl:l1f;l1-edfrec"        ("ccl:l1;l1-edfrec.lisp"
                                                "ccl:lib;fredenv.lisp"))
 (l1-edbuf         "ccl:l1f;l1-edbuf"         ("ccl:l1;l1-edbuf.lisp"
                                                "ccl:lib;fredenv.lisp"))
 (l1-listener      "ccl:l1f;l1-listener"      ("ccl:l1;l1-listener.lisp"
                                                "ccl:lib;fredenv.lisp"))
 (l1-events        "ccl:l1f;l1-events"        ("ccl:l1;l1-events.lisp"))
 (ppc-trap-support "ccl:l1f;ppc-trap-support" ("ccl:l1;ppc-trap-support.lisp"))
 (sparc-trap-support "ccl:l1f;sparc-trap-support" ("ccl:l1;sparc-trap-support.lisp"))
 (l1-highlevel-events
                   "ccl:l1f;l1-highlevel-events"
                                                   ("ccl:l1;l1-highlevel-events.lisp"))
 (l1-format        "ccl:l1f;l1-format"        ("ccl:l1;l1-format.lisp"))
 (l1-readloop      "ccl:l1f;l1-readloop"      ("ccl:l1;l1-readloop.lisp"))
 (l1-readloop-lds  "ccl:l1f;l1-readloop-lds"  ("ccl:l1;l1-readloop-lds.lisp"))
 (l1-reader        "ccl:l1f;l1-reader"        ("ccl:l1;l1-reader.lisp"))
 (l1-error-system  "ccl:l1f;l1-error-system"  ("ccl:l1;l1-error-system.lisp"))
 (ppc-error-signal "ccl:l1f;ppc-error-signal" ("ccl:l1;ppc-error-signal.lisp"))
 (sparc-error-signal "ccl:l1f;sparc-error-signal" ("ccl:l1;sparc-error-signal.lisp"))
 (l1-error-signal  "ccl:l1f;l1-error-signal"  ("ccl:l1;l1-error-signal.lisp"))
 (appgen-defs      "ccl:lib;appgen-defs"      ("ccl:lib;appgen-defs.lisp"))
 (l1-base-app      "ccl:l1f;l1-base-app"      ("ccl:l1;l1-base-app.lisp"
                                                    "ccl:lib;appgen-defs.lisp"))
 (l1-initmenus     "ccl:l1f;l1-initmenus"     ("ccl:l1;l1-initmenus.lisp"))
 (l1-initmenus-lds "ccl:l1f;l1-initmenus-lds" ("ccl:l1;l1-initmenus-lds.lisp"))
 (l1-aprims        "ccl:l1f;l1-aprims"        ("ccl:l1;l1-aprims.lisp"))
 (l1-callbacks     "ccl:l1f;l1-callbacks"    ("ccl:l1;l1-callbacks.lisp"))
 (ppc-callback-support "ccl:l1f;ppc-callback-support" ("ccl:l1;ppc-callback-support.lisp"))
 (sparc-callback-support "ccl:l1f;sparc-callback-support" ("ccl:l1;sparc-callback-support.lisp"))			     
 (l1-sysio         "ccl:l1f;l1-sysio"         ("ccl:l1;l1-sysio.lisp"))
 (l1-symhash       "ccl:l1f;l1-symhash"       ("ccl:l1;l1-symhash.lisp"))
 (l1-pathnames     "ccl:l1f;l1-pathnames"     ("ccl:l1;l1-pathnames.lisp"))
 (l1-lisp-threads  "ccl:l1f;l1-lisp-threads"  ("ccl:l1;l1-lisp-threads.lisp"))
 (l1-sockets       "ccl:l1f;l1-sockets"       ("ccl:l1;l1-sockets.lisp"))
 (ppc-threads-utils "ccl:l1f;ppc-threads-utils" ("ccl:l1;ppc-threads-utils.lisp"))
 (l1-application   "ccl:l1f;l1-application"   ("ccl:l1;l1-application.lisp"))
 (l1-processes     "ccl:l1f;l1-processes"     ("ccl:l1;l1-processes.lisp"))
 #+interfaces-2
 (l1-traps         "ccl:l1f;l1-traps"         ("ccl:l1;l1-traps.lisp"))
 (l1-typesys       "ccl:l1f;l1-typesys"       ("ccl:l1;l1-typesys.lisp"))
 (sysutils         "ccl:l1f;sysutils"         ("ccl:l1;sysutils.lisp"))
 (nx               "ccl:l1f;nx"               ("ccl:compiler;nx.lisp"
                                                    "ccl:compiler;nx0.lisp"
                                                    "ccl:compiler;lambda-list.lisp"
                                                    "ccl:compiler;nx-basic.lisp"
                                                    "ccl:compiler;nx1.lisp"))
 (nxenv            "ccl:bin;nxenv"            ("ccl:compiler;nxenv.lisp"))
 (nx2              "ccl:bin;nx2"              ("ccl:compiler;nx2.lisp"
                                                    "ccl:compiler;nx2a.lisp"))
 (nx-base-app      "ccl:l1f;nx-base-app"      ("ccl:compiler;nx-base-app.lisp"
                                                    "ccl:compiler;lambda-list.lisp"))
 ; PPC compiler
 (dll-node         "ccl:bin;dll-node"         ("ccl:compiler;dll-node.lisp"))
 (ppc32-arch       "ccl:bin;ppc32-arch"       ("ccl:compiler;PPC;PPC32;ppc32-arch.lisp"))
 (ppc-arch         "ccl:bin;ppc-arch"         ("ccl:compiler;PPC;ppc-arch.lisp"))
 (ppc64-arch       "ccl:bin;ppc64-arch"       ("ccl:compiler;PPC;PPC64;ppc64-arch.lisp"))			     
 (arch             "ccl:bin;arch"             ("ccl:compiler;arch.lisp"))
 (ppcenv           "ccl:bin;ppcenv"           ("ccl:lib;ppcenv.lisp"))
 (vreg             "ccl:bin;vreg"             ("ccl:compiler;vreg.lisp"))
 (ppc-asm          "ccl:bin;ppc-asm"          ("ccl:compiler;PPC;ppc-asm.lisp"))
 (vinsn            "ccl:bin;vinsn"            ("ccl:compiler;vinsn.lisp"))
 (ppc32-vinsns     "ccl:bin;ppc32-vinsns"     ("ccl:compiler;PPC;PPC32;ppc32-vinsns.lisp"))
 (ppc-reg          "ccl:bin;ppc-reg"          ("ccl:compiler;PPC;ppc-reg.lisp"))
 (reg              "ccl:bin;reg"              ("ccl:compiler;reg.lisp"))
 (subprims         "ccl:bin;subprims"         ("ccl:compiler;subprims.lisp"))
 (risc-lap         "ccl:bin;risc-lap"         ("ccl:compiler;risc-lap.lisp"))
 (ppc-lap          "ccl:bin;ppc-lap"          ("ccl:compiler;PPC;ppc-lap.lisp"))
 (backend          "ccl:bin;backend"          ("ccl:compiler;backend.lisp"))
 (ppc32-backend    "ccl:bin;ppc32-backend"    ("ccl:compiler;PPC;PPC32;ppc32-backend.lisp"))			   
 (ppc64-backend    "ccl:bin;ppc64-backend"    ("ccl:compiler;PPC;PPC64;ppc64-backend.lisp"))
 (ppc-backend      "ccl:bin;ppc-backend"     ("ccl:compiler;PPC;ppc-backend.lisp"))			   
 (ppc2             "ccl:bin;ppc2"             ("ccl:compiler;PPC;ppc2.lisp"))

 (ppc-lapmacros    "ccl:bin;ppc-lapmacros"    ("ccl:compiler;PPC;ppc-lapmacros.lisp"))
 (ppc-disassemble  "ccl:bin;ppc-disassemble"  ("ccl:compiler;PPC;ppc-disassemble.lisp"))
 (xfasload         "ccl:xdump;xfasload"       ("ccl:xdump;xfasload.lisp"))
 (xsparcfasload    "ccl:xdump;xsparcfasload"  ("ccl:xdump;xsparcfasload.lisp"))
 (xppcfasload      "ccl:xdump;xppcfasload"    ("ccl:xdump;xppcfasload.lisp"))
 (heap-image       "ccl:xdump;heap-image"     ("ccl:xdump;heap-image.lisp"))
 (xsym             "ccl:xdump;xsym"           ("ccl:xdump;xsym.lisp"))
 (number-macros "ccl:bin;number-macros"    ("ccl:lib;number-macros.lisp"))
 (number-case-macro  "ccl:bin;number-case-macro" ("ccl:lib;number-case-macro.lisp"))
 (sparc-arch       "ccl:bin;sparc-arch"       ("ccl:compiler;sparc;sparc-arch.lisp"))
 (sparc-asm        "ccl:bin;sparc-asm"        ("ccl:compiler;sparc;sparc-asm.lisp"))
 (sparc-lap        "ccl:bin;sparc-lap"        ("ccl:compiler;sparc;sparc-lap.lisp"))
 (sparc-disassemble "ccl:bin;sparc-disassemble" ("ccl:compiler;sparc;sparc-disassemble.lisp"))
 (sparc-lapmacros  "ccl:bin;sparc-lapmacros"  ("ccl:compiler;sparc;sparc-lapmacros.lisp"))
 (sparcenv         "ccl:bin;sparcenv"         ("ccl:lib;sparcenv.lisp"))
 (sparc-backend    "ccl:bin;sparc-backend"    ("ccl:compiler;sparc;sparc-backend.lisp"))
 (sparc-vinsns     "ccl:bin;sparc-vinsns"     ("ccl:compiler;sparc;sparc-vinsns.lisp"))
 (sparc2           "ccl:bin;sparc2"           ("ccl:compiler;sparc;sparc2.lisp"))
 (sparc-bootstrap  "ccl:bin;sparc-bootstrap"  ("ccl:compiler;sparc;sparc-bootstrap.lisp"))
 (optimizers       "ccl:bin;optimizers"       ("ccl:compiler;optimizers.lisp")) 
 (backquote        "ccl:bin;backquote"        ("ccl:lib;backquote.lisp"))
 (lispequ          "ccl:library;lispequ"      ("ccl:library;lispequ.lisp"))
 (sysequ           "ccl:bin;sysequ"           ("ccl:lib;sysequ.lisp"))
 (toolequ          "ccl:bin;toolequ"          ("ccl:lib;toolequ.lisp"))
 (level-2          "ccl:bin;level-2"          ("ccl:lib;level-2.lisp"))
 (macros           "ccl:bin;macros"           ("ccl:lib;macros.lisp"))
			   
 (trap-support     "ccl:bin;trap-support"     ("ccl:lib;trap-support.lisp"))
 (traps            "ccl:bin;traps"            ("ccl:library;traps.lisp"))
 (defstruct-macros "ccl:bin;defstruct-macros" ("ccl:lib;defstruct-macros.lisp"))
 (mactypes         "ccl:bin;mactypes"         ("ccl:lib;mactypes.lisp"))
 (defrecord        "ccl:bin;defrecord"        ("ccl:lib;defrecord.lisp"))
 (foreign-types    "ccl:bin;foreign-types"    ("ccl:lib;foreign-types.lisp"))
 (db-io            "ccl:bin;db-io"            ("ccl:lib;db-io.lisp"))
 #+interfaces-2
 (new-traps        "ccl:bin;new-traps"        ("ccl:lib;new-traps.lisp"))
 ;(lap              "ccl:library;lap"          ("ccl:lib;lap.lisp"))  ; should really be in lib;68K;
 ;(lapmacros        "ccl:library;lapmacros"    ("ccl:lib;lapmacros.lisp"))
 (aplink-macros    "ccl:bin;aplink-macros"    ("ccl:lib;aplink-macros.lisp"))

 (hash             "ccl:bin;hash"             ("ccl:lib;hash.lisp"))
 (nfcomp           "ccl:bin;nfcomp"           ("ccl:lib;nfcomp.lisp"))
 (lists            "ccl:bin;lists"            ("ccl:lib;lists.lisp"))
 (chars            "ccl:bin;chars"            ("ccl:lib;chars.lisp"))
 (streams          "ccl:bin;streams"          ("ccl:lib;streams.lisp"))
 (pathnames        "ccl:bin;pathnames"        ("ccl:lib;pathnames.lisp"))
 (describe         "ccl:bin;describe"         ("ccl:lib;describe.lisp")) 
 (mcl-compat       "ccl:bin;mcl-compat"       ("ccl:lib;mcl-compat.lisp"))
 (backtrace        "ccl:bin;backtrace"        ("ccl:lib;backtrace.lisp"))
 (backtrace-lds    "ccl:bin;backtrace-lds"    ("ccl:lib;backtrace-lds.lisp"))
 (apropos          "ccl:bin;apropos"          ("ccl:lib;apropos.lisp"))

 (appgen-misc      "ccl:lib;appgen-misc"      ("ccl:lib;appgen-misc.lisp"))
 (build-application "ccl:lib;build-application" ("ccl:lib;build-application.lisp"
                                                      "ccl:lib;appgen-misc.lisp"))
 (numbers          "ccl:bin;numbers"          ("ccl:lib;numbers.lisp"))
 (dumplisp         "ccl:bin;dumplisp"         ("ccl:lib;dumplisp.lisp"))
 (aplink           "ccl:bin;aplink"           ("ccl:lib;aplink.lisp"
                                                "ccl:lib;aplink-macros.lisp"))
 (aplink-dump      "ccl:bin;aplink-dump"      ("ccl:lib;aplink-dump.lisp"
                                                "ccl:lib;aplink-macros.lisp"))
 (defstruct        "ccl:bin;defstruct"        ("ccl:lib;defstruct.lisp"
                                                    "ccl:lib;defstruct-macros.lisp"))
 (defstruct-lds    "ccl:bin;defstruct-lds"    ("ccl:lib;defstruct-lds.lisp"
                                                    "ccl:lib;defstruct-macros.lisp"))
 (method-combination
                   "ccl:bin;method-combination"
                                                   ("ccl:lib;method-combination.lisp"))
 (encapsulate      "ccl:bin;encapsulate"      ("ccl:lib;encapsulate.lisp"))
 (read             "ccl:bin;read"           ("ccl:lib;read.lisp"))
 (misc             "ccl:bin;misc"           ("ccl:lib;misc.lisp"))
 (doc-window       "ccl:bin;doc-window"       ("ccl:lib;doc-window.lisp"))
 (arrays-fry       "ccl:bin;arrays-fry"     ("ccl:lib;arrays-fry.lisp"))
 (sequences        "ccl:bin;sequences"      ("ccl:lib;sequences.lisp"))
 (sort             "ccl:bin;sort"           ("ccl:lib;sort.lisp"))
 (setf             "ccl:bin;setf"           ("ccl:lib;setf.lisp"))
 (setf-runtime     "ccl:bin;setf-runtime"   ("ccl:lib;setf-runtime.lisp"))
 (format           "ccl:bin;format"         ("ccl:lib;format.lisp"))
 (case-error       "ccl:bin;case-error"     ("ccl:lib;case-error.lisp"))
 (pprint           "ccl:bin;pprint"         ("ccl:lib;pprint.lisp"))
 (time             "ccl:bin;time"           ("ccl:lib;time.lisp"))
 (print-db         "ccl:bin;print-db"       ("ccl:lib;print-db.lisp"))
 (eval             "ccl:bin;eval"           ("ccl:lib;eval.lisp"))

 (arglist          "ccl:bin;arglist"          ("ccl:lib;arglist.lisp"))

 (edit-callers	   "ccl:bin;edit-callers"   ("ccl:lib;edit-callers.lisp"))
 (step             "ccl:bin;step"           ("ccl:lib;step.lisp"))
 (ccl-export-syms  "ccl:bin;ccl-export-syms"  ("ccl:lib;ccl-export-syms.lisp"))
 (systems          "ccl:bin;systems"        ("ccl:lib;systems.lisp"))
 (compile-ccl      "ccl:bin;compile-ccl"    ("ccl:lib;compile-ccl.lisp"))
 (ppc-init-ccl     "ccl:bin;ppc-init-ccl"   ("ccl:lib;ppc-init-ccl.lisp"))
 (merge-ccl-update "ccl:bin;merge-ccl-update" ("ccl:lib;merge-ccl-update.lisp"))
 (distrib-inits    "ccl:bin;distrib-inits"  ("ccl:lib;distrib-inits.lisp"))
 (lisp-package     "ccl:library;lisp-package" ("ccl:library;lisp-package.lisp"))
 ; need to add swapping, xdump to CCL's *module-search-path*
 (xdump            "ccl:xdump;xdump"          ("ccl:xdump;xdump.lisp"))
 (fasload          "ccl:xdump;fasload"        ("ccl:xdump;fasload.lisp"))
 (loop             "ccl:library;loop"         ("ccl:library;loop.lisp"))
 (mcl-extensions   "ccl:bin;mcl-extensions"   ("ccl:lib;mcl-extensions.lisp"))
 (linux-files      "ccl:l1f;linux-files"      ("ccl:level-1;linux-files.lisp"))
 (solaris-files    "ccl:l1f;solaris-files"    ("ccl:level-1;solaris-files.lisp"))
 (solaris-records  "ccl:library;solaris-records" ("ccl:library;solaris-records.lisp"))
 (source-files     "ccl:bin;source-files"     ("ccl:lib;source-files.lisp"))
 
 (prepare-mcl-environment "ccl:bin;prepare-mcl-environment" ("ccl:lib;prepare-mcl-environment.lisp"))
 (defsystem        "ccl:tools;defsystem"      ("ccl:tools;defsystem.lisp"))
 (asdf             "ccl:tools;asdf"	    ("ccl:tools;asdf.lisp"))))

