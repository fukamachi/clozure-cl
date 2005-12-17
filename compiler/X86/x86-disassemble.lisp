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

(in-package "CCL")

(defstruct (x86-disassembly-state (:conc-name x86-ds-))
  (mode-64 t)
  (prefixes 0)
  (used-prefixes 0)
  (rex 0)
  (rex-used 0)
  (need-modrm nil)
  (mod 0)
  (reg 0)
  (rm 0)
  (blocks nil)
  (start-pc 0)
  current-block
  code-vector
  code-pointer
  constants-vector
  pending-labels
)

(defun x86-ds-peek-u8 (ds)
  (aref (x86-ds-code-vector ds) (x86-ds-code-pointer ds)))

(defun x86-ds-skip (ds &optional (n 1))
  (incf (x86-ds-code-pointer ds) n))

(defun x86-ds-next-u8 (ds)
  (let* ((idx (x86-ds-code-pointer ds)))
    (incf (x86-ds-code-pointer ds))
    (aref (x86-ds-code-vector ds) idx)))

(defun x86-ds-next-s8 (ds)
  (let* ((u8 (x86-ds-next-u8 ds)))
    (if (logbitp 7 u8)
      (- u8 #x100)
      u8)))

(defun x86-ds-next-u16 (ds)
  (let* ((low (x86-ds-next-u8 ds))
         (high (x86-ds-next-u8 ds)))
    (declare (type (unsigned-byte 8) low high))
    (logior (the fixnum (ash high 8)) low)))

(defun x86-ds-next-s16 (ds)
  (let* ((low (x86-ds-next-u8 ds))
         (high (x86-ds-next-s8 ds)))
    (declare (type (unsigned-byte 8) low)
             (type (signed-byte 8) high))
    (logior (the fixnum (ash high 8)) low)))

(defun x86-ds-next-u32 (ds)
  (let* ((low (x86-ds-next-u16 ds))
         (high (x86-ds-next-u16 ds)))
    (declare (type (unsigned-byte 16) low high))
    (logior (the fixnum (ash high 16)) low)))

(defun x86-ds-next-s32 (ds)
  (let* ((low (x86-ds-next-u16 ds))
         (high (x86-ds-next-u16 ds)))
    (declare (type (unsigned-byte 16) low)
             (type (signed-byte 16) high))
    (logior (the fixnum (ash high 16)) low)))

(defun x86-ds-next-u64 (ds)
  (let* ((low (x86-ds-next-u32 ds))
         (high (x86-ds-next-u32 ds)))
    (logior (the fixnum (ash high 32)) low)))

(defun x86-ds-next-s64 (ds)
  (let* ((low (x86-ds-next-u32 ds))
         (high (x86-ds-next-s32 ds)))
    (logior (the fixnum (ash high 32)) low)))  

(defun used-rex (ds value)
  (if (not (zerop value))
    (setf (x86-ds-rex-used ds)
          (logior (x86-ds-rex-used ds)
                  (if (logtest (x86-ds-rex ds) value)
                    #x40
                    0)))
    (setf (x86-ds-rex-used ds)
          (logior (x86-ds-rex-used ds) #x40))))
  

;;; An x86-disassembly-block is -something- like a basic block in a
;;; compiler flow graph; it ends with an unconditional jump and it's
;;; either the entry node in that graph or it's reachable via a jump
;;; or branch from some other reachable block.  There may, however, be
;;; internal labels that are referenced from within the block's
;;; instructions, from some other block, or both.  Each disassembled
;;; instruction within a block keeps track of its address and whether
;;; or not it's a label (a branch or jump target or a tagged return
;;; address.)  The first instruction in each block is a label; others
;;; (initally) aren't.  Whenever we encounter a branch or jmp
;;; instruction (or a manipulation of a tagged return address, which
;;; is a kind of jmp) and determine the address of the label, we add
;;; that address to the disassembly-state's PENDING-LABELS set.  When
;;; we're through processing the block (having encountered an
;;; unconditional jmp), we remove a pending label from that set.  If
;;; it's within a block that's already been processed, we ensure that
;;; the instruction at that address is marked as a label; otherwise,
;;; we process the new block which starts at that address.
;;; Eventually, this'll terminate with all reachable code having been
;;; processed.  There's embedded data and alignment padding in OpenMCL
;;; x86 functions and this approach means that we won't try to
;;; disassemble any of that; if the compiler generates any unreachable
;;; code, we won't seen that, either.

;;; There might be a large number of blocks, in which case
;;; keeping them in a search tree might be a better idea.
(defstruct (x86-dis-block (:include dll-node))
  start-address
  end-address
  instructions
)

;;; Insert the block before the first existing block whose
;;; start address is greater than or equal to this block's
;;; end address.  (Yes, they can be equal; no, there should
;;; never be any overlap.)
(defun insert-x86-block (block blocks)
  (let* ((this-end (x86-dis-block-end-address block)))
    (declare (fixnum this-end))
    (do-dll-nodes (other blocks (append-dll-node block blocks))
      (when (>= (the fixnum (dis-x86-block-start-address other))
                this-end)
        (return (insert-dll-node-before block other))))))

(defun x86-dis-find-label (address blocks)
  (declare (fixnum address))
  (do-dll-nodes (block blocks)
    (when (and (>= address (the fixnum (x86-dis-block-start-address block)))
               (< address (the fixnum (x86-dis-block-end-address block))))
      (let* ((instruction (assoc address (x86-dis-block-instructions block))))
        (unless instruction
          (error "Bug: no instruction at address #x~x" address))
        (return (or (cadr instruction)
                    (setf (cadr instruction) t)))))))


;;; Flags stored in PREFIXES
(defconstant +PREFIX-REPZ+ 1)
(defconstant +PREFIX-REPNZ+ 2)
(defconstant +PREFIX-LOCK+ 4)
(defconstant +PREFIX-CS+ 8)
(defconstant +PREFIX-SS+ #x10)
(defconstant +PREFIX-DS+ #x20)
(defconstant +PREFIX-ES+ #x40)
(defconstant +PREFIX-FS+ #x80)
(defconstant +PREFIX-GS+ #x100)
(defconstant +PREFIX-DATA+ #x200)
(defconstant +PREFIX-ADDR+ #x400)
(defconstant +PREFIX-FWAIT+ #x800)


  
                                  
(defstruct (x86-dis (:constructor %make-x86-dis))
  mnemonic                              ; may be nil
  flags                                 ; affect printing of mnemonic
  op1                                   ; function to obtain 1st operand
  bytemode1                             ; flags associated with operand1
  op2                                   ; function for second operand
  bytemode2                             ; flags for operand2
  op3                                   ; function,
  bytemode3                             ; flags for operand3
  )

(defconstant +SUFFIX-ALWAYS+ 4)
(defconstant +AFLAG+ 2)
(defconstant +DFLAG+ 1)

(defconstant +b-mode+ 1)                ; byte operand 
(defconstant +v-mode+ 2)                ; operand size depends on prefixes 
(defconstant +w-mode+ 3)                ; word operand 
(defconstant +d-mode+ 4)                ; double word operand  
(defconstant +q-mode+ 5)                ; quad word operand 
(defconstant +t-mode+ 6)                ; ten-byte operand 
(defconstant +x-mode+ 7)                ; 16-byte XMM operand 
(defconstant +m-mode+ 8)                ; d-mode in 32bit, q-mode in 64bit mode.  
(defconstant +cond-jump-mode+ 9)
(defconstant +loop-jcxz-mode+ 10)
(defconstant +dq-mode+ 11)              ; operand size depends on REX prefixes.  
(defconstant +dqw-mode+ 12)             ; registers like dq-mode, memory like w-mode.  
(defconstant +f-mode+ 13)               ; 4- or 6-byte pointer operand 
(defconstant +const-1-mode+ 14)

(defconstant +es-reg+ 100)
(defconstant +cs-reg+ 101)
(defconstant +ss-reg+ 102)
(defconstant +ds-reg+ 103)
(defconstant +fs-reg+ 104)
(defconstant +gs-reg+ 105)

(defconstant +eAX-reg+ 108)
(defconstant +eCX-reg+ 109)
(defconstant +eDX-reg+ 110)
(defconstant +eBX-reg+ 111)
(defconstant +eSP-reg+ 112)
(defconstant +eBP-reg+ 113)
(defconstant +eSI-reg+ 114)
(defconstant +eDI-reg+ 115)

(defconstant +al-reg+ 116)
(defconstant +cl-reg+ 117)
(defconstant +dl-reg+ 118)
(defconstant +bl-reg+ 119)
(defconstant +ah-reg+ 120)
(defconstant +ch-reg+ 121)
(defconstant +dh-reg+ 122)
(defconstant +bh-reg+ 123)

(defconstant +ax-reg+ 124)
(defconstant +cx-reg+ 125)
(defconstant +dx-reg+ 126)
(defconstant +bx-reg+ 127)
(defconstant +sp-reg+ 128)
(defconstant +bp-reg+ 129)
(defconstant +si-reg+ 130)
(defconstant +di-reg+ 131)

(defconstant +rAX-reg+ 132)
(defconstant +rCX-reg+ 133)
(defconstant +rDX-reg+ 134)
(defconstant +rBX-reg+ 135)
(defconstant +rSP-reg+ 136)
(defconstant +rBP-reg+ 137)
(defconstant +rSI-reg+ 138)
(defconstant +rDI-reg+ 139)

(defconstant +indir-dx-reg+ 150)

(defconstant +FLOATCODE+ 1)
(defconstant +USE-GROUPS+ 2)
(defconstant +USE-PREFIX-USER-TABLE+ 3)
(defconstant +X86-64-SPECIAL+ 4)

(defconstant +REX-MODE64+ 8)
(defconstant +REX-EXTX+ 4)
(defconstant +REX-EXTY+ 2)
(defconstant +REX-EXTZ+ 1)

(defparameter *x86-segment-prefix-alist*
  `((,+prefix-cs+ . "cs")
    (,+prefix-ds+ . "ds")
    (,+prefix-ss+ . "ss")
    (,+prefix-es+ . "es")
    (,+prefix-fs+ . "fs")
    (,+prefix-gs+ . "gs")))

    
(defun segment-register-from-prefixes (ds)
  (let* ((prefixes (x86-ds-prefixes ds)))
    (dolist (pair *x86-segment-prefix-alist*)
      (when (logtest (car pair) prefixes)
        (setf (x86-ds-used-prefixes ds)
              (logior (x86-ds-used-prefixes ds)
                      (car pair)))
        (return `(% ,(cdr pair)))))))
    

(defun op-st (ds bytemode sizeflag)
  (declare (ignore ds bytemode sizeflag))
  '(@ st))

(defun op-sti (ds bytemode sizeflag)
  (declare (ignore bytemode sizeflag))
  `(@ ,(svref x86::*x86-float-regs* (x86-ds-rm ds))))

(defun op-indire (ds bytemode sizeflag)
  `(* ,(op-e ds bytemode sizeflag)))


(defun op-e (ds bytemode sizeflag)
  (let* ((add 0)
         (riprel nil))
    (used-rex ds +rex-extz+)
    (if (logtest (x86-ds-rex ds) +rex-extz+)
      (setq add 8))
    (x86-ds-skip ds)                    ;skip MODRM byte
    (cond ((eql (x86-ds-mod ds) 3)      ; EA is just a register
           (cond ((eql bytemode +b-mode+)
                  (used-rex ds 0)
                  ;; This is wrong: if we don't have an REX prefix,
                  ;; we should use the old byte register names
                  ;; (dh, ah, ...) instead of the new ones (bpl, sil ...)
                  ;; That'll matter if Lisp code ever needs to
                  ;; access the #xff00 byte, but that seems unlikely
                  `(% ,(x86::reg-entry-reg-name (x86::x86-reg8 (+ (x86-ds-rm ds)
                                                              add)))))
                 ((eql bytemode +w-mode+)
                  `(% ,(x86::reg-entry-reg-name (x86::x86-reg16 (+ (x86-ds-rm ds)
                                                              add)))))
                 ((eql bytemode +d-mode+)
                  `(% ,(x86::reg-entry-reg-name (x86::x86-reg32 (+ (x86-ds-rm ds)
                                                               add)))))
                 ((eql bytemode +q-mode+)
                  `(% ,(x86::reg-entry-reg-name (x86::x86-reg64 (+ (x86-ds-rm ds)
                                                               add)))))
                 ((eql bytemode +m-mode+)
                  (if (x86-ds-mode-64 ds)
                    `(% ,(x86::reg-entry-reg-name (x86::x86-reg64 (+ (x86-ds-rm ds)
                                                                 add))))
                    `(% ,(x86::reg-entry-reg-name (x86::x86-reg32 (+ (x86-ds-rm ds)
                                                               add))))))
                 ((or (eql bytemode +v-mode+)
                      (eql bytemode +dq-mode+)
                      (eql bytemode +dqw-mode+))
                  (used-rex ds +rex-mode64+)
                  (setf (x86-ds-used-prefixes ds)
                        (logior
                         (logand (x86-ds-prefixes ds) +prefix-data+)))
                  (cond ((logtest (x86-ds-rex ds) +rex-mode64+)
                         `(% ,(x86::reg-entry-reg-name (x86::x86-reg64 (+ (x86-ds-rm ds)
                                                                      add)))))
                        ((or (logtest sizeflag +dflag+)
                             (not (eql bytemode +v-mode+)))
                         `(% ,(x86::reg-entry-reg-name (x86::x86-reg32 (+ (x86-ds-rm ds)
                                                                      add)))))
                        (t
                         `(% ,(x86::reg-entry-reg-name (x86::x86-reg16 (+ (x86-ds-rm ds)
                                                                      add)))))))
                 ((eql bytemode 0) nil)
                 (t (error "Disassembly error"))))
          (t                            ; memory operand
           (let* ((disp nil)
                  (base (x86-ds-rm ds))
                  (index nil)
                  (scale nil)
                  (have-base nil)
                  (have-sib nil))
             (collect ((memop))
               (memop '@)
               (let* ((seg (segment-register-from-prefixes ds)))
                 (when seg (memop seg)))
               (when (= base 4)
                 (setq have-sib t)
                 (let* ((sib (x86-ds-next-u8 ds)))
                   (setq index (ldb (byte 3 3) sib))
                   (if (or (x86-ds-mode-64 ds)
                           (not (eql index 4)))
                     (setq scale (ldb (byte 2 6) sib)))
                   (setq base (ldb (byte 3 0) sib))
                   (used-rex ds +rex-exty+)
                   (used-rex ds +rex-extz+)
                   (when (logtest (x86-ds-rex ds) +rex-exty+)
                     (incf index 8))
                   (when (logtest  (x86-ds-rex ds) +rex-extz+)
                     (incf base 8))))
               
               (case (x86-ds-mod ds)
                 (0
                  (when (= 5 (logand base 7))
                    (setq have-base nil)
                    (if (and (x86-ds-mode-64 ds) (not have-sib))
                      (setq riprel t))
                    (setq disp (x86-ds-next-s32 ds))))
                 (1
                  (setq disp (x86-ds-next-s8 ds)))
                 (2
                  (setq disp (x86-ds-next-s32 ds))))

               (when (or (not (eql (x86-ds-mod ds) 0))
                         (eql 5 (logand base 7)))
                 (memop disp)
                 (when riprel
                   (memop '(% "rip"))))

               (when (or have-base
                         (and have-sib
                              (or (not (eql index 4))
                                  (not (eql scale 0)))))
                 (used-rex ds +rex-extz+)
                 (if (and (not have-sib)
                          (logtest (x86-ds-rex ds) +rex-extz+))
                   (incf base 8))
                 (if have-base
                   (memop
                    (if (and (x86-ds-mode-64 ds)
                             (logtest sizeflag +aflag+))
                      `(@ ,(x86::reg-entry-reg-name (x86::x86-reg64 base)))
                      `(@ ,(x86::reg-entry-reg-name (x86::x86-reg32 base))))))
                 (when have-sib
                   (unless (= index 4)
                     (memop
                      (if (and (x86-ds-mode-64 ds)
                               (logtest sizeflag +aflag+))
                        `(@ ,(x86::reg-entry-reg-name (x86::x86-reg64 index)))
                        `(@ ,(x86::reg-entry-reg-name (x86::x86-reg32 index))))))
                   (unless scale
                     (setq scale 0))
                   (when (or (not (eql scale 0))
                             (not (eql index 4)))
                     (memop (ash 1 scale)))))
               (memop)))))))


(defun op-g (ds bytemode sizeflag)
  (let* ((add 0)
         (reg (x86-ds-reg ds)))
    (used-rex ds +rex-extx+)
    (if (logtest (x86-ds-rex ds) +rex-extx+)
      (setq add 8))
    (cond ((eql bytemode +b-mode+)
           (used-rex ds 0)
           ;; This is wrong: if we don't have an REX prefix,
           ;; we should use the old byte register names
           ;; (dh, ah, ...) instead of the new ones (bpl, sil ...)
           ;; That'll matter if Lisp code ever needs to
           ;; access the #xff00 byte, but that seems unlikely
           `(% ,(x86::reg-entry-reg-name (x86::x86-reg8 (+ reg add)))))
          ((eql bytemode +w-mode+)
           `(% ,(x86::reg-entry-reg-name (x86::x86-reg16 (+ reg add)))))
          ((eql bytemode +d-mode+)
           `(% ,(x86::reg-entry-reg-name (x86::x86-reg32 (+ reg add)))))
          ((eql bytemode +q-mode+)
           `(% ,(x86::reg-entry-reg-name (x86::x86-reg64 (+ (x86-ds-rm ds)
                                                            add)))))
          ((eql bytemode +m-mode+)
           (if (x86-ds-mode-64 ds)
             `(% ,(x86::reg-entry-reg-name (x86::x86-reg64 (+ reg add))))
             `(% ,(x86::reg-entry-reg-name (x86::x86-reg32 (+ reg add))))))
          ((or (eql bytemode +v-mode+)
               (eql bytemode +dq-mode+)
               (eql bytemode +dqw-mode+))
           (used-rex ds +rex-mode64+)
           (setf (x86-ds-used-prefixes ds)
                 (logior
                  (logand (x86-ds-prefixes ds) +prefix-data+)))
           (cond ((logtest (x86-ds-rex ds) +rex-mode64+)
                  `(% ,(x86::reg-entry-reg-name (x86::x86-reg64 (+ reg add)))))
                 ((or (logtest sizeflag +dflag+)
                      (not (eql bytemode +v-mode+)))
                  `(% ,(x86::reg-entry-reg-name (x86::x86-reg32 (+ reg add)))))
                 (t
                  `(% ,(x86::reg-entry-reg-name (x86::x86-reg16 (+ reg add)))))))
          ((eql bytemode 0) nil)
          (t (error "Disassembly error")))))

(defun op-reg (ds code sizeflag)
  (declare (fixnum code))
  (let* ((add 0))
    (used-rex ds +rex-extz+)
    (if (logtest (x86-ds-rex ds) +rex-extz+)
      (setq add 8))
    (cond ((= code +indir-dx-reg+)
           '(@ (% "dx")))
          (t
           (let* ((r (cond ((and (>= code +ax-reg+)
                                 (<= code +di-reg+))
                            (x86::x86-reg16 (+ (- code +ax-reg+) add)))
                           ((= code +es-reg+) (lookup-x86-register "es" :%))
                           ((= code +cs-reg+) (lookup-x86-register "cs" :%))
                           ((= code +ds-reg+) (lookup-x86-register "ds" :%))
                           ((= code +ss-reg+) (lookup-x86-register "ss" :%))
                           ((= code +fs-reg+) (lookup-x86-register "fs" :%))
                           ((= code +gs-reg+) (lookup-x86-register "gs" :%))
                           ((and (>= code +al-reg+)
                                 (<= code +dh-reg+))
                            ;; Again, this is wrong if there's no REX
                            ;; prefix.
                            (used-rex ds 0)
                            (x86::x86-reg8 (+ add (- code +al-reg+))))
                           ((and (>= code +rax-reg+)
                                 (<= code +rdi-reg+)
                                 (x86-ds-mode-64 ds))
                            (x86::x86-reg64 (+ add (- code +rax-reg+))))
                           ((progn
                              (setq code (+ code (- +eax-reg+ +rax-reg+)))
                              (and (>= code +eax-reg+)
                                   (<= code +edi-reg+)))
                            (used-rex ds +rex-mode64+)
                            (setf (x86-ds-used-prefixes ds)
                                  (logior (x86-ds-prefixes ds) +prefix-data+))
                            (if (logtest (x86-ds-rex ds) +rex-mode64+)
                              (x86::x86-reg64 (+ add (- code +eax-reg+)))
                              (if (logtest sizeflag +dflag+)
                                (x86::x86-reg32 (+ add (- code +eax-reg+)))
                                (x86::x86-reg16 (+ add (- code +eax-reg+))))))
                           (t (error "Disassembly error")))))
             `(% ,(x86::reg-entry-reg-name r)))))))

;;; Like OP-REG, but doesn't deal with extended 64-bit registers.
(defun op-imreg (ds code sizeflag)
  (declare (fixnum code))
  (cond ((= code +indir-dx-reg+)
         '(@ (% "dx")))
        (t
         (let* ((r (cond ((and (>= code +ax-reg+)
                               (<= code +di-reg+))
                          (x86::x86-reg16 (- code +ax-reg+)))
                         ((= code +es-reg+) (lookup-x86-register "es" :%))
                         ((= code +cs-reg+) (lookup-x86-register "cs" :%))
                         ((= code +ds-reg+) (lookup-x86-register "ds" :%))
                         ((= code +ss-reg+) (lookup-x86-register "ss" :%))
                         ((= code +fs-reg+) (lookup-x86-register "fs" :%))
                         ((= code +gs-reg+) (lookup-x86-register "gs" :%))
                         ((and (>= code +al-reg+)
                               (<= code +dh-reg+))
                          ;; Again, this is wrong if there's no REX
                          ;; prefix.
                          (used-rex ds 0)
                          (x86::x86-reg8 (- code +al-reg+)))
                         ((and (>= code +rax-reg+)
                               (<= code +rdi-reg+)
                               (x86-ds-mode-64 ds))
                          (x86::x86-reg64 (- code +rax-reg+)))
                         ((progn
                            (setq code (+ code (- +eax-reg+ +rax-reg+)))
                            (and (>= code +eax-reg+)
                                 (<= code +edi-reg+)))
                          (used-rex ds +rex-mode64+)
                          (setf (x86-ds-used-prefixes ds)
                                (logior (x86-ds-prefixes ds) +prefix-data+))
                          (if (logtest (x86-ds-rex ds) +rex-mode64+)
                            (x86::x86-reg64 (- code +eax-reg+))
                            (if (logtest sizeflag +dflag+)
                              (x86::x86-reg32 (- code +eax-reg+))
                              (x86::x86-reg16 (- code +eax-reg+)))))
                         (t (error "Disassembly error")))))
           `(% ,(x86::reg-entry-reg-name r))))))

;;; A (possibly unsigned) immediate.
(defun op-i (ds bytemode sizeflag)
  (let* ((mask -1)
         (op (cond ((= bytemode +b-mode+)
                    (setq mask #xff)
                    (x86-ds-next-u8 ds))
                   ((and (= bytemode +q-mode+)
                         (x86-ds-mode-64 ds))
                    (x86-ds-next-s32 ds))
                   ((or (= bytemode +q-mode+)
                        (= bytemode +v-mode+))
                    (used-rex ds +rex-mode64+)
                    (setf (x86-ds-used-prefixes ds)
                          (logior (x86-ds-prefixes ds) +prefix-data+)) 
                    (if (logtest (x86-ds-rex ds) +rex-mode64+)
                      (x86-ds-next-s32 ds)
                      (if (logtest sizeflag +dflag+)
                        (progn
                          (setq mask #xffffffff)
                          (x86-ds-next-u32 ds))
                        (progn
                          (setq mask #xfffff)
                          (x86-ds-next-u16 ds)))))
                   ((= bytemode +w-mode+)
                    (setq mask #xfffff)
                    (x86-ds-next-u16 ds))
                   ((= bytemode +const-1-mode+)
                    nil))))
    (when op
      (setq op (logand op mask))
      `($ ,op))))

(defun op-i64 (ds bytemode sizeflag)
  (if (not (x86-ds-mode-64 ds))
    (op-i ds bytemode sizeflag)
    (let* ((mask -1)
           (op (cond ((= bytemode +b-mode+)
                      (setq mask #xff)
                      (x86-ds-next-u8 ds))
                     ((= bytemode +v-mode+)
                      (used-rex ds +rex-mode64+)
                      (setf (x86-ds-used-prefixes ds)
                            (logior (x86-ds-prefixes ds) +prefix-data+)) 
                      (if (logtest (x86-ds-rex ds) +rex-mode64+)
                        (x86-ds-next-u64 ds)
                        (if (logtest sizeflag +dflag+)
                          (progn
                            (setq mask #xffffffff)
                            (x86-ds-next-u32 ds))
                          (progn
                            (setq mask #xfffff)
                            (x86-ds-next-u16 ds)))))
                     ((= bytemode +w-mode+)
                      (setq mask #xfffff)
                      (x86-ds-next-u16 ds))
                     (t (error "Disassembly error")))))
      (when op
        (setq op (logand op mask))
        `($ ,op)))))

(defun op-si (ds bytemode sizeflag)
  (let* ((op
          (cond ((= bytemode +b-mode+)
                 (x86-ds-next-s8 ds))
                ((= bytemode +v-mode+)
                 (used-rex ds +rex-mode64+)
                 (setf (x86-ds-used-prefixes ds)
                            (logior (x86-ds-prefixes ds) +prefix-data+))
                 (if (logtest (x86-ds-rex ds) +rex-mode64+)
                   (x86-ds-next-s32 ds)
                   (if (logtest sizeflag +dflag+)
                     (x86-ds-next-s32 ds)
                     (x86-ds-next-s16 ds))))
                ((= bytemode +w-mode+)
                 (x86-ds-next-s16 ds))
                (t (error "Disassembly error")))))
    `($ ,op)))

(defun op-j (ds bytemode sizeflag)
  (let* ((mask -1)
         (start-codep (x86-ds-code-pointer ds))
         (disp (cond ((= bytemode +b-mode+)
                      (x86-ds-next-s8 ds))
                     ((= bytemode +v-mode+)
                      (if (logtest sizeflag +dflag+)
                        (x86-ds-next-s32 ds)
                        (progn
                          (setq mask #xffff)
                          (x86-ds-next-u16 ds))))
                     (t (error "Disassembly error"))))
         (label-address (logand (+ (- (+ (x86-ds-start-pc ds)
                                (x86-ds-code-pointer ds))
                             start-codep)
                          disp)
                       mask)))
    ;; Should finish the block and add a pending label address
    `(^ ,label-address)))

(defun op-seg (ds x y)
  (declare (ignore x y))
  `(% ,(x86::reg-entr-reg-name
        (x86::x86-segment-register (x86-ds-reg ds)))))

(defun op-dir (ds x sizeflag)
  (declare (ignore x))
  ;;; This basically sets two operands: an immediate segment number
  ;;; and  a displacement.  Need to figure out how to express that.
  (let* ((offset (if (logtest sizeflag +dflag+)
                   (x86-ds-next-u32 ds)
                   (x86-ds-next-u16 ds)))
         (seg (x86-ds-next-u16 ds)))
    `(? ($ ,seg) (@ ,offset))))

(defun op-off (ds x sizeflag)
  (declare (ignore x))
  (collect ((memop))
    (memop '@)
    (let* ((seg (segment-register-from-prefixes ds)))
      (when seg (memop seg))
      (memop (cond ((or (x86-ds-mode-64 ds)
                              (logtest sizeflag +aflag+))
                          (x86-ds-next-u32 ds))
                         (t (x86-ds-next-u16 ds))))
      (memop))))

(defun op-off64 (ds bytemode sizeflag)
  (if (not (x86-ds-mode-64 ds))
    (op-off ds bytemode sizeflag)
    (collect ((memop))
      (memop '@)
      (let* ((seg (segment-register-from-prefixes ds)))
        (when seg (memop seg))
      (memop (x86-ds-next-u64 ds))
      (memop)))))
           

(defun %ptr-reg (ds code sizeflag)
  (setf (x86-ds-used-prefixes ds)
        (logior (x86-ds-prefixes ds) +prefix-addr+))
  (let* ((idx (- code +eax-reg+))
         (r (if (x86-ds-mode-64 ds)
              (if (not (logtest sizeflag +aflag+))
                (x86::x86-reg32 idx)
                (x86::x86-reg64 idx))
              (if (logtest sizeflag +aflag+)
                (x86::x86-reg32 idx)
                (x86::x86-reg16 idx)))))
    `(% ,(x86::reg-entry-reg-name r))))

(defun op-esreg (ds code sizeflag)
  `(@ (% "es") ,(%ptr-reg ds code sizeflag)))
         
(defun op-dsreg (ds code sizeflag)
  (unless (logtest (x86-ds-prefixes ds)
                   (logior +prefix-cs+
                           +prefix-ds+
                           +prefix-ss+
                           +prefix-es+
                           +prefix-fs+
                           +prefix-gs+))
    (setf (x86-ds-prefixes ds)
          (logior (x86-ds-prefixes ds) +prefix-ds+)))
  (collect ((memop))
    (memop '@)
    (let* ((seg (segment-register-from-prefixes ds)))
      (when seg (memop seg)))
    (memop (%ptr-reg ds code sizeflag))
    (memop)))

(defun op-c (ds x sizeflag)
  (declare (ignore sizeflag))

  )

      
                           
                            
                                
                      
                   
             
                    
                    

                 
  
(defun make-x86-dis (opstring &optional
                             op1-fun
                             (op1-byte 0)
                             op2-fun
                             (op2-byte 0)
                             op3-fun
                             (op3-byte 0))
  (%make-x86-dis :mnemonic opstring
                 :op1 op1-fun
                 :bytemode1 op1-byte
                 :op2 op2-fun
                 :bytemode2 op2-byte
                 :op3 op3-fun
                 :bytemode3 op3-byte))
                             

;;; The root of all evil, unless the first byte of the opcode
;;; is #xf
(defparameter *disx86*
  (vector
   ;; #x00 
   (make-x86-dis "addB" 'op-e +b-mode+ 'op-g +b-mode+) 
   (make-x86-dis "addS" 'op-e +v-mode+ 'op-g +v-mode+) 
   (make-x86-dis "addB" 'op-g +b-mode+ 'op-e +b-mode+) 
   (make-x86-dis "addS" 'op-g +v-mode+ 'op-e +v-mode+) 
   (make-x86-dis "addB" 'op-imreg +al-reg+ 'op-i +b-mode+) 
   (make-x86-dis "addS" 'op-imreg +eax-reg+ 'op-i +v-mode+) 
   (make-x86-dis "push{T|)" 'op-reg +es-reg+) 
   (make-x86-dis "pop{T|)" 'op-reg +es-reg+) 
   ;; #x08 
   (make-x86-dis "orB" 'op-e +b-mode+ 'op-g +b-mode+) 
   (make-x86-dis "orS" 'op-e +v-mode+ 'op-g +v-mode+) 
   (make-x86-dis "orB" 'op-g +b-mode+ 'op-e +b-mode+) 
   (make-x86-dis "orS" 'op-g +v-mode+ 'op-e +v-mode+) 
   (make-x86-dis "orB" 'op-imreg +al-reg+ 'op-i +b-mode+) 
   (make-x86-dis "orS" 'op-imreg +eax-reg+ 'op-i +v-mode+) 
   (make-x86-dis "push{T|)" 'op-reg +cs-reg+) 
   (make-x86-dis "(bad)")               ; #x0f extended opcode escape 
   ;; #x10 
   (make-x86-dis "adcB" 'op-e +b-mode+ 'op-g +b-mode+) 
   (make-x86-dis "adcS" 'op-e +v-mode+ 'op-g +v-mode+) 
   (make-x86-dis "adcB" 'op-g +b-mode+ 'op-e +b-mode+) 
   (make-x86-dis "adcS" 'op-g +v-mode+ 'op-e +v-mode+) 
   (make-x86-dis "adcB" 'op-imreg +al-reg+ 'op-i +b-mode+) 
   (make-x86-dis "adcS" 'op-imreg +eax-reg+ 'op-i +v-mode+) 
   (make-x86-dis "push{T|)" 'op-reg +ss-reg+) 
   (make-x86-dis "popT|)" 'op-reg +ss-reg+) 
   ;; #x18 
   (make-x86-dis "sbbB" 'op-e +b-mode+ 'op-g +b-mode+) 
   (make-x86-dis "sbbS" 'op-e +v-mode+ 'op-g +v-mode+) 
   (make-x86-dis "sbbB" 'op-g +b-mode+ 'op-e +b-mode+) 
   (make-x86-dis "sbbS" 'op-g +v-mode+ 'op-e +v-mode+) 
   (make-x86-dis "sbbB" 'op-imreg +al-reg+ 'op-i +b-mode+) 
   (make-x86-dis "sbbS" 'op-imreg +eax-reg+ 'op-i +v-mode+) 
   (make-x86-dis "push{T|)" 'op-reg +ds-reg+) 
   (make-x86-dis "pop{T|)" 'op-reg +ds-reg+) 
   ;; #x20 
   (make-x86-dis "andB" 'op-e +b-mode+ 'op-g +b-mode+) 
   (make-x86-dis "andS" 'op-e +v-mode+ 'op-g +v-mode+) 
   (make-x86-dis "andB" 'op-g +b-mode+ 'op-e +b-mode+) 
   (make-x86-dis "andS" 'op-g +v-mode+ 'op-e +v-mode+) 
   (make-x86-dis "andB" 'op-imreg +al-reg+ 'op-i +b-mode+) 
   (make-x86-dis "andS" 'op-imreg +eax-reg+ 'op-i +v-mode+) 
   (make-x86-dis "(bad)")               ; SEG ES prefix 
   (make-x86-dis "daa{|)") 
   ;; #x28 
   (make-x86-dis "subB" 'op-e +b-mode+ 'op-g +b-mode+) 
   (make-x86-dis "subS" 'op-e +v-mode+ 'op-g +v-mode+) 
   (make-x86-dis "subB" 'op-g +b-mode+ 'op-e +b-mode+) 
   (make-x86-dis "subS" 'op-g +v-mode+ 'op-e +v-mode+) 
   (make-x86-dis "subB" 'op-imreg +al-reg+ 'op-i +b-mode+) 
   (make-x86-dis "subS" 'op-imreg +eax-reg+ 'op-i +v-mode+) 
   (make-x86-dis "(bad)")               ; SEG CS prefix 
   (make-x86-dis "das{|)") 
   ;; #x30 
   (make-x86-dis "xorB" 'op-e +b-mode+ 'op-g +b-mode+) 
   (make-x86-dis "xorS" 'op-e +v-mode+ 'op-g +v-mode+) 
   (make-x86-dis "xorB" 'op-g +b-mode+ 'op-e +b-mode+) 
   (make-x86-dis "xorS" 'op-g +v-mode+ 'op-e +v-mode+) 
   (make-x86-dis "xorB" 'op-imreg +al-reg+ 'op-i +b-mode+) 
   (make-x86-dis "xorS" 'op-imreg +eax-reg+ 'op-i +v-mode+) 
   (make-x86-dis "(bad)")               ; SEG SS prefix 
   (make-x86-dis "aaa{|)") 
   ;; #x38 
   (make-x86-dis "cmpB" 'op-e +b-mode+ 'op-g +b-mode+) 
   (make-x86-dis "cmpS" 'op-e +v-mode+ 'op-g +v-mode+) 
   (make-x86-dis "cmpB" 'op-g +b-mode+ 'op-e +b-mode+) 
   (make-x86-dis "cmpS" 'op-g +v-mode+ 'op-e +v-mode+) 
   (make-x86-dis "cmpB" 'op-imreg +al-reg+ 'op-i +b-mode+) 
   (make-x86-dis "cmpS" 'op-imreg +eax-reg+ 'op-i +v-mode+) 
   (make-x86-dis "(bad)")               ; SEG DS prefix 
   (make-x86-dis "aas{|)") 
   ;; #x40 
   (make-x86-dis "inc{S|)" 'op-reg +eax-reg+) 
   (make-x86-dis "inc{S|)" 'op-reg +ecx-reg+) 
   (make-x86-dis "inc{S|)" 'op-reg +edx-reg+) 
   (make-x86-dis "inc{S|)" 'op-reg +ebx-reg+) 
   (make-x86-dis "inc{S|)" 'op-reg +esp-reg+) 
   (make-x86-dis "inc{S|)" 'op-reg +ebp-reg+) 
   (make-x86-dis "inc{S|)" 'op-reg +esi-reg+) 
   (make-x86-dis "inc{S|)" 'op-reg +edi-reg+) 
   ;; #x48 
   (make-x86-dis "dec{S|)" 'op-reg +eax-reg+) 
   (make-x86-dis "dec{S|)" 'op-reg +ecx-reg+) 
   (make-x86-dis "dec{S|)" 'op-reg +edx-reg+) 
   (make-x86-dis "dec{S|)" 'op-reg +ebx-reg+) 
   (make-x86-dis "dec{S|)" 'op-reg +esp-reg+) 
   (make-x86-dis "dec{S|)" 'op-reg +ebp-reg+) 
   (make-x86-dis "dec{S|)" 'op-reg +esi-reg+) 
   (make-x86-dis "dec{S|)" 'op-reg +edi-reg+) 
   ;; #x50 
   (make-x86-dis "pushS" 'op-reg +rax-reg+) 
   (make-x86-dis "pushS" 'op-reg +rcx-reg+) 
   (make-x86-dis "pushS" 'op-reg +rdx-reg+) 
   (make-x86-dis "pushS" 'op-reg +rbx-reg+) 
   (make-x86-dis "pushS" 'op-reg +rsp-reg+) 
   (make-x86-dis "pushS" 'op-reg +rbp-reg+) 
   (make-x86-dis "pushS" 'op-reg +rsi-reg+) 
   (make-x86-dis "pushS" 'op-reg +rdi-reg+) 
   ;; #x58 
   (make-x86-dis "popS" 'op-reg +rax-reg+) 
   (make-x86-dis "popS" 'op-reg +rcx-reg+) 
   (make-x86-dis "popS" 'op-reg +rdx-reg+) 
   (make-x86-dis "popS" 'op-reg +rbx-reg+) 
   (make-x86-dis "popS" 'op-reg +rsp-reg+) 
   (make-x86-dis "popS" 'op-reg +rbp-reg+) 
   (make-x86-dis "popS" 'op-reg +rsi-reg+) 
   (make-x86-dis "popS" 'op-reg +rdi-reg+) 
   ;; #x60 
   (make-x86-dis "pusha{P|)") 
   (make-x86-dis "popa{P|)") 
   (make-x86-dis "bound{S|)" 'op-g +v-mode+ 'op-e +v-mode+) 
   (make-x86-dis nil nil +x86-64-special+) 
   (make-x86-dis "(bad)")               ; seg fs 
   (make-x86-dis "(bad)")               ; seg gs 
   (make-x86-dis "(bad)")               ; op size prefix 
   (make-x86-dis "(bad)")               ; adr size prefix 
   ;; #x68 
   (make-x86-dis "pushT" 'op-i +q-mode+) 
   (make-x86-dis "imulS" 'op-g +v-mode+ 'op-e +v-mode+ 'op-i +v-mode+ ) 
   (make-x86-dis "pushT" 'op-si +b-mode+) 
   (make-x86-dis "imulS" 'op-g +v-mode+ 'op-e +v-mode+ 'op-si +b-mode+ ) 
   (make-x86-dis "ins{b||b|)" 'op-dsreg +esi-reg+ 'op-imreg +indir-dx-reg+) 
   (make-x86-dis "ins{R||R|)" 'op-esreg +edi-reg+ 'op-imreg +indir-dx-reg+) 
   (make-x86-dis "outs{b||b|)" 'op-imreg +indir-dx-reg+ 'op-dsreg +esi-reg+) 
   (make-x86-dis "outs{R||R|)" 'op-imreg +indir-dx-reg+ 'op-dsreg +esi-reg+) 
   ;; #x70 
   (make-x86-dis "joH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jnoH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jbH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jaeH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jeH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jneH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jbeH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jaH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   ;; #x78 
   (make-x86-dis "jsH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jnsH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jpH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jnpH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jlH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jgeH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jleH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   (make-x86-dis "jgH" 'op-j +b-mode+ nil +cond-jump-mode+ ) 
   ;; #x80 
   (make-x86-dis nil nil +use-groups+) 
   (make-x86-dis nil nil +use-groups+ nil 1) 
   (make-x86-dis "(bad)") 
   (make-x86-dis nil nil +use-groups+ nil 2 ) 
   (make-x86-dis "testB" 'op-e +b-mode+ 'op-g +b-mode+) 
   (make-x86-dis "testS" 'op-e +v-mode+ 'op-g +v-mode+) 
   (make-x86-dis "xchgB" 'op-e +b-mode+ 'op-g +b-mode+) 
   (make-x86-dis "xchgS" 'op-e +v-mode+ 'op-g +v-mode+) 
   ;; #x88 
   (make-x86-dis "movB" 'op-e +b-mode+ 'op-g +b-mode+) 
   (make-x86-dis "movS" 'op-e +v-mode+ 'op-g +v-mode+) 
   (make-x86-dis "movB" 'op-g +b-mode+ 'op-e +b-mode+) 
   (make-x86-dis "movS" 'op-g +v-mode+ 'op-e +v-mode+) 
   (make-x86-dis "movQ" 'op-e +v-mode+ 'op-seg +w-mode+) 
   (make-x86-dis "leaS" 'op-g +v-mode+ 'op-m 0) 
   (make-x86-dis "movQ" 'op-seg +w-mode+ 'op-e +v-mode+) 
   (make-x86-dis "popU" 'op-e +v-mode+) 
   ;; #x90 
   (make-x86-dis "nop" 'nop-fixup 0) 
   (make-x86-dis "xchgS" 'op-reg +ecx-reg+ 'op-imreg +eax-reg+) 
   (make-x86-dis "xchgS" 'op-reg +edx-reg+ 'op-imreg +eax-reg+) 
   (make-x86-dis "xchgS" 'op-reg +ebx-reg+ 'op-imreg +eax-reg+) 
   (make-x86-dis "xchgS" 'op-reg +esp-reg+ 'op-imreg +eax-reg+) 
   (make-x86-dis "xchgS" 'op-reg +ebp-reg+ 'op-imreg +eax-reg+) 
   (make-x86-dis "xchgS" 'op-reg +esi-reg+ 'op-imreg +eax-reg+) 
   (make-x86-dis "xchgS" 'op-reg +edi-reg+ 'op-imreg +eax-reg+) 
   ;; #x98 
   (make-x86-dis "cW{tR||tR|)") 
   (make-x86-dis "cR{tO||tO|)") 
   (make-x86-dis "Jcall{T|)" 'op-dir 0) 
   (make-x86-dis "(bad)")               ; fwait 
   (make-x86-dis "pushfT") 
   (make-x86-dis "popfT") 
   (make-x86-dis "sahf{|)") 
   (make-x86-dis "lahf{|)") 
   ;; #xa0 
   (make-x86-dis "movB" 'op-imreg +al-reg+ 'op-off64 +b-mode+) 
   (make-x86-dis "movS" 'op-imreg +eax-reg+ 'op-off64 +v-mode+) 
   (make-x86-dis "movB" 'op-off64 +b-mode+  'op-imreg +al-reg+) 
   (make-x86-dis "movS" 'op-off64 +v-mode+ 'op-imreg +eax-reg+) 
   (make-x86-dis "movs{b||b|)" 'op-dsreg +esi-reg+ 'op-dsreg +esi-reg+) 
   (make-x86-dis "movs{R||R|)" 'op-esreg +edi-reg+ 'op-dsreg +esi-reg+) 
   (make-x86-dis "cmps{b||b|)" 'op-dsreg +esi-reg+ 'op-dsreg +esi-reg+) 
   (make-x86-dis "cmps{R||R|)" 'op-dsreg +esi-reg+ 'op-esreg +edi-reg+) 
   ;; #xa8 
   (make-x86-dis "testB" 'op-imreg +al-reg+ 'op-i +b-mode+) 
   (make-x86-dis "testS" 'op-imreg +eax-reg+ 'op-i +v-mode+) 
   (make-x86-dis "stosB" 'op-dsreg +esi-reg+ 'op-imreg +al-reg+) 
   (make-x86-dis "stosS" 'op-esreg +edi-reg+ 'op-imreg +eax-reg+) 
   (make-x86-dis "lodsB" 'op-imreg +al-reg+ 'op-dsreg +esi-reg+) 
   (make-x86-dis "lodsS" 'op-imreg +eax-reg+ 'op-dsreg +esi-reg+) 
   (make-x86-dis "scasB" 'op-imreg +al-reg+ 'op-dsreg +esi-reg+) 
   (make-x86-dis "scasS" 'op-imreg +eax-reg+ 'op-esreg +edi-reg+) 
   ;; #xb0 
   (make-x86-dis "movB" 'op-reg +al-reg+ 'op-i +b-mode+) 
   (make-x86-dis "movB" 'op-reg +cl-reg+ 'op-i +b-mode+) 
   (make-x86-dis "movB" 'op-reg +dl-reg+ 'op-i +b-mode+) 
   (make-x86-dis "movB" 'op-reg +bl-reg+ 'op-i +b-mode+) 
   (make-x86-dis "movB" 'op-reg +ah-reg+ 'op-i +b-mode+) 
   (make-x86-dis "movB" 'op-reg +ch-reg+ 'op-i +b-mode+) 
   (make-x86-dis "movB" 'op-reg +dh-reg+ 'op-i +b-mode+) 
   (make-x86-dis "movB" 'op-reg +bh-reg+ 'op-i +b-mode+) 
   ;; #xb8 
   (make-x86-dis "movS" 'op-reg +eax-reg+ 'op-i64 +v-mode+) 
   (make-x86-dis "movS" 'op-reg +ecx-reg+ 'op-i64 +v-mode+) 
   (make-x86-dis "movS" 'op-reg +edx-reg+ 'op-i64 +v-mode+) 
   (make-x86-dis "movS" 'op-reg +ebx-reg+ 'op-i64 +v-mode+) 
   (make-x86-dis "movS" 'op-reg +esp-reg+ 'op-i64 +v-mode+) 
   (make-x86-dis "movS" 'op-reg +ebp-reg+ 'op-i64 +v-mode+) 
   (make-x86-dis "movS" 'op-reg +esi-reg+ 'op-i64 +v-mode+) 
   (make-x86-dis "movS" 'op-reg +edi-reg+ 'op-i64 +v-mode+) 
   ;; #xc0 
   (make-x86-dis nil nil +use-groups+ nil 3) 
   (make-x86-dis nil nil +use-groups+ nil 4) 
   (make-x86-dis "retT" 'op-i +w-mode+) 
   (make-x86-dis "retT") 
   (make-x86-dis "les{S|)" 'op-g +v-mode+ 'op-m +f-mode+) 
   (make-x86-dis "ldsS" 'op-g +v-mode+ 'op-m +f-mode+) 
   (make-x86-dis "movA" 'op-e +b-mode+ 'op-i +b-mode+) 
   (make-x86-dis "movQ" 'op-e +v-mode+ 'op-i +v-mode+) 
   ;; #xc8 
   (make-x86-dis "enterT" 'op-i +w-mode+ 'op-i +b-mode+) 
   (make-x86-dis "leaveT") 
   (make-x86-dis "lretP" 'op-i +w-mode+) 
   (make-x86-dis "lretP") 
   (make-x86-dis "int3") 
   (make-x86-dis "int" 'op-i +b-mode+) 
   (make-x86-dis "into{|)") 
   (make-x86-dis "iretP") 
   ;; #xd0 
   (make-x86-dis nil nil +use-groups+ nil 5) 
   (make-x86-dis nil nil +use-groups+ nil 6) 
   (make-x86-dis nil nil +use-groups+ nil 7) 
   (make-x86-dis nil nil +use-groups+ nil 5) 
   (make-x86-dis "aam{|)" 'op-si +b-mode+) 
   (make-x86-dis "aad{|)" 'op-si +b-mode+) 
   (make-x86-dis "(bad)") 
   (make-x86-dis "xlat" 'op-dsreg +ebx-reg+) 
   ;; #xd8 
   (make-x86-dis nil nil +floatcode+) 
   (make-x86-dis nil nil +floatcode+) 
   (make-x86-dis nil nil +floatcode+) 
   (make-x86-dis nil nil +floatcode+) 
   (make-x86-dis nil nil +floatcode+) 
   (make-x86-dis nil nil +floatcode+) 
   (make-x86-dis nil nil +floatcode+) 
   (make-x86-dis nil nil +floatcode+) 
   ;; #xe0 
   (make-x86-dis "loopneFH" 'op-j +b-mode+ nil +loop-jcxz-mode+ ) 
   (make-x86-dis "loopeFH" 'op-j +b-mode+ nil +loop-jcxz-mode+ ) 
   (make-x86-dis "loopFH" 'op-j +b-mode+ nil +loop-jcxz-mode+ ) 
   (make-x86-dis "jEcxzH" 'op-j +b-mode+ nil +loop-jcxz-mode+ ) 
   (make-x86-dis "inB" 'op-imreg +al-reg+ 'op-i +b-mode+) 
   (make-x86-dis "inS" 'op-imreg +eax-reg+ 'op-i +b-mode+) 
   (make-x86-dis "outB" 'op-i +b-mode+ 'op-imreg +al-reg+) 
   (make-x86-dis "outS" 'op-i +b-mode+ 'op-imreg +eax-reg+) 
   ;; #xe8 
   (make-x86-dis "callT" 'op-j +v-mode+) 
   (make-x86-dis "jmpT" 'op-j +v-mode+) 
   (make-x86-dis "Jjmp{T|)" 'op-dir 0) 
   (make-x86-dis "jmp" 'op-j +b-mode+) 
   (make-x86-dis "inB" 'op-imreg +al-reg+ 'op-imreg +indir-dx-reg+) 
   (make-x86-dis "inS" 'op-imreg +eax-reg+ 'op-imreg +indir-dx-reg+) 
   (make-x86-dis "outB" 'op-imreg +indir-dx-reg+ 'op-imreg +al-reg+) 
   (make-x86-dis "outS" 'op-imreg +indir-dx-reg+ 'op-imreg +eax-reg+) 
   ;; #xf0 
   (make-x86-dis "(bad)")               ; lock prefix 
   (make-x86-dis "icebp") 
   (make-x86-dis "(bad)")               ; repne 
   (make-x86-dis "(bad)")               ; repz 
   (make-x86-dis "hlt") 
   (make-x86-dis "cmc") 
   (make-x86-dis nil nil +use-groups+ nil 9) 
   (make-x86-dis nil nil +use-groups+ nil 10) 
   ;; #xf8 
   (make-x86-dis "clc") 
   (make-x86-dis "stc") 
   (make-x86-dis "cli") 
   (make-x86-dis "sti") 
   (make-x86-dis "cld") 
   (make-x86-dis "std") 
   (make-x86-dis nil nil +use-groups+ nil 11) 
   (make-x86-dis nil nil +use-groups+ nil 12) 
   ))

(defparameter *disx86-twobyte*
  (vector
   ;; #x00 
   (make-x86-dis nil nil +use-groups+ nil 13)
   (make-x86-dis nil nil +use-groups+ nil 14) 
   (make-x86-dis "larS" 'op-g +v-mode+ 'op-e +w-mode+)
   (make-x86-dis "lslS" 'op-g +v-mode+ 'op-e +w-mode+)
   (make-x86-dis "(bad)")
   (make-x86-dis "syscall")
   (make-x86-dis "clts")
   (make-x86-dis "sysretP")
   ;; #x08 
   (make-x86-dis "invd")
   (make-x86-dis "wbinvd")
   (make-x86-dis "(bad)")
   (make-x86-dis "ud2a")
   (make-x86-dis "(bad)")
   (make-x86-dis nil nil +use-groups+ nil 22) 
   (make-x86-dis "femms")
   (make-x86-dis "" 'op-mmx 0 'op-em +v-mode+ 'op-3dnowsuffix 0) ; See OP-3DNowSuffix. 
   ;; #x10 
   (make-x86-dis nil nil +use-prefix-user-table+ nil 8)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 9) 
   (make-x86-dis nil nil +use-prefix-user-table+ nil 30)
   (make-x86-dis "movlpX" 'op-ex +v-mode+ 'op-xmm 0 'SIMD-Fixup #\h)
   (make-x86-dis "unpcklpX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis "unpckhpX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 31)
   (make-x86-dis "movhpX" 'op-ex +v-mode+ 'op-xmm 0 'SIMD-Fixup #\l)
   ;; #x18 
   (make-x86-dis nil nil +use-groups+ nil 21)
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   ;; #x20 
   (make-x86-dis "movL" 'op-rd +m-mode+ 'op-c +m-mode+)
   (make-x86-dis "movL" 'op-rd +m-mode+ 'op-d +m-mode+)
   (make-x86-dis "movL" 'op-c +m-mode+ 'op-rd +m-mode+)
   (make-x86-dis "movL" 'op-d +m-mode+ 'op-rd +m-mode+)
   (make-x86-dis "movL" 'op-rd +d-mode+ 'op-t +d-mode+)
   (make-x86-dis "(bad)")
   (make-x86-dis "movL" 'op-t +d-mode+ 'op-rd +d-mode+)
   (make-x86-dis "(bad)")
   ;; #x28 
   (make-x86-dis "movapX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis "movapX" 'op-ex +v-mode+ 'op-xmm 0)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 2)
   (make-x86-dis "movntpX" 'op-e +v-mode+ 'op-xmm 0)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 4)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 3)
   (make-x86-dis "ucomisX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis "comisX" 'op-xmm 0 'op-ex +v-mode+)
   ;; #x30 
   (make-x86-dis "wrmsr")
   (make-x86-dis "rdtsc")
   (make-x86-dis "rdmsr")
   (make-x86-dis "rdpmc")
   (make-x86-dis "sysenter")
   (make-x86-dis "sysexit")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   ;; #x38 
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   ;; #x40 
   (make-x86-dis "cmovo" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovno" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovb" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovae" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmove" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovne" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovbe" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmova" 'op-g +v-mode+ 'op-e +v-mode+)
   ;; #x48 
   (make-x86-dis "cmovs" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovns" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovp" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovnp" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovl" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovge" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovle" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "cmovg" 'op-g +v-mode+ 'op-e +v-mode+)
   ;; #x50 
   (make-x86-dis "movmskpX" 'op-g +dq-mode+ 'op-xs +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 13)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 12)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 11)
   (make-x86-dis "andpX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis "andnpX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis "orpX" 'op-xmm 0 'op-ex +v-mode+)
   (make-x86-dis "xorpX" 'op-xmm 0 'op-ex +v-mode+)
   ;; #x58 
   (make-x86-dis nil nil +use-prefix-user-table+ nil 0)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 10)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 17)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 16)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 14)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 7)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 5)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 6)
   ;; #x60 
   (make-x86-dis "punpcklbw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "punpcklwd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "punpckldq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "packsswb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pcmpgtb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pcmpgtw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pcmpgtd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "packuswb" 'op-mmx 0 'op-em +v-mode+)
   ;; #x68 
   (make-x86-dis "punpckhbw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "punpckhwd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "punpckhdq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "packssdw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 26)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 24)
   (make-x86-dis "movd" 'op-mmx 0 'op-e +dq-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 19)
   ;; #x70 
   (make-x86-dis nil nil +use-prefix-user-table+ nil 22)
   (make-x86-dis nil nil +use-groups+ nil 17)
   (make-x86-dis nil nil +use-groups+ nil 18)
   (make-x86-dis nil nil +use-groups+ nil 19)
   (make-x86-dis "pcmpeqb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pcmpeqw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pcmpeqd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "emms")
   ;; #x78 
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis "(bad)")
   (make-x86-dis nil nil +use-prefix-user-table+ nil 28)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 29)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 23)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 20)
   ;; #x80 
   (make-x86-dis "joH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jnoH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jbH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jaeH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jeH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jneH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jbeH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jaH" 'op-j +v-mode+ nil +cond-jump-mode+)
   ;; #x88 
   (make-x86-dis "jsH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jnsH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jpH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jnpH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jlH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jgeH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jleH" 'op-j +v-mode+ nil +cond-jump-mode+)
   (make-x86-dis "jgH" 'op-j +v-mode+ nil +cond-jump-mode+)
   ;; #x90 
   (make-x86-dis "seto" 'op-e +b-mode+)
   (make-x86-dis "setno" 'op-e +b-mode+)
   (make-x86-dis "setb" 'op-e +b-mode+)
   (make-x86-dis "setae" 'op-e +b-mode+)
   (make-x86-dis "sete" 'op-e +b-mode+)
   (make-x86-dis "setne" 'op-e +b-mode+)
   (make-x86-dis "setbe" 'op-e +b-mode+)
   (make-x86-dis "seta" 'op-e +b-mode+)
   ;; #x98 
   (make-x86-dis "sets" 'op-e +b-mode+)
   (make-x86-dis "setns" 'op-e +b-mode+)
   (make-x86-dis "setp" 'op-e +b-mode+)
   (make-x86-dis "setnp" 'op-e +b-mode+)
   (make-x86-dis "setl" 'op-e +b-mode+)
   (make-x86-dis "setge" 'op-e +b-mode+)
   (make-x86-dis "setle" 'op-e +b-mode+)
   (make-x86-dis "setg" 'op-e +b-mode+)
   ;; #xa0 
   (make-x86-dis "pushT" 'op-reg +fs-reg+)
   (make-x86-dis "popT" 'op-reg +fs-reg+)
   (make-x86-dis "cpuid")
   (make-x86-dis "btS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "shldS" 'op-e +v-mode+ 'op-g +v-mode+ 'op-i +b-mode+)
   (make-x86-dis "shldS" 'op-e +v-mode+ 'op-g +v-mode+ 'op-imreg +cl-reg+)
   (make-x86-dis nil nil +use-groups+ nil 24)
   (make-x86-dis nil nil +use-groups+ nil 23)
   ;; #xa8 
   (make-x86-dis "pushT" 'op-reg +gs-reg+)
   (make-x86-dis "popT" 'op-reg +gs-reg+)
   (make-x86-dis "rsm")
   (make-x86-dis "btsS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "shrdS" 'op-e +v-mode+ 'op-g +v-mode+ 'op-i +b-mode+)
   (make-x86-dis "shrdS" 'op-e +v-mode+ 'op-g +v-mode+ 'op-imreg +cl-reg+)
   (make-x86-dis nil nil +use-groups+ nil 20)
   (make-x86-dis "imulS" 'op-g +v-mode+ 'op-e +v-mode+)
   ;; #xb0 
   (make-x86-dis "cmpxchgB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "cmpxchgS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "lssS" 'op-g +v-mode+ 'op-m +f-mode+)
   (make-x86-dis "btrS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "lfsS" 'op-g +v-mode+ 'op-m +f-mode+)
   (make-x86-dis "lgsS" 'op-g +v-mode+ 'op-m +f-mode+)
   (make-x86-dis "movz{bR|x|bR|x}" 'op-g +v-mode+ 'op-e +b-mode+)
   (make-x86-dis "movz{wR|x|wR|x}" 'op-g +v-mode+ 'op-e +w-mode+) ; yes there really is movzww ! 
   ;; #xb8 
   (make-x86-dis "(bad)")
   (make-x86-dis "ud2b")
   (make-x86-dis nil nil +use-groups+ nil 15)
   (make-x86-dis "btcS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "bsfS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "bsrS" 'op-g +v-mode+ 'op-e +v-mode+)
   (make-x86-dis "movs{bR|x|bR|x}" 'op-g +v-mode+ 'op-e +b-mode+)
   (make-x86-dis "movs{wR|x|wR|x}" 'op-g +v-mode+ 'op-e +w-mode+) ; yes there really is movsww ! 
   ;; #xc0 
   (make-x86-dis "xaddB" 'op-e +b-mode+ 'op-g +b-mode+)
   (make-x86-dis "xaddS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 1)
   (make-x86-dis "movntiS" 'op-e +v-mode+ 'op-g +v-mode+)
   (make-x86-dis "pinsrw" 'op-mmx 0 'op-e +dqw-mode+ 'op-i +b-mode+)
   (make-x86-dis "pextrw" 'op-g +dq-mode+ 'op-ms +v-mode+ 'op-i +b-mode+)
   (make-x86-dis "shufpX" 'op-xmm 0 'op-ex +v-mode+ 'op-i +b-mode+)
   (make-x86-dis nil nil +use-groups+ nil 16)
   ;; #xc8 
   (make-x86-dis "bswap" 'op-reg +eax-reg+)
   (make-x86-dis "bswap" 'op-reg +ecx-reg+)
   (make-x86-dis "bswap" 'op-reg +edx-reg+)
   (make-x86-dis "bswap" 'op-reg +ebx-reg+)
   (make-x86-dis "bswap" 'op-reg +esp-reg+)
   (make-x86-dis "bswap" 'op-reg +ebp-reg+)
   (make-x86-dis "bswap" 'op-reg +esi-reg+)
   (make-x86-dis "bswap" 'op-reg +edi-reg+)
   ;; #xd0 
   (make-x86-dis nil nil +use-prefix-user-table+ nil 27)
   (make-x86-dis "psrlw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psrld" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psrlq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmullw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 21)
   (make-x86-dis "pmovmskb" 'op-g +dq-mode+ 'op-ms +v-mode+)
   ;; #xd8 
   (make-x86-dis "psubusb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psubusw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pminub" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pand" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddusb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddusw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmaxub" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pandn" 'op-mmx 0 'op-em +v-mode+)
   ;; #xe0 
   (make-x86-dis "pavgb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psraw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psrad" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pavgw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmulhuw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmulhw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 15)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 25)
   ;; #xe8 
   (make-x86-dis "psubsb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psubsw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pminsw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "por" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddsb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddsw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmaxsw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pxor" 'op-mmx 0 'op-em +v-mode+)
   ;; #xf0 
   (make-x86-dis nil nil +use-prefix-user-table+ nil 32)
   (make-x86-dis "psllw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pslld" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psllq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmuludq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "pmaddwd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psadbw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis nil nil +use-prefix-user-table+ nil 18)
   ;; #xf8 
   (make-x86-dis "psubb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psubw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psubd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "psubq" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddb" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddw" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "paddd" 'op-mmx 0 'op-em +v-mode+)
   (make-x86-dis "(bad)")
   ))

(defparameter *onebyte-has-modrm*
  (make-array 256 :element-type 'bit
              :initial-contents '(
  #|       0 1 2 3 4 5 6 7 8 9 a b c d e f        |#
  #|       -------------------------------        |#
  #| 00 |# 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0  #| 00 |#
  #| 10 |# 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0  #| 10 |#
  #| 20 |# 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0  #| 20 |#
  #| 30 |# 1 1 1 1 0 0 0 0 1 1 1 1 0 0 0 0  #| 30 |#
  #| 40 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 40 |#
  #| 50 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 50 |#
  #| 60 |# 0 0 1 1 0 0 0 0 0 1 0 1 0 0 0 0  #| 60 |#
  #| 70 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 70 |#
  #| 80 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| 80 |#
  #| 90 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 90 |#
  #| a0 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| a0 |#
  #| b0 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| b0 |#
  #| c0 |# 1 1 0 0 1 1 1 1 0 0 0 0 0 0 0 0  #| c0 |#
  #| d0 |# 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1  #| d0 |#
  #| e0 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| e0 |#
  #| f0 |# 0 0 0 0 0 0 1 1 0 0 0 0 0 0 1 1  #| f0 |#
  #|       -------------------------------        |#
  #|       0 1 2 3 4 5 6 7 8 9 a b c d e f        |#
)))

(defparameter *twobyte-has-modrm*
  (make-array 256 :element-type 'bit
              :initial-contents '(
  #|       0 1 2 3 4 5 6 7 8 9 a b c d e f        |#
  #|       -------------------------------        |#
  #| 00 |# 1 1 1 1 0 0 0 0 0 0 0 0 0 1 0 1  #| 0f |#
  #| 10 |# 1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0  #| 1f |#
  #| 20 |# 1 1 1 1 1 0 1 0 1 1 1 1 1 1 1 1  #| 2f |#
  #| 30 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 3f |#
  #| 40 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| 4f |#
  #| 50 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| 5f |#
  #| 60 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| 6f |#
  #| 70 |# 1 1 1 1 1 1 1 0 0 0 0 0 1 1 1 1  #| 7f |#
  #| 80 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 8f |#
  #| 90 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| 9f |#
  #| a0 |# 0 0 0 1 1 1 1 1 0 0 0 1 1 1 1 1  #| af |#
  #| b0 |# 1 1 1 1 1 1 1 1 0 0 1 1 1 1 1 1  #| bf |#
  #| c0 |# 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0  #| cf |#
  #| d0 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| df |#
  #| e0 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1  #| ef |#
  #| f0 |# 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 0  #| ff |#
  #|       -------------------------------        |#
  #|       0 1 2 3 4 5 6 7 8 9 a b c d e f        |#
)))

(defparameter *twobyte-uses-sse-prefix*
  (make-array 256 :element-type 'bit
              :initial-contents '(
  #|       0 1 2 3 4 5 6 7 8 9 a b c d e f        |#
  #|       -------------------------------        |#
  #| 00 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 0f |#
  #| 10 |# 1 1 1 0 0 0 1 0 0 0 0 0 0 0 0 0  #| 1f |#
  #| 20 |# 0 0 0 0 0 0 0 0 0 0 1 0 1 1 0 0  #| 2f |#
  #| 30 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 3f |#
  #| 40 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 4f |#
  #| 50 |# 0 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1  #| 5f |#
  #| 60 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 1  #| 6f |#
  #| 70 |# 1 0 0 0 0 0 0 0 0 0 0 0 1 1 1 1  #| 7f |#
  #| 80 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 8f |#
  #| 90 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| 9f |#
  #| a0 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| af |#
  #| b0 |# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0  #| bf |#
  #| c0 |# 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0  #| cf |#
  #| d0 |# 1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0  #| df |#
  #| e0 |# 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0  #| ef |#
  #| f0 |# 1 0 0 0 0 0 0 1 0 0 0 0 0 0 0 0  #| ff |#
  #|       -------------------------------        |#
  #|       0 1 2 3 4 5 6 7 8 9 a b c d e f        |#
)))



(defparameter *grps*
  (vector
   ;; GRP1b 
   (vector
    (make-x86-dis "addA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "orA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "adcA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "sbbA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "andA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "subA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "xorA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "cmpA" 'op-e +b-mode+ 'op-i +b-mode+))
   ;; GRP1S 
   (vector
    (make-x86-dis "addQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "orQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "adcQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "sbbQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "andQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "subQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "xorQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "cmpQ" 'op-e +v-mode+ 'op-i +v-mode+))
   ;; GRP1Ss 
   (vector
    (make-x86-dis "addQ" 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "orQ" 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "adcQ" 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "sbbQ" 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "andQ" 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "subQ" 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "xorQ" 'op-e +v-mode+ 'op-si +b-mode+)
    (make-x86-dis "cmpQ" 'op-e +v-mode+ 'op-si +b-mode+))
   ;; GRP2b 
   (vector
    (make-x86-dis "rolA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "rorA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "rclA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "rcrA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "shlA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "shrA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "sarA" 'op-e +b-mode+ 'op-i +b-mode+))
   ;; GRP2S 
   (vector
    (make-x86-dis "rolQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "rorQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "rclQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "rcrQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "shlQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "shrQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "sarQ" 'op-e +v-mode+ 'op-i +b-mode+))
   ;; GRP2b-one 
   (vector
    (make-x86-dis "rolA" 'op-e +b-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "rorA" 'op-e +b-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "rclA" 'op-e +b-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "rcrA" 'op-e +b-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "shlA" 'op-e +b-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "shrA" 'op-e +b-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "sarA" 'op-e +b-mode+ 'op-i +const-1-mode+))
   ;; GRP2S-one 
   (vector
    (make-x86-dis "rolQ" 'op-e +v-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "rorQ" 'op-e +v-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "rclQ" 'op-e +v-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "rcrQ" 'op-e +v-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "shlQ" 'op-e +v-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "shrQ" 'op-e +v-mode+ 'op-i +const-1-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "sarQ" 'op-e +v-mode+ 'op-i +const-1-mode+))
   ;; GRP2b-cl 
   (vector
    (make-x86-dis "rolA" 'op-e +b-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "rorA" 'op-e +b-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "rclA" 'op-e +b-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "rcrA" 'op-e +b-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "shlA" 'op-e +b-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "shrA" 'op-e +b-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "(bad)")
    (make-x86-dis "sarA" 'op-e +b-mode+ 'op-imreg +cl-reg+))
   ;; GRP2S-cl 
   (vector
    (make-x86-dis "rolQ" 'op-e +v-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "rorQ" 'op-e +v-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "rclQ" 'op-e +v-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "rcrQ" 'op-e +v-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "shlQ" 'op-e +v-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "shrQ" 'op-e +v-mode+ 'op-imreg +cl-reg+)
    (make-x86-dis "(bad)")
    (make-x86-dis "sarQ" 'op-e +v-mode+ 'op-imreg +cl-reg+))
   ;; GRP3b 
   (vector
    (make-x86-dis "testA" 'op-e +b-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)" 'op-e +b-mode+)
    (make-x86-dis "notA" 'op-e +b-mode+)
    (make-x86-dis "negA" 'op-e +b-mode+)
    (make-x86-dis "mulA" 'op-e +b-mode+)            ; Don't print the implicit %al register 
    (make-x86-dis "imulA" 'op-e +b-mode+)           ; to distinguish these opcodes from other 
    (make-x86-dis "divA" 'op-e +b-mode+)            ; mul/imul opcodes. Do the same for div 
    (make-x86-dis "idivA" 'op-e +b-mode+)           ; and idiv for consistency.
    )
   ;; GRP3S 
   (vector
    (make-x86-dis "testQ" 'op-e +v-mode+ 'op-i +v-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "notQ" 'op-e +v-mode+)
    (make-x86-dis "negQ" 'op-e +v-mode+)
    (make-x86-dis "mulQ" 'op-e +v-mode+)            ; Don't print the implicit register. 
    (make-x86-dis "imulQ" 'op-e +v-mode+)
    (make-x86-dis "divQ" 'op-e +v-mode+)
    (make-x86-dis "idivQ" 'op-e +v-mode+))
   ;; GRP4 
   (vector
    (make-x86-dis "incA" 'op-e +b-mode+)
    (make-x86-dis "decA" 'op-e +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)"))
   ;; GRP5 
   (vector
    (make-x86-dis "incQ" 'op-e +v-mode+)
    (make-x86-dis "decQ" 'op-e +v-mode+)
    (make-x86-dis "callT" 'op-indire +v-mode+)
    (make-x86-dis "JcallT" 'op-indire +f-mode+)
    (make-x86-dis "jmpT" 'op-indire +v-mode+)
    (make-x86-dis "JjmpT" 'indire +f-mode+)
    (make-x86-dis "pushU" 'op-e +v-mode+)
    (make-x86-dis "(bad)"))
   ;; GRP6 
   (vector
    (make-x86-dis "sldtQ" 'op-e +v-mode+)
    (make-x86-dis "strQ" 'op-e +v-mode+)
    (make-x86-dis "lldt" 'op-e +w-mode+)
    (make-x86-dis "ltr" 'op-e +w-mode+)
    (make-x86-dis "verr" 'op-e +w-mode+)
    (make-x86-dis "verw" 'op-e +w-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)"))
   ;; GRP7 
   (vector
    (make-x86-dis "sgdtIQ" 'op-m 0)
    (make-x86-dis "sidtIQ" 'pni-fixup 0)
    (make-x86-dis "lgdt(make-x86-disQ|Q||)" 'op-m 0)
    (make-x86-dis "lidt(make-x86-disQ|Q||)" 'op-m 0)
    (make-x86-dis "smswQ" 'op-e +v-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "lmsw" 'op-e +w-mode+)
    (make-x86-dis "invlpg" 'INVLPG-Fixup +w-mode+))
   ;; GRP8 
   (vector
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "btQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "btsQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "btrQ" 'op-e +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "btcQ" 'op-e +v-mode+ 'op-i +b-mode+))
   ;; GRP9 
   (vector
    (make-x86-dis "(bad)")
    (make-x86-dis "cmpxchg8b" 'op-e +q-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)"))
   ;; GRP10 
   (vector
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "psrlw" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "psraw" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "psllw" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)"))
   ;; GRP11 
   (vector
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "psrld" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "psrad" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "pslld" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)"))
   ;; GRP12 
   (vector
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "psrlq" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "psrldq" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "psllq" 'op-ms +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "pslldq" 'op-ms +v-mode+ 'op-i +b-mode+))
   ;; GRP13 
   (vector
    (make-x86-dis "fxsave" 'op-e +v-mode+)
    (make-x86-dis "fxrstor" 'op-e +v-mode+)
    (make-x86-dis "ldmxcsr" 'op-e +v-mode+)
    (make-x86-dis "stmxcsr" 'op-e +v-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "lfence" 'OP-0fae 0)
    (make-x86-dis "mfence" 'OP-0fae 0)
    (make-x86-dis "clflush" 'OP-0fae 0))
   ;; GRP14 
   (vector
    (make-x86-dis "prefetchnta" 'op-e +v-mode+)
    (make-x86-dis "prefetcht0" 'op-e +v-mode+)
    (make-x86-dis "prefetcht1" 'op-e +v-mode+)
    (make-x86-dis "prefetcht2" 'op-e +v-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)"))
   ;; GRPAMD 
   (vector
    (make-x86-dis "prefetch" 'op-e +b-mode+)
    (make-x86-dis "prefetchw" 'op-e +b-mode+)
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)")
    (make-x86-dis "(bad)"))
   ;; GRPPADLCK1 
   (vector
    (make-x86-dis "xstorerng" 'op-0f07 0)
    (make-x86-dis "xcryptecb" 'op-0f07 0)
    (make-x86-dis "xcryptcbc" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0)
    (make-x86-dis "xcryptcfb" 'op-0f07 0)
    (make-x86-dis "xcryptofb" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0))
   ;; GRPPADLCK2 
   (vector
    (make-x86-dis "montmul" 'op-0f07 0)
    (make-x86-dis "xsha1" 'op-0f07 0)
    (make-x86-dis "xsha256" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0)
    (make-x86-dis "(bad)" 'op-0f07 0))))

(defparameter *prefix-user-table*
  (vector
   ;; PREGRP0 
   (vector
    (make-x86-dis "addps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "addss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "addpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "addsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP1 
   (vector
    (make-x86-dis "" 'op-xmm 0 'op-ex +v-mode+ 'op-simd-suffix 0);; See OP-SIMD-SUFFIX. 
    (make-x86-dis "" 'op-xmm 0 'op-ex +v-mode+ 'op-simd-suffix 0)
    (make-x86-dis "" 'op-xmm 0 'op-ex +v-mode+ 'op-simd-suffix 0)
    (make-x86-dis "" 'op-xmm 0 'op-ex +v-mode+ 'op-simd-suffix 0))
   ;; PREGRP2 
   (vector
    (make-x86-dis "cvtpi2ps" 'op-xmm 0 'op-em +v-mode+)
    (make-x86-dis "cvtsi2ssY" 'op-xmm 0 'op-e +v-mode+)
    (make-x86-dis "cvtpi2pd" 'op-xmm 0 'op-em +v-mode+)
    (make-x86-dis "cvtsi2sdY" 'op-xmm 0 'op-e +v-mode+))
   ;; PREGRP3 
   (vector
    (make-x86-dis "cvtps2pi" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "cvtss2siY" 'op-g +v-mode+ 'op-ex +v-mode+)
    (make-x86-dis "cvtpd2pi" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "cvtsd2siY" 'op-g +v-mode+ 'op-ex +v-mode+))
   ;; PREGRP4 
   (vector
    (make-x86-dis "cvttps2pi" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "cvttss2siY" 'op-g +v-mode+ 'op-ex +v-mode+)
    (make-x86-dis "cvttpd2pi" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "cvttsd2siY" 'op-g +v-mode+ 'op-ex +v-mode+))
   ;; PREGRP5 
   (vector
    (make-x86-dis "divps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "divss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "divpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "divsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP6 
   (vector
    (make-x86-dis "maxps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "maxss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "maxpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "maxsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP7 
   (vector
    (make-x86-dis "minps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "minss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "minpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "minsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP8 
   (vector
    (make-x86-dis "movups" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movupd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP9 
   (vector
    (make-x86-dis "movups" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "movss" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "movupd" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "movsd" 'op-ex +v-mode+ 'op-xmm 0))
   ;; PREGRP10 
   (vector
    (make-x86-dis "mulps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "mulss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "mulpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "mulsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP11 
   (vector
    (make-x86-dis "rcpps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "rcpss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP12 
   (vector
    (make-x86-dis "rsqrtps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "rsqrtss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP13 
   (vector
    (make-x86-dis "sqrtps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "sqrtss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "sqrtpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "sqrtsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP14 
   (vector
    (make-x86-dis "subps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "subss" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "subpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "subsd" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP15 
   (vector
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvtdq2pd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvttpd2dq" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvtpd2dq" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP16 
   (vector
    (make-x86-dis "cvtdq2ps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvttps2dq" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvtps2dq" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP17 
   (vector
    (make-x86-dis "cvtps2pd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvtss2sd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvtpd2ps" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "cvtsd2ss" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP18 
   (vector
    (make-x86-dis "maskmovq" 'op-mmx 0 'op-s +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "maskmovdqu" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP19 
   (vector
    (make-x86-dis "movq" 'op-mmx 0 'op-em +v-mode+)
    (make-x86-dis "movdqu" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movdqa" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP20 
   (vector
    (make-x86-dis "movq" 'op-em +v-mode+ 'op-mmx 0)
    (make-x86-dis "movdqu" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "movdqa" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "(bad)" 'op-ex +v-mode+ 'op-xmm 0))
   ;; PREGRP21 
   (vector
    (make-x86-dis "(bad)" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "movq2dq" 'op-xmm 0 'op-s +v-mode+)
    (make-x86-dis "movq" 'op-ex +v-mode+ 'op-xmm 0)
    (make-x86-dis "movdq2q" 'op-mmx 0 'op-xs +v-mode+))
   ;; PREGRP22 
   (vector
    (make-x86-dis "pshufw" 'op-mmx 0 'op-em +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "pshufhw" 'op-xmm 0 'op-ex +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "pshufd" 'op-xmm 0 'op-ex +v-mode+ 'op-i +b-mode+)
    (make-x86-dis "pshuflw" 'op-xmm 0 'op-ex +v-mode+ 'op-i +b-mode+))
   ;; PREGRP23 
   (vector
    (make-x86-dis "movd" 'op-e +dq-mode+ 'op-mmx 0)
    (make-x86-dis "movq" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movd" 'op-e +dq-mode+ 'op-xmm 0)
    (make-x86-dis "(bad)" 'op-e +d-mode+ 'op-xmm 0))
   ;; PREGRP24 
   (vector
    (make-x86-dis "(bad)" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "punpckhqdq" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP25 
   (vector
    (make-x86-dis "movntq" 'op-em +v-mode+ 'op-mmx 0)
    (make-x86-dis "(bad)" 'op-em +v-mode+ 'op-xmm 0)
    (make-x86-dis "movntdq" 'op-em +v-mode+ 'op-xmm 0)
    (make-x86-dis "(bad)" 'op-em +v-mode+ 'op-xmm 0))
   ;; PREGRP26 
   (vector
    (make-x86-dis "(bad)" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "punpcklqdq" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP27 
   (vector
    (make-x86-dis "(bad)" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "addsubpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "addsubps" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP28 
   (vector
    (make-x86-dis "(bad)" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "haddpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "haddps" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP29 
   (vector
    (make-x86-dis "(bad)" 'op-mmx 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "hsubpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "hsubps" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP30 
   (vector
    (make-x86-dis "movlpX" 'op-xmm 0 'op-ex +v-mode+ 'SIMD-Fixup #\h);; really only 2 operands 
    (make-x86-dis "movsldup" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movlpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movddup" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP31 
   (vector
    (make-x86-dis "movhpX" 'op-xmm 0 'op-ex +v-mode+ 'SIMD-Fixup #\l)
    (make-x86-dis "movshdup" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "movhpd" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+))
   ;; PREGRP32 
   (vector
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "(bad)" 'op-xmm 0 'op-ex +v-mode+)
    (make-x86-dis "lddqu" 'op-xmm 0 'op-m 0))))

(defparameter *x86-64-table*
    (vector
     (vector
      (make-x86-dis "arpl" 'op-e +w-mode+ 'op-g +w-mode+)
      (make-x86-dis "movs{||lq|xd}" #+op-g +v-mode+ 'op-e +d-mode+))))
