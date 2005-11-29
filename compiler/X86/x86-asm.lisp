;;;-*- Mode: Lisp; Package: (X86 :use CL) -*-
;;;
;;;   Copyright (C) 2005 Clozure Associates and contributors.
;;;   This file is part of OpenMCL.
;;;
;;;   OpenMCL is licensed under the terms of the Lisp Lesser GNU Public
;;;   License   known as the LLGPL and distributed with OpenMCL as the
;;;   file "LICENSE".  The LLGPL consists of a preamble and the LGPL
;;;   which is distributed with OpenMCL as the file "LGPL".  Where these
;;;   conflict  the preamble takes precedence.
;;;
;;;   OpenMCL is referenced in the preamble as the "LIBRARY."
;;;
;;;   The LLGPL is also available online at
;;;   http://opensource.franz.com/preamble.html

(require "X86-ARCH" "ccl:compiler;X86;x86-arch")

(in-package "X86")

(defconstant MAX-OPERANDS 3) ; max operands per insn
(defconstant MAX-IMMEDIATE-OPERANDS 2) ; max immediates per insn (lcall  ljmp)
(defconstant MAX-MEMORY-OPERANDS 2) ; max memory refs per insn (string ops)

;;; Prefixes will be emitted in the order defined below.
;;; WAIT-PREFIX must be the first prefix since FWAIT is really is an
;;; instruction  and so must come before any prefixes.

(defconstant WAIT-PREFIX 0)
(defconstant LOCKREP-PREFIX 1)
(defconstant ADDR-PREFIX 2)
(defconstant DATA-PREFIX 3)
(defconstant SEG-PREFIX 4)
(defconstant REX-PREFIX 5) ; must come last.
(defconstant MAX-PREFIXES 6) ; max prefixes per opcode

;;; we define the syntax here (modulo base index scale syntax)
(defconstant REGISTER-PREFIX #\%)
(defconstant IMMEDIATE-PREFIX #\$)
(defconstant ABSOLUTE-PREFIX #\*)

(defconstant TWO-BYTE-OPCODE-ESCAPE #x0f)
(defconstant NOP-OPCODE #x90)

;;; register numbers
(defconstant EBP-REG-NUM 5)
(defconstant ESP-REG-NUM 4)

;;; modrm-byte.regmem for twobyte escape
(defconstant ESCAPE-TO-TWO-BYTE-ADDRESSING ESP-REG-NUM)
;;; index-base-byte.index for no index register addressing
(defconstant NO-INDEX-REGISTER ESP-REG-NUM)
;;; index-base-byte.base for no base register addressing
(defconstant NO-BASE-REGISTER EBP-REG-NUM)
(defconstant NO-BASE-REGISTER-16 6)

;;; these are the instruction mnemonic suffixes.
(defconstant WORD-MNEM-SUFFIX #\w)
(defconstant BYTE-MNEM-SUFFIX #\b)
(defconstant SHORT-MNEM-SUFFIX #\s)
(defconstant LONG-MNEM-SUFFIX #\l)
(defconstant QWORD-MNEM-SUFFIX #\q)
(defconstant LONG-DOUBLE-MNEM-SUFFIX #\x)

;;; modrm.mode = REGMEM-FIELD-HAS-REG when a register is in there
(defconstant REGMEM-FIELD-HAS-REG #x3) ; always = #x3
(defconstant REGMEM-FIELD-HAS-MEM (lognot REGMEM-FIELD-HAS-REG))


;;; cpu feature flags
(defconstant Cpu086 #x1)                ; Any old cpu will do  0 does the same
(defconstant Cpu186 #x2)                ; i186 or better required
(defconstant Cpu286 #x4)                ; i286 or better required
(defconstant Cpu386 #x8)                ; i386 or better required
(defconstant Cpu486 #x10)               ; i486 or better required
(defconstant Cpu586 #x20)               ; i585 or better required
(defconstant Cpu686 #x40)               ; i686 or better required
(defconstant CpuP4 #x80)                ; Pentium4 or better required
(defconstant CpuK6 #x100)               ; AMD K6 or better required
(defconstant CpuAthlon #x200)           ; AMD Athlon or better required
(defconstant CpuSledgehammer #x400)     ; Sledgehammer or better required
(defconstant CpuMMX #x800)              ; MMX support required
(defconstant CpuMMX2 #x1000)            ; extended MMX support (with SSE or 3DNow!Ext) required
(defconstant CpuSSE #x2000)             ; Streaming SIMD extensions required
(defconstant CpuSSE2 #x4000)            ; Streaming SIMD extensions 2 required
(defconstant Cpu3dnow #x8000)           ; 3dnow! support required
(defconstant Cpu3dnowA #x10000)         ; 3dnow!Extensions support required
(defconstant CpuPNI #x20000)            ; Prescott New Instructions required
(defconstant CpuPadLock #x40000)        ; VIA PadLock required
;;; These flags are set by gas depending on the flag-code.
(defconstant Cpu64 #x4000000)           ; 64bit support required
(defconstant CpuNo64 #x8000000)         ; Not supported in the 64bit mode
;;; The default value for unknown CPUs - enable all features to avoid problems.
(defconstant CpuUnknownFlags (logior Cpu086 Cpu186 Cpu286 Cpu386 Cpu486 Cpu586 Cpu686 CpuP4 CpuSledgehammer CpuMMX CpuMMX2 CpuSSE CpuSSE2 CpuPNI Cpu3dnow Cpu3dnowA CpuK6 CpuAthlon CpuPadLock))

(defparameter *cpu-feature-names*
  `((:Cpu086 . #x1) ; Any old cpu will do  0 does the same
    (:Cpu186 . #x2) ; i186 or better required
    (:Cpu286 . #x4) ; i286 or better required
    (:Cpu386 . #x8) ; i386 or better required
    (:Cpu486 . #x10) ; i486 or better required
    (:Cpu586 . #x20) ; i585 or better required
    (:Cpu686 . #x40) ; i686 or better required
    (:CpuP4 . #x80) ; Pentium4 or better required
    (:CpuK6 . #x100) ; AMD K6 or better required
    (:CpuAthlon . #x200) ; AMD Athlon or better required
    (:CpuSledgehammer . #x400) ; Sledgehammer or better required
    (:CpuMMX . #x800) ; MMX support required
    (:CpuMMX2 . #x1000) ; extended MMX support (with SSE or 3DNow!Ext) required
    (:CpuSSE . #x2000) ; Streaming SIMD extensions required
    (:CpuSSE2 . #x4000) ; Streaming SIMD extensions 2 required
    (:Cpu3dnow . #x8000) ; 3dnow! support required
    (:Cpu3dnowA . #x10000) ; 3dnow!Extensions support required
    (:CpuPNI . #x20000) ; Prescott New Instructions required
    (:CpuPadLock . #x40000) ; VIA PadLock required
    ;; These flags are set depending on the flag-code.
    (:Cpu64 . #x4000000) ; 64bit support required
    (:CpuNo64 . #x8000000))) ; Not supported in the 64bit mode

(defun %encode-cpu-flags (flags)
  (flet ((encode-atomic-cpu-flag (f)
           (cdr (assoc f *cpu-feature-names* :test #'eq))))
    (if flags
      (if (atom flags)
        (encode-atomic-cpu-flag flags)
        (let* ((k 0))
          (dolist (flag flags k)
            (let* ((k0 (encode-atomic-cpu-flag flag)))
              (if k0
                (setq k (logior k k0))
                (return))))))
      1)))
         

;;; opcode-modifier bits:
(defconstant opcode-modifier-W #x1) ; set if operands can be words or dwords  encoded the canonical way
(defconstant opcode-modifier-D #x2) ; D = 0 if Reg --> Regmem  D = 1 if Regmem --> Reg:    MUST BE #x2
(defconstant opcode-modifier-Modrm #x4)
(defconstant opcode-modifier-FloatR #x8) ; src/dest swap for floats:   MUST BE #x8
(defconstant opcode-modifier-ShortForm #x10) ; register is in low 3 bits of opcode
(defconstant opcode-modifier-FloatMF #x20) ; FP insn memory format bit  sized by #x4
(defconstant opcode-modifier-Jump #x40) ; special case for jump insns.
(defconstant opcode-modifier-JumpDword #x80) ; call and jump
(defconstant opcode-modifier-JumpByte #x100) ; loop and jecxz
(defconstant opcode-modifier-JumpInterSegment #x200) ; special case for intersegment leaps/calls
(defconstant opcode-modifier-FloatD #x400) ; direction for float insns:  MUST BE #x400
(defconstant opcode-modifier-Seg2ShortForm #x800) ; encoding of load segment reg insns
(defconstant opcode-modifier-Seg3ShortForm #x1000) ; fs/gs segment register insns.
(defconstant opcode-modifier-Size16 #x2000) ; needs size prefix if in 32-bit mode
(defconstant opcode-modifier-Size32 #x4000) ; needs size prefix if in 16-bit mode
(defconstant opcode-modifier-Size64 #x8000) ; needs size prefix if in 16-bit mode
(defconstant opcode-modifier-IgnoreSize #x10000) ; instruction ignores operand size prefix
(defconstant opcode-modifier-DefaultSize #x20000) ; default insn size depends on mode
(defconstant opcode-modifier-No-bSuf #x40000) ; b suffix on instruction illegal
(defconstant opcode-modifier-No-wSuf #x80000) ; w suffix on instruction illegal
(defconstant opcode-modifier-No-lSuf #x100000) ; l suffix on instruction illegal
(defconstant opcode-modifier-No-sSuf #x200000) ; s suffix on instruction illegal
(defconstant opcode-modifier-No-qSuf #x400000) ; q suffix on instruction illegal
(defconstant opcode-modifier-No-xSuf #x800000) ; x suffix on instruction illegal
(defconstant opcode-modifier-FWait #x1000000) ; instruction needs FWAIT
(defconstant opcode-modifier-IsString #x2000000) ; quick test for string instructions
(defconstant opcode-modifier-regKludge #x4000000) ; fake an extra reg operand for clr  imul
(defconstant opcode-modifier-IsPrefix #x8000000) ; opcode is a prefix
(defconstant opcode-modifier-ImmExt #x10000000) ; instruction has extension in 8 bit imm
(defconstant opcode-modifier-NoRex64 #x20000000) ; instruction don't need Rex64 prefix.
(defconstant opcode-modifier-Rex64 #x40000000) ; instruction require Rex64 prefix.
(defconstant opcode-modifier-Ugh #x80000000) ; deprecated fp insn  gets a warning


(defconstant opcode-modifier-NoSuf (logior opcode-modifier-No-bSuf
                                           opcode-modifier-No-wSuf
                                           opcode-modifier-No-lSuf
                                           opcode-modifier-No-sSuf
                                           opcode-modifier-No-xSuf
                                           opcode-modifier-No-qSuf))
(defconstant opcode-modifier-b-Suf (logior opcode-modifier-No-wSuf opcode-modifier-No-lSuf opcode-modifier-No-sSuf opcode-modifier-No-xSuf opcode-modifier-No-qSuf))
(defconstant opcode-modifier-w-Suf (logior opcode-modifier-No-bSuf opcode-modifier-No-lSuf opcode-modifier-No-sSuf opcode-modifier-No-xSuf opcode-modifier-No-qSuf))
(defconstant opcode-modifier-l-Suf (logior opcode-modifier-No-bSuf opcode-modifier-No-wSuf opcode-modifier-No-sSuf opcode-modifier-No-xSuf opcode-modifier-No-qSuf))
(defconstant opcode-modifier-q-Suf (logior opcode-modifier-No-bSuf opcode-modifier-No-wSuf opcode-modifier-No-sSuf opcode-modifier-No-lSuf opcode-modifier-No-xSuf))
(defconstant opcode-modifier-x-Suf (logior opcode-modifier-No-bSuf opcode-modifier-No-wSuf opcode-modifier-No-sSuf opcode-modifier-No-lSuf opcode-modifier-No-qSuf))
(defconstant opcode-modifier-bw-Suf (logior opcode-modifier-No-lSuf opcode-modifier-No-sSuf opcode-modifier-No-xSuf opcode-modifier-No-qSuf))
(defconstant opcode-modifier-bl-Suf (logior opcode-modifier-No-wSuf opcode-modifier-No-sSuf opcode-modifier-No-xSuf opcode-modifier-No-qSuf))
(defconstant opcode-modifier-wl-Suf (logior opcode-modifier-No-bSuf opcode-modifier-No-sSuf opcode-modifier-No-xSuf opcode-modifier-No-qSuf))
(defconstant opcode-modifier-wlq-Suf (logior opcode-modifier-No-bSuf opcode-modifier-No-sSuf opcode-modifier-No-xSuf))
(defconstant opcode-modifier-lq-Suf (logior opcode-modifier-No-bSuf opcode-modifier-No-wSuf opcode-modifier-No-sSuf opcode-modifier-No-xSuf))
(defconstant opcode-modifier-wq-Suf (logior opcode-modifier-No-bSuf opcode-modifier-No-lSuf opcode-modifier-No-sSuf opcode-modifier-No-xSuf))
(defconstant opcode-modifier-sl-Suf (logior opcode-modifier-No-bSuf opcode-modifier-No-wSuf opcode-modifier-No-xSuf opcode-modifier-No-qSuf))
(defconstant opcode-modifier-bwl-Suf (logior opcode-modifier-No-sSuf opcode-modifier-No-xSuf opcode-modifier-No-qSuf))
(defconstant opcode-modifier-bwlq-Suf (logior opcode-modifier-No-sSuf opcode-modifier-No-xSuf))
(defconstant opcode-modifier-FP opcode-modifier-NoSuf)
(defconstant opcode-modifier-l-FP opcode-modifier-l-Suf)
(defconstant opcode-modifier-q-FP (logior opcode-modifier-q-Suf opcode-modifier-NoRex64))
(defconstant opcode-modifier-x-FP (logior opcode-modifier-x-Suf opcode-modifier-FloatMF))
(defconstant opcode-modifier-sl-FP (logior opcode-modifier-sl-Suf opcode-modifier-FloatMF))
;;; Someone forgot that the FloatR bit reverses the operation when not
;;; equal to the FloatD bit.  ie. Changing only FloatD results in the
;;; destination being swapped *and* the direction being reversed.
(defconstant opcode-modifier-FloatDR opcode-modifier-FloatD)

(defparameter *opcode-modifier-names*
  `((:w . ,opcode-modifier-W)
    (:d . ,opcode-modifier-D)
    (:modrm . ,opcode-modifier-Modrm)
    (:shortform . ,opcode-modifier-ShortForm)
    (:floatr . ,opcode-modifier-FloatR)
    (:floatmf . ,opcode-modifier-FloatMF)
    (:jump . ,opcode-modifier-Jump)
    (:jumpdword . ,opcode-modifier-JumpDword)
    (:jumpbyte . ,opcode-modifier-JumpByte)
    (:jumpintersegment . ,opcode-modifier-JumpInterSegment)
    (:floatd . ,opcode-modifier-FloatD)
    (:seg2shortform . ,opcode-modifier-Seg2ShortForm)
    (:seg3shortform . ,opcode-modifier-Seg3ShortForm)
    (:size16 . ,opcode-modifier-Size16)
    (:size32 . ,opcode-modifier-Size32)
    (:size64 . ,opcode-modifier-Size64)
    (:ignoresize . ,opcode-modifier-IgnoreSize)
    (:no-bsuf . ,opcode-modifier-No-bsuf)
    (:no-wsuf . ,opcode-modifier-No-wsuf)
    (:no-lsuf . ,opcode-modifier-No-lsuf)
    (:no-ssuf . ,opcode-modifier-No-ssuf)
    (:no-qsuf . ,opcode-modifier-No-qsuf)
    (:no-xsuf . ,opcode-modifier-No-xsuf)
    (:defaultsize . ,opcode-modifier-DefaultSize)
    (:fwait . ,opcode-modifier-FWait)
    (:isstring . ,opcode-modifier-IsString)
    (:regkludge . ,opcode-modifier-regKludge)
    (:isprefix . ,opcode-modifier-IsPrefix)
    (:immext . ,opcode-modifier-ImmExt)
    (:norex64 . ,opcode-modifier-NoRex64)
    (:rex64 . ,opcode-modifier-Rex64)
    (:ugh . ,opcode-modifier-Ugh)
    (:nosuf . ,opcode-modifier-NoSuf)
    (:b-suf . ,opcode-modifier-b-Suf)
    (:w-suf . ,opcode-modifier-w-Suf)
    (:l-suf . ,opcode-modifier-l-Suf)
    (:q-suf . ,opcode-modifier-q-Suf)
    (:x-suf . ,opcode-modifier-x-suf)
    (:wl-suf . ,opcode-modifier-wl-Suf)
    (:wlq-suf . ,opcode-modifier-wlq-Suf)
    (:lq-suf . ,opcode-modifier-lq-Suf)
    (:wq-suf . ,opcode-modifier-wq-Suf)
    (:sl-suf . ,opcode-modifier-sl-Suf)
    (:bwl-suf . ,opcode-modifier-bwl-Suf)
    (:bwlq-suf . ,opcode-modifier-bwlq-Suf)
    (:fp . ,opcode-modifier-FP)
    (:l-fp . ,opcode-modifier-l-FP)
    (:q-fp . ,opcode-modifier-q-FP)
    (:x-fp . ,opcode-modifier-x-FP)
    (:sl-fp . ,opcode-modifier-sl-FP)
    (:floatd . ,opcode-modifier-FloatD)
    (:floatdr . ,opcode-modifier-FloatDR)))


;;; By default, this returns NIL if the modifier can't be encoded.
;;; That's an error, but the caller can provide better error context.
(defun %encode-opcode-modifier (mod &optional errorp)
  (flet ((encode-atomic-opcode-modifier (m)
           (if m
             (cdr (assoc m *opcode-modifier-names*))
             0)))
    (or
     (if (atom mod)
       (encode-atomic-opcode-modifier mod)
       (let* ((k 0))
         (dolist (m mod k)
           (let* ((k0 (encode-atomic-opcode-modifier m)))
             (if k0
               (setq k (logior k0 k))
               (return))))))
     (if errorp (error "Unknown x86 opcode modifier: ~s" mod)))))

(defmacro encode-opcode-modifier (&rest mod)
  (%encode-opcode-modifier mod t))


;;; operand-types[i] bits
;;; register
(defconstant operand-type-Reg8 #x1) ; 8 bit reg
(defconstant operand-type-Reg16 #x2) ; 16 bit reg
(defconstant operand-type-Reg32 #x4) ; 32 bit reg
(defconstant operand-type-Reg64 #x8) ; 64 bit reg
;;; immediate
(defconstant operand-type-Imm8 #x10) ; 8 bit immediate
(defconstant operand-type-Imm8S #x20) ; 8 bit immediate sign extended
(defconstant operand-type-Imm16 #x40) ; 16 bit immediate
(defconstant operand-type-Imm32 #x80) ; 32 bit immediate
(defconstant operand-type-Imm32S #x100) ; 32 bit immediate sign extended
(defconstant operand-type-Imm64 #x200) ; 64 bit immediate
(defconstant operand-type-Imm1 #x400) ; 1 bit immediate
;;; memory
(defconstant operand-type-BaseIndex #x800)
;;; Disp8 16 32 are used in different ways  depending on the
;;; instruction.  For jumps  they specify the size of the PC relative
;;; displacement  for baseindex type instructions  they specify the
;;; size of the offset relative to the base register  and for memory
;;; offset instructions such as `mov 1234 %al' they specify the size of
;;; the offset relative to the segment base.
(defconstant operand-type-Disp8 #x1000) ; 8 bit displacement
(defconstant operand-type-Disp16 #x2000) ; 16 bit displacement
(defconstant operand-type-Disp32 #x4000) ; 32 bit displacement
(defconstant operand-type-Disp32S #x8000) ; 32 bit signed displacement
(defconstant operand-type-Disp64 #x10000) ; 64 bit displacement
;;; specials
(defconstant operand-type-InOutPortReg #x20000) ; register to hold in/out port addr = dx
(defconstant operand-type-ShiftCount #x40000) ; register to hold shift cound = cl
(defconstant operand-type-Control #x80000) ; Control register
(defconstant operand-type-Debug #x100000) ; Debug register
(defconstant operand-type-Test #x200000) ; Test register
(defconstant operand-type-FloatReg #x400000) ; Float register
(defconstant operand-type-FloatAcc #x800000) ; Float stack top %st(0)
(defconstant operand-type-SReg2 #x1000000) ; 2 bit segment register
(defconstant operand-type-SReg3 #x2000000) ; 3 bit segment register
(defconstant operand-type-Acc #x4000000) ; Accumulator %al or %ax or %eax
(defconstant operand-type-JumpAbsolute #x8000000)
(defconstant operand-type-RegMMX #x10000000) ; MMX register
(defconstant operand-type-RegXMM #x20000000) ; XMM registers in PIII
(defconstant operand-type-EsSeg #x40000000) ; String insn operand with fixed es segment

;;; InvMem is for instructions with a modrm byte that only allow a
;;; general register encoding in the i.tm.mode and i.tm.regmem fields
;;; eg. control reg moves.  They really ought to support a memory form
;;; but don't  so we add an InvMem flag to the register operand to
;;; indicate that it should be encoded in the i.tm.regmem field.
(defconstant operand-type-InvMem #x80000000)

(defconstant operand-type-Reg (logior operand-type-Reg8 operand-type-Reg16 operand-type-Reg32 operand-type-Reg64)) ; gen'l register
(defconstant operand-type-WordReg (logior operand-type-Reg16 operand-type-Reg32 operand-type-Reg64))
(defconstant operand-type-ImplicitRegister (logior operand-type-InOutPortReg operand-type-ShiftCount operand-type-Acc operand-type-FloatAcc))
(defconstant operand-type-Imm (logior operand-type-Imm8 operand-type-Imm8S operand-type-Imm16 operand-type-Imm32S operand-type-Imm32 operand-type-Imm64)) ; gen'l immediate
(defconstant operand-type-EncImm (logior operand-type-Imm8 operand-type-Imm16 operand-type-Imm32 operand-type-Imm32S)) ; Encodable gen'l immediate
(defconstant operand-type-Disp (logior operand-type-Disp8 operand-type-Disp16 operand-type-Disp32 operand-type-Disp32S operand-type-Disp64)) ; General displacement
(defconstant operand-type-AnyMem (logior operand-type-Disp8 operand-type-Disp16 operand-type-Disp32 operand-type-Disp32S operand-type-BaseIndex operand-type-InvMem)) ; General memory
;;; The following aliases are defined because the opcode table
;;; carefully specifies the allowed memory types for each instruction.
;;; At the moment we can only tell a memory reference size by the
;;; instruction suffix  so there's not much point in defining Mem8
;;; Mem16  Mem32 and Mem64 opcode modifiers - We might as well just use
;;; the suffix directly to check memory operands.
(defconstant operand-type-LLongMem operand-type-AnyMem); 64 bits (or more)
(defconstant operand-type-LongMem  operand-type-AnyMem) ; 32 bit memory ref
(defconstant operand-type-ShortMem operand-type-AnyMem) ; 16 bit memory ref
(defconstant operand-type-WordMem operand-type-AnyMem) ; 16 or 32 bit memory ref
(defconstant operand-type-ByteMem operand-type-AnyMem) ; 8 bit memory ref

(defparameter *x86-operand-type-names*
  `((:Reg8 . ,operand-type-Reg8)
    (:Reg16 . ,operand-type-Reg16)
    (:Reg32 . ,operand-type-Reg32)
    (:Reg64 . ,operand-type-Reg64)
    (:Imm8 . ,operand-type-Imm8)
    (:Imm8S . ,operand-type-Imm8S)
    (:Imm16 . ,operand-type-Imm16)
    (:Imm32 . ,operand-type-Imm32)
    (:Imm32S . ,operand-type-Imm32S)
    (:Imm64 . ,operand-type-Imm64)
    (:Imm1 . ,operand-type-Imm1)
    (:BaseIndex . ,operand-type-BaseIndex)
    (:Disp8 . ,operand-type-Disp8)
    (:Disp16 . ,operand-type-Disp16)
    (:Disp32 . ,operand-type-Disp32)
    (:Disp32S . ,operand-type-Disp32S)
    (:Disp64 . ,operand-type-Disp64)
    (:InOutPortReg . ,operand-type-InOutPortReg)
    (:ShiftCount . ,operand-type-ShiftCount)
    (:Control . ,operand-type-Control)
    (:Debug . ,operand-type-Debug)
    (:Test . ,operand-type-Test)
    (:FloatReg . ,operand-type-FloatReg)
    (:FloatAcc . ,operand-type-FloatAcc)
    (:SReg2 . ,operand-type-SReg2)
    (:SReg3 . ,operand-type-SReg3)
    (:Acc . ,operand-type-Acc)
    (:JumpAbsolute . ,operand-type-JumpAbsolute)
    (:RegMMX . ,operand-type-RegMMX)
    (:RegXMM . ,operand-type-RegXMM)
    (:EsSeg . ,operand-type-EsSeg)
    (:InvMem . ,operand-type-InvMem)
    (:Reg . ,operand-type-Reg)
    (:WordReg . ,operand-type-WordReg)
    (:ImplicitRegister . ,operand-type-ImplicitRegister)
    (:Imm . ,operand-type-Imm)
    (:EncImm . ,operand-type-EncImm)
    (:Disp . ,operand-type-Disp)
    (:AnyMem . ,operand-type-AnyMem)
    (:LLongMem . ,operand-type-LLongMem)
    (:LongMem . ,operand-type-LongMem)
    (:ShortMem . ,operand-type-ShortMem)
    (:WordMem . ,operand-type-WordMem)
    (:ByteMem . ,operand-type-ByteMem)
  ))

(defun %encode-operand-type (optype &optional errorp)
  (flet ((encode-atomic-operand-type (op)
           (if op
             (cdr (assoc op *x86-operand-type-names* :test #'eq))
             0)))
    (or
     (if (atom optype)
       (encode-atomic-operand-type optype)
       (let* ((k 0))
         (dolist (op optype k)
           (let* ((k0 (encode-atomic-operand-type op)))
             (if k0
               (setq k (logior k k0))
               (return))))))
     (if errorp (error "Unknown x86 operand type ~s" optype)))))

(defmacro encode-operand-type (&rest op)
  (%encode-operand-type op t))

(defstruct x86-instruction-template
  ;; instruction name sans width suffix ("mov" for movl insns)
  name
  ;; a small integer: an index into the *x86-instruction-templates* vector
  (index 0)
  ;; how many operands
  operands
  ;; base-opcode is the fundamental opcode byte without optional prefix(es).
  base-opcode
  ;; extension-opcode is the 3 bit extension for group <n> insns.
  ;; This field is also used to store the 8-bit opcode suffix for the
  ;; AMD 3DNow! instructions.
  ;; If this template has no extension opcode (the usual case) use nil
  extension-opcode
  ;; CPU constraints.
  cpu-flags
  ;; the bits in opcode-modifier are used to generate the final opcode from
  ;; the base-opcode.  These bits also are used to detect alternate forms of
  ;; the same instruction.
  opcode-modifier
  ;; operand-types[i] describes the type of operand i.  This is made
  ;;   by OR'ing together all of the possible type masks.  (e.g.
  ;;   'operand-types[i] = (logior Reg Imm)' specifies that operand i can be
  ;;   either a register or an immediate operand.
  operand-types
)


(defconstant RegRex #x1) ; Extended register.
(defconstant RegRex64 #x2) ; Extended 8 bit register.

;;; these are for register name --> number & type hash lookup
(defstruct reg-entry
  reg-name
  reg-type
  reg-flags
  reg-num                               ; for encoding in instruction fields
  ordinal64                             ; canonical, ordinal register number
  ordinal32
)

(defstruct seg-entry
  seg-name
  seg-prefix
)

;;; 386 operand encoding bytes:  see 386 book for details of this.
(defstruct modrm-byte
  regmem ; codes register or memory operand
  reg ; codes register operand (or extended opcode)
  mode ; how to interpret regmem & reg
)

;;; x86-64 extension prefix.
;; typedef int rex-byte
(defconstant REX-OPCODE #x40)

;;; Indicates 64 bit operand size.
(defconstant REX-MODE64 8)
;;; High extension to reg field of modrm byte.
(defconstant REX-EXTX 4)
;;; High extension to SIB index field.
(defconstant REX-EXTY 2)
;;; High extension to base field of modrm or SIB  or reg field of opcode.
(defconstant REX-EXTZ 1)

;;; 386 opcode byte to code indirect addressing.
(defstruct sib-byte
  base
  index
  scale
)


;;; x86 arch names and features
(defstruct arch-entry
  name  ; arch name
  flags ; cpu feature flags
)


;;; The SystemV/386 SVR3.2 assembler  and probably all AT&T derived
;;; ix86 Unix assemblers  generate floating point instructions with
;;; reversed source and destination registers in certain cases.
;;; Unfortunately  gcc and possibly many other programs use this
;;; reversed syntax  so we're stuck with it.
;;;
;;; eg. `fsub %st(3) %st' results in st = st - st(3) as expected  but
;;;`fsub %st %st(3)' results in st(3) = st - st(3)  rather than
;;; the expected st(3) = st(3) - st
;;;
;;; This happens with all the non-commutative arithmetic floating point
;;; operations with two register operands  where the source register is
;;; %st  and destination register is %st(i).  See FloatDR below.
;;;
;;; The affected opcode map is dceX  dcfX  deeX  defX.

(defconstant MOV-AX-DISP32 #xa0)
(defconstant POP-SEG-SHORT #x07)
(defconstant JUMP-PC-RELATIVE #xe9)
(defconstant INT-OPCODE  #xcd)
(defconstant INT3-OPCODE #xcc)
(defconstant FWAIT-OPCODE #x9b)
(defconstant ADDR-PREFIX-OPCODE #x67)
(defconstant DATA-PREFIX-OPCODE #x66)
(defconstant LOCK-PREFIX-OPCODE #xf0)
(defconstant CS-PREFIX-OPCODE #x2e)
(defconstant DS-PREFIX-OPCODE #x3e)
(defconstant ES-PREFIX-OPCODE #x26)
(defconstant FS-PREFIX-OPCODE #x64)
(defconstant GS-PREFIX-OPCODE #x65)
(defconstant SS-PREFIX-OPCODE #x36)
(defconstant REPNE-PREFIX-OPCODE #xf2)
(defconstant REPE-PREFIX-OPCODE  #xf3)


(defparameter *x86-instruction-template-data*
  '(
    ;; Move instructions.
    ;; In the 64bit mode the short form mov immediate is redefined to have
    ;; 64bit displacement value.
    ("mov" ((:disp16 :disp32) :acc) (#xa0) :cpuno64 (:bwl-suf :d :w))
    ("mov" (:reg (:reg :anymem)) (#x88) :cpu086 (:bwlq-suf :d :w :modrm))
    ;; In the 64bit mode the short form mov immediate is redefined to have
    ;; 64bit displacement value.
    ("mov" (:encimm (:reg8 :reg16 :reg32)) (#xb0) :cpu086 (:bwl-suf :w :shortform))
    ("mov" (:encimm (:reg :anymem)) (#xc6 0) :cpu086 (:bwlq-suf :w :modrm))
    ("mov" (:imm64 :reg64) (#xb0) :cpu64 (:q-suf :w :shortform))

    ;; The segment register moves accept WordReg so that a segment register
    ;; can be copied to a 32 bit register and vice versa without using a
    ;; size prefix. When moving to a 32 bit register the upper 16 bits
    ;; are set to an implementation defined value (on the Pentium Pro
    ;; the implementation defined value is zero).

    ("mov" (:sreg2 (:wordreg :invmem)) (#x8c) :cpu086 (:wl-suf :modrm))
    ("mov" (:sreg2 :wordmem) (#x8c) :cpu086 (:wl-suf :modrm :ignoresize))
    ("mov" (:sreg3 (:wordreg :invmem)) (#x8c) :cpu386 (:wl-suf :modrm))
    ("mov" (:sreg3 :wordmem) (#x8c) :cpu386 (:wl-suf :modrm :ignoresize))
    ("mov" ((:wordreg :wordmem) :sreg2) (#x8e) :cpu086 (:wl-suf :modrm :ignoresize))
    ("mov" ((:wordreg :wordmem) :sreg3) (#x8e) :cpu386 (:wl-suf :modrm :ignoresize))
    ;; Move to/from control debug registers. In the 16 or 32bit modes
    ;; they are 32bit. In the 64bit mode they are 64bit.
    ("mov" (:control (:reg32 :invmem)) (#x0f20) (:cpu386 :cpuno64) (:l-suf :d :modrm :ignoresize))
    ("mov" (:control (:reg64 :invmem)) (#x0f20) :cpu64 (:q-suf :d :modrm :ignoresize :norex64))
    ("mov" (:debug (:reg32 :invmem)) (#x0f21) (:cpu386 :cpuno64) (:l-suf :d :modrm :ignoresize))
    ("mov" (:debug (:reg64 :invmem)) (#x0f21) :cpu64 (:q-suf :d :modrm :ignoresize :norex64))
    ("mov" (:test (:reg32 :invmem)) (#x0f24) (:cpu386 :cpuno64) (:l-suf :d :modrm :ignoresize))
    ("movabs" (:disp64 :acc) (#xa0) :cpu64 (:bwlq-suf :d :w))
    ("movabs" (:imm64 :reg64) (#xb0) :cpu64 (:q-suf :w :shortform))

    ;; Move with sign extend.  "movsbl" & "movsbw" must not be unified
    ;; into "movsb" to avoid conflict with the "movs" string move
    ;; instruction.
    ("movsbl" ((:reg8 :bytemem) :reg32) (#x0fbe) :cpu386 (:nosuf :modrm))
    ("movsbw" ((:reg8 :bytemem) :reg16) (#x0fbe) :cpu386 (:nosuf :modrm))
    ("movswl" ((:reg16 :shortmem) :reg32) (#x0fbf) :cpu386 (:nosuf :modrm))
    ("movsbq" ((:reg8 :bytemem) :reg64) (#x0fbe) :cpu64 (:nosuf :modrm :rex64))
    ("movswq" ((:reg16 :shortmem) :reg64) (#x0fbf) :cpu64 (:nosuf :modrm :rex64))
    ("movslq" ((:reg32 :wordmem) :reg64) (#x63) :cpu64 (:nosuf :modrm :rex64))
    ;; Intel Syntax next 3 insns
    ;; ("movsx" ((:reg :bytemem) :wordreg) (#x0fbe) :cpu386 (:b-suf :modrm))
    ;; ("movsx" ((:reg16 :shortmem) (:reg32 :reg64)) (#x0fbf) :cpu386 (:w-suf :modrm))
    ;; ("movsx" ((:reg32 :wordmem) :reg64) (#x63) :cpu64 (:l-suf :modrm :rex64))

    ;; Move with zero extend. We can't remove "movzb" since existing
    ;; assembly codes may use it.
    ("movzb" ((:reg8 :bytemem) :wordreg) (#x0fb6) :cpu386 (:wl-suf :modrm))
    ;; "movzbl" & "movzbw" should not be unified into "movzb" for
    ;; consistency with the sign extending moves above.
    ("movzbl" ((:reg8 :bytemem) :reg32) (#x0fb6) :cpu386 (:nosuf :modrm))
    ("movzbw" ((:reg8) :bytemem :reg16) (#x0fb6) :cpu386 (:nosuf :modrm))
    ("movzwl" ((:reg16 :shortmem) :reg32) (#x0fb7) :cpu386 (:nosuf :modrm))
    ;; These instructions are not particulary useful since the zero extend
    ;;  32->64 is implicit but we can encode them.
    ("movzbq" ((:reg8 :bytemem) :reg64) (#x0fb6) :cpu64 (:nosuf :modrm :rex64))
    ("movzwq" ((:reg16 :shortmem) :reg64) (#x0fb7) :cpu64 (:nosuf :modrm :rex64))
    ;; Intel Syntax next 2 insns (the 64-bit variants are not particulary useful
    ;;  since the zero extend 32->64 is implicit but we can encode them).
    ;; ("movzx" ((:reg8 :bytemem) :wordreg) (#x0fb6) :cpu386 (:b-suf :modrm))
    ;; ("movzx" ((:reg16 :shortmem) (:reg32 :reg64)) (#x0fb7) :cpu386 (:w-suf :modrm))

    ;; Push instructions.
    ("push" (:wordreg) (#x50) :cpuno64 (:wl-suf :shortform :defaultsize))
    ("push" ((:wordreg :wordmem)) (#xff 6) :cpuno64 (:wl-suf :modrm :defaultsize))
    ("push" (:imm8s) (#x6a) (:cpu186 :cpuno64) (:wl-suf :defaultsize))
    ("push" ((:imm16 :imm32)) (#x68) (:cpu186 :cpuno64) (:wl-suf :defaultsize))
    ("push" (:sreg2) (#x06) :cpuno64 (:wl-suf :seg2shortform :defaultsize))
    ("push" (:sreg3) (#x0fa0) (:cpu386 :cpuno64) (:wl-suf :seg3shortform :defaultsize))
    ;; In 64bit mode the operand size is implicitly 64bit.
    ("push" ((:reg16 :reg64)) (#x50) :cpu64 (:wq-suf :shortform :defaultsize :norex64))
    ("push" ((:reg16 :reg64 :wordmem)) (#xff 6) :cpu64 (:wq-suf :modrm :defaultsize :norex64))
    ("push" (:imm8s) (#x6a) :cpu64 (:wq-suf :defaultsize :norex64))
    ("push" ((:imm32s :imm16)) (#x68) :cpu64 (:wq-suf :defaultsize :norex64))
    ("push" (:sreg3) (#x0fa0) :cpu64 (:wq-suf :seg3shortform :defaultsize :norex64))

    ("pusha" () (#x60) (:cpu186 :cpuno64) (:wl-suf :defaultsize))

    ;; Pop instructions.
    ("pop" (:wordreg) (#x58) :cpuno64 (:wl-suf :shortform :defaultsize))
    ("pop" ((:wordreg :wordmem)) (#x8f 0) :cpuno64 (:wl-suf :modrm :defaultsize))
    ("pop" (:sreg2) (#x07) :cpuno64 (:wl-suf :seg2shortform :defaultsize))
    ("pop" (:sreg3) (#x0fa1) (:cpu386 :cpuno64) (:wl-suf :seg3shortform :defaultsize))
    ;; In 64bit mode the operand size is implicitly 64bit.
    ("pop" ((:reg16 :reg64)) (#x58) :cpu64 (:wq-suf :shortform :defaultsize :norex64))
    ("pop" ((:reg16 :reg64 :wordmem)) (#x8f 0) :cpu64 (:wq-suf :modrm :defaultsize :norex64))
    ("pop" (:sreg3) (#x0fa1) :cpu64 (:wq-suf :seg3shortform :defaultsize :norex64))

    ("popa" () (#x61) (:cpu186 :cpuno64) (:wl-suf :defaultsize))

    ;; Exchange instructions.
    ;;  xchg commutes: we allow both operand orders.

    ;; In the 64bit code xchg eax eax is reused for new nop instruction.
    ;; While the two entries that are disabled generate shorter code
    ;; for xchg eax reg (on x86-64) the special case xchg eax eax
    ;; does not get handled correctly - it degenerates into nop but
    ;; that way the side effect of zero-extending eax to rax is lost.
    #+nil ("xchg" (:wordreg :acc) (#x90) :cpu086 (:wlq-suf :shortform))
    #+nil ("xchg" (:acc :wordreg) (#x90) :cpu086 (:wlq-suf :shortform))

    ("xchg" (:wordreg :acc) (#x90) :cpuno64 (:wl-suf :shortform))
    ("xchg" (:acc :wordreg) (#x90) :cpuno64 (:wl-suf :shortform))
    ("xchg" ((:reg16 :reg64) :acc) (#x90) :cpu64 (:wq-suf :shortform))
    ("xchg" (:acc (:reg16 :reg64)) (#x90) :cpu64 (:wq-suf :shortform))

    ("xchg" (:reg (:reg :anymem)) (#x86) :cpu086 (:bwlq-suf :w :modrm))
    ("xchg" ((:reg :anymem) :reg) (#x86) :cpu086 (:bwlq-suf :w :modrm))

    ;; In/out from ports.
    ;; XXX should reject %rax
    ("in" (:imm8 :acc) (#xe4) :cpu086 (:bwl-suf :w))
    ("in" (:inoutportreg :acc) (#xec) :cpu086 (:bwl-suf :w))
    ("in" (:imm8) (#xe4) :cpu086 (:bwl-suf :w))
    ("in" (:inoutportreg) (#xec) :cpu086 (:bwl-suf :w))
    ("out" (:acc :imm8) (#xe6) :cpu086 (:bwl-suf :w))
    ("out" (:acc :inoutportreg) (#xee) :cpu086 (:bwl-suf :w))
    ("out" (:imm8) (#xe6) :cpu086 (:bwl-suf :w))
    ("out" (:inoutportreg) (#xee) :cpu086 (:bwl-suf :w))

    ;; Load effective address.
    ("lea" (:wordmem :wordreg) (#x8d) :cpu086 (:wlq-suf :modrm))

    ;; Load segment registers from memory.
    ("lds" (:wordmem :wordreg) (#xc5) :cpuno64 (:wl-suf :modrm))
    ("les" (:wordmem :wordreg) (#xc4) :cpuno64 (:wl-suf :modrm))
    ("lfs" (:wordmem :wordreg) (#x0fb4) :cpu386 (:wl-suf :modrm))
    ("lgs" (:wordmem :wordreg) (#x0fb5) :cpu386 (:wl-suf :modrm))
    ("lss" (:wordmem :wordreg) (#x0fb2) :cpu386 (:wl-suf :modrm))

    ;; Flags register instructions.
    ("clc" () (#xf8) :cpu086 :nosuf)
    ("cld" () (#xfc) :cpu086 :nosuf)
    ("cli" () (#xfa) :cpu086 :nosuf)
    ("clts" () (#x0f06) :cpu286 :nosuf)
    ("cmc" () (#xf5) :cpu086 :nosuf)
    ("lahf" () (#x9f) :cpu086 :nosuf)
    ("sahf" () (#x9e) :cpu086 :nosuf)
    ("pushf" () (#x9c) :cpuno64 (:wl-suf :defaultsize))
    ("pushf" () (#x9c) :cpu64 (:wq-suf :defaultsize :norex64))
    ("popf" () (#x9d) :cpuno64 (:wl-suf :defaultsize))
    ("popf" () (#x9d) :cpu64 (:wq-suf :defaultsize :norex64))
    ("stc" () (#xf9) :cpu086 :nosuf)
    ("std" () (#xfd) :cpu086 :nosuf)
    ("sti" () (#xfb) :cpu086 :nosuf)

    ;; Arithmetic.
    ("add" (:reg (:reg :anymem)) (#x00) :cpu086 (:bwlq-suf :d :w :modrm))
    ("add" (:imm8s (:wordreg :wordmem)) (#x83 0) :cpu086 (:wlq-suf :modrm))
    ("add" (:encimm :acc) (#x04) :cpu086 (:bwlq-suf :w))
    ("add" (:encimm (:reg :anymem)) (#x80 0) :cpu086 (:bwlq-suf :w :modrm))

    ("inc" (:wordreg) (#x40) :cpuno64 (:wl-suf :shortform))
    ("inc" ((:reg :anymem)) (#xfe 0) :cpu086 (:bwlq-suf :w :modrm))

    ("sub" (:reg (:reg :anymem)) (#x28) :cpu086 (:bwlq-suf :d :w :modrm))
    ("sub" (:imm8s (:wordreg :wordmem)) (#x83 5) :cpu086 (:wlq-suf :modrm))
    ("sub" (:encimm :acc) (#x2c) :cpu086 (:bwlq-suf :w))
    ("sub" (:encimm (:reg :anymem)) (#x80 5) :cpu086 (:bwlq-suf :w :modrm))

    ("dec" (:wordreg) (#x48) :cpuno64 (:wl-suf :shortform))
    ("dec" ((:reg :anymem)) (#xfe 1) :cpu086 (:bwlq-suf :w :modrm))

    ("sbb" (:reg (:reg :anymem)) (#x18) :cpu086 (:bwlq-suf :d :w :modrm))
    ("sbb" (:imm8s (:wordreg :wordmem)) (#x83 3) :cpu086 (:wlq-suf :modrm))
    ("sbb" (:encimm :acc) (#x1c) :cpu086 (:bwlq-suf :w))
    ("sbb" (:encimm (:reg :anymem)) (#x80 3) :cpu086 (:bwlq-suf :w :modrm))

    ("cmp" (:reg (:reg :anymem)) (#x38) :cpu086 (:bwlq-suf :d :w :modrm))
    ("cmp" (:imm8s (:wordreg :wordmem)) (#x83 7) :cpu086 (:wlq-suf :modrm))
    ("cmp" (:encimm :acc) (#x3c) :cpu086 (:bwlq-suf :w))
    ("cmp" (:encimm (:reg :anymem)) (#x80 7) :cpu086 (:bwlq-suf :w :modrm))

    ("test" ((:reg :anymem) :reg) (#x84) :cpu086 (:bwlq-suf :w :modrm))
    ("test" (:reg (:reg :anymem)) (#x84) :cpu086 (:bwlq-suf :w :modrm))
    ("test" (:encimm :acc) (#xa8) :cpu086 (:bwlq-suf :w))
    ("test" (:encimm (:reg :anymem)) (#xf6 0) :cpu086 (:bwlq-suf :w :modrm))

    ("and" (:reg (:reg :anymem)) (#x20) :cpu086 (:bwlq-suf :d :w :modrm))
    ("and" (:imm8s (:wordreg :wordmem)) (#x83 4) :cpu086 (:wlq-suf :modrm))
    ("and" (:encimm :acc) (#x24) :cpu086 (:bwlq-suf :w))
    ("and" (:encimm (:reg :anymem)) (#x80 4) :cpu086 (:bwlq-suf :w :modrm))

    ("or" (:reg (:reg :anymem)) (#x08) :cpu086 (:bwlq-suf :d :w :modrm))
    ("or" (:imm8s (:wordreg :wordmem)) (#x83 1) :cpu086 (:wlq-suf :modrm))
    ("or" (:encimm :acc) (#x0c) :cpu086 (:bwlq-suf :w))
    ("or" (:encimm (:reg :anymem)) (#x80 1) :cpu086 (:bwlq-suf :w :modrm))

    ("xor" (:reg (:reg :anymem)) (#x30) :cpu086 (:bwlq-suf :d :w :modrm))
    ("xor" (:imm8s (:wordreg :wordmem)) (#x83 6) :cpu086 (:wlq-suf :modrm))
    ("xor" (:encimm :acc) (#x34) :cpu086 (:bwlq-suf :w))
    ("xor" (:encimm (:reg :anymem)) (#x80 6) :cpu086 (:bwlq-suf :w :modrm))

    ;; clr with 1 operand is really xor with 2 operands.
    ("clr" (:reg) (#x30) :cpu086 (:bwlq-suf :w :modrm :regkludge))

    ("adc" (:reg (:reg :anymem)) (#x10) :cpu086 (:bwlq-suf :d :w :modrm))
    ("adc" (:imm8s (:wordreg :wordmem)) (#x83 2) :cpu086 (:wlq-suf :modrm))
    ("adc" (:encimm :acc) (#x14) :cpu086 (:bwlq-suf :w))
    ("adc" (:encimm (:reg :anymem)) (#x80 2) :cpu086 (:bwlq-suf :w :modrm))

    ("neg" ((:reg :anymem)) (#xf6 3) :cpu086 (:bwlq-suf :w :modrm))
    ("not" ((:reg :anymem)) (#xf6 2) :cpu086 (:bwlq-suf :w :modrm))

    ("aaa" () (#x37) :cpuno64 :nosuf)
    ("aas" () (#x3f) :cpuno64 :nosuf)
    ("daa" () (#x27) :cpuno64 :nosuf)
    ("das" () (#x2f) :cpuno64 :nosuf)
    ("aad" () (#xd50a) :cpuno64 :nosuf)
    ("aad" (:imm8s) (#xd5) :cpuno64 :nosuf)
    ("aam" () (#xd40a) :cpuno64 :nosuf)
    ("aam" (:imm8s) (#xd4) :cpuno64 :nosuf)

    ;; Conversion insns.
    ;; Intel naming
    ;; ("cbw" () (#x98) :cpu086 (:nosuf :size16))
    ;; ("cdqe" () (#x98) :cpu64 (:nosuf :size64))
    ;; ("cwde" () (#x98) :cpu086 (:nosuf :size32))
    ;; ("cwd" () (#x99) :cpu086 (:nosuf :size16))
    ;; ("cdq" () (#x99) :cpu086 (:nosuf :size32))
    ;; ("cqo" () (#x99) :cpu64 (:nosuf :size64))
    ;; AT&T naming
    ("cbtw" () (#x98) :cpu086 (:nosuf :size16))
    ("cltq" () (#x98) :cpu64 (:nosuf :size64))
    ("cwtl" () (#x98) :cpu086 (:nosuf :size32))
    ("cwtd" () (#x99) :cpu086 (:nosuf :size16))
    ("cltd" () (#x99) :cpu086 (:nosuf :size32))
    ("cqto" () (#x99) :cpu64 (:nosuf :size64))

    ;; Warning! the mul/imul (opcode #xf6) must only have 1 operand! They are
    ;; expanding 64-bit multiplies and *cannot* be selected to accomplish
    ;; 'imul %ebx %eax' (opcode #x0faf must be used in this case)
    ;; These multiplies can only be selected with single operand forms.
    ("mul" ((:reg :anymem)) (#xf6 4) :cpu086 (:bwlq-suf :w :modrm))
    ("imul" ((:reg :anymem)) (#xf6 5) :cpu086 (:bwlq-suf :w :modrm))
    ("imul" ((:wordreg :wordmem) :wordreg) (#x0faf) :cpu386 (:wlq-suf :modrm))
    ("imul" (:imm8s (:wordreg :wordmem) :wordreg) (#x6b) :cpu186 (:wlq-suf :modrm))
    ("imul" ((:imm16 :imm32s :imm32) (:wordreg :wordmem) :wordreg) (#x69) :cpu186 (:wlq-suf :modrm))
    ;; imul with 2 operands mimics imul with 3 by putting the register in
    ;; both i.rm.reg & i.rm.regmem fields. :regkludge enables this
    ;; transformation.
    ("imul" (:imm8s :wordreg) (#x6b) :cpu186 (:wlq-suf :modrm :regkludge))
    ("imul" ((:imm16 :imm32s :imm32) :wordreg) (#x69) :cpu186 (:wlq-suf :modrm :regkludge))

    ("div" ((:reg :anymem)) (#xf6 6) :cpu086 (:bwlq-suf :w :modrm))
    ("div" ((:reg :anymem) :acc) (#xf6 6) :cpu086 (:bwlq-suf :w :modrm))
    ("idiv" ((:reg :anymem)) (#xf6 7) :cpu086 (:bwlq-suf :w :modrm))
    ("idiv" ((:reg :anymem) :acc) (#xf6 7) :cpu086 (:bwlq-suf :w :modrm))

    ("rol" (:imm1 (:reg :anymem)) (#xd0 0) :cpu086 (:bwlq-suf :w :modrm))
    ("rol" (:imm8 (:reg :anymem)) (#xc0 0) :cpu186 (:bwlq-suf :w :modrm))
    ("rol" (:shiftcount (:reg :anymem)) (#xd2 0) :cpu086 (:bwlq-suf :w :modrm))
    ("rol" ((:reg :anymem)) (#xd0 0) :cpu086 (:bwlq-suf :w :modrm))

    ("ror" (:imm1 (:reg :anymem)) (#xd0 1) :cpu086 (:bwlq-suf :w :modrm))
    ("ror" (:imm8 (:reg :anymem)) (#xc0 1) :cpu186 (:bwlq-suf :w :modrm))
    ("ror" (:shiftcount (:reg :anymem)) (#xd2 1) :cpu086 (:bwlq-suf :w :modrm))
    ("ror" ((:reg :anymem)) (#xd0 1) :cpu086 (:bwlq-suf :w :modrm))

    ("rcl" (:imm1 (:reg :anymem)) (#xd0 2) :cpu086 (:bwlq-suf :w :modrm))
    ("rcl" (:imm8 (:reg :anymem)) (#xc0 2) :cpu186 (:bwlq-suf :w :modrm))
    ("rcl" (:shiftcount (:reg :anymem)) (#xd2 2) :cpu086 (:bwlq-suf :w :modrm))
    ("rcl" ((:reg :anymem)) (#xd0 2) :cpu086 (:bwlq-suf :w :modrm))

    ("rcr" (:imm1 (:reg :anymem)) (#xd0 3) :cpu086 (:bwlq-suf :w :modrm))
    ("rcr" (:imm8 (:reg :anymem)) (#xc0 3) :cpu186 (:bwlq-suf :w :modrm))
    ("rcr" (:shiftcount (:reg :anymem)) (#xd2 3) :cpu086 (:bwlq-suf :w :modrm))
    ("rcr" ((:reg :anymem)) (#xd0 3) :cpu086 (:bwlq-suf :w :modrm))

    ("sal" (:imm1 (:reg :anymem)) (#xd0 4) :cpu086 (:bwlq-suf :w :modrm))
    ("sal" (:imm8 (:reg :anymem)) (#xc0 4) :cpu186 (:bwlq-suf :w :modrm))
    ("sal" (:shiftcount (:reg :anymem)) (#xd2 4) :cpu086 (:bwlq-suf :w :modrm))
    ("sal" ((:reg :anymem)) (#xd0 4) :cpu086 (:bwlq-suf :w :modrm))

    ("shl" (:imm1 (:reg :anymem)) (#xd0 4) :cpu086 (:bwlq-suf :w :modrm))
    ("shl" (:imm8 (:reg :anymem)) (#xc0 4) :cpu186 (:bwlq-suf :w :modrm))
    ("shl" (:shiftcount (:reg :anymem)) (#xd2 4) :cpu086 (:bwlq-suf :w :modrm))
    ("shl" ((:reg :anymem)) (#xd0 4) :cpu086 (:bwlq-suf :w :modrm))

    ("shr" (:imm1 (:reg :anymem)) (#xd0 5) :cpu086 (:bwlq-suf :w :modrm))
    ("shr" (:imm8 (:reg :anymem)) (#xc0 5) :cpu186 (:bwlq-suf :w :modrm))
    ("shr" (:shiftcount (:reg :anymem)) (#xd2 5) :cpu086 (:bwlq-suf :w :modrm))
    ("shr" ((:reg :anymem)) (#xd0 5) :cpu086 (:bwlq-suf :w :modrm))

    ("sar" (:imm1 (:reg :anymem)) (#xd0 7) :cpu086 (:bwlq-suf :w :modrm))
    ("sar" (:imm8 (:reg :anymem)) (#xc0 7) :cpu186 (:bwlq-suf :w :modrm))
    ("sar" (:shiftcount (:reg :anymem)) (#xd2 7) :cpu086 (:bwlq-suf :w :modrm))
    ("sar" ((:reg :anymem)) (#xd0 7) :cpu086 (:bwlq-suf :w :modrm))

    ("shld" (:imm8 :wordreg (:wordreg :wordmem)) (#x0fa4) :cpu386 (:wlq-suf :modrm))
    ("shld" (:shiftcount :wordreg (:wordreg :wordmem)) (#x0fa5) :cpu386 (:wlq-suf :modrm))
    ("shld" (:wordreg (:wordreg :wordmem)) (#x0fa5) :cpu386 (:wlq-suf :modrm))

    ("shrd" (:imm8 :wordreg (:wordreg :wordmem)) (#x0fac) :cpu386 (:wlq-suf :modrm))
    ("shrd" (:shiftcount :wordreg (:wordreg :wordmem)) (#x0fad) :cpu386 (:wlq-suf :modrm))
    ("shrd" (:wordreg (:wordreg :wordmem)) (#x0fad) :cpu386 (:wlq-suf :modrm))

    ;; Control transfer instructions.
    ("call" ((:disp16 :disp32)) (#xe8) :cpuno64 (:wl-suf :jumpdword :defaultsize))
    ("call" ((:disp16 :disp32)) (#xe8) :cpu64 (:wq-suf :jumpdword :defaultsize :norex64))
    ("call" ((:wordreg :wordmem :jumpabsolute)) (#xff 2) :cpuno64 (:wl-suf :modrm :defaultsize))
    ("call" ((:reg16 :reg64 :wordmem :llongmem :jumpabsolute)) (#xff 2) :cpu64 (:wq-suf :modrm :defaultsize :norex64))
    ;; Intel Syntax
    ;; ("call" (:imm16 (:imm16 :imm32)) (#x9a) :cpuno64 (:wl-suf :jumpintersegment :defaultsize))
    ;; Intel Syntax
    ;; ("call" ((:wordmem :jumpabsolute)) (#xff 3) :cpu086 (:x-suf :modrm :defaultsize))
    ("lcall" (:imm16 (:imm16 :imm32)) (#x9a) :cpuno64 (:wl-suf :jumpintersegment :defaultsize))
    ("lcall" ((:wordmem :jumpabsolute)) (#xff 3) :cpu086 (:wl-suf :modrm :defaultsize))


    ("jmp" (:disp) (#xe9) :cpu086 (:nosuf :jump))
    ("jmp" ((:wordreg :wordmem :jumpabsolute)) (#xff 4) :cpuno64 (:wl-suf :modrm))
    ("jmp" ((:reg16 :reg64 :shortmem :llongmem :jumpabsolute)) (#xff 4) :cpu64 (:wq-suf :modrm :norex64))
    ;; Intel Syntax.
    ;;("jmp" (:imm16 (:imm16 :imm32)) (#xea) :cpuno64 (:wl-suf :jumpintersegment))
    ;; Intel Syntax.
    ;; ("jmp" ((:wordmem :jumpabsolute)) (#xff 5) :cpu086 (:x-suf :modrm))
    ("ljmp" (:imm16 (:imm16 :imm32)) (#xea) :cpuno64 (:wl-suf :jumpintersegment))
    ("ljmp" ((:wordmem :jumpabsolute)) (#xff 5) :cpu086 (:wl-suf :modrm))

    ("ret" () (#xc3) :cpuno64 (:wl-suf :defaultsize))
    ("ret" (:imm16) (#xc2) :cpuno64 (:wl-suf :defaultsize))
    ("ret" () (#xc3) :cpu64 (:wq-suf :defaultsize :norex64))
    ("ret" (:imm16) (#xc2) :cpu64 (:wq-suf :defaultsize :norex64))
    ("lret" () (#xcb) :cpu086 (:wlq-suf :defaultsize))
    ("lret" (:imm16) (#xca) :cpu086 (:wlq-suf :defaultsize))
    ("enter" (:imm16 :imm8) (#xc8) (:cpu186 :cpuno64) (:wl-suf :defaultsize))
    ("enter" (:imm16 :imm8) (#xc8) :cpu64 (:wq-suf :defaultsize :norex64))
    ("leave" () (#xc9) (:cpu186 :cpuno64) (:wl-suf :defaultsize))
    ("leave" () (#xc9) :cpu64 (:wq-suf :defaultsize :norex64))

    ;; Conditional jumps.
    ("jo" (:disp) (#x0f80) :cpu086 (:nosuf :jump))
    ("jno" (:disp) (#x0f81) :cpu086 (:nosuf :jump))
    ("jb" (:disp) (#x0f82) :cpu086 (:nosuf :jump))
    ("jc" (:disp) (#x0f82) :cpu086 (:nosuf :jump))
    ("jnae" (:disp) (#x0f82) :cpu086 (:nosuf :jump))
    ("jnb" (:disp) (#x0f83) :cpu086 (:nosuf :jump))
    ("jnc" (:disp) (#x0f83) :cpu086 (:nosuf :jump))
    ("jae" (:disp) (#x0f83) :cpu086 (:nosuf :jump))
    ("je" (:disp) (#x0f84) :cpu086 (:nosuf :jump))
    ("jz" (:disp) (#x0f84) :cpu086 (:nosuf :jump))
    ("jne" (:disp) (#x0f85) :cpu086 (:nosuf :jump))
    ("jnz" (:disp) (#x0f85) :cpu086 (:nosuf :jump))
    ("jbe" (:disp) (#x0f86) :cpu086 (:nosuf :jump))
    ("jna" (:disp) (#x0f86) :cpu086 (:nosuf :jump))
    ("jnbe" (:disp) (#x0f87) :cpu086 (:nosuf :jump))
    ("ja" (:disp) (#x0f87) :cpu086 (:nosuf :jump))
    ("js" (:disp) (#x0f88) :cpu086 (:nosuf :jump))
    ("jns" (:disp) (#x0f89) :cpu086 (:nosuf :jump))
    ("jp" (:disp) (#x0f8a) :cpu086 (:nosuf :jump))
    ("jpe" (:disp) (#x0f8a) :cpu086 (:nosuf :jump))
    ("jnp" (:disp) (#x0f8b) :cpu086 (:nosuf :jump))
    ("jpo" (:disp) (#x0f8b) :cpu086 (:nosuf :jump))
    ("jl" (:disp) (#x0f8c) :cpu086 (:nosuf :jump))
    ("jnge" (:disp) (#x0f8c) :cpu086 (:nosuf :jump))
    ("jnl" (:disp) (#x0f8d) :cpu086 (:nosuf :jump))
    ("jge" (:disp) (#x0f8d) :cpu086 (:nosuf :jump))
    ("jle" (:disp) (#x0f8e) :cpu086 (:nosuf :jump))
    ("jng" (:disp) (#x0f8e) :cpu086 (:nosuf :jump))
    ("jnle" (:disp) (#x0f8f) :cpu086 (:nosuf :jump))
    ("jg" (:disp) (#x0f8f) :cpu086 (:nosuf :jump))

    ;; jcxz vs. jecxz is chosen on the basis of the address size prefix.
    ("jcxz" (:disp) (#xe3) :cpuno64 (:nosuf :jumpbyte :size16))
    ("jecxz" (:disp) (#xe3) :cpuno64 (:nosuf :jumpbyte :size32))
    ("jecxz" (:disp) (#x67e3) :cpu64 (:nosuf :jumpbyte :size32))
    ("jrcxz" (:disp) (#xe3) :cpu64 (:nosuf :jumpbyte :size64 :norex64))

    ;; The loop instructions also use the address size prefix to select
    ;; %cx rather than %ecx for the loop count so the `w' form of these
    ;; instructions emit an address size prefix rather than a data size
    ;; prefix.
    ("loop" (:disp) (#xe2) :cpuno64 (:wl-suf :jumpbyte))
    ("loop" (:disp) (#xe2) :cpu64 (:lq-suf :jumpbyte :norex64))
    ("loopz" (:disp) (#xe1) :cpuno64 (:wl-suf :jumpbyte))
    ("loopz" (:disp) (#xe1) :cpu64 (:lq-suf :jumpbyte :norex64))
    ("loope" (:disp) (#xe1) :cpuno64 (:wl-suf :jumpbyte))
    ("loope" (:disp) (#xe1) :cpu64 (:lq-suf :jumpbyte :norex64))
    ("loopnz" (:disp) (#xe0) :cpuno64 (:wl-suf :jumpbyte))
    ("loopnz" (:disp) (#xe0) :cpu64 (:lq-suf :jumpbyte :norex64))
    ("loopne" (:disp) (#xe0) :cpuno64 (:wl-suf :jumpbyte))
    ("loopne" (:disp) (#xe0) :cpu64 (:lq-suf :jumpbyte :norex64))

    ;; Set byte on flag instructions.
    ("seto" ((:reg8 :bytemem)) (#x0f90 0) :cpu386 (:b-suf :modrm))
    ("setno" ((:reg8 :bytemem)) (#x0f91 0) :cpu386 (:b-suf :modrm))
    ("setb" ((:reg8 :bytemem)) (#x0f92 0) :cpu386 (:b-suf :modrm))
    ("setc" ((:reg8 :bytemem)) (#x0f92 0) :cpu386 (:b-suf :modrm))
    ("setnae" ((:reg8 :bytemem)) (#x0f92 0) :cpu386 (:b-suf :modrm))
    ("setnb" ((:reg8 :bytemem)) (#x0f93 0) :cpu386 (:b-suf :modrm))
    ("setnc" ((:reg8 :bytemem)) (#x0f93 0) :cpu386 (:b-suf :modrm))
    ("setae" ((:reg8 :bytemem)) (#x0f93 0) :cpu386 (:b-suf :modrm))
    ("sete" ((:reg8 :bytemem)) (#x0f94 0) :cpu386 (:b-suf :modrm))
    ("setz" ((:reg8 :bytemem)) (#x0f94 0) :cpu386 (:b-suf :modrm))
    ("setne" ((:reg8 :bytemem)) (#x0f95 0) :cpu386 (:b-suf :modrm))
    ("setnz" ((:reg8 :bytemem)) (#x0f95 0) :cpu386 (:b-suf :modrm))
    ("setbe" ((:reg8 :bytemem)) (#x0f96 0) :cpu386 (:b-suf :modrm))
    ("setna" ((:reg8 :bytemem)) (#x0f96 0) :cpu386 (:b-suf :modrm))
    ("setnbe" ((:reg8 :bytemem)) (#x0f97 0) :cpu386 (:b-suf :modrm))
    ("seta" ((:reg8 :bytemem)) (#x0f97 0) :cpu386 (:b-suf :modrm))
    ("sets" ((:reg8 :bytemem)) (#x0f98 0) :cpu386 (:b-suf :modrm))
    ("setns" ((:reg8 :bytemem)) (#x0f99 0) :cpu386 (:b-suf :modrm))
    ("setp" ((:reg8 :bytemem)) (#x0f9a 0) :cpu386 (:b-suf :modrm))
    ("setpe" ((:reg8 :bytemem)) (#x0f9a 0) :cpu386 (:b-suf :modrm))
    ("setnp" ((:reg8 :bytemem)) (#x0f9b 0) :cpu386 (:b-suf :modrm))
    ("setpo" ((:reg8 :bytemem)) (#x0f9b 0) :cpu386 (:b-suf :modrm))
    ("setl" ((:reg8 :bytemem)) (#x0f9c 0) :cpu386 (:b-suf :modrm))
    ("setnge" ((:reg8 :bytemem)) (#x0f9c 0) :cpu386 (:b-suf :modrm))
    ("setnl" ((:reg8 :bytemem)) (#x0f9d 0) :cpu386 (:b-suf :modrm))
    ("setge" ((:reg8 :bytemem)) (#x0f9d 0) :cpu386 (:b-suf :modrm))
    ("setle" ((:reg8 :bytemem)) (#x0f9e 0) :cpu386 (:b-suf :modrm))
    ("setng" ((:reg8 :bytemem)) (#x0f9e 0) :cpu386 (:b-suf :modrm))
    ("setnle" ((:reg8 :bytemem)) (#x0f9f 0) :cpu386 (:b-suf :modrm))
    ("setg" ((:reg8 :bytemem)) (#x0f9f 0) :cpu386 (:b-suf :modrm))

    ;; String manipulation.
    ("cmps" () (#xa6) :cpu086 (:bwlq-suf :w :isstring))
    ("cmps" ((:anymem :esseg) :anymem) (#xa6) :cpu086 (:bwlq-suf :w :isstring))
    ("scmp" () (#xa6) :cpu086 (:bwlq-suf :w :isstring))
    ("scmp" ((:anymem :esseg) :anymem) (#xa6) :cpu086 (:bwlq-suf :w :isstring))
    ("ins" () (#x6c) :cpu186 (:bwl-suf :w :isstring))
    ("ins" (:inoutportreg (:anymem :esseg)) (#x6c) :cpu186 (:bwl-suf :w :isstring))
    ("outs" () (#x6e) :cpu186 (:bwl-suf :w :isstring))
    ("outs" (:anymem :inoutportreg) (#x6e) :cpu186 (:bwl-suf :w :isstring))
    ("lods" () (#xac) :cpu086 (:bwlq-suf :w :isstring))
    ("lods" (:anymem) (#xac) :cpu086 (:bwlq-suf :w :isstring))
    ("lods" (:anymem :acc) (#xac) :cpu086 (:bwlq-suf :w :isstring))
    ("slod" () (#xac) :cpu086 (:bwlq-suf :w :isstring))
    ("slod" (:anymem) (#xac) :cpu086 (:bwlq-suf :w :isstring))
    ("slod" (:anymem :acc) (#xac) :cpu086 (:bwlq-suf :w :isstring))
    ("movs" () (#xa4) :cpu086 (:bwlq-suf :w :isstring))
    ("movs" (:anymem (:anymem :esseg)) (#xa4) :cpu086 (:bwlq-suf :w :isstring))
    ("smov" () (#xa4) :cpu086 (:bwlq-suf :w :isstring))
    ("smov" (:anymem (:anymem :esseg)) (#xa4) :cpu086 (:bwlq-suf :w :isstring))
    ("scas" () (#xae) :cpu086 (:bwlq-suf :w :isstring))
    ("scas" ((:anymem :esseg)) (#xae) :cpu086 (:bwlq-suf :w :isstring))
    ("scas" ((:anymem :esseg) :acc) (#xae) :cpu086 (:bwlq-suf :w :isstring))
    ("ssca" () (#xae) :cpu086 (:bwlq-suf :w :isstring))
    ("ssca" ((:anymem :esseg)) (#xae) :cpu086 (:bwlq-suf :w :isstring))
    ("ssca" ((:anymem :esseg) :acc) (#xae) :cpu086 (:bwlq-suf :w :isstring))
    ("stos" () (#xaa) :cpu086 (:bwlq-suf :w :isstring))
    ("stos" ((:anymem :esseg)) (#xaa) :cpu086 (:bwlq-suf :w :isstring))
    ("stos" (:acc (:anymem :esseg)) (#xaa) :cpu086 (:bwlq-suf :w :isstring))
    ("ssto" () (#xaa) :cpu086 (:bwlq-suf :w :isstring))
    ("ssto" ((:anymem :esseg)) (#xaa) :cpu086 (:bwlq-suf :w :isstring))
    ("ssto" (:acc (:anymem :esseg)) (#xaa) :cpu086 (:bwlq-suf :w :isstring))
    ("xlat" () (#xd7) :cpu086 (:b-suf :isstring))
    ("xlat" (:anymem) (#xd7) :cpu086 (:b-suf :isstring))

    ;; Bit manipulation.
    ("bsf" ((:wordreg :wordmem) :wordreg) (#x0fbc) :cpu386 (:wlq-suf :modrm))
    ("bsr" ((:wordreg :wordmem) :wordreg) (#x0fbd) :cpu386 (:wlq-suf :modrm))
    ("bt" (:wordreg (:wordreg :wordmem)) (#x0fa3) :cpu386 (:wlq-suf :modrm))
    ("bt" (:imm8 (:wordreg :wordmem)) (#x0fba 4) :cpu386 (:wlq-suf :modrm))
    ("btc" (:wordreg (:wordreg :wordmem)) (#x0fbb) :cpu386 (:wlq-suf :modrm))
    ("btc" (:imm8 (:wordreg :wordmem)) (#x0fba 7) :cpu386 (:wlq-suf :modrm))
    ("btr" (:wordreg (:wordreg :wordmem)) (#x0fb3) :cpu386 (:wlq-suf :modrm))
    ("btr" (:imm8 (:wordreg :wordmem)) (#x0fba 6) :cpu386 (:wlq-suf :modrm))
    ("bts" (:wordreg (:wordreg :wordmem)) (#x0fab) :cpu386 (:wlq-suf :modrm))
    ("bts" (:imm8 (:wordreg :wordmem)) (#x0fba 5) :cpu386 (:wlq-suf :modrm))

    ;; Interrupts & op. sys insns.
    ("int" (:imm8) (#xcd) :cpu086 :nosuf)
    ("int3" () (#xcc) :cpu086 :nosuf)
    ("into" () (#xce) :cpuno64 :nosuf)
    ("iret" () (#xcf) :cpu086 (:wlq-suf :defaultsize))
    ;; i386sl i486sl later 486 and Pentium.
    ("rsm" () (#x0faa) :cpu386 :nosuf)

    ("bound" (:wordreg :wordmem) (#x62) (:cpu186 :cpuno64) (:wl-suf :modrm))

    ("hlt" () (#xf4) :cpu086 :nosuf)
    ;; nop is actually 'xchgl %eax %eax'.
    ("nop" () (#x90) :cpu086 :nosuf)

    ;; Protection control.
    ("arpl" (:reg16 (:reg16 :shortmem)) (#x63) (:cpu286 :cpuno64) (:w-suf :modrm :ignoresize))
    ("lar" ((:wordreg :wordmem) :wordreg) (#x0f02) :cpu286 (:wlq-suf :modrm))
    ("lgdt" (:wordmem) (#x0f01 2) (:cpu286 :cpuno64) (:wl-suf :modrm))
    ("lgdt" (:llongmem) (#x0f01 2) :cpu64 (:q-suf :modrm :norex64))
    ("lidt" (:wordmem) (#x0f01 3) (:cpu286 :cpuno64) (:wl-suf :modrm))
    ("lidt" (:llongmem) (#x0f01 3) :cpu64 (:q-suf :modrm :norex64))
    ("lldt" ((:reg16 :shortmem)) (#x0f00 2) :cpu286 (:w-suf :modrm :ignoresize))
    ("lmsw" ((:reg16 :shortmem)) (#x0f01 6) :cpu286 (:w-suf :modrm :ignoresize))
    ("lsl" ((:wordreg :wordmem) :wordreg) (#x0f03) :cpu286 (:wlq-suf :modrm))
    ("ltr" ((:reg16 :shortmem)) (#x0f00 3) :cpu286 (:w-suf :modrm :ignoresize))
    ("sgdt" (:wordmem) (#x0f01 0) (:cpu286 :cpuno64) (:wl-suf :modrm))
    ("sgdt" (:llongmem) (#x0f01 0) :cpu64 (:q-suf :modrm :norex64))
    ("sidt" (:wordmem) (#x0f01 1) (:cpu286 :cpuno64) (:wl-suf :modrm))
    ("sidt" (:llongmem) (#x0f01 1) :cpu64 (:q-suf :modrm :norex64))
    ("sldt" ((:wordreg :invmem)) (#x0f00 0) :cpu286 (:wlq-suf :modrm))
    ("sldt" (:shortmem) (#x0f00 0) :cpu286 (:w-suf :modrm :ignoresize))
    ("smsw" ((:wordreg :invmem)) (#x0f01 4) :cpu286 (:wlq-suf :modrm))
    ("smsw" (:shortmem) (#x0f01 4) :cpu286 (:w-suf :modrm :ignoresize))
    ("str" ((:wordreg :invmem)) (#x0f00 1) :cpu286 (:wlq-suf :modrm))
    ("str" (:shortmem) (#x0f00 1) :cpu286 (:w-suf :modrm :ignoresize))

    ("verr" ((:reg16 :shortmem)) (#x0f00 4) :cpu286 (:w-suf :modrm :ignoresize))
    ("verw" ((:reg16 :shortmem)) (#x0f00 5) :cpu286 (:w-suf :modrm :ignoresize))

    ;; Floating point instructions.

    ;; load
    ("fld" (:floatreg) (#xd9c0) :cpu086 (:fp :shortform))
    ("fld" ((:longmem :llongmem)) (#xd9 0) :cpu086 (:sl-fp :modrm))
    ("fld" (:floatreg) (#xd9c0) :cpu086 (:l-fp :shortform :ignoresize :ugh))
    ;; Intel Syntax
    ;;("fld" (:llongmem) (#xdb 5) :cpu086 (:x-fp :modrm))
    ("fild" ((:shortmem :longmem)) (#xdf 0) :cpu086 (:sl-fp :modrm))
    ("fild" (:llongmem) (#xdf 5) :cpu086 (:q-fp :modrm))
    ("fildll" (:llongmem) (#xdf 5) :cpu086 (:fp :modrm))
    ("fldt" (:llongmem) (#xdb 5) :cpu086 (:fp :modrm))
    ("fbld" (:llongmem) (#xdf 4) :cpu086 (:x-suf :modrm))

    ;; store (no pop)
    ("fst" (:floatreg) (#xddd0) :cpu086 (:fp :shortform))
    ("fst" ((:longmem :llongmem)) (#xd9 2) :cpu086 (:sl-fp :modrm))
    ("fst" (:floatreg) (#xddd0) :cpu086 (:l-fp :shortform :ignoresize :ugh))
    ("fist" ((:shortmem :longmem)) (#xdf 2) :cpu086 (:sl-fp :modrm))

    ;; store (with pop)
    ("fstp" (:floatreg) (#xddd8) :cpu086 (:fp :shortform))
    ("fstp" ((:longmem :llongmem)) (#xd9 3) :cpu086 (:sl-fp :modrm))
    ("fstp" (:floatreg) (#xddd8) :cpu086 (:l-fp :shortform :ignoresize :ugh))
    ;; Intel Syntax
    ;;("fstp" (:llongmem) (#xdb 7) :cpu086 (:x-fp :modrm))
    ("fistp" ((:shortmem :longmem)) (#xdf 3) :cpu086 (:sl-fp :modrm))
    ("fistp" (:llongmem) (#xdf 7) :cpu086 (:q-fp :modrm))
    ("fistpll" (:llongmem) (#xdf 7) :cpu086 (:fp :modrm))
    ("fstpt" (:llongmem) (#xdb 7) :cpu086 (:fp :modrm))
    ("fbstp" (:llongmem) (#xdf 6) :cpu086 (:x-suf :modrm))

    ;; exchange %st<n> with %st0
    ("fxch" (:floatreg) (#xd9c8) :cpu086 (:fp :shortform))
    ;; alias for fxch %st(1)
    ("fxch" () (#xd9c9) :cpu086 :fp)

    ;; comparison (without pop)
    ("fcom" (:floatreg) (#xd8d0) :cpu086 (:fp :shortform))
    ;; alias for fcom %st(1)
    ("fcom" () (#xd8d1) :cpu086 :fp)
    ("fcom" ((:longmem :llongmem)) (#xd8 2) :cpu086 (:sl-fp :modrm))
    ("fcom" (:floatreg) (#xd8d0) :cpu086 (:l-fp :shortform :ignoresize :ugh))
    ("ficom" ((:shortmem :longmem)) (#xde 2) :cpu086 (:sl-fp :modrm))

    ;; comparison (with pop)
    ("fcomp" (:floatreg) (#xd8d8) :cpu086 (:fp :shortform))
    ;; alias for fcomp %st(1)
    ("fcomp" () (#xd8d9) :cpu086 :fp)
    ("fcomp" ((:longmem :llongmem)) (#xd8 3) :cpu086 (:sl-fp :modrm))
    ("fcomp" (:floatreg) (#xd8d8) :cpu086 (:l-fp :shortform :ignoresize :ugh))
    ("ficomp" ((:shortmem :longmem)) (#xde 3) :cpu086 (:sl-fp :modrm))
    ("fcompp" () (#xded9) :cpu086 :fp)

    ;; unordered comparison (with pop)
    ("fucom" (:floatreg) (#xdde0) :cpu286 (:fp :shortform))
    ;; alias for fucom %st(1)
    ("fucom" () (#xdde1) :cpu286 :fp)
    ("fucomp" (:floatreg) (#xdde8) :cpu286 (:fp :shortform))
    ;; alias for fucomp %st(1)
    ("fucomp" () (#xdde9) :cpu286 :fp)
    ("fucompp" () (#xdae9) :cpu286 :fp)

    ("ftst" () (#xd9e4) :cpu086 :fp)
    ("fxam" () (#xd9e5) :cpu086 :fp)

    ;; load constants into %st0
    ("fld1" () (#xd9e8) :cpu086 :fp)
    ("fldl2t" () (#xd9e9) :cpu086 :fp)
    ("fldl2e" () (#xd9ea) :cpu086 :fp)
    ("fldpi" () (#xd9eb) :cpu086 :fp)
    ("fldlg2" () (#xd9ec) :cpu086 :fp)
    ("fldln2" () (#xd9ed) :cpu086 :fp)
    ("fldz" () (#xd9ee) :cpu086 :fp)

    ;; Arithmetic.

    ;; add
    ("fadd" (:floatreg :floatacc) (#xd8c0) :cpu086 (:fp :shortform :floatd))
    ;; alias for fadd %st(i) %st
    ("fadd" (:floatreg) (#xd8c0) :cpu086 (:fp :shortform))
    ;; alias for faddp
    ("fadd" () (#xdec1) :cpu086 (:fp :ugh))
    ("fadd" ((:longmem :llongmem)) (#xd8 0) :cpu086 (:sl-fp :modrm))
    ("fiadd" ((:shortmem :longmem)) (#xde 0) :cpu086 (:sl-fp :modrm))

    ("faddp" (:floatacc :floatreg) (#xdec0) :cpu086 (:fp :shortform))
    ("faddp" (:floatreg) (#xdec0) :cpu086 (:fp :shortform))
    ;; alias for faddp %st %st(1)
    ("faddp" () (#xdec1) :cpu086 :fp)
    ("faddp" (:floatreg :floatacc) (#xdec0) :cpu086 (:fp :shortform :ugh))

    ;; subtract
    ("fsub" (:floatreg :floatacc) (#xd8e0) :cpu086 (:fp :shortform :floatdr))
    ("fsub" (:floatreg) (#xd8e0) :cpu086 (:fp :shortform))
    ("fsub" () (#xdee1) :cpu086 (:fp :ugh))
    ("fsub" ((:longmem :llongmem)) (#xd8 4) :cpu086 (:sl-fp :modrm))
    ("fisub" ((:shortmem :longmem)) (#xde 4) :cpu086 (:sl-fp :modrm))

    ("fsubp" (:floatacc :floatreg) (#xdee0) :cpu086 (:fp :shortform))
    ("fsubp" (:floatreg) (#xdee0) :cpu086 (:fp :shortform))
    ("fsubp" () (#xdee1) :cpu086 :fp)

    ("fsubp" (:floatacc :floatreg) (#xdee8) :cpu086 (:fp :shortform))
    ("fsubp" (:floatreg) (#xdee8) :cpu086 (:fp :shortform))
    ("fsubp" () (#xdee9) :cpu086 :fp)


    ;; subtract reverse
    ("fsubr" (:floatreg :floatacc) (#xd8e8) :cpu086 (:fp :shortform :floatdr))
    ("fsubr" (:floatreg) (#xd8e8) :cpu086 (:fp :shortform))
    ;; alias for fsubrp
    ("fsubr" () (#xdee9) :cpu086 (:fp :ugh))
    ("fsubr" ((:longmem :llongmem)) (#xd8 5) :cpu086 (:sl-fp :modrm))
    ("fisubr" ((:shortmem :longmem)) (#xde 5) :cpu086 (:sl-fp :modrm))
    ("fsubrp" (:floatacc :floatreg) (#xdee8) :cpu086 (:fp :shortform))
    ("fsubrp" (:floatreg) (#xdee8) :cpu086 (:fp :shortform))
    ("fsubrp" () (#xdee9) :cpu086 :fp)
    ("fsubrp" (:floatacc :floatreg) (#xdee0) :cpu086 (:fp :shortform))
    ("fsubrp" (:floatreg) (#xdee0) :cpu086 (:fp :shortform))
    ("fsubrp" () (#xdee1) :cpu086 :fp)

    ;; multiply
    ("fmul" (:floatreg :floatacc) (#xd8c8) :cpu086 (:fp :shortform :floatd))
    ("fmul" (:floatreg) (#xd8c8) :cpu086 (:fp :shortform))
    ;; alias for fmulp
    ("fmul" () (#xdec9) :cpu086 (:fp :ugh))
    ("fmul" ((:longmem :llongmem)) (#xd8 1) :cpu086 (:sl-fp :modrm))
    ("fimul" ((:shortmem :longmem)) (#xde 1) :cpu086 (:sl-fp :modrm))

    ("fmulp" (:floatacc :floatreg) (#xdec8) :cpu086 (:fp :shortform))
    ("fmulp" (:floatreg) (#xdec8) :cpu086 (:fp :shortform))
    ("fmulp" () (#xdec9) :cpu086 :fp)
    ("fmulp" (:floatreg :floatacc) (#xdec8) :cpu086 (:fp :shortform :ugh))

    ;; divide
    ("fdiv" (:floatreg :floatacc) (#xd8f0) :cpu086 (:fp :shortform :floatdr))
    ("fdiv" (:floatreg) (#xd8f0) :cpu086 (:fp :shortform))
    ;; alias for fdivp
    ("fdiv" () (#xdef1) :cpu086 (:fp :ugh))
    ("fdiv" ((:longmem :llongmem)) (#xd8 6) :cpu086 (:sl-fp :modrm))
    ("fidiv" ((:shortmem :longmem)) (#xde 6) :cpu086 (:sl-fp :modrm))
    ("fdivp" (:floatacc :floatreg) (#xdef0) :cpu086 (:fp :shortform))
    ("fdivp" (:floatreg) (#xdef0) :cpu086 (:fp :shortform))
    ("fdivp" () (#xdef1) :cpu086 :fp)
    ("fdivp" (:floatacc :floatreg) (#xdef8) :cpu086 (:fp :shortform))
    ("fdivp" (:floatreg) (#xdef8) :cpu086 (:fp :shortform))
    ("fdivp" () (#xdef9) :cpu086 :fp)


    ;; divide reverse
    ("fdivr" (:floatreg :floatacc) (#xd8f8) :cpu086 (:fp :shortform :floatdr))
    ("fdivr" (:floatreg) (#xd8f8) :cpu086 (:fp :shortform))
    ;; alias for fdivrp
    ("fdivr" () (#xdef9) :cpu086 (:fp :ugh))
    ("fdivr" ((:longmem :llongmem)) (#xd8 7) :cpu086 (:sl-fp :modrm))
    ("fidivr" ((:shortmem :longmem)) (#xde 7) :cpu086 (:sl-fp :modrm))
    ("fdivrp" (:floatacc :floatreg) (#xdef8) :cpu086 (:fp :shortform))
    ("fdivrp" (:floatreg) (#xdef8) :cpu086 (:fp :shortform))
    ("fdivrp" () (#xdef9) :cpu086 :fp)
    ("fdivrp" (:floatacc :floatreg) (#xdef0) :cpu086 (:fp :shortform))
    ("fdivrp" (:floatreg) (#xdef0) :cpu086 (:fp :shortform))
    ("fdivrp" () (#xdef1) :cpu086 :fp)

    ("f2xm1" () (#xd9f0) :cpu086 :fp)
    ("fyl2x" () (#xd9f1) :cpu086 :fp)
    ("fptan" () (#xd9f2) :cpu086 :fp)
    ("fpatan" () (#xd9f3) :cpu086 :fp)
    ("fxtract" () (#xd9f4) :cpu086 :fp)
    ("fprem1" () (#xd9f5) :cpu286 :fp)
    ("fdecstp" () (#xd9f6) :cpu086 :fp)
    ("fincstp" () (#xd9f7) :cpu086 :fp)
    ("fprem" () (#xd9f8) :cpu086 :fp)
    ("fyl2xp1" () (#xd9f9) :cpu086 :fp)
    ("fsqrt" () (#xd9fa) :cpu086 :fp)
    ("fsincos" () (#xd9fb) :cpu286 :fp)
    ("frndint" () (#xd9fc) :cpu086 :fp)
    ("fscale" () (#xd9fd) :cpu086 :fp)
    ("fsin" () (#xd9fe) :cpu286 :fp)
    ("fcos" () (#xd9ff) :cpu286 :fp)
    ("fchs" () (#xd9e0) :cpu086 :fp)
    ("fabs" () (#xd9e1) :cpu086 :fp)

    ;; processor control
    ("fninit" () (#xdbe3) :cpu086 :fp)
    ("finit" () (#xdbe3) :cpu086 (:fp :fwait))
    ("fldcw" (:shortmem) (#xd9 5) :cpu086 (:w-suf :floatmf :modrm))
    ("fnstcw" (:shortmem) (#xd9 7) :cpu086 (:w-suf :floatmf :modrm))
    ("fstcw" (:shortmem) (#xd9 7) :cpu086 (:w-suf :floatmf :fwait :modrm))
    ;; XXX should reject %al %eax and %rax
    ("fnstsw" (:acc) (#xdfe0) :cpu086 (:fp :ignoresize))
    ("fnstsw" (:shortmem) (#xdd 7) :cpu086 (:w-suf :floatmf :modrm))
    ("fnstsw" () (#xdfe0) :cpu086 :fp)
    ;; XXX should reject %al %eax and %rax
    ("fstsw" (:acc) (#xdfe0) :cpu086 (:fp :fwait :ignoresize))
    ("fstsw" (:shortmem) (#xdd 7) :cpu086 (:w-suf :floatmf :fwait :modrm))
    ("fstsw" () (#xdfe0) :cpu086 (:fp :fwait))
    ("fnclex" () (#xdbe2) :cpu086 :fp)
    ("fclex" () (#xdbe2) :cpu086 (:fp :fwait))
    ;; Short forms of fldenv fstenv use data size prefix.
    ("fnstenv" (:llongmem) (#xd9 6) :cpu086 (:sl-suf :modrm :defaultsize))
    ("fstenv" (:llongmem) (#xd9 6) :cpu086 (:sl-suf :fwait :modrm :defaultsize))
    ("fldenv" (:llongmem) (#xd9 4) :cpu086 (:sl-suf :modrm :defaultsize))
    ("fnsave" (:llongmem) (#xdd 6) :cpu086 (:sl-suf :modrm :defaultsize))
    ("fsave" (:llongmem) (#xdd 6) :cpu086 (:sl-suf :fwait :modrm :defaultsize))
    ("frstor" (:llongmem) (#xdd 4) :cpu086 (:sl-suf :modrm :defaultsize))

    ("ffree" (:floatreg) (#xddc0) :cpu086 (:fp :shortform))
    ;; P6:free st(i) pop st
    ("ffreep" (:floatreg) (#xdfc0) :cpu686 (:fp :shortform))
    ("fnop" () (#xd9d0) :cpu086 :fp)
    ("fwait" () (#x9b) :cpu086 :fp)

    ;; Opcode prefixes; we allow them as separate insns too.
    ("addr16" () (#x67) (:cpu386 :cpuno64) (:nosuf :isprefix :size16 :ignoresize))
    ("addr32" () (#x67) :cpu386 (:nosuf :isprefix :size32 :ignoresize))
    ("aword" () (#x67) (:cpu386 :cpuno64) (:nosuf :isprefix :size16 :ignoresize))
    ("adword" () (#x67) :cpu386 (:nosuf :isprefix :size32 :ignoresize))
    ("data16" () (#x66) :cpu386 (:nosuf :isprefix :size16 :ignoresize))
    ("data32" () (#x66) (:cpu386 :cpuno64) (:nosuf :isprefix :size32 :ignoresize))
    ("word" () (#x66) :cpu386 (:nosuf :isprefix :size16 :ignoresize))
    ("dword" () (#x66) (:cpu386 :cpuno64) (:nosuf :isprefix :size32 :ignoresize))
    ("lock" () (#xf0) :cpu086 (:nosuf :isprefix))
    ("wait" () (#x9b) :cpu086 (:nosuf :isprefix))
    ("cs" () (#x2e) :cpu086 (:nosuf :isprefix))
    ("ds" () (#x3e) :cpu086 (:nosuf :isprefix))

    ("es" () (#x26) :cpuno64 (:nosuf :isprefix))
    ("fs" () (#x64) :cpu386 (:nosuf :isprefix))
    ("gs" () (#x65) :cpu386 (:nosuf :isprefix))

    ("ss" () (#x36) :cpuno64 (:nosuf :isprefix))
    ("rep" () (#xf3) :cpu086 (:nosuf :isprefix))
    ("repe" () (#xf3) :cpu086 (:nosuf :isprefix))
    ("repz" () (#xf3) :cpu086 (:nosuf :isprefix))
    ("repne" () (#xf2) :cpu086 (:nosuf :isprefix))
    ("repnz" () (#xf2) :cpu086 (:nosuf :isprefix))
    ("rex" () (#x40) :cpu64 (:nosuf :isprefix))
    ("rexz" () (#x41) :cpu64 (:nosuf :isprefix))
    ("rexy" () (#x42) :cpu64 (:nosuf :isprefix))
    ("rexyz" () (#x43) :cpu64 (:nosuf :isprefix))
    ("rexx" () (#x44) :cpu64 (:nosuf :isprefix))
    ("rexxz" () (#x45) :cpu64 (:nosuf :isprefix))
    ("rexxy" () (#x46) :cpu64 (:nosuf :isprefix))
    ("rexxyz" () (#x47) :cpu64 (:nosuf :isprefix))
    ("rex64" () (#x48) :cpu64 (:nosuf :isprefix))
    ("rex64z" () (#x49) :cpu64 (:nosuf :isprefix))
    ("rex64y" () (#x4a) :cpu64 (:nosuf :isprefix))
    ("rex64yz" () (#x4b) :cpu64 (:nosuf :isprefix))
    ("rex64x" () (#x4c) :cpu64 (:nosuf :isprefix))
    ("rex64xz" () (#x4d) :cpu64 (:nosuf :isprefix))
    ("rex64xy" () (#x4e) :cpu64 (:nosuf :isprefix))
    ("rex64xyz" () (#x4f) :cpu64 (:nosuf :isprefix))

    ;; 486 extensions.

    ("bswap" ((:reg32 :reg64)) (#x0fc8) :cpu486 (:lq-suf :shortform))
    ("xadd" (:reg (:reg :anymem)) (#x0fc0) :cpu486 (:bwlq-suf :w :modrm))
    ("cmpxchg" (:reg (:reg :anymem)) (#x0fb0) :cpu486 (:bwlq-suf :w :modrm))
    ("invd" () (#x0f08) :cpu486 :nosuf)
    ("wbinvd" () (#x0f09) :cpu486 :nosuf)
    ("invlpg" (:anymem) (#x0f01 7) :cpu486 (:nosuf :modrm :ignoresize))

    ;; 586 and late 486 extensions.
    ("cpuid" () (#x0fa2) :cpu486 :nosuf)

    ;; Pentium extensions.
    ("wrmsr" () (#x0f30) :cpu586 :nosuf)
    ("rdtsc" () (#x0f31) :cpu586 :nosuf)
    ("rdmsr" () (#x0f32) :cpu586 :nosuf)
    ("cmpxchg8b" (:llongmem) (#x0fc7 1) :cpu586 (:q-suf :modrm))

    ;; Pentium II/Pentium Pro extensions.
    ("sysenter" () (#x0f34) :cpu686 :nosuf)
    ("sysexit" () (#x0f35) :cpu686 :nosuf)
    ("fxsave" (:llongmem) (#x0fae 0) :cpu686 (:q-suf :modrm))
    ("fxrstor" (:llongmem) (#x0fae 1) :cpu686 (:q-suf :modrm))
    ("rdpmc" () (#x0f33) :cpu686 :nosuf)
    ;; official undefined instr.
    ("ud2" () (#x0f0b) :cpu686 :nosuf)
    ;; alias for ud2
    ("ud2a" () (#x0f0b) :cpu686 :nosuf)
    ;; 2nd. official undefined instr.
    ("ud2b" () (#x0fb9) :cpu686 :nosuf)

    ("cmovo" ((:wordreg :wordmem) :wordreg) (#x0f40) :cpu686 (:wlq-suf :modrm))
    ("cmovno" ((:wordreg :wordmem) :wordreg) (#x0f41) :cpu686 (:wlq-suf :modrm))
    ("cmovb" ((:wordreg :wordmem) :wordreg) (#x0f42) :cpu686 (:wlq-suf :modrm))
    ("cmovc" ((:wordreg :wordmem) :wordreg) (#x0f42) :cpu686 (:wlq-suf :modrm))
    ("cmovnae" ((:wordreg :wordmem) :wordreg) (#x0f42) :cpu686 (:wlq-suf :modrm))
    ("cmovae" ((:wordreg :wordmem) :wordreg) (#x0f43) :cpu686 (:wlq-suf :modrm))
    ("cmovnc" ((:wordreg :wordmem) :wordreg) (#x0f43) :cpu686 (:wlq-suf :modrm))
    ("cmovnb" ((:wordreg :wordmem) :wordreg) (#x0f43) :cpu686 (:wlq-suf :modrm))
    ("cmove" ((:wordreg :wordmem) :wordreg) (#x0f44) :cpu686 (:wlq-suf :modrm))
    ("cmovz" ((:wordreg :wordmem) :wordreg) (#x0f44) :cpu686 (:wlq-suf :modrm))
    ("cmovne" ((:wordreg :wordmem) :wordreg) (#x0f45) :cpu686 (:wlq-suf :modrm))
    ("cmovnz" ((:wordreg :wordmem) :wordreg) (#x0f45) :cpu686 (:wlq-suf :modrm))
    ("cmovbe" ((:wordreg :wordmem) :wordreg) (#x0f46) :cpu686 (:wlq-suf :modrm))
    ("cmovna" ((:wordreg :wordmem) :wordreg) (#x0f46) :cpu686 (:wlq-suf :modrm))
    ("cmova" ((:wordreg :wordmem) :wordreg) (#x0f47) :cpu686 (:wlq-suf :modrm))
    ("cmovnbe" ((:wordreg :wordmem) :wordreg) (#x0f47) :cpu686 (:wlq-suf :modrm))
    ("cmovs" ((:wordreg :wordmem) :wordreg) (#x0f48) :cpu686 (:wlq-suf :modrm))
    ("cmovns" ((:wordreg :wordmem) :wordreg) (#x0f49) :cpu686 (:wlq-suf :modrm))
    ("cmovp" ((:wordreg :wordmem) :wordreg) (#x0f4a) :cpu686 (:wlq-suf :modrm))
    ("cmovnp" ((:wordreg :wordmem) :wordreg) (#x0f4b) :cpu686 (:wlq-suf :modrm))
    ("cmovl" ((:wordreg :wordmem) :wordreg) (#x0f4c) :cpu686 (:wlq-suf :modrm))
    ("cmovnge" ((:wordreg :wordmem) :wordreg) (#x0f4c) :cpu686 (:wlq-suf :modrm))
    ("cmovge" ((:wordreg :wordmem) :wordreg) (#x0f4d) :cpu686 (:wlq-suf :modrm))
    ("cmovnl" ((:wordreg :wordmem) :wordreg) (#x0f4d) :cpu686 (:wlq-suf :modrm))
    ("cmovle" ((:wordreg :wordmem) :wordreg) (#x0f4e) :cpu686 (:wlq-suf :modrm))
    ("cmovng" ((:wordreg :wordmem) :wordreg) (#x0f4e) :cpu686 (:wlq-suf :modrm))
    ("cmovg" ((:wordreg :wordmem) :wordreg) (#x0f4f) :cpu686 (:wlq-suf :modrm))
    ("cmovnle" ((:wordreg :wordmem) :wordreg) (#x0f4f) :cpu686 (:wlq-suf :modrm))

    ("fcmovb" (:floatreg :floatacc) (#xdac0) :cpu686 (:fp :shortform))
    ("fcmovnae" (:floatreg :floatacc) (#xdac0) :cpu686 (:fp :shortform))
    ("fcmove" (:floatreg :floatacc) (#xdac8) :cpu686 (:fp :shortform))
    ("fcmovbe" (:floatreg :floatacc) (#xdad0) :cpu686 (:fp :shortform))
    ("fcmovna" (:floatreg :floatacc) (#xdad0) :cpu686 (:fp :shortform))
    ("fcmovu" (:floatreg :floatacc) (#xdad8) :cpu686 (:fp :shortform))
    ("fcmovae" (:floatreg :floatacc) (#xdbc0) :cpu686 (:fp :shortform))
    ("fcmovnb" (:floatreg :floatacc) (#xdbc0) :cpu686 (:fp :shortform))
    ("fcmovne" (:floatreg :floatacc) (#xdbc8) :cpu686 (:fp :shortform))
    ("fcmova" (:floatreg :floatacc) (#xdbd0) :cpu686 (:fp :shortform))
    ("fcmovnbe" (:floatreg :floatacc) (#xdbd0) :cpu686 (:fp :shortform))
    ("fcmovnu" (:floatreg :floatacc) (#xdbd8) :cpu686 (:fp :shortform))

    ("fcomi" (:floatreg :floatacc) (#xdbf0) :cpu686 (:fp :shortform))
    ("fcomi" () (#xdbf1) :cpu686 (:fp :shortform))
    ("fcomi" (:floatreg) (#xdbf0) :cpu686 (:fp :shortform))
    ("fucomi" (:floatreg :floatacc) (#xdbe8) :cpu686 (:fp :shortform))
    ("fucomi" () (#xdbe9) :cpu686 (:fp :shortform))
    ("fucomi" (:floatreg) (#xdbe8) :cpu686 (:fp :shortform))
    ("fcomip" (:floatreg :floatacc) (#xdff0) :cpu686 (:fp :shortform))
    ("fcompi" (:floatreg :floatacc) (#xdff0) :cpu686 (:fp :shortform))
    ("fcompi" () (#xdff1) :cpu686 (:fp :shortform))
    ("fcompi" (:floatreg) (#xdff0) :cpu686 (:fp :shortform))
    ("fucomip" (:floatreg :floatacc) (#xdfe8) :cpu686 (:fp :shortform))
    ("fucompi" (:floatreg :floatacc) (#xdfe8) :cpu686 (:fp :shortform))
    ("fucompi" () (#xdfe9) :cpu686 (:fp :shortform))
    ("fucompi" (:floatreg) (#xdfe8) :cpu686 (:fp :shortform))

    ;; Pentium4 extensions.

    ("movnti" (:wordreg :wordmem) (#x0fc3) :cpup4 (:wlq-suf :modrm))
    ("clflush" (:bytemem) (#x0fae 7) :cpup4 (:nosuf :modrm :ignoresize))
    ("lfence" () (#x0fae #xe8) :cpup4 (:nosuf :immext))
    ("mfence" () (#x0fae #xf0) :cpup4 (:nosuf :immext))
    ("pause" () (#xf390) :cpup4 :nosuf)

    ;; MMX/SSE2 instructions.

    ("emms" () (#x0f77) :cpummx :nosuf)
    ("movd" ((:reg32 :reg64 :longmem) :regmmx) (#x0f6e) :cpummx (:nosuf :ignoresize :modrm))
    ("movd" (:regmmx (:reg32 :reg64 :longmem)) (#x0f7e) :cpummx (:nosuf :ignoresize :modrm))
    ("movd" ((:reg32 :reg64 :llongmem) :regxmm) (#x660f6e) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movd" (:regxmm (:reg32 :reg64 :llongmem)) (#x660f7e) :cpusse2 (:nosuf :ignoresize :modrm))
    ;; In the 64bit mode the short form mov immediate is redefined to have
    ;; 64bit displacement value.
    ("movq" ((:regmmx :longmem) :regmmx) (#x0f6f) :cpummx (:nosuf :ignoresize :modrm))
    ("movq" (:regmmx (:regmmx :longmem)) (#x0f7f) :cpummx (:nosuf :ignoresize :modrm))
    ("movq" ((:regxmm :llongmem) :regxmm) (#xf30f7e) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movq" (:regxmm (:regxmm :llongmem)) (#x660fd6) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movq" (:reg64 (:reg64 :anymem)) (#x88) :cpu64 (:nosuf :d :w :modrm :size64))
    ("movq" (:imm32s (:reg64 :wordmem)) (#xc6 0) :cpu64 (:nosuf :w :modrm :size64))
    ("movq" (:imm64 :reg64) (#xb0) :cpu64 (:nosuf :w :shortform :size64))
    ;; Move to/from control debug registers. In the 16 or 32bit modes
    ;; they are 32bit. In the 64bit mode they are 64bit.
    ("movq" (:control (:reg64 :invmem)) (#x0f20) :cpu64 (:nosuf :d :modrm :ignoresize :norex64 :size64))
    ("movq" (:debug (:reg64 :invmem)) (#x0f21) :cpu64 (:nosuf :d :modrm :ignoresize :norex64 :size64))
    ;; Real MMX instructions.
    ("packssdw" ((:regmmx :longmem) :regmmx) (#x0f6b) :cpummx (:nosuf :ignoresize :modrm))
    ("packssdw" ((:regxmm :llongmem) :regxmm) (#x660f6b) :cpusse2 (:nosuf :ignoresize :modrm))
    ("packsswb" ((:regmmx :longmem) :regmmx) (#x0f63) :cpummx (:nosuf :ignoresize :modrm))
    ("packsswb" ((:regxmm :llongmem) :regxmm) (#x660f63) :cpusse2 (:nosuf :ignoresize :modrm))
    ("packuswb" ((:regmmx :longmem) :regmmx) (#x0f67) :cpummx (:nosuf :ignoresize :modrm))
    ("packuswb" ((:regxmm :llongmem) :regxmm) (#x660f67) :cpusse2 (:nosuf :ignoresize :modrm))
    ("paddb" ((:regmmx :longmem) :regmmx) (#x0ffc) :cpummx (:nosuf :ignoresize :modrm))
    ("paddb" ((:regxmm :llongmem) :regxmm) (#x660ffc) :cpusse2 (:nosuf :ignoresize :modrm))
    ("paddw" ((:regmmx :longmem) :regmmx) (#x0ffd) :cpummx (:nosuf :ignoresize :modrm))
    ("paddw" ((:regxmm :llongmem) :regxmm) (#x660ffd) :cpusse2 (:nosuf :ignoresize :modrm))
    ("paddd" ((:regmmx :longmem) :regmmx) (#x0ffe) :cpummx (:nosuf :ignoresize :modrm))
    ("paddd" ((:regxmm :llongmem) :regxmm) (#x660ffe) :cpusse2 (:nosuf :ignoresize :modrm))
    ("paddq" ((:regmmx :llongmem) :regmmx) (#x0fd4) :cpusse2 (:nosuf :ignoresize :modrm))
    ("paddq" ((:regxmm :llongmem) :regxmm) (#x660fd4) :cpusse2 (:nosuf :ignoresize :modrm))
    ("paddsb" ((:regmmx :longmem) :regmmx) (#x0fec) :cpummx (:nosuf :ignoresize :modrm))
    ("paddsb" ((:regxmm :llongmem) :regxmm) (#x660fec) :cpusse2 (:nosuf :ignoresize :modrm))
    ("paddsw" ((:regmmx :longmem) :regmmx) (#x0fed) :cpummx (:nosuf :ignoresize :modrm))
    ("paddsw" ((:regxmm :llongmem) :regxmm) (#x660fed) :cpusse2 (:nosuf :ignoresize :modrm))
    ("paddusb" ((:regmmx :longmem) :regmmx) (#x0fdc) :cpummx (:nosuf :ignoresize :modrm))
    ("paddusb" ((:regxmm :llongmem) :regxmm) (#x660fdc) :cpusse2 (:nosuf :ignoresize :modrm))
    ("paddusw" ((:regmmx :longmem) :regmmx) (#x0fdd) :cpummx (:nosuf :ignoresize :modrm))
    ("paddusw" ((:regxmm :llongmem) :regxmm) (#x660fdd) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pand" ((:regmmx :longmem) :regmmx) (#x0fdb) :cpummx (:nosuf :ignoresize :modrm))
    ("pand" ((:regxmm :llongmem) :regxmm) (#x660fdb) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pandn" ((:regmmx :longmem) :regmmx) (#x0fdf) :cpummx (:nosuf :ignoresize :modrm))
    ("pandn" ((:regxmm :llongmem) :regxmm) (#x660fdf) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pcmpeqb" ((:regmmx :longmem) :regmmx) (#x0f74) :cpummx (:nosuf :ignoresize :modrm))
    ("pcmpeqb" ((:regxmm :llongmem) :regxmm) (#x660f74) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pcmpeqw" ((:regmmx :longmem) :regmmx) (#x0f75) :cpummx (:nosuf :ignoresize :modrm))
    ("pcmpeqw" ((:regxmm :llongmem) :regxmm) (#x660f75) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pcmpeqd" ((:regmmx :longmem) :regmmx) (#x0f76) :cpummx (:nosuf :ignoresize :modrm))
    ("pcmpeqd" ((:regxmm :llongmem) :regxmm) (#x660f76) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pcmpgtb" ((:regmmx :longmem) :regmmx) (#x0f64) :cpummx (:nosuf :ignoresize :modrm))
    ("pcmpgtb" ((:regxmm :llongmem) :regxmm) (#x660f64) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pcmpgtw" ((:regmmx :longmem) :regmmx) (#x0f65) :cpummx (:nosuf :ignoresize :modrm))
    ("pcmpgtw" ((:regxmm :llongmem) :regxmm) (#x660f65) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pcmpgtd" ((:regmmx :longmem) :regmmx) (#x0f66) :cpummx (:nosuf :ignoresize :modrm))
    ("pcmpgtd" ((:regxmm :llongmem) :regxmm) (#x660f66) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pmaddwd" ((:regmmx :longmem) :regmmx) (#x0ff5) :cpummx (:nosuf :ignoresize :modrm))
    ("pmaddwd" ((:regxmm :llongmem) :regxmm) (#x660ff5) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pmulhw" ((:regmmx :longmem) :regmmx) (#x0fe5) :cpummx (:nosuf :ignoresize :modrm))
    ("pmulhw" ((:regxmm :llongmem) :regxmm) (#x660fe5) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pmullw" ((:regmmx :longmem) :regmmx) (#x0fd5) :cpummx (:nosuf :ignoresize :modrm))
    ("pmullw" ((:regxmm :llongmem) :regxmm) (#x660fd5) :cpusse2 (:nosuf :ignoresize :modrm))
    ("por" ((:regmmx :longmem) :regmmx) (#x0feb) :cpummx (:nosuf :ignoresize :modrm))
    ("por" ((:regxmm :llongmem) :regxmm) (#x660feb) :cpusse2 (:nosuf :ignoresize :modrm))
    ("psllw" ((:regmmx :longmem) :regmmx) (#x0ff1) :cpummx (:nosuf :ignoresize :modrm))
    ("psllw" ((:regxmm :llongmem) :regxmm) (#x660ff1) :cpusse2 (:nosuf :ignoresize :modrm))
    ("psllw" (:imm8 :regmmx) (#x0f71 6) :cpummx (:nosuf :ignoresize :modrm))
    ("psllw" (:imm8 :regxmm) (#x660f71 6) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pslld" ((:regmmx :longmem) :regmmx) (#x0ff2) :cpummx (:nosuf :ignoresize :modrm))
    ("pslld" ((:regxmm :llongmem) :regxmm) (#x660ff2) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pslld" (:imm8 :regmmx) (#x0f72 6) :cpummx (:nosuf :ignoresize :modrm))
    ("pslld" (:imm8 :regxmm) (#x660f72 6) :cpusse2 (:nosuf :ignoresize :modrm))
    ("psllq" ((:regxmm :llongmem) :regxmm) (#x660ff3) :cpusse2 (:nosuf :ignoresize :modrm))
    ("psllq" ((:regmmx :longmem) :regmmx) (#x0ff3) :cpummx (:nosuf :ignoresize :modrm))
    ("psllq" (:imm8 :regxmm) (#x660f73 6) :cpusse2(:nosuf :ignoresize :modrm))
    ("psraw" ((:regmmx :longmem) :regmmx) (#x0fe1) :cpummx (:nosuf :ignoresize :modrm))
    ("psraw" ((:regxmm :llongmem) :regxmm) (#x660fe1) :cpusse2(:nosuf :ignoresize :modrm))
    ("psraw" (:imm8 :regmmx) (#x0f71 4) :cpummx (:nosuf :ignoresize :modrm))
    ("psraw" (:imm8 :regxmm) (#x660f71 4) :cpusse2(:nosuf :ignoresize :modrm))
    ("psrad" ((:regmmx :longmem) :regmmx) (#x0fe2) :cpummx (:nosuf :ignoresize :modrm))
    ("psrad" ((:regxmm :llongmem) :regxmm) (#x660fe2) :cpusse2(:nosuf :ignoresize :modrm))
    ("psrad" (:imm8 :regmmx) (#x0f72 4) :cpummx (:nosuf :ignoresize :modrm))
    ("psrad" (:imm8 :regxmm) (#x660f72 4) :cpusse2(:nosuf :ignoresize :modrm))
    ("psrlw" ((:regmmx :longmem) :regmmx) (#x0fd1) :cpummx (:nosuf :ignoresize :modrm))
    ("psrlw" ((:regxmm :llongmem) :regxmm) (#x660fd1) :cpusse2(:nosuf :ignoresize :modrm))
    ("psrlw" (:imm8 :regmmx) (#x0f71 2) :cpummx (:nosuf :ignoresize :modrm))
    ("psrlw" (:imm8 :regxmm) (#x660f71 2) :cpusse2(:nosuf :ignoresize :modrm))
    ("psrld" ((:regmmx :longmem) :regmmx) (#x0fd2) :cpummx (:nosuf :ignoresize :modrm))
    ("psrld" ((:regxmm :llongmem) :regxmm) (#x660fd2) :cpusse2(:nosuf :ignoresize :modrm))
    ("psrld" (:imm8 :regmmx) (#x0f72 2) :cpummx (:nosuf :ignoresize :modrm))
    ("psrld" (:imm8 :regxmm) (#x660f72 2) :cpusse2(:nosuf :ignoresize :modrm))
    ("psrlq" ((:regmmx :longmem) :regmmx) (#x0fd3) :cpummx (:nosuf :ignoresize :modrm))
    ("psrlq" ((:regxmm :llongmem) :regxmm) (#x660fd3) :cpusse2(:nosuf :ignoresize :modrm))
    ("psrlq" (:imm8 :regmmx) (#x0f73 2) :cpummx (:nosuf :ignoresize :modrm))
    ("psrlq" (:imm8 :regxmm) (#x660f73 2) :cpusse2(:nosuf :ignoresize :modrm))
    ("psubb" ((:regmmx :longmem) :regmmx) (#x0ff8) :cpummx (:nosuf :ignoresize :modrm))
    ("psubb" ((:regxmm :llongmem) :regxmm) (#x660ff8) :cpusse2(:nosuf :ignoresize :modrm))
    ("psubw" ((:regmmx :longmem) :regmmx) (#x0ff9) :cpummx (:nosuf :ignoresize :modrm))
    ("psubw" ((:regxmm :llongmem) :regxmm) (#x660ff9) :cpusse2(:nosuf :ignoresize :modrm))
    ("psubd" ((:regmmx :longmem) :regmmx) (#x0ffa) :cpummx (:nosuf :ignoresize :modrm))
    ("psubd" ((:regxmm :llongmem) :regxmm) (#x660ffa) :cpusse2(:nosuf :ignoresize :modrm))
    ("psubq" ((:regmmx :llongmem) :regmmx) (#x0ffb) :cpusse2(:nosuf :ignoresize :modrm))
    ("psubq" ((:regxmm :llongmem) :regxmm) (#x660ffb) :cpusse2(:nosuf :ignoresize :modrm))
    ("psubsb" ((:regmmx :longmem) :regmmx) (#x0fe8) :cpummx (:nosuf :ignoresize :modrm))
    ("psubsb" ((:regxmm :llongmem) :regxmm) (#x660fe8) :cpusse2(:nosuf :ignoresize :modrm))
    ("psubsw" ((:regmmx :longmem) :regmmx) (#x0fe9) :cpummx (:nosuf :ignoresize :modrm))
    ("psubsw" ((:regxmm :llongmem) :regxmm) (#x660fe9) :cpusse2(:nosuf :ignoresize :modrm))
    ("psubusb" ((:regmmx :longmem) :regmmx) (#x0fd8) :cpummx (:nosuf :ignoresize :modrm))
    ("psubusb" ((:regxmm :llongmem) :regxmm) (#x660fd8) :cpusse2(:nosuf :ignoresize :modrm))
    ("psubusw" ((:regmmx :longmem) :regmmx) (#x0fd9) :cpummx (:nosuf :ignoresize :modrm))
    ("psubusw" ((:regxmm :llongmem) :regxmm) (#x660fd9) :cpusse2(:nosuf :ignoresize :modrm))
    ("punpckhbw" ((:regmmx :longmem) :regmmx) (#x0f68) :cpummx (:nosuf :ignoresize :modrm))
    ("punpckhbw" ((:regxmm :llongmem) :regxmm) (#x660f68) :cpusse2(:nosuf :ignoresize :modrm))
    ("punpckhwd" ((:regmmx :longmem) :regmmx) (#x0f69) :cpummx (:nosuf :ignoresize :modrm))
    ("punpckhwd" ((:regxmm :llongmem) :regxmm) (#x660f69) :cpusse2(:nosuf :ignoresize :modrm))
    ("punpckhdq" ((:regmmx :longmem) :regmmx) (#x0f6a) :cpummx (:nosuf :ignoresize :modrm))
    ("punpckhdq" ((:regxmm :llongmem) :regxmm) (#x660f6a) :cpusse2(:nosuf :ignoresize :modrm))
    ("punpcklbw" ((:regmmx :longmem) :regmmx) (#x0f60) :cpummx (:nosuf :ignoresize :modrm))
    ("punpcklbw" ((:regxmm :llongmem) :regxmm) (#x660f60) :cpusse2(:nosuf :ignoresize :modrm))
    ("punpcklwd" ((:regmmx :longmem) :regmmx) (#x0f61) :cpummx (:nosuf :ignoresize :modrm))
    ("punpcklwd" ((:regxmm :llongmem) :regxmm) (#x660f61) :cpusse2(:nosuf :ignoresize :modrm))
    ("punpckldq" ((:regmmx :longmem) :regmmx) (#x0f62) :cpummx (:nosuf :ignoresize :modrm))
    ("punpckldq" ((:regxmm :llongmem) :regxmm) (#x660f62) :cpusse2(:nosuf :ignoresize :modrm))
    ("pxor" ((:regmmx :longmem) :regmmx) (#x0fef) :cpummx (:nosuf :ignoresize :modrm))
    ("pxor" ((:regxmm :llongmem) :regxmm) (#x660fef) :cpusse2(:nosuf :ignoresize :modrm))

    ;; PIII Katmai New Instructions / SIMD instructions.

    ("addps" ((:regxmm :llongmem) :regxmm) (#x0f58) :cpusse (:nosuf :ignoresize :modrm))
    ("addss" ((:regxmm :wordmem) :regxmm) (#xf30f58) :cpusse (:nosuf :ignoresize :modrm))
    ("andnps" ((:regxmm :llongmem) :regxmm) (#x0f55) :cpusse (:nosuf :ignoresize :modrm))
    ("andps" ((:regxmm :wordmem) :regxmm) (#x0f54) :cpusse (:nosuf :ignoresize :modrm))
    ("cmpeqps" ((:regxmm :llongmem) :regxmm) (#x0fc2 0) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpeqss" ((:regxmm :wordmem) :regxmm) (#xf30fc2 0) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpleps" ((:regxmm :llongmem) :regxmm) (#x0fc2 2) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpless" ((:regxmm :wordmem) :regxmm) (#xf30fc2 2) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpltps" ((:regxmm :llongmem) :regxmm) (#x0fc2 1) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpltss" ((:regxmm :wordmem) :regxmm) (#xf30fc2 1) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpneqps" ((:regxmm :llongmem) :regxmm) (#x0fc2 4) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpneqss" ((:regxmm :wordmem) :regxmm) (#xf30fc2 4) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpnleps" ((:regxmm :llongmem) :regxmm) (#x0fc2 6) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpnless" ((:regxmm :wordmem) :regxmm) (#xf30fc2 6) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpnltps" ((:regxmm :llongmem) :regxmm) (#x0fc2 5) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpnltss" ((:regxmm :wordmem) :regxmm) (#xf30fc2 5) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpordps" ((:regxmm :llongmem) :regxmm) (#x0fc2 7) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpordss" ((:regxmm :wordmem) :regxmm) (#xf30fc2 7) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpunordps" ((:regxmm :llongmem) :regxmm) (#x0fc2 3) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpunordss" ((:regxmm :wordmem) :regxmm) (#xf30fc2 3) :cpusse (:nosuf :ignoresize :modrm :immext))
    ("cmpps" (:imm8 (:regxmm :llongmem) :regxmm) (#x0fc2) :cpusse (:nosuf :ignoresize :modrm))
    ("cmpss" (:imm8 (:regxmm :wordmem) :regxmm) (#xf30fc2) :cpusse (:nosuf :ignoresize :modrm))
    ("comiss" ((:regxmm :wordmem) :regxmm) (#x0f2f) :cpusse (:nosuf :ignoresize :modrm))
    ("cvtpi2ps" ((:regmmx :llongmem) :regxmm) (#x0f2a) :cpusse (:nosuf :ignoresize :modrm))
    ("cvtps2pi" ((:regxmm :llongmem) :regmmx) (#x0f2d) :cpusse (:nosuf :ignoresize :modrm))
    ("cvtsi2ss" ((:reg32 :reg64 :wordmem :llongmem) :regxmm) (#xf30f2a) :cpusse (:lq-suf :ignoresize :modrm))
    ("cvtss2si" ((:regxmm :wordmem) (:reg32 :reg64)) (#xf30f2d) :cpusse (:lq-suf :ignoresize :modrm))
    ("cvttps2pi" ((:regxmm :llongmem) :regmmx) (#x0f2c) :cpusse (:nosuf :ignoresize :modrm))
    ("cvttss2si" ((:regxmm :wordmem) (:reg32 :reg64)) (#xf30f2c) :cpusse (:lq-suf :ignoresize :modrm))
    ("divps" ((:regxmm :llongmem) :regxmm) (#x0f5e) :cpusse (:nosuf :ignoresize :modrm))
    ("divss" ((:regxmm :wordmem) :regxmm) (#xf30f5e) :cpusse (:nosuf :ignoresize :modrm))
    ("ldmxcsr" (:wordmem) (#x0fae 2) :cpusse (:nosuf :ignoresize :modrm))
    ("maskmovq" ((:regmmx :invmem) :regmmx) (#x0ff7) :cpummx2(:nosuf :ignoresize :modrm))
    ("maxps" ((:regxmm :llongmem) :regxmm) (#x0f5f) :cpusse (:nosuf :ignoresize :modrm))
    ("maxss" ((:regxmm :wordmem) :regxmm) (#xf30f5f) :cpusse (:nosuf :ignoresize :modrm))
    ("minps" ((:regxmm :llongmem) :regxmm) (#x0f5d) :cpusse (:nosuf :ignoresize :modrm))
    ("minss" ((:regxmm :wordmem) :regxmm) (#xf30f5d) :cpusse (:nosuf :ignoresize :modrm))
    ("movaps" ((:regxmm :llongmem) :regxmm) (#x0f28) :cpusse (:nosuf :ignoresize :modrm))
    ("movaps" (:regxmm (:regxmm :llongmem)) (#x0f29) :cpusse (:nosuf :ignoresize :modrm))
    ("movhlps" ((:regxmm :invmem) :regxmm) (#x0f12) :cpusse (:nosuf :ignoresize :modrm))
    ("movhps" (:llongmem :regxmm) (#x0f16) :cpusse (:nosuf :ignoresize :modrm))
    ("movhps" (:regxmm :llongmem) (#x0f17) :cpusse (:nosuf :ignoresize :modrm))
    ("movlhps" ((:regxmm :invmem) :regxmm) (#x0f16) :cpusse (:nosuf :ignoresize :modrm))
    ("movlps" (:llongmem :regxmm) (#x0f12) :cpusse (:nosuf :ignoresize :modrm))
    ("movlps" (:regxmm :llongmem) (#x0f13) :cpusse (:nosuf :ignoresize :modrm))
    ("movmskps" ((:regxmm :invmem) (:reg32 :reg64)) (#x0f50) :cpusse (:lq-suf :ignoresize :modrm))
    ("movntps" (:regxmm :llongmem) (#x0f2b) :cpusse (:nosuf :ignoresize :modrm))
    ("movntq" (:regmmx :llongmem) (#x0fe7) :cpummx2(:nosuf :ignoresize :modrm))
    ("movntdq" (:regxmm :llongmem) (#x660fe7) :cpusse2(:nosuf :ignoresize :modrm))
    ("movss" ((:regxmm :wordmem) :regxmm) (#xf30f10) :cpusse (:nosuf :ignoresize :modrm))
    ("movss" (:regxmm (:regxmm :wordmem)) (#xf30f11) :cpusse (:nosuf :ignoresize :modrm))
    ("movups" ((:regxmm :llongmem) :regxmm) (#x0f10) :cpusse (:nosuf :ignoresize :modrm))
    ("movups" (:regxmm (:regxmm :llongmem)) (#x0f11) :cpusse (:nosuf :ignoresize :modrm))
    ("mulps" ((:regxmm :llongmem) :regxmm) (#x0f59) :cpusse (:nosuf :ignoresize :modrm))
    ("mulss" ((:regxmm :wordmem) :regxmm) (#xf30f59) :cpusse (:nosuf :ignoresize :modrm))
    ("orps" ((:regxmm :llongmem) :regxmm) (#x0f56) :cpusse (:nosuf :ignoresize :modrm))
    ("pavgb" ((:regmmx :llongmem) :regmmx) (#x0fe0) :cpummx2(:nosuf :ignoresize :modrm))
    ("pavgb" ((:regxmm :llongmem) :regxmm) (#x660fe0) :cpusse2(:nosuf :ignoresize :modrm))
    ("pavgw" ((:regmmx :llongmem) :regmmx) (#x0fe3) :cpummx2(:nosuf :ignoresize :modrm))
    ("pavgw" ((:regxmm :llongmem) :regxmm) (#x660fe3) :cpusse2(:nosuf :ignoresize :modrm))
    ("pextrw" (:imm8 (:regmmx :invmem) (:reg32 :reg64)) (#x0fc5) :cpummx2 (:lq-suf :ignoresize :modrm))
    ("pextrw" (:imm8 (:regxmm :invmem) (:reg32 :reg64)) (#x660fc5) :cpusse2 (:lq-suf :ignoresize :modrm))
    ("pinsrw" (:imm8 (:reg32 :reg64 :shortmem) :regmmx) (#x0fc4) :cpummx2 (:lq-suf :ignoresize :modrm))
    ("pinsrw" (:imm8 (:reg32 :reg64 :shortmem) :regxmm) (#x660fc4) :cpusse2 (:lq-suf :ignoresize :modrm))
    ("pmaxsw" ((:regmmx :llongmem) :regmmx) (#x0fee) :cpummx2(:nosuf :ignoresize :modrm))
    ("pmaxsw" ((:regxmm :llongmem) :regxmm) (#x660fee) :cpusse2(:nosuf :ignoresize :modrm))
    ("pmaxub" ((:regmmx :llongmem) :regmmx) (#x0fde) :cpummx2(:nosuf :ignoresize :modrm))
    ("pmaxub" ((:regxmm :llongmem) :regxmm) (#x660fde) :cpusse2(:nosuf :ignoresize :modrm))
    ("pminsw" ((:regmmx :llongmem) :regmmx) (#x0fea) :cpummx2(:nosuf :ignoresize :modrm))
    ("pminsw" ((:regxmm :llongmem) :regxmm) (#x660fea) :cpusse2(:nosuf :ignoresize :modrm))
    ("pminub" ((:regmmx :llongmem) :regmmx) (#x0fda) :cpummx2(:nosuf :ignoresize :modrm))
    ("pminub" ((:regxmm :llongmem) :regxmm) (#x660fda) :cpusse2(:nosuf :ignoresize :modrm))
    ("pmovmskb" ((:regmmx :invmem) (:reg32 :reg64)) (#x0fd7) :cpummx2 (:lq-suf :ignoresize :modrm))
    ("pmovmskb" ((:regxmm :invmem) (:reg32 :reg64)) (#x660fd7) :cpusse2 (:lq-suf :ignoresize :modrm))
    ("pmulhuw" ((:regmmx :llongmem) :regmmx) (#x0fe4) :cpummx2(:nosuf :ignoresize :modrm))
    ("pmulhuw" ((:regxmm :llongmem) :regxmm) (#x660fe4) :cpusse2(:nosuf :ignoresize :modrm))
    ("prefetchnta" (:llongmem) (#x0f18 0) :cpummx2(:nosuf :ignoresize :modrm))
    ("prefetcht0" (:llongmem) (#x0f18 1) :cpummx2(:nosuf :ignoresize :modrm))
    ("prefetcht1" (:llongmem) (#x0f18 2) :cpummx2(:nosuf :ignoresize :modrm))
    ("prefetcht2" (:llongmem) (#x0f18 3) :cpummx2(:nosuf :ignoresize :modrm))
    ("psadbw" ((:regmmx :llongmem) :regmmx) (#x0ff6) :cpummx2(:nosuf :ignoresize :modrm))
    ("psadbw" ((:regxmm :llongmem) :regxmm) (#x660ff6) :cpusse2(:nosuf :ignoresize :modrm))
    ("pshufw" (:imm8 (:regmmx :llongmem) :regmmx) (#x0f70) :cpummx2(:nosuf :ignoresize :modrm))
    ("rcpps" ((:regxmm :llongmem) :regxmm) (#x0f53) :cpusse (:nosuf :ignoresize :modrm))
    ("rcpss" ((:regxmm :wordmem) :regxmm) (#xf30f53) :cpusse (:nosuf :ignoresize :modrm))
    ("rsqrtps" ((:regxmm :llongmem) :regxmm) (#x0f52) :cpusse (:nosuf :ignoresize :modrm))
    ("rsqrtss" ((:regxmm :wordmem) :regxmm) (#xf30f52) :cpusse (:nosuf :ignoresize :modrm))
    ("sfence" () (#x0fae #xf8) :cpummx2 (:nosuf :ignoresize :immext))
    ("shufps" (:imm8 (:regxmm :llongmem) :regxmm) (#x0fc6) :cpusse (:nosuf :ignoresize :modrm))
    ("sqrtps" ((:regxmm :llongmem) :regxmm) (#x0f51) :cpusse (:nosuf :ignoresize :modrm))
    ("sqrtss" ((:regxmm :wordmem) :regxmm) (#xf30f51) :cpusse (:nosuf :ignoresize :modrm))
    ("stmxcsr" (:wordmem) (#x0fae 3) :cpusse (:nosuf :ignoresize :modrm))
    ("subps" ((:regxmm :llongmem) :regxmm) (#x0f5c) :cpusse (:nosuf :ignoresize :modrm))
    ("subss" ((:regxmm :wordmem) :regxmm) (#xf30f5c) :cpusse (:nosuf :ignoresize :modrm))
    ("ucomiss" ((:regxmm :wordmem) :regxmm) (#x0f2e) :cpusse (:nosuf :ignoresize :modrm))
    ("unpckhps" ((:regxmm :llongmem) :regxmm) (#x0f15) :cpusse (:nosuf :ignoresize :modrm))
    ("unpcklps" ((:regxmm :llongmem) :regxmm) (#x0f14) :cpusse (:nosuf :ignoresize :modrm))
    ("xorps" ((:regxmm :llongmem) :regxmm) (#x0f57) :cpusse (:nosuf :ignoresize :modrm))

    ;; SSE-2 instructions.

    ("addpd" ((:regxmm :llongmem) :regxmm) (#x660f58) :cpusse2 (:nosuf :ignoresize :modrm))
    ("addsd" ((:regxmm :longmem) :regxmm) (#xf20f58) :cpusse2 (:nosuf :ignoresize :modrm))
    ("andnpd" ((:regxmm :llongmem) :regxmm) (#x660f55) :cpusse2 (:nosuf :ignoresize :modrm))
    ("andpd" ((:regxmm :wordmem) :regxmm) (#x660f54) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cmpeqpd" ((:regxmm :llongmem) :regxmm) (#x660fc2 0) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpeqsd" ((:regxmm :longmem) :regxmm) (#xf20fc2 0) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmplepd" ((:regxmm :llongmem) :regxmm) (#x660fc2 2) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmplesd" ((:regxmm :longmem) :regxmm) (#xf20fc2 2) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpltpd" ((:regxmm :llongmem) :regxmm) (#x660fc2 1) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpltsd" ((:regxmm :longmem) :regxmm) (#xf20fc2 1) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpneqpd" ((:regxmm :llongmem) :regxmm) (#x660fc2 4) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpneqsd" ((:regxmm :longmem) :regxmm) (#xf20fc2 4) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpnlepd" ((:regxmm :llongmem) :regxmm) (#x660fc2 6) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpnlesd" ((:regxmm :longmem) :regxmm) (#xf20fc2 6) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpnltpd" ((:regxmm :llongmem) :regxmm) (#x660fc2 5) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpnltsd" ((:regxmm :longmem) :regxmm) (#xf20fc2 5) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpordpd" ((:regxmm :llongmem) :regxmm) (#x660fc2 7) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpordsd" ((:regxmm :longmem) :regxmm) (#xf20fc2 7) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpunordpd" ((:regxmm :llongmem) :regxmm) (#x660fc2 3) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmpunordsd" ((:regxmm :longmem) :regxmm) (#xf20fc2 3) :cpusse2 (:nosuf :ignoresize :modrm :immext))
    ("cmppd" (:imm8 (:regxmm :llongmem) :regxmm) (#x660fc2) :cpusse2 (:nosuf :ignoresize :modrm))
    ;; Intel mode string compare.
    ;; ("cmpsd" () (#xa7) :cpu086 (:nosuf :size32 :isstring))
    ("cmpsd" (:anymem (:anymem :esseg)) (#xa7) :cpu086 (:nosuf :size32 :isstring))
    ("cmpsd" (:imm8 (:regxmm :longmem) :regxmm) (#xf20fc2) :cpusse2 (:nosuf :ignoresize :modrm))
    ("comisd" ((:regxmm :longmem) :regxmm) (#x660f2f) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvtpi2pd" ((:regmmx :llongmem) :regxmm) (#x660f2a) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvtsi2sd" ((:reg32 :reg64 :wordmem :llongmem) :regxmm) (#xf20f2a) :cpusse2 (:lq-suf :ignoresize :modrm))
    ("divpd" ((:regxmm :llongmem) :regxmm) (#x660f5e) :cpusse2 (:nosuf :ignoresize :modrm))
    ("divsd" ((:regxmm :longmem) :regxmm) (#xf20f5e) :cpusse2 (:nosuf :ignoresize :modrm))
    ("maxpd" ((:regxmm :llongmem) :regxmm) (#x660f5f) :cpusse2 (:nosuf :ignoresize :modrm))
    ("maxsd" ((:regxmm :longmem) :regxmm) (#xf20f5f) :cpusse2 (:nosuf :ignoresize :modrm))
    ("minpd" ((:regxmm :llongmem) :regxmm) (#x660f5d) :cpusse2 (:nosuf :ignoresize :modrm))
    ("minsd" ((:regxmm :longmem) :regxmm) (#xf20f5d) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movapd" ((:regxmm :llongmem) :regxmm) (#x660f28) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movapd" (:regxmm (:regxmm :llongmem)) (#x660f29) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movhpd" (:llongmem :regxmm) (#x660f16) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movhpd" (:regxmm :llongmem) (#x660f17) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movlpd" (:llongmem :regxmm) (#x660f12) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movlpd" (:regxmm :llongmem) (#x660f13) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movmskpd" ((:regxmm :invmem) (:reg32 :reg64)) (#x660f50) :cpusse2 (:lq-suf :ignoresize :modrm))
    ("movntpd" (:regxmm :llongmem) (#x660f2b) :cpusse2 (:nosuf :ignoresize :modrm))
    ;; Intel mode string move.
    ;; ("movsd" () (#xa5) :cpu086 (:nosuf :size32 :isstring))
    ("movsd" (:anymem (:anymem :esseg)) (#xa5) :cpu086 (:nosuf :size32 :isstring))
    ("movsd" ((:regxmm :longmem) :regxmm) (#xf20f10) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movsd" (:regxmm (:regxmm :longmem)) (#xf20f11) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movupd" ((:regxmm :llongmem) :regxmm) (#x660f10) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movupd" (:regxmm (:regxmm :llongmem)) (#x660f11) :cpusse2 (:nosuf :ignoresize :modrm))
    ("mulpd" ((:regxmm :llongmem) :regxmm) (#x660f59) :cpusse2 (:nosuf :ignoresize :modrm))
    ("mulsd" ((:regxmm :longmem) :regxmm) (#xf20f59) :cpusse2 (:nosuf :ignoresize :modrm))
    ("orpd" ((:regxmm :llongmem) :regxmm) (#x660f56) :cpusse2 (:nosuf :ignoresize :modrm))
    ("shufpd" (:imm8 (:regxmm :llongmem) :regxmm) (#x660fc6) :cpusse2 (:nosuf :ignoresize :modrm))
    ("sqrtpd" ((:regxmm :llongmem) :regxmm) (#x660f51) :cpusse2 (:nosuf :ignoresize :modrm))
    ("sqrtsd" ((:regxmm :longmem) :regxmm) (#xf20f51) :cpusse2 (:nosuf :ignoresize :modrm))
    ("subpd" ((:regxmm :llongmem) :regxmm) (#x660f5c) :cpusse2 (:nosuf :ignoresize :modrm))
    ("subsd" ((:regxmm :longmem) :regxmm) (#xf20f5c) :cpusse2 (:nosuf :ignoresize :modrm))
    ("ucomisd" ((:regxmm :longmem) :regxmm) (#x660f2e) :cpusse2 (:nosuf :ignoresize :modrm))
    ("unpckhpd" ((:regxmm :llongmem) :regxmm) (#x660f15) :cpusse2 (:nosuf :ignoresize :modrm))
    ("unpcklpd" ((:regxmm :llongmem) :regxmm) (#x660f14) :cpusse2 (:nosuf :ignoresize :modrm))
    ("xorpd" ((:regxmm :llongmem) :regxmm) (#x660f57) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvtdq2pd" ((:regxmm :llongmem) :regxmm) (#xf30fe6) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvtpd2dq" ((:regxmm :llongmem) :regxmm) (#xf20fe6) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvtdq2ps" ((:regxmm :llongmem) :regxmm) (#x0f5b) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvtpd2pi" ((:regxmm :llongmem) :regmmx) (#x660f2d) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvtpd2ps" ((:regxmm :llongmem) :regxmm) (#x660f5a) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvtps2pd" ((:regxmm :llongmem) :regxmm) (#x0f5a) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvtps2dq" ((:regxmm :llongmem) :regxmm) (#x660f5b) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvtsd2si" ((:regxmm :llongmem) (:reg32 :reg64)) (#xf20f2d) :cpusse2 (:lq-suf :ignoresize :modrm))
    ("cvtsd2ss" ((:regxmm :llongmem) :regxmm) (#xf20f5a) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvtss2sd" ((:regxmm :llongmem) :regxmm) (#xf30f5a) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvttpd2pi" ((:regxmm :llongmem) :regmmx) (#x660f2c) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvttsd2si" ((:regxmm :wordmem) (:reg32 :reg64)) (#xf20f2c) :cpusse2 (:lq-suf :ignoresize :modrm))
    ("cvttpd2dq" ((:regxmm :llongmem) :regxmm) (#x660fe6) :cpusse2 (:nosuf :ignoresize :modrm))
    ("cvttps2dq" ((:regxmm :llongmem) :regxmm) (#xf30f5b) :cpusse2 (:nosuf :ignoresize :modrm))
    ("maskmovdqu" ((:regxmm :invmem) :regxmm) (#x660ff7) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movdqa" ((:regxmm :llongmem) :regxmm) (#x660f6f) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movdqa" (:regxmm (:regxmm :llongmem)) (#x660f7f) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movdqu" ((:regxmm :llongmem) :regxmm) (#xf30f6f) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movdqu" (:regxmm (:regxmm :llongmem)) (#xf30f7f) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movdq2q" ((:regxmm :invmem) :regmmx) (#xf20fd6) :cpusse2 (:nosuf :ignoresize :modrm))
    ("movq2dq" ((:regmmx :invmem) :regxmm) (#xf30fd6) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pmuludq" ((:regmmx :longmem) :regmmx) (#x0ff4) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pmuludq" ((:regxmm :longmem) :regxmm) (#x660ff4) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pshufd" (:imm8 (:regxmm :llongmem) :regxmm) (#x660f70) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pshufhw" (:imm8 (:regxmm :llongmem) :regxmm) (#xf30f70) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pshuflw" (:imm8 (:regxmm :llongmem) :regxmm) (#xf20f70) :cpusse2 (:nosuf :ignoresize :modrm))
    ("pslldq" (:imm8 :regxmm) (#x660f73 7) :cpusse2 (:nosuf :ignoresize :modrm))
    ("psrldq" (:imm8 :regxmm) (#x660f73 3) :cpusse2 (:nosuf :ignoresize :modrm))
    ("punpckhqdq" ((:regxmm :llongmem) :regxmm) (#x660f6d) :cpusse2 (:nosuf :ignoresize :modrm))
    ("punpcklqdq" ((:regxmm :llongmem) :regxmm) (#x660f6c) :cpusse2 (:nosuf :ignoresize :modrm))

    ;; Prescott New Instructions.

    ("addsubpd" ((:regxmm :llongmem) :regxmm) (#x660fd0) :cpupni (:nosuf :ignoresize :modrm))
    ("addsubps" ((:regxmm :llongmem) :regxmm) (#xf20fd0) :cpupni (:nosuf :ignoresize :modrm))
    ("cmpxchg16b" (:llongmem) (#x0fc7 1) (:cpupni :cpu64) (:nosuf :modrm :rex64))
    ("fisttp" ((:shortmem :longmem)) (#xdf 1) :cpupni (:sl-fp :modrm))
    ("fisttp" (:llongmem) (#xdd 1) :cpupni (:q-fp :modrm))
    ("fisttpll" (:llongmem) (#xdd 1) :cpupni (:fp :modrm))
    ("haddpd" ((:regxmm :llongmem) :regxmm) (#x660f7c) :cpupni (:nosuf :ignoresize :modrm))
    ("haddps" ((:regxmm :llongmem) :regxmm) (#xf20f7c) :cpupni (:nosuf :ignoresize :modrm))
    ("hsubpd" ((:regxmm :llongmem) :regxmm) (#x660f7d) :cpupni (:nosuf :ignoresize :modrm))
    ("hsubps" ((:regxmm :llongmem) :regxmm) (#xf20f7d) :cpupni (:nosuf :ignoresize :modrm))
    ("lddqu" (:llongmem :regxmm) (#xf20ff0) :cpupni (:nosuf :ignoresize :modrm))
    ("monitor" () (#x0f01 #xc8) :cpupni (:nosuf :immext))
    ;; Need to ensure only "monitor %eax %ecx %edx" is accepted.
    ("monitor" (:reg32 :reg32 :reg32) (#x0f01 #xc8) :cpupni (:nosuf :immext))
    ("movddup" ((:regxmm :llongmem) :regxmm) (#xf20f12) :cpupni (:nosuf :ignoresize :modrm))
    ("movshdup" ((:regxmm :llongmem) :regxmm) (#xf30f16) :cpupni (:nosuf :ignoresize :modrm))
    ("movsldup" ((:regxmm :llongmem) :regxmm) (#xf30f12) :cpupni (:nosuf :ignoresize :modrm))
    ("mwait" () (#x0f01 #xc9) :cpupni (:nosuf :immext))
    ;; Need to ensure only "mwait %eax %ecx" is accepted.
    ("mwait" (:reg32 :reg32) (#x0f01 #xc9) :cpupni (:nosuf :immext))

    ;; AMD 3DNow! instructions.

    ("prefetch" (:bytemem) (#x0f0d 0) :cpu3dnow (:nosuf :ignoresize :modrm))
    ("prefetchw" (:bytemem) (#x0f0d 1) :cpu3dnow (:nosuf :ignoresize :modrm))
    ("femms" () (#x0f0e) :cpu3dnow :nosuf)
    ("pavgusb" ((:regmmx :longmem) :regmmx) (#x0f0f #xbf) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pf2id" ((:regmmx :longmem) :regmmx) (#x0f0f #x1d) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pf2iw" ((:regmmx :longmem) :regmmx) (#x0f0f #x1c) :cpu3dnowa (:nosuf :ignoresize :modrm :immext))
    ("pfacc" ((:regmmx :longmem) :regmmx) (#x0f0f #xae) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfadd" ((:regmmx :longmem) :regmmx) (#x0f0f #x9e) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfcmpeq" ((:regmmx :longmem) :regmmx) (#x0f0f #xb0) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfcmpge" ((:regmmx :longmem) :regmmx) (#x0f0f #x90) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfcmpgt" ((:regmmx :longmem) :regmmx) (#x0f0f #xa0) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfmax" ((:regmmx :longmem) :regmmx) (#x0f0f #xa4) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfmin" ((:regmmx :longmem) :regmmx) (#x0f0f #x94) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfmul" ((:regmmx :longmem) :regmmx) (#x0f0f #xb4) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfnacc" ((:regmmx :longmem) :regmmx) (#x0f0f #x8a) :cpu3dnowa(:nosuf :ignoresize :modrm :immext))
    ("pfpnacc" ((:regmmx :longmem) :regmmx) (#x0f0f #x8e) :cpu3dnowa(:nosuf :ignoresize :modrm :immext))
    ("pfrcp" ((:regmmx :longmem) :regmmx) (#x0f0f #x96) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfrcpit1" ((:regmmx :longmem) :regmmx) (#x0f0f #xa6) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfrcpit2" ((:regmmx :longmem) :regmmx) (#x0f0f #xb6) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfrsqit1" ((:regmmx :longmem) :regmmx) (#x0f0f #xa7) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfrsqrt" ((:regmmx :longmem) :regmmx) (#x0f0f #x97) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfsub" ((:regmmx :longmem) :regmmx) (#x0f0f #x9a) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pfsubr" ((:regmmx :longmem) :regmmx) (#x0f0f #xaa) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pi2fd" ((:regmmx :longmem) :regmmx) (#x0f0f #x0d) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pi2fw" ((:regmmx :longmem) :regmmx) (#x0f0f #x0c) :cpu3dnowa(:nosuf :ignoresize :modrm :immext))
    ("pmulhrw" ((:regmmx :longmem) :regmmx) (#x0f0f #xb7) :cpu3dnow (:nosuf :ignoresize :modrm :immext))
    ("pswapd" ((:regmmx :longmem) :regmmx) (#x0f0f #xbb) :cpu3dnowa(:nosuf :ignoresize :modrm :immext))

    ;; AMD extensions.
    ("syscall" () (#x0f05) :cpuk6 :nosuf)
    ("sysret" () (#x0f07) :cpuk6 (:lq-suf :defaultsize))
    ("swapgs" () (#x0f01 #xf8) :cpu64 (:nosuf :immext))
    ("rdtscp" () (#x0f01 #xf9) :cpusledgehammer (:nosuf :immext))

    ;; VIA PadLock extensions.
    ("xstorerng" () (#x000fa7c0) (:cpu686 :cpupadlock) (:nosuf :isstring))
    ("xcryptecb" () (#xf30fa7c8) (:cpu686 :cpupadlock) (:nosuf :isstring))
    ("xcryptcbc" () (#xf30fa7d0) (:cpu686 :cpupadlock) (:nosuf :isstring))
    ("xcryptcfb" () (#xf30fa7e0) (:cpu686 :cpupadlock) (:nosuf :isstring))
    ("xcryptofb" () (#xf30fa7e8) (:cpu686 :cpupadlock) (:nosuf :isstring))
    ("montmul" () (#xf30fa6c0) (:cpu686 :cpupadlock) (:nosuf :isstring))
    ("xsha1" () (#xf30fa6c8) (:cpu686 :cpupadlock) (:nosuf :isstring))
    ("xsha256" () (#xf30fa6d0) (:cpu686 :cpupadlock) (:nosuf :isstring))
    ;; Alias for xstorerng.
    ("xstore" () (#x000fa7c0) (:cpu686 :cpupadlock) (:nosuf :isstring))
    ))

(defun x86-template-data-excluding-cpu-flags (&rest excluded-flags)
  (ccl::collect ((data))
    (dolist (whole *x86-instruction-template-data*)
      (destructuring-bind (name
                           operand-types
                           (base-opcode &optional extension-opcode)
                           cpuflags
                           opcode-modifier) whole
        (declare (ignore name operand-types base-opcode extension-opcode
                         opcode-modifier))
        (when (dolist (flag excluded-flags t)
                (when (if (atom cpuflags)
                          (eq flag cpuflags)
                          (member flag cpuflags))
                  (return nil)))
          (data whole))))
    (data)))
  

      
(defparameter *x86-32-instruction-template-lists*
  (make-hash-table :test #'equalp))


(defparameter *x86-64-instruction-template-lists*
  (make-hash-table :test #'equalp))


(defparameter *x86-32-instruction-templates* ())
(defparameter *x86-64-instruction-templates* ())



(defun initialize-x86-instruction-templates ()
    (flet ((setup-templates-hash (hash templates)
             (let* ((n (length templates))
                    (vector (make-array n)))
               (declare (fixnum n))
               (clrhash hash)
               (dotimes (i n vector)
                 (let* ((whole (pop templates)))
                   (destructuring-bind (name
                                        operand-types
                                        (base-opcode &optional extension-opcode)
                                        cpuflags
                                        opcode-modifier) whole
                     (let* ((operand-count (length operand-types))
                            (modifier (or (%encode-opcode-modifier opcode-modifier)
                                          (error "Bad opcode modifier ~s in ~s" opcode-modifier whole)))
                            (encoded-types (apply #'vector
                                                  (mapcar #'(lambda (type)
                                                              (or (%encode-operand-type type)
                                                                  (error "Bad operand type ~s in ~w"
                                                                         type whole)))
                                                          operand-types)))
                            (encoded-cpu-flags (or (%encode-cpu-flags cpuflags)
                                                   (error "Bad CPU flags ~s in ~s" cpuflags whole)))
                            (template (make-x86-instruction-template
                                       :name name
                                       :index i
                                       :operands operand-count
                                       :base-opcode base-opcode
                                       :extension-opcode extension-opcode
                                       :cpu-flags encoded-cpu-flags
                                       :opcode-modifier modifier
                                       :operand-types encoded-types)))
                       (setf (svref vector i) template)
                       (push template (gethash name hash)))))))))
      (setq *x86-32-instruction-templates*
            (setup-templates-hash
             *x86-32-instruction-template-lists*
             (x86-template-data-excluding-cpu-flags :cpu64))
            *x86-64-instruction-templates*
            (setup-templates-hash
             *x86-64-instruction-template-lists*
             (x86-template-data-excluding-cpu-flags :cpuno64)))
      t))

(defparameter *x86-instruction-template-lists* ())
(defparameter *x86-instruction-templates* ())

(defun setup-x86-assembler (&optional (cpu :x86-64))
  (initialize-x86-instruction-templates)
  (ecase cpu
    (:x86-32 (setq *x86-instruction-template-lists*
                   *x86-32-instruction-template-lists*
                   *x86-instruction-templates*
                   *x86-32-instruction-templates*))
    (:x86-64 (setq *x86-instruction-template-lists*
                   *x86-64-instruction-template-lists*
                   *x86-instruction-templates*
                   *x86-64-instruction-templates*)))
  t)

(setup-x86-assembler :x86-64)

(defun get-x86-instruction-templates-and-suffix (name)
  (let* ((s (string name))
         (templates (gethash s *x86-instruction-template-lists*)))
    (if templates
      (values templates nil)
      (let* ((n (length s))
             (m (1- n))
             (suffix nil))
        (declare (fixnum m n))
        (if (and (> m 0)
                 (position (setq suffix (char-downcase (schar s m)))
                           "bwlqx"
                           :test #'char=))
          (let* ((sub (make-string m)))
            (declare (dynamic-extent sub))
            (replace sub s :end1 m)
            (if (setq templates (gethash sub *x86-instruction-template-lists*))
              (values templates suffix)
              (error "Unknown instruction: ~s" name)))
          (error "Unknown instruction: ~s" name))))))
                          



;;; 386 register table.

(defconstant REGNAM-AL 1) ; Entry in i386-regtab.
(defconstant REGNAM-AX 25)
(defconstant REGNAM-EAX 41)

(defvar *x86-regtab*
  (vector
   ;; Make %st first as we test for it.
   (make-reg-entry :reg-name "st"
                   :reg-type (encode-operand-type :FloatReg :floatacc)
                   :reg-flags 0
                   :reg-num 0 )
   ;; 8 bit regs
   (make-reg-entry :reg-name "al"
                   :reg-type (encode-operand-type :Reg8 :Acc)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "cl"
                   :reg-type (encode-operand-type :Reg8 :ShiftCount)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "dl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "bl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "ah"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "ch"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "dh"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "bh"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "axl"
                   :reg-type (encode-operand-type :Reg8 :Acc)
                   :reg-flags RegRex64
                   :reg-num 0 ) ; Must be in the "al + 8" slot.
   (make-reg-entry :reg-name "cxl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags RegRex64
                   :reg-num 1)
   (make-reg-entry :reg-name "dxl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags RegRex64
                   :reg-num 2)
   (make-reg-entry :reg-name "bxl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags RegRex64
                   :reg-num 3)
   (make-reg-entry :reg-name "spl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags RegRex64
                   :reg-num 4)
   (make-reg-entry :reg-name "bpl"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags RegRex64
                   :reg-num 5)
   (make-reg-entry :reg-name "sil"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags RegRex64
                   :reg-num 6)
   (make-reg-entry :reg-name "dil"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags RegRex64
                   :reg-num 7)
   (make-reg-entry :reg-name "r8b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior RegRex64 RegRex)
                   :reg-num 0 )
   (make-reg-entry :reg-name "r9b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior RegRex64 RegRex)
                   :reg-num 1)
   (make-reg-entry :reg-name "r10b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior RegRex64 RegRex)
                   :reg-num 2)
   (make-reg-entry :reg-name "r11b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior RegRex64 RegRex)
                   :reg-num 3)
   (make-reg-entry :reg-name "r12b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior RegRex64 RegRex)
                   :reg-num 4)
   (make-reg-entry :reg-name "r13b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior RegRex64 RegRex)
                   :reg-num 5)
   (make-reg-entry :reg-name "r14b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior RegRex64 RegRex)
                   :reg-num 6)
   (make-reg-entry :reg-name "r15b"
                   :reg-type (encode-operand-type :Reg8)
                   :reg-flags (logior RegRex64 RegRex)
                   :reg-num 7)
   ;; 16 bit regs
   (make-reg-entry :reg-name "ax"
                   :reg-type (encode-operand-type :Reg16 :Acc)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "cx"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "dx"
                   :reg-type (encode-operand-type :Reg16 :InOutPortReg)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "bx"
                   :reg-type (encode-operand-type :Reg16 :BaseIndex)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "sp"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "bp"
                   :reg-type (encode-operand-type :Reg16 :BaseIndex)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "si"
                   :reg-type (encode-operand-type :Reg16 :BaseIndex)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "di"
                   :reg-type (encode-operand-type :Reg16 :BaseIndex)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "r8w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags RegRex
                   :reg-num 0 )
   (make-reg-entry :reg-name "r9w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags RegRex
                   :reg-num 1)
   (make-reg-entry :reg-name "r10w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags RegRex
                   :reg-num 2)
   (make-reg-entry :reg-name "r11w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags RegRex
                   :reg-num 3)
   (make-reg-entry :reg-name "r12w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags RegRex
                   :reg-num 4)
   (make-reg-entry :reg-name "r13w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags RegRex
                   :reg-num 5)
   (make-reg-entry :reg-name "r14w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags RegRex
                   :reg-num 6)
   (make-reg-entry :reg-name "r15w"
                   :reg-type (encode-operand-type :Reg16)
                   :reg-flags RegRex
                   :reg-num 7)
        ; 32 bit regs
   (make-reg-entry :reg-name "eax"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex :Acc)
                   :reg-flags 0
                   :reg-num 0 ) ; Must be in ax + 16 slot.
   (make-reg-entry :reg-name "ecx"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "edx"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "ebx"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "esp"
                   :reg-type (encode-operand-type :Reg32)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "ebp"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "esi"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "edi"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "r8d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 0 )
   (make-reg-entry :reg-name "r9d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 1)
   (make-reg-entry :reg-name "r10d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 2)
   (make-reg-entry :reg-name "r11d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 3)
   (make-reg-entry :reg-name "r12d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 4)
   (make-reg-entry :reg-name "r13d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 5)
   (make-reg-entry :reg-name "r14d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 6)
   (make-reg-entry :reg-name "r15d"
                   :reg-type (encode-operand-type :Reg32 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 7)
   (make-reg-entry :reg-name "rax"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex :Acc)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "rcx"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "rdx"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "rbx"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "rsp"
                   :reg-type (encode-operand-type :Reg64)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "rbp"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "rsi"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "rdi"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "r8"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 0 )
   (make-reg-entry :reg-name "r9"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 1)
   (make-reg-entry :reg-name "r10"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 2)
   (make-reg-entry :reg-name "r11"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 3)
   (make-reg-entry :reg-name "r12"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 4)
   (make-reg-entry :reg-name "r13"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 5)
   (make-reg-entry :reg-name "r14"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 6)
   (make-reg-entry :reg-name "r15"
                   :reg-type (encode-operand-type :Reg64 :BaseIndex)
                   :reg-flags RegRex
                   :reg-num 7)
        ; Segment registers.
   (make-reg-entry :reg-name "es"
                   :reg-type (encode-operand-type :SReg2)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "cs"
                   :reg-type (encode-operand-type :SReg2)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "ss"
                   :reg-type (encode-operand-type :SReg2)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "ds"
                   :reg-type (encode-operand-type :SReg2)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "fs"
                   :reg-type (encode-operand-type :SReg3)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "gs"
                   :reg-type (encode-operand-type :SReg3)
                   :reg-flags 0
                   :reg-num 5)
   ;; Control registers.
   (make-reg-entry :reg-name "cr0"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "cr1"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "cr2"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "cr3"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "cr4"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "cr5"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "cr6"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "cr7"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "cr8"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags RegRex
                   :reg-num 0 )
   (make-reg-entry :reg-name "cr9"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags RegRex
                   :reg-num 1)
   (make-reg-entry :reg-name "cr10"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags RegRex
                   :reg-num 2)
   (make-reg-entry :reg-name "cr11"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags RegRex
                   :reg-num 3)
   (make-reg-entry :reg-name "cr12"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags RegRex
                   :reg-num 4)
   (make-reg-entry :reg-name "cr13"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags RegRex
                   :reg-num 5)
   (make-reg-entry :reg-name "cr14"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags RegRex
                   :reg-num 6)
   (make-reg-entry :reg-name "cr15"
                   :reg-type (encode-operand-type :Control)
                   :reg-flags RegRex
                   :reg-num 7)
   ;; Debug registers.
   (make-reg-entry :reg-name "db0"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "db1"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "db2"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "db3"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "db4"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "db5"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "db6"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "db7"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "db8"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 0 )
   (make-reg-entry :reg-name "db9"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 1)
   (make-reg-entry :reg-name "db10"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 2)
   (make-reg-entry :reg-name "db11"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 3)
   (make-reg-entry :reg-name "db12"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 4)
   (make-reg-entry :reg-name "db13"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 5)
   (make-reg-entry :reg-name "db14"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 6)
   (make-reg-entry :reg-name "db15"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 7)
   (make-reg-entry :reg-name "dr0"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "dr1"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "dr2"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "dr3"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "dr4"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "dr5"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "dr6"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "dr7"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "dr8"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 0 )
   (make-reg-entry :reg-name "dr9"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 1)
   (make-reg-entry :reg-name "dr10"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 2)
   (make-reg-entry :reg-name "dr11"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 3)
   (make-reg-entry :reg-name "dr12"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 4)
   (make-reg-entry :reg-name "dr13"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 5)
   (make-reg-entry :reg-name "dr14"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 6)
   (make-reg-entry :reg-name "dr15"
                   :reg-type (encode-operand-type :Debug)
                   :reg-flags RegRex
                   :reg-num 7)
   ;; Test registers.
   (make-reg-entry :reg-name "tr0"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "tr1"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "tr2"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "tr3"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "tr4"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "tr5"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "tr6"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "tr7"
                   :reg-type (encode-operand-type :Test)
                   :reg-flags 0
                   :reg-num 7)
   ;; MMX and simd registers.
   (make-reg-entry :reg-name "mm0"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "mm1"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "mm2"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "mm3"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "mm4"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "mm5"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "mm6"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "mm7"
                   :reg-type (encode-operand-type :RegMMX)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "xmm0"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 0 )
   (make-reg-entry :reg-name "xmm1"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "xmm2"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "xmm3"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "xmm4"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "xmm5"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "xmm6"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "xmm7"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags 0
                   :reg-num 7)
   (make-reg-entry :reg-name "xmm8"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags RegRex
                   :reg-num 0 )
   (make-reg-entry :reg-name "xmm9"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags RegRex
                   :reg-num 1)
   (make-reg-entry :reg-name "xmm10"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags RegRex
                   :reg-num 2)
   (make-reg-entry :reg-name "xmm11"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags RegRex
                   :reg-num 3)
   (make-reg-entry :reg-name "xmm12"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags RegRex
                   :reg-num 4)
   (make-reg-entry :reg-name "xmm13"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags RegRex
                   :reg-num 5)
   (make-reg-entry :reg-name "xmm14"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags RegRex
                   :reg-num 6)
   (make-reg-entry :reg-name "xmm15"
                   :reg-type (encode-operand-type :RegXMM)
                   :reg-flags RegRex
                   :reg-num 7)
   ;; No type will make this register rejected for all purposes except
   ;; for addressing. This saves creating one extra type for RIP.
   (make-reg-entry :reg-name "rip"
                   :reg-type (encode-operand-type :BaseIndex)
                   :reg-flags 0
                   :reg-num 0 )
   ))

(defvar *x86-float-regs*
  (vector
   (make-reg-entry :reg-name "st[0]"
                   :reg-type (encode-operand-type :FloatReg :FloatAcc)
                   :reg-flags 0
                   :reg-num 0)
   (make-reg-entry :reg-name "st[1]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 1)
   (make-reg-entry :reg-name "st[2]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 2)
   (make-reg-entry :reg-name "st[3]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 3)
   (make-reg-entry :reg-name "st[4]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 4)
   (make-reg-entry :reg-name "st[5]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 5)
   (make-reg-entry :reg-name "st[6]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 6)
   (make-reg-entry :reg-name "st[7]"
                   :reg-type (encode-operand-type :FloatReg)
                   :reg-flags 0
                   :reg-num 7)))


;;; Segment stuff.
(defvar *cs-segment-register* (make-seg-entry :seg-name "cs" :seg-prefix #x23))
(defvar *ds-segment-register* (make-seg-entry :seg-name "ds" :seg-prefix #x3e))
(defvar *ss-segment-register* (make-seg-entry :seg-name "ss" :seg-prefix #x36))
(defvar *es-segment-register* (make-seg-entry :seg-name "es" :seg-prefix #x26))
(defvar *fs-segment-register* (make-seg-entry :seg-name "fs" :seg-prefix #x64))
(defvar *gs-segment-register* (make-seg-entry :seg-name "gs" :seg-prefix #x65))

;;; We may need some sort of local aliasing/nicknaming mechanism in LAP.
(defvar *x86-registers* (make-hash-table :test #'equalp))

(defun init-x86-registers ()
  (flet ((hash-registers (vector)
           (dotimes (i (length vector))
             (let* ((entry (svref vector i)))
               (setf (gethash (reg-entry-reg-name entry) *x86-registers*)
                     entry)))))
    (hash-registers *x86-regtab*)
    (hash-registers *x86-float-regs*)))

(init-x86-registers)


(defstruct x86-operand
  (type ))

(defstruct (x86-immediate-operand (:include x86-operand))
  ;; The "value" of an immediate operand may be an expression (that we
  ;; have to do some sort of delayed evaluation on.)  It could just be
  ;; a lisp form (that we call EVAL on), but there might be scoping or
  ;; similar issues in that case.
  value)

(defstruct (x86-register-operand (:include x86-operand))
  entry                                 ;the reg-entry
)

(defstruct (x86-label-operand (:include x86-operand))
  label)


(defstruct (x86-memory-operand (:include x86-operand))
  ;; Any of these fields can be null.  Some combinations of fields -
  ;; like a segment register or scale factor by itself - make no
  ;; sense.
  seg                                   ; a segment register
  disp                                  ; a signed displacement added to base
  base                                  ; a GPR
  index                                 ; another GPR
  scale                                 ; scale factor, multiplied with index
  )

(defmethod unparse-operand ((x x86-register-operand))
  `(% ,(reg-entry-reg-name (x86-register-operand-entry x))))

(defmethod unparse-operand ((x x86-immediate-operand))
  `($ ,(x86-immediate-operand-value x)))

(defmethod unparse-operand ((x x86-label-operand))
  `(^ ,(x86-lap-label-name (x86-label-operand-label x))))

(defmethod unparse-operand ((x x86-memory-operand))
  (ccl::collect ((subforms))
    (flet ((maybe (f) (if f (subforms f))))
      (maybe (x86-memory-operand-seg x))
      (maybe (x86-memory-operand-disp x))
      (maybe (x86-memory-operand-base x))
      (maybe (x86-memory-operand-index x))
      (maybe (x86-memory-operand-scale x)))
  `(@ ,@(subforms))))

(macrolet ((register-operand (name)
             (let* ((r (gethash name *x86-registers*)))
                      (unless r (error "unknown register ~s" name))
                      (make-x86-register-operand :type (logandc2 (reg-entry-reg-type r)
                                                                 (encode-operand-type :baseIndex))
                                                 :entry r))))
  (defparameter *x8664-register-operands*
      (vector
       ;; 64-bit general-purpose registers
       (register-operand "rax")
       (register-operand "rcx")
       (register-operand "rdx")
       (register-operand "rbx")
       (register-operand "rsp")
       (register-operand "rbp")
       (register-operand "rsi")
       (register-operand "rdi")
       (register-operand "r8")
       (register-operand "r9")
       (register-operand "r10")
       (register-operand "r11")
       (register-operand "r12")
       (register-operand "r13")
       (register-operand "r14")
       (register-operand "r15")
       ;; 32-bit registers
       (register-operand "eax")
       (register-operand "ecx")
       (register-operand "edx")
       (register-operand "ebx")
       (register-operand "esp")
       (register-operand "ebp")
       (register-operand "esi")
       (register-operand "edi")
       (register-operand "r8d")
       (register-operand "r9d")
       (register-operand "r10d")
       (register-operand "r11d")
       (register-operand "r12d")
       (register-operand "r13d")
       (register-operand "r14d")
       (register-operand "r15d")
       ;; 16-bit-registers
       (register-operand "ax")
       (register-operand "cx")
       (register-operand "dx")
       (register-operand "bx")
       (register-operand "sp")
       (register-operand "bp")
       (register-operand "si")
       (register-operand "di")
       (register-operand "r8w")
       (register-operand "r9w")
       (register-operand "r10w")
       (register-operand "r11w")
       (register-operand "r12w")
       (register-operand "r13w")
       (register-operand "r14w")
       (register-operand "r15w")
       ;; 8-bit registers
       (register-operand "al")
       (register-operand "cl")
       (register-operand "dl")
       (register-operand "bl")
       (register-operand "spl")
       (register-operand "bpl")
       (register-operand "sil")
       (register-operand "dil")
       (register-operand "r8b")
       (register-operand "r9b")
       (register-operand "r10b")
       (register-operand "r11b")
       (register-operand "r12b")
       (register-operand "r13b")
       (register-operand "r14b")
       (register-operand "r15b"))))

(dotimes (i (length *x8664-register-operands*))
  (let* ((op (svref *x8664-register-operands* i)))
    (when op
      (let* ((entry (x86-register-operand-entry op)))
        (setf (reg-entry-ordinal64 entry) i)))))

(defconstant x86-64-bit-register #x00)
(defconstant x86-32-bit-register #x10)
(defconstant x86-16-bit-register #x20)
(defconstant x86-8-bit-register #x30)

(defun gpr-ordinal (r)
  (or
   (etypecase r
     ((mod 64) r)
     ((or string symbol)
      (let* ((entry (gethash r *x86-registers*)))
        (if entry
          (reg-entry-ordinal64 entry))))
     (reg-entry (reg-entry-ordinal64 r))
     (x86-register-operand
      (reg-entry-ordinal64 (x86-register-operand-entry r))))
   (error "Can't determine register ordinal of ~s" r)))


(defun x86-reg8 (r)
  (svref *x8664-register-operands* (dpb (gpr-ordinal r)
                                        (byte 4 0)
                                        x86-8-bit-register)))

(defun x86-reg16 (r)
  (svref *x8664-register-operands* (dpb (gpr-ordinal r)
                                        (byte 4 0)
                                        x86-16-bit-register)))

(defun x86-reg32 (r)
  (svref *x8664-register-operands* (dpb (gpr-ordinal r)
                                        (byte 4 0)
                                        x86-32-bit-register)))       

(defun x86-reg64 (r)
  (svref *x8664-register-operands* (dpb (gpr-ordinal r)
                                        (byte 4 0)
                                        x86-64-bit-register)))
