(in-package "CCL")

;;; Copy N bytes from pointer src, starting at byte offset src-offset,
;;; to ivector dest, starting at offset dest-offset.
;;; It's fine to leave this in lap.
;;; Depending on alignment, it might make sense to move more than
;;; a byte at a time.
;;; Does no arg checking of any kind.  Really.

;;; I went ahead and used the INC and DEC instructions here, since
;;; they're shorter than the equivalent ADD/SUB.  Intel's optimization
;;; manual advises avoiding INC and DEC because they might cause
;;; dependencies on earlier instructions that set the flags.  So, if
;;; these functions end up being hot, replacing the inc/dec insns
;;; might be worth a try.

(defx8632lapfunction %copy-ptr-to-ivector ((src 12)
					   (src-byte-offset 8)
					   (dest 4)
					   #|(ra 0)|#
					   (dest-byte-offset arg_y)
					   (nbytes arg_z))
  (mark-as-imm temp0)
  (mark-as-imm arg_y)
  (let ((foreign-ptr temp0)		;raw foreign pointer
	(ivector temp1)			;destination ivector
	(j arg_y))			;unboxed index into ivector
    (movl (@ src (% esp)) (% temp1))
    (macptr-ptr temp1 foreign-ptr)
    (movl (@ src-byte-offset (% esp)) (% temp1))
    (unbox-fixnum temp1 imm0)
    (addl (% imm0) (% foreign-ptr))	;point to starting byte in src
    (movl (@ dest (% esp)) (% ivector))
    (sarl ($ x8632::fixnumshift) (% j))	;unbox dest-byte-offset
    (testl (% nbytes) (% nbytes))
    (jmp @test)
    @loop
    (movb (@ (% foreign-ptr)) (%b imm0))
    (incl (% foreign-ptr))
    (movb (%b imm0) (@ x8632::misc-data-offset (% ivector) (% j)))
    (incl (% j))
    (subl ($ '1) (% nbytes))
    @test
    (jne @loop)
    (movl (% ivector) (% arg_z)))
  (mark-as-node temp0)
  (mark-as-node arg_y)
  (single-value-return 5))

(defx8632lapfunction %copy-ivector-to-ptr ((src 12)
					   (src-byte-offset 8)
					   (dest 4)
					   #|(ra 0)|#
					   (dest-byte-offset arg_y)
					   (nbytes arg_z))
  (mark-as-imm temp0)
  (mark-as-imm arg_y)
  (let ((foreign-ptr temp0)		;raw foreign pointer
	(ivector temp1)			;source ivector
	(j arg_y))			;unboxed index into ivector
    (movl (@ dest (% esp)) (% temp1))
    (macptr-ptr temp1 foreign-ptr)
    (unbox-fixnum dest-byte-offset imm0)
    (addl (% imm0) (% foreign-ptr))	;point to starting byte in dest
    (movl (@ src (% esp)) (% ivector))
    (movl (@ src-byte-offset (% esp)) (% j))
    (sarl ($ x8632::fixnumshift) (% j))	;unbox src-byte-offset
    (test (% nbytes) (% nbytes))
    (jmp @test)
    @loop
    (movb (@ x8632::misc-data-offset (% ivector) (% j)) (%b imm0))
    (incl (% j))
    (movb (%b imm0) (@ (% foreign-ptr)))
    (incl (% foreign-ptr))
    (subl ($ '1) (% nbytes))
    @test
    (jne @loop)
    (movl (@ dest (% esp)) (% arg_z)))
  (mark-as-node temp0)
  (mark-as-node arg_y)
  (single-value-return 5))

(defx8632lapfunction %copy-ivector-to-ivector ((src 12) 
					       (src-byte-offset 8)
					       (dest 4)
					       #|(ra 0)|#
					       (dest-byte-offset arg_y)
					       (nbytes arg_z))
  (movl (@ src (% esp)) (% temp0))
  (movl (@ src-byte-offset (% esp)) (% temp1))
  (unbox-fixnum nbytes imm0)		;will be used below
  (push (% nbytes))			;put loop counter on stack
  (movl (@ (+ 4 dest) (% esp)) (% arg_z))
  (mark-as-imm temp1)
  (mark-as-imm arg_y)
  (sarl ($ x8632::fixnumshift) (% temp1)) ;unboxed src index
  (sarl ($ x8632::fixnumshift) (% arg_y)) ;unboxed dest index
  (let ((a temp0)
	(i temp1)
	(b arg_z)
	(j arg_y))
    ;; copy nbytes starting at a[i] to b[j]
    (cmpl (% b) (% a))
    (jne @front)
    (cmpl (% i) (% j))
    (jg @back)
    @front
    (testl (% imm0) (% imm0))		;test nbytes
    (jmp @front-test)
    @front-loop
    (movb (@ x8632::misc-data-offset (% a) (% i)) (%b imm0))
    (movb (%b imm0) (@ x8632::misc-data-offset (% b) (% j)))
    (incl (% i))
    (incl (% j))
    (subl ($ '1) (@ (% esp)))
    @front-test
    (jne @front-loop)
    (jmp @done)
    @back
    ;; unboxed nbytes in imm0
    (addl (% imm0) (% i))
    (addl (% imm0) (% j))
    (testl (% imm0) (% imm0))
    (jmp @back-test)
    @back-loop
    (decl (% i))
    (decl (% j))
    (movb (@ x8632::misc-data-offset (% a) (% i)) (%b imm0))
    (movb (%b imm0) (@ x8632::misc-data-offset (% b) (% j)))
    (subl ($ '1) (@ (% esp)))
    @back-test
    (jne @back-loop)
    @done
    ;; dest already in arg_z
    (addl ($ 4) (% esp))		;pop nbytes
    (mark-as-node temp1)
    (mark-as-node arg_y)
    (single-value-return 5)))

(defx8632lapfunction %copy-gvector-to-gvector ((src 12)
					       (src-element 8)
					       (dest 4)
					       #|(ra 0)|#
					       (dest-element arg_y)
					       (nelements arg_z))
  (let ((a temp0)
	(i imm0)
	(b arg_z)
	(j arg_y)
	(val temp1))
    (movl (% nelements) (% val))     ;will be used below
    (push (% nelements))	     ;loop counter on stack (use ebp?)
    (movl (@ (+ 4 src) (% esp)) (% a))
    (movl (@ (+ 4 src-element) (% esp)) (% i))
    (movl (@ (+ 4 dest) (% esp)) (% b))
    ;; j/arg_y already set
    (cmpl (% a) (% b))
    (jne @front)
    (rcmp (% i) (% j))
    (jl @back)
    @front
    (testl (% val) (% val))		;test nelements
    (jmp @front-test)
    @front-loop
    (movl (@ x8632::misc-data-offset (% a) (% i)) (% val))
    (movl (% val) (@ x8632::misc-data-offset (% b) (% j)))
    (addl ($ '1) (% i))
    (addl ($ '1) (% j))
    (subl ($ '1) (@ (% esp)))
    @front-test
    (jne @front-loop)
    (jmp @done)
    @back
    ;; nelements in val (from above)
    (addl (% val) (% i))
    (addl (% val) (% j))
    (testl (% val) (% val))
    (jmp @back-test)
    @back-loop
    (subl ($ '1) (% i))
    (subl ($ '1) (% j))
    (movl (@ x8632::misc-data-offset (% a) (% i)) (% val))
    (movl (% val) (@ x8632::misc-data-offset (% b) (% j)))
    (subl ($ '1) (@ (% esp)))
    @back-test
    (jne @back-loop)
    @done
    ;; dest already in arg_z
    (addl ($ 4) (% esp))		;pop loop counter
    (single-value-return 5)))

(defx8632lapfunction %heap-bytes-allocated ()
  (movl (@ (% :rcontext) x8632::tcr.save-allocptr) (% temp1))
  (movl (@ (% :rcontext) x8632::tcr.last-allocptr) (% temp0))
  (cmpl ($ -8) (% temp1))		;void_allocptr
  (movq (@ (% :rcontext) x8632::tcr.total-bytes-allocated-low) (% mm0))
  (jz @go)
  (movl (% temp0) (% arg_y))
  (subl (% temp1) (% temp0))
  (testl (% arg_y) (% arg_y))
  (jz @go)
  (movd (% temp0) (% mm1))
  (paddq (% mm1) (% mm0))
  @go
  (jmp-subprim .SPmakeu64))

(defx8632lapfunction values ()
  (:arglist (&rest values))
  (save-frame-variable-arg-count)
  (push-argregs)
  (jmp-subprim .SPnvalret))

(defx8632lapfunction rdtsc ()
  (mark-as-imm temp1)			;aka edx
  (:byte #x0f)                          ;two-byte rdtsc opcode
  (:byte #x31)                          ;is #x0f #x31
  (box-fixnum imm0 arg_z)
  (mark-as-node temp1)
  (single-value-return))

;;; Return all 64 bits of the time-stamp counter as an unsigned integer.
(defx8632lapfunction rdtsc64 ()
  (mark-as-imm temp1)			;aka edx
  (:byte #x0f)                          ;two-byte rdtsc opcode
  (:byte #x31)                          ;is #x0f #x31
  (movd (% eax) (% mm0))
  (movd (% edx) (% mm1))
  (psllq ($ 32) (% mm1))
  (por (% mm1) (% mm0))
  (mark-as-node temp1)
  (jmp-subprim .SPmakeu64))

;;; It would be nice if (%setf-macptr macptr (ash (the fixnum value)
;;; ash::fixnumshift)) would do this inline.

(defx8632lapfunction %setf-macptr-to-object ((macptr arg_y) (object arg_z))
  (check-nargs 2)
  (trap-unless-typecode= macptr x8632::subtag-macptr)
  (movl (% object) (@ x8632::macptr.address (% macptr)))
  (single-value-return))

(defx8632lapfunction %fixnum-from-macptr ((macptr arg_z))
  (check-nargs 1)
  (trap-unless-typecode= arg_z x8632::subtag-macptr)
  (movl (@ x8632::macptr.address (% arg_z)) (% imm0))
  (mark-as-imm temp0)
  (let ((imm1 temp0))
    (trap-unless-lisptag= imm0 x8632::tag-fixnum imm1))
  (mark-as-node temp0)
  (movl (% imm0) (% arg_z))
  (single-value-return))

(defx8632lapfunction %%get-unsigned-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr x8632::subtag-macptr)
  (mark-as-imm temp0)
  (let ((imm1 temp0))
    (macptr-ptr ptr imm1)
    (unbox-fixnum offset imm0)
    (movq (@ (% imm1) (% imm0)) (% mm0)))
  (mark-as-node temp0)
  (jmp-subprim .SPmakeu64))

(defx8632lapfunction %%get-signed-longlong ((ptr arg_y) (offset arg_z))
  (trap-unless-typecode= ptr x8632::subtag-macptr)
  (mark-as-imm temp0)
  (let ((imm1 temp0))
    (macptr-ptr ptr imm1)
    (unbox-fixnum offset imm0)
    (movq (@ (% imm1) (% imm0)) (% mm0)))
  (mark-as-node temp0)
  (jmp-subprim .SPmakes64))

(defx8632lapfunction %%set-unsigned-longlong ((ptr 4)
					      #|(ra 0)|#
					      (offset arg_y)
					      (val arg_z))
  (mark-as-imm temp1)
  (let ((rptr temp0)
	(imm1 temp1))
    (save-simple-frame)
    (movl (@ ptr (% esp)) (% rptr))
    (trap-unless-typecode= rptr x8632::subtag-macptr)
    (call-subprim .SPgetu64)
    (macptr-ptr rptr imm0)
    (unbox-fixnum offset imm1)
    (movq (% mm0) (@ (% imm0) (% imm1)))
    (restore-simple-frame))
  (mark-as-node temp1)
  (single-value-return 3))

(defx8632lapfunction %%set-signed-longlong ((ptr 4)
					    #|(ra 0)|#
                                            (offset arg_y)
                                            (val arg_z))
  (mark-as-imm temp1)
  (let ((rptr temp0)
	(imm1 temp1))
    (save-simple-frame)
    (movl (@ ptr (% esp)) (% rptr))
    (trap-unless-typecode= rptr x8632::subtag-macptr)
    (call-subprim .SPgets64)
    (macptr-ptr rptr imm0)
    (unbox-fixnum offset imm1)
    (movq (% mm0) (@ (% imm0) (% imm1)))
    (restore-simple-frame))
  (mark-as-node temp1)
  (single-value-return 3))

(defx8632lapfunction interrupt-level ()
  (movl (@ (% :rcontext) x8632::tcr.tlb-pointer) (% imm0))
  (movl (@ x8632::interrupt-level-binding-index (% imm0)) (% arg_z))
  (single-value-return))

(defx8632lapfunction set-interrupt-level ((new arg_z))
  (movl (@ (% :rcontext) x8632::tcr.tlb-pointer) (% imm0))
  (trap-unless-fixnum new)
  (movl (% new) (@ x8632::interrupt-level-binding-index (% imm0)))
  (single-value-return))

(defx8632lapfunction %current-tcr ()
  (movl (@ (% :rcontext) x8632::tcr.linear) (% arg_z))
  (single-value-return))

(defx8632lapfunction %tcr-toplevel-function ((tcr arg_z))
  (check-nargs 1)
  (movl (@ x8632::tcr.vs-area (% tcr)) (% temp0))
  (movl (@ x8632::area.high (% temp0)) (% imm0)) ;bottom of vstack
  (cmpl (% tcr) (@ (% :rcontext) x8632::tcr.linear))
  (jz @myself)
  (cmpl (% imm0) (@ x8632::area.active (% temp0)))
  (jmp @finish)
  @myself
  (cmpl (% imm0) (% esp))
  @finish
  (movl ($ x8632::nil-value) (% arg_z))
  (cmovnel (@ (- x8632::node-size) (% imm0)) (% arg_z))
  (single-value-return))
  
(defx8632lapfunction %set-tcr-toplevel-function ((tcr arg_y) (fun arg_z))
  (check-nargs 2)
  (movl (@ x8632::tcr.vs-area (% tcr)) (% temp0))
  (movl (@ x8632::area.high (% temp0)) (% imm0))
  (cmpl (% tcr) (@ (% :rcontext) x8632::tcr.linear))
  (jz @myself)
  (cmpl (% imm0) (@ x8632::area.active (% temp0))) ;vstack empty?
  (jmp @room)
  @myself
  (cmpl (% imm0) (% esp))
  @room
  (leal (@ (- x8632::node-size) (% imm0)) (% imm0))
  (movl ($ 0) (@ (% imm0)))
  (jne @have-room)
  (movl (% imm0) (@ x8632::area.active (% temp0)))
  (movl (% imm0) (@ x8632::tcr.save-vsp (% tcr)))
  (jmp @have-room)
  @have-room
  (movl (% fun) (@ (% imm0)))
  (single-value-return))

;;; This needs to be done out-of-line, to handle EGC memoization.
(defx8632lapfunction %store-node-conditional ((offset 8)
					      (object 4)
					      #|(ra 0)|#
					      (old arg_y)
					      (new arg_z))
  (movl (@ offset (% esp)) (% temp0))
  (movl (@ object (% esp)) (% temp1))
  (save-simple-frame)
  (call-subprim .SPstore-node-conditional)
  (restore-simple-frame)
  (single-value-return 4))

(defx8632lapfunction %store-immediate-conditional ((offset 8)
						   (object 4)
						   #|(ra 0)|#
						   (old arg_y)
						   (new arg_z))
  (mark-as-imm temp0)
  (let ((imm1 temp0)
	(robject temp1))
    (movl (@ offset (% esp)) (% imm1))
    (sarl ($ x8632::fixnumshift) (% imm1))
    (movl (@ object (% esp)) (% robject))
    @again
    (movl (@ (% robject) (% imm1)) (% eax))
    (cmpl (% eax) (% old))
    (jne @lose)
    (lock)
    (cmpxchgl (% new) (@ (% robject) (% imm1)))
    (jne @again)
    (movl ($ x8632::t-value) (% arg_z))
    (mark-as-node temp0)
    (single-value-return 4)
    @lose
    (movl ($ x8632::nil-value) (% arg_z))
    (mark-as-node temp0)
    (single-value-return 4)))

(defx8632lapfunction set-%gcable-macptrs% ((ptr arg_z))
  @again
  (movl (@ (+ x8632::nil-value (x8632::kernel-global gcable-pointers))) (% eax))
  (movl (% eax) (@ x8632::xmacptr.link (% ptr)))
  (lock)
  (cmpxchgl (% ptr) (@ (+ x8632::nil-value (x8632::kernel-global gcable-pointers))))
  (jne @again)
  (single-value-return))

;;; Atomically increment or decrement the gc-inhibit-count kernel-global
;;; (It's decremented if it's currently negative, incremented otherwise.)
(defx8632lapfunction %lock-gc-lock ()
  @again
  (movl (@ (+ x8632::nil-value (x8632::kernel-global gc-inhibit-count))) (% eax))
  (lea (@ '-1 (% eax)) (% temp0))
  (lea (@ '1 (% eax)) (% arg_z))
  (test (% eax) (% eax))
  (cmovsl (% temp0) (% arg_z))
  (lock)
  (cmpxchgl (% arg_z) (@ (+ x8632::nil-value (x8632::kernel-global gc-inhibit-count))))
  (jnz @again)
  (single-value-return))

;;; Atomically decrement or increment the gc-inhibit-count kernel-global
;;; (It's incremented if it's currently negative, incremented otherwise.)
;;; If it's incremented from -1 to 0, try to GC (maybe just a little.)
(defx8632lapfunction %unlock-gc-lock ()
  @again
  (movl (@ (+ x8632::nil-value (x8632::kernel-global gc-inhibit-count)))
        (% eax))
  (lea (@ '1 (% eax)) (% temp0))
  (cmpl ($ -1) (% eax))
  (lea (@ '-1 (% eax)) (% arg_z))
  (cmovlel (% temp0) (% arg_z))
  (lock)
  (cmpxchgl (% arg_z) (@ (+ x8632::nil-value (x8632::kernel-global gc-inhibit-count))))
  (jne @again)
  (cmpl ($ '-1) (% eax))
  (jne @done)
  ;; The GC tried to run while it was inhibited.  Unless something else
  ;; has just inhibited it, it should be possible to GC now.
  (mov ($ arch::gc-trap-function-immediate-gc) (% imm0))
  (uuo-gc-trap)
  @done
  (single-value-return))

;;; Return true iff we were able to increment a non-negative
;;; lock._value
(defx8632lapfunction %try-read-lock-rwlock ((lock arg_z))
  (check-nargs 1)
  (mark-as-imm temp0)
  (let ((imm1 temp0))
    @try
    (movl (@ x8632::lock._value (% lock)) (% eax))
    (movl (% eax) (% imm1))
    (addl ($ '1) (% imm1))
    (jle @fail)
    (lock)
    (cmpxchgl (% imm1) (@ x8632::lock._value (% lock)))
    (jne @try)
    (mark-as-node temp0)
    (single-value-return)		; return the lock
    @fail
    (mark-as-node temp0)
    (movl ($ x8632::nil-value) (% arg_z))
    (single-value-return)))

(defx8632lapfunction unlock-rwlock ((lock arg_z))
  (cmpl ($ 0) (@ x8632::lock._value (% lock)))
  (jle @unlock-write)
  @unlock-read
  (mark-as-imm temp0)
  (let ((imm1 temp0))
    (movl (@ x8632::lock._value (% lock)) (% eax))
    (lea (@ '-1 (% imm0)) (% imm1))
    (lock)
    (cmpxchgl (% imm1) (@ x8632::lock._value (% lock)))
    (jne @unlock-read))
  (mark-as-node temp0)
  (single-value-return)
  @unlock-write
  ;;; If we aren't the writer, return NIL.
  ;;; If we are and the value's about to go to 0, clear the writer field.
  (movl (@ x8632::lock.writer (% lock)) (% imm0))
  (cmpl (% imm0) (@ (% :rcontext) x8632::tcr.linear))
  (jne @fail)
  (cmpl ($ '-1) (@ x8632::lock._value (% lock)))
  (jne @still-owner)
  (movss (% fpzero) (@ x8632::lock.writer (% lock)))
  @still-owner
  (addl ($ '1) (@ x8632::lock._value (% lock)))
  (single-value-return)
  @fail
  (movl ($ x8632::nil-value) (%l arg_z))
  (single-value-return))

(defx8632lapfunction %atomic-incf-node ((by 4) #|(ra 0)|# (node arg_y) (disp arg_z))
  (check-nargs 3)
  (mark-as-imm temp0)
  (let ((imm1 temp0)
	(rby temp1))
    (movl (@ by (% esp)) (% rby))
    (unbox-fixnum disp imm1)
    @again
    (movl (@ (% node) (% imm1)) (% eax))
    (lea (@ (% eax) (% rby)) (% arg_z))
    (lock)
    (cmpxchgl (% arg_z) (@ (% node) (% imm1)))
    (jne @again))
  (mark-as-node temp0)
  (single-value-return 3))

(defx8632lapfunction %atomic-incf-ptr ((ptr arg_z))
  (mark-as-imm temp0)
  (mark-as-imm temp1)
  (let ((imm1 temp0)
	(imm2 temp1))
    (macptr-ptr ptr imm2)
    @again
    (movl (@ (% imm2)) (% eax))
    (lea (@ 1 (% eax)) (% imm1))
    (lock)
    (cmpxchgl (% imm1) (@ (% imm2)))
    (jne @again)
    (box-fixnum imm1 arg_z))
  (mark-as-node temp0)
  (mark-as-node temp1)
  (single-value-return))

(defx8632lapfunction %atomic-incf-ptr-by ((ptr arg_y) (by arg_z))
  (mark-as-imm temp0)
  (mark-as-imm temp1)
  (let ((imm1 temp0)
	(imm2 temp1))
    (macptr-ptr ptr imm2)
    @again
    (movl (@ (% imm2)) (% eax))
    (unbox-fixnum by imm1)
    (add (% eax) (% imm1))
    (lock)
    (cmpxchgl (% imm1) (@ (% imm2)))
    (jnz @again)
    (box-fixnum imm1 arg_z))
  (mark-as-node temp0)
  (mark-as-node temp1)
  (single-value-return))

(defx8632lapfunction %atomic-decf-ptr ((ptr arg_z))
  (mark-as-imm temp0)
  (mark-as-imm temp1)
  (let ((imm1 temp0)
	(imm2 temp1))
    (macptr-ptr ptr imm2)
    @again
    (movl (@ (% imm2)) (% eax))
    (lea (@ -1 (% eax)) (% imm1))
    (lock)
    (cmpxchgl (% imm1) (@ (% imm2)))
    (jne @again)
    (box-fixnum imm1 arg_z))
  (mark-as-node temp0)
  (mark-as-node temp1)
  (single-value-return))

(defx8632lapfunction %atomic-decf-ptr-if-positive ((ptr arg_z))
  (mark-as-imm temp0)
  (mark-as-imm temp1)
  (let ((imm1 temp0)
	(imm2 temp1))
    (macptr-ptr ptr imm2)
    @again
    (movl (@ (% imm2)) (% eax))
    (testl (% eax) (% eax))
    (lea (@ -1 (% eax)) (% imm1))
    (jz @done)
    (lock)
    (cmpxchgl (% imm1) (@ (% imm2)))
    (jnz @again)
    @done
    (box-fixnum imm1 arg_z))
  (mark-as-node temp0)
  (mark-as-node temp1)
  (single-value-return))

(defx8632lapfunction %atomic-swap-ptr ((ptr arg_y) (newval arg_z))
  (mark-as-imm temp0)
  (let ((imm1 temp0))
    (macptr-ptr arg_y imm1)
    (unbox-fixnum newval imm0)
    (lock)
    (xchgl (% imm0) (@ (% imm1)))
    (box-fixnum imm0 arg_z))
  (mark-as-node temp0)
  (single-value-return))

;;; Try to store the fixnum NEWVAL at PTR, if and only if the old value
;;; was equal to OLDVAL.  Return the old value
(defx8632lapfunction %ptr-store-conditional ((ptr 4) #|(ra 0)|# (expected-oldval arg_y) (newval arg_z))
  (mark-as-imm temp1)
  (let ((imm2 temp1))
    (movl (@ ptr (% esp)) (% temp0))
    (macptr-ptr temp0 imm2)
    (mark-as-imm temp0)
    (let ((imm1 temp0))
      @again
      (movl (@ (% imm2)) (% imm0))
      (box-fixnum imm0 imm0)
      (cmpl (% imm0) (% expected-oldval))
      (jne @done)
      (unbox-fixnum newval imm1)
      (lock)
      (cmpxchgl (% imm1) (@ (% imm2)))
      (jne @again)
      @done
      (movl (% imm0) (% arg_z)))
    (mark-as-node temp0))
  (mark-as-node temp1)
  (single-value-return 3))

(defx86lapfunction %ptr-store-fixnum-conditional ((ptr 4) #|(ra 0)|# (expected-oldval arg_y) (newval arg_z))
  (mark-as-imm temp0)
  (let ((address temp0))
    (movl (@ ptr (% esp)) (% temp1))
    (macptr-ptr temp1 address)
    @again
    (movl (@ (% address)) (% imm0))
    (cmpl (% imm0) (% expected-oldval))
    (jne @done)
    (lock)
    (cmpxchgl (% newval) (@ (% address)))
    (jne @again)
    @done
    (movl (% imm0) (% arg_z)))
  (mark-as-node temp0)
  (single-value-return 3))

(defx8632lapfunction %macptr->dead-macptr ((macptr arg_z))
  (check-nargs 1)
  (movb ($ x8632::subtag-dead-macptr) (@ x8632::misc-subtag-offset (% macptr)))
  (single-value-return))

;;; %%apply-in-frame

(defx8632lapfunction %%save-application ((flags arg_y) (fd arg_z))
  (unbox-fixnum fd imm0)
  (movd (% imm0) (% mm0))
  (unbox-fixnum flags imm0)
  (orl ($ arch::gc-trap-function-save-application) (% imm0))
  (uuo-gc-trap)
  (single-value-return))

(defx8632lapfunction %misc-address-fixnum ((misc-object arg_z))
  (check-nargs 1)
  (lea (@ x8632::misc-data-offset (% misc-object)) (% arg_z))
  (single-value-return))

(defx8632lapfunction fudge-heap-pointer ((ptr 4) #|(ra 0)|# (subtype arg_y) (len arg_z))
  (check-nargs 3)
  (mark-as-imm temp0)
  (let ((imm1 temp0))
    (movl (@ ptr (% esp)) (% temp1))
    (macptr-ptr temp1 imm1)	      ; address in macptr
    (lea (@ 9 (% imm1)) (% imm0))     ; 2 for delta + 7 for alignment
    (andb ($ -8) (%b  imm0))	      ; Clear low three bits to align
    (subl (% imm0) (% imm1))	      ; imm1 = -delta
    (negw (%w imm1))
    (movw (%w imm1) (@  -2 (% imm0)))	; save delta halfword
    (unbox-fixnum subtype imm1)		; subtype at low end of imm1
    (shll ($ (- x8632::num-subtag-bits x8632::fixnum-shift)) (% len))
    (orl (% len) (% imm1))
    (movl (% imm1) (@ (% imm0)))	; store subtype & length
    (lea (@ x8632::fulltag-misc (% imm0)) (% arg_z))) ; tag it, return it
  (mark-as-node temp0)
  (single-value-return 3))

(defx8632lapfunction %%make-disposable ((ptr arg_y) (vector arg_z))
  (check-nargs 2)
  (mark-as-imm temp0)
  (let ((imm1 temp0))
    (lea (@ (- x8632::fulltag-misc) (% vector)) (% imm0)) ; imm0 is addr = vect less tag
    (movzwl (@ -2 (% imm0)) (% imm1))     ; get delta
    (subl (% imm1) (% imm0))              ; vector addr (less tag)  - delta is orig addr
    (movl (% imm0) (@ x8632::macptr.address (% ptr))))
  (mark-as-node temp0)
  (single-value-return))

(defx8632lapfunction %vect-data-to-macptr ((vect arg_y) (ptr arg_z))
  (lea (@ x8632::misc-data-offset (% vect)) (% imm0))
  (movl (% imm0) (@ x8632::macptr.address (% ptr)))
  (single-value-return))

;;; Sadly, we have no NVRs on x8632.
(defun get-saved-register-values ()
  (values))

(defx8632lapfunction %current-db-link ()
  (movl (@ (% :rcontext) x8632::tcr.db-link) (% arg_z))
  (single-value-return))

(defx8632lapfunction %no-thread-local-binding-marker ()
  (movl ($ x8632::subtag-no-thread-local-binding) (% arg_z))
  (single-value-return))

(defx8632lapfunction break-event-pending-p ()
  (xorl (% temp0) (% temp0))
  (ref-global x8632::intflag imm0)
  (set-global temp0 x8632::intflag)
  (testl (% imm0) (% imm0))
  (setne (%b imm0))
  (andl ($ x8632::t-offset) (%l imm0))
  (lea (@ x8632::nil-value (% imm0)) (% arg_z))
  (single-value-return))