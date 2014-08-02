;;;; numbers.scm
;
; Copyright (c) 2008-2012 The CHICKEN Team
; Copyright (c) 2000-2007, Felix L. Winkelmann
; All rights reserved.
;
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 1. Redistributions of source code must retain the above copyright
;    notice, this list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in the
;    documentation and/or other materials provided with the distribution.
; 3. The name of the authors may not be used to endorse or promote products
;    derived from this software without specific prior written permission.
;
; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
; IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


(declare 
  (no-bound-checks)
  (no-procedure-checks))

(module numbers 
    (+ - * / = > < >= <= eqv?
       add1 sub1 signum number->string string->number integer-length
       bitwise-and bitwise-ior bitwise-xor bitwise-not arithmetic-shift
       equal? ; From scheme. Structural & bytevector comparisons Just Work
       exp log sin cos tan atan acos asin conj
       expt sqrt exact-integer-sqrt exact-integer-nth-root
       quotient modulo remainder quotient&modulo quotient&remainder
       numerator denominator
       abs max min gcd lcm
       positive? negative? odd? even? zero?
       exact? inexact?
       rationalize
       random randomize
       floor ceiling truncate round
       inexact->exact exact->inexact
       number? complex? real? rational? integer? exact-integer?
       make-rectangular make-polar real-part imag-part magnitude angle
       bignum? ratnum? cflonum? rectnum? compnum? cintnum? cplxnum?
       nan? finite? infinite?)

  (import (except scheme
		  + - * / = > < >= <= eqv?
		  number->string string->number 
		  exp log sin cos tan atan acos asin expt sqrt
		  quotient modulo remainder
		  numerator denominator
		  abs max min gcd lcm
		  positive? negative? odd? even? zero?
		  exact? inexact?
		  rationalize
		  floor ceiling truncate round
		  inexact->exact exact->inexact
		  number? complex? real? rational? integer?
		  make-rectangular make-polar real-part imag-part magnitude angle)
	  (except chicken add1 sub1 random randomize conj signum finite?
		  bitwise-and bitwise-ior bitwise-xor bitwise-not arithmetic-shift)
	  foreign)

(foreign-declare "#include \"numbers-c.h\"")
(foreign-declare "#include \"numbers-c.c\"")

(define-foreign-variable FIX integer)
(define-foreign-variable FLO integer)
(define-foreign-variable BIG integer)
(define-foreign-variable RAT integer)
(define-foreign-variable COMP integer)
(define-foreign-variable NONE integer)

(define-foreign-variable bignum_and_op integer)
(define-foreign-variable bignum_ior_op integer)
(define-foreign-variable bignum_xor_op integer)

;;; Error handling

(define (bad-number loc x) (##sys#signal-hook #:type-error loc "bad argument type - not a number" x))
(define (bad-real loc x) (##sys#signal-hook #:type-error loc "bad argument type - not a real number" x))
(define (bad-ratnum loc x) (##sys#signal-hook #:type-error loc "bad argument type - not a rational number" x))
(define (bad-integer loc x) (##sys#signal-hook #:type-error loc "bad argument type - not an integer" x))
(define (bad-natural loc x) (##sys#signal-hook #:type-error loc "bad argument type - must be an nonnegative integer" x))
(define (bad-complex/o loc x) (##sys#signal-hook #:type-error loc "bad argument type - complex number has no ordering" x))
(define (bad-base loc x) (##sys#signal-hook #:type-error loc "bad argument type - not a valid base" x))
(define (bad-inexact loc x) (##sys#signal-hook #:type-error loc "bad argument type - inexact number has no exact representation" x))
(define (bad-exact loc x) (##sys#signal-hook #:type-error loc "bad argument type - must be an exact number" x))
(define (log0 loc x) (##sys#signal-hook #:arithmetic-error loc "log of exact 0 is undefined" x))
(define (expt0 loc x y) (##sys#signal-hook #:arithmetic-error loc "exponent of exact 0 with complex argument is undefined" x y))
(define (div/0 loc x y) (##sys#signal-hook #:arithmetic-error loc "division by zero" x y))

(define-inline (%init-tags tagvec) (##core#inline "init_tags" tagvec))
(define-inline (%check-number x) (##core#inline "check_number" x))

(define-inline (assert-number x loc)
  (when (eq? NONE (%check-number x))
    (bad-number loc x) ) )

(define-inline (fix-div/0 x y loc)
  (if (eq? y 0)
      (div/0 loc x y)
      y) )

;;; Primitives

(define-inline (fp/ x y) (##core#inline_allocate ("C_a_i_flonum_quotient" 4) x y))
(define-inline (fp+ x y) (##core#inline_allocate ("C_a_i_flonum_plus" 4) x y))
(define-inline (fp- x y) (##core#inline_allocate ("C_a_i_flonum_difference" 4) x y))
(define-inline (fp* x y) (##core#inline_allocate ("C_a_i_flonum_times" 4) x y))
(define-inline (fp= x y) (##core#inline "C_flonum_equalp" x y))
(define-inline (fp> x y) (##core#inline "C_flonum_greaterp" x y))
(define-inline (fp< x y) (##core#inline "C_flonum_lessp" x y))

(define-inline (%flonum? x) (##core#inline "flonump" x))
(define-inline (%flo-integer? x) (##core#inline "C_u_i_fpintegerp" x))

(define-inline (complex-real c) (##sys#slot c 1))
(define-inline (complex-imag c) (##sys#slot c 2))
(define-inline (%make-complex r i) (##sys#make-structure 'compnum r i))

(define-inline (rat-numerator c) (##sys#slot c 1))
(define-inline (rat-denominator c) (##sys#slot c 2))
(define-inline (%make-rat r i) (##sys#make-structure 'ratnum r i))

(define-inline (%fix->flo n) (##core#inline_allocate ("fix_to_flo" 4) n))
(define-inline (%big->flo n) (##core#inline_allocate ("big_to_flo" 4) n))

(define %fix+fix (##core#primitive "fix_plus_fix"))
(define %fix+big (##core#primitive "fix_plus_big"))
(define %big+big (##core#primitive "big_plus_big"))

(define %big-neg (##core#primitive "big_neg"))
;; Can't use fxneg because that breaks in the edge case of negating
;; the most negative fixnum.  Yes, 2's complement is fun!
(define %fix-neg (##core#primitive "fix_neg"))

(define %fix-big (##core#primitive "fix_minus_big"))
(define %big-fix (##core#primitive "big_minus_fix"))
(define %big-big (##core#primitive "big_minus_big"))

(define %fix*fix (##core#primitive "fix_times_fix"))
(define %fix*big (##core#primitive "fix_times_big"))
(define %big*big (##core#primitive "big_times_big"))

(define %big-quotient-fix (##core#primitive "big_quotient_fix"))
(define %big-quotient-big (##core#primitive "big_quotient_big"))

(define %big-remainder-fix (##core#primitive "big_remainder_fix"))
(define %big-remainder-big (##core#primitive "big_remainder_big"))

(define %big-divrem-fix (##core#primitive "big_divrem_fix"))
(define %big-divrem-big (##core#primitive "big_divrem_big"))

;; This one should really be part of Chicken, hence the name
(define fxgcd (##core#primitive "C_fixnum_gcd"))
(define biggcd (##core#primitive "big_gcd"))
(define (fpgcd x y) (##core#inline_allocate ("C_a_i_flonum_gcd" 4) x y))

(define-inline (%big-comp-big x y) (##core#inline "big_comp_big" x y))

(define %big-abs (##core#primitive "big_abs"))

(define-inline (%big-odd? x) (##core#inline "big_oddp" x))
(define-inline (%big-negative? x) (##core#inline "big_negp" x))

(define %%expt-0 (##core#primitive "C_expt"))

(define (%expt-0 a b)
  (if (and (negative? a) (not (##sys#integer? b)))
      (%exp (%* b (%log a)))
      (%%expt-0 a b)))

(define %quotient-0 (##core#primitive "C_quotient"))

(define %flo->integer (##core#primitive "flo_to_int"))

(define %int-bitwise-int (##core#primitive "int_bitwise_int"))
(define %int-not (##core#primitive "int_not"))
(define %int-shift-fix (##core#primitive "int_shift_fix"))
(define %int-length (##core#primitive "int_length"))

(define number->string-0 (##core#primitive "C_number_to_string"))

(define %big->string (##core#primitive "big_to_string"))
(define %digits->number (##core#primitive "digits_to_big"))

(define-inline (%subchar s i) (##core#inline "C_subchar" s i))

(define-inline (%fix-randomize n) (##core#inline "fix_randomize" n))
(define-inline (%big-randomize n) (##core#inline "big_randomize" n))

(define-inline (%fix-random n) (##core#inline "fix_random" n))
(define %big-random (##core#primitive "big_random"))


;;; Support macros

(define-syntax switchq
  (syntax-rules (else)
    ((_ "aux" _) (##core#undefined))
    ((_ "aux" _ (else body ...))
     (begin body ...))
    ((_ "aux" tmp (val body ...) more ...)
     (if (eq? tmp val)
	 (begin body ...)
	 (switchq "aux" tmp more ...)))
    ((_ exp body ...)
     (let ((tmp exp))
       (switchq "aux" tmp body ...)))))


;;; Setup

(%init-tags
 (vector 'bignum			; BIG_TAG
	 'ratnum			; RAT_TAG
	 'compnum))			; COMP_TAG

(##sys#gc #f)				; move tag-vector into 2nd generation


;;; Basic arithmetic:

(define (+ . args)
  (if (null? args) 
      0
      (let ((x (##sys#slot args 0))
	    (rest (##sys#slot args 1)))
	(cond ((null? rest)
	       (assert-number x '+) 
	       x)
	      (else
	       (let loop ((args rest) (x x))
		 (if (null? args)
		     x
		     (loop (##sys#slot args 1) (%+ x (##sys#slot args 0))) ) ) ) ) ) ) )

(define (%+ x y)
  (switchq (%check-number x)
    [FIX 
     (switchq (%check-number y)
       [FIX (%fix+fix x y)]
       [FLO (fp+ (%fix->flo x) y)]
       [BIG (%fix+big x y)]
       ;; a/b + c/d = (a*d + b*c)/(b*d)  [with b = 1]
       [RAT (let ((d (rat-denominator y)))
              (%/ (%+ (%* x d) (rat-numerator y)) d))]
       [COMP (%comp+comp (%make-complex x 0) y)]
       [else (bad-number '+ y)] ) ]
    [FLO 
     (switchq (%check-number y)
       [FIX (fp+ x (%fix->flo y))]
       [FLO (fp+ x y)]
       [BIG (fp+ x (%big->flo y))]
       ;; a/b + c/d = (a*d + b*c)/(b*d)  [with b = 1]
       [RAT (let ((d (rat-denominator y)))
              (%/ (%+ (%* x d) (rat-numerator y)) d))]
       [COMP (%comp+comp (%make-complex x 0) y)]
       [else (bad-number '+ y)] ) ]
    [BIG
     (switchq (%check-number y)
       [FIX (%fix+big y x)]
       [FLO (fp+ (%big->flo x) y)]
       [BIG (%big+big x y)]
       ;; a/b + c/d = (a*d + b*c)/(b*d)  [with b = 1]
       [RAT (let ((d (rat-denominator y)))
              (%/ (%+ (%* x d) (rat-numerator y)) d))]
       [COMP (%comp+comp (%make-complex x 0) y)]
       [else (bad-number '+ y)] ) ]
    [RAT
     (switchq (%check-number y)
       [RAT (rat+/- '+ %+ x y)]
       [COMP (%comp+comp (%make-complex x 0) y)]
       [NONE (bad-number '+ y)]
       ;; a/b + c/d = (a*d + b*c)/(b*d)  [with d = 1]
       [else (let ((b (rat-denominator x)))
              (%/ (%+ (rat-numerator x) (%* b y)) b))] ) ]
    [COMP
     (switchq (%check-number y)
       [COMP (%comp+comp x y)]
       [NONE (bad-number '+ y)]
       [else (%comp+comp x (%make-complex y 0))] ) ]
    [else (bad-number '+ x)] ) )

(define (%comp+comp x y)
  (let ([r (%+ (complex-real x) (complex-real y))]
	[i (%+ (complex-imag x) (complex-imag y))] )
    (make-complex r i) ) )

(define (- arg1 . args)
  (if (null? args) 
      (switchq (%check-number arg1)
	[FIX (%fix-neg arg1)]
	[FLO (fpneg arg1)]
	[BIG (%big-neg arg1)]
	[RAT (%make-rat (- (rat-numerator arg1)) (rat-denominator arg1))]
	[COMP (%make-complex (%- 0 (complex-real arg1)) (%- 0 (complex-imag arg1)))]
	[else (bad-number '- arg1)] )
      (let loop ([args (##sys#slot args 1)] [x (%- arg1 (##sys#slot args 0))])
	(if (null? args)
	    x
	    (loop (##sys#slot args 1) (%- x (##sys#slot args 0))) ) ) ) )

(define (%- x y)
  (switchq (%check-number x)
    [FIX 
     (switchq (%check-number y)
       [FIX (let ((n (%fix-neg y)))
              (if (= (%check-number n) BIG) ;; fix-neg(most negative) => bignum
                  (%fix+big x n)
                  (%fix+fix x n)))]
       [FLO (fp- (%fix->flo x) y)]
       [BIG (%fix-big x y)]
       ;; a/b - c/d = (a*d - b*c)/(b*d)  [with b = 1]
       [RAT (let ((d (rat-denominator y)))
              (%/ (%- (%* x d) (rat-numerator y)) d))]
       [COMP (%comp-comp (%make-complex x 0) y)]
       [else (bad-number '- y)] ) ]
    [FLO 
     (switchq (%check-number y)
       [FIX (fp- x (%fix->flo y))]
       [FLO (fp- x y)]
       [BIG (fp- x (%big->flo y))]
       ;; a/b - c/d = (a*d - b*c)/(b*d)  [with b = 1]
       [RAT (let ((d (rat-denominator y)))
              (%/ (%- (%* x d) (rat-numerator y)) d))]
       [COMP (%comp-comp (%make-complex x 0) y)]
       [else (bad-number '- y)] ) ]
    [BIG
     (switchq (%check-number y)
       [FIX (%big-fix x y)]
       [FLO (fp- (%big->flo x) y)]
       [BIG (%big-big x y)]		
       ;; a/b - c/d = (a*d - b*c)/(b*d)  [with b = 1]
       [RAT (let ((d (rat-denominator y)))
              (%/ (%- (%* x d) (rat-numerator y)) d))]
       [COMP (%comp-comp (%make-complex x 0) y)]
       [else (bad-number '- y)] ) ]
    [RAT
     (switchq (%check-number y)
       [RAT (rat+/- '- %- x y)]
       [COMP (%comp-comp (%make-complex x 0) y)]
       [NONE (bad-number '- y)]
       ;; a/b - c/d = (a*d - b*c)/(b*d)  [with d = 1]
       [else (let ((b (rat-denominator x)))
               (%/ (%- (rat-numerator x) (%* b y)) b))] ) ]
    [COMP
     (switchq (%check-number y)
       [COMP (%comp-comp x y)]
       [NONE (bad-number '- y)] 
       [else (%comp-comp x (%make-complex y 0))] ) ]
    [else (bad-number '- x)] ) )

(define (%comp-comp x y)
  (let ([r (%- (complex-real x) (complex-real y))]
	[i (%- (complex-imag x) (complex-imag y))] )
    (make-complex r i) ) )

(define (* . args)
  (if (null? args) 
      1
      (let ((x (##sys#slot args 0))
	    (rest (##sys#slot args 1)))
	(cond ((null? rest)
	       (assert-number x '*) 
	       x)
	      (else
	       (let loop ((args rest) (x x))
		 (if (null? args)
		     x
		     (loop (##sys#slot args 1) (%* x (##sys#slot args 0))) ) ) ) ) ) ) )

(define-inline (%nonrat*rat x y)
  ;; a/b * c/d = a*c / b*d  [with b = 1]
  ;;  =  ((a / g) * c) / (d / g)
  ;; With   g = gcd(a, d)   and  a = x   [Knuth, 4.5.1]
  (let* ((d (rat-denominator y))
         (g (%gcd-0 '* x d)))
    (ratnum (%* (%quotient '* x g) (rat-numerator y))
            (%quotient '* d g))))

(define (%* x y)
  (switchq (%check-number x)
    [FIX
     (switchq (%check-number y)
       [FIX (%fix*fix x y)]
       [FLO (fp* (%fix->flo x) y)]
       [BIG (%fix*big x y)]
       [RAT (%nonrat*rat x y)]
       [COMP (%comp*comp (%make-complex x 0) y)]
       [else (bad-number '* y)] ) ]
    [FLO
     (switchq (%check-number y)
       [FIX (fp* x (%fix->flo y))]
       [FLO (fp* x y)]
       [BIG (fp* x (%big->flo y))]
       ;; TODO: This can be incorrect when the ratnum consists of bignums
       [RAT (fp* x (%exact->inexact y))]
       [COMP (%comp*comp (%make-complex x 0) y)]
       [else (bad-number '* y)] ) ]
    [BIG
     (switchq (%check-number y)
       [FIX (%fix*big y x)]
       [FLO (fp* (%big->flo x) y)]
       [BIG (%big*big x y)]
       [RAT (%nonrat*rat x y)]
       [COMP (%comp*comp (%make-complex x 0) y)]
       [else (bad-number '* y)] ) ]
    [RAT
     (switchq (%check-number y)
       ;; a/b * c/d = a*c / b*d  [generic]
       ;;   = ((a / g1) * (c / g2)) / ((b / g2) * (d / g1))
       ;; With   g1 = gcd(a, d)   and    g2 = gcd(b, c) [Knuth, 4.5.1]
       [RAT (let* ((a (rat-numerator x)) (b (rat-denominator x))
                   (c (rat-numerator y)) (d (rat-denominator y))
                   (g1 (%gcd-0 '* a d)) (g2 (%gcd-0 '* b c)))
              (ratnum (%* (%quotient '* a g1) (%quotient '* c g2))
                      (%* (%quotient '* b g2) (%quotient '* d g1))))]
       [COMP (%comp*comp (%make-complex x 0) y)]
       ;; TODO: This can be incorrect when the ratnum consists of bignums
       [FLO (fp* y (%exact->inexact x))]
       [NONE (bad-number '* y)]
       [else (%nonrat*rat y x)]) ]
    [COMP
     (switchq (%check-number y)
       [COMP (%comp*comp x y)]
       [NONE (bad-number '* y)] 
       [else (%comp*comp x (%make-complex y 0))] ) ]
    [else (bad-number '* x)] ) )

(define (%comp*comp x y)
  (let* ([a (complex-real x)]
	 [b (complex-imag x)]
	 [c (complex-real y)]
	 [d (complex-imag y)] 
	 [r (%- (%* a c) (%* b d))]
	 [i (%+ (%* a d) (%* b c))] )
    (make-complex r i) ) )

(define (/ arg1 . args)
  (if (null? args) 
      (%/ 1 arg1)
      (let loop ([args (##sys#slot args 1)] [x (%/ arg1 (##sys#slot args 0))])
	(if (null? args)
	    x
	    (loop (##sys#slot args 1) (%/ x (##sys#slot args 0))) ) ) ) )

(define-inline (%nonrat/rat x y)
  ;; a/b / c/d = a*d / b*c  [with b = 1]
  ;;   = ((a / g1) * d * sign(a)) / abs(c / g1)
  ;; With   g1 = gcd(a, c)   and   a = x  [Knuth, 4.5.1 ex. 4]
  (let* ((c (rat-numerator y))
         (g (%gcd-0 '/ x c)))
    (%/ (%* (%quotient '/ x g) (rat-denominator y))
        (%quotient '/ c g))))

(define (%/ x y)
  (switchq (%check-number x)
    [FIX 
     (switchq (%check-number y)
       [FIX (let ((g (fxgcd x (fix-div/0 x y '/))))
              (ratnum (fx/ x g) (fx/ y g)))]
       [FLO (fp/ (%fix->flo x) y)]
       [BIG (let ((g (%gcd-0 '/ x y)))
              (ratnum (%quotient '/ x g) (%quotient '/ y g)))]
       [RAT (%nonrat/rat x y)]
       [COMP (%comp/comp (%make-complex x 0) y)]
       [else (bad-number '/ y)] ) ]
    [FLO 
     (switchq (%check-number y)
       [FIX (fp/ x (%fix->flo (fix-div/0 x y '/)))]
       [FLO (fp/ x y)]
       [BIG (fp/ x (%big->flo y))]
       ;; TODO: This can be incorrect when the ratnum consists of bignums
       [RAT (fp/ x (%exact->inexact y))]
       [COMP (%comp/comp (%make-complex x 0) y)]
       [else (bad-number '/ y)] ) ]
    [BIG
     (switchq (%check-number y)
       [FIX (let ((g (%gcd-0 '/ x (fix-div/0 x y '/))))
              (ratnum (%quotient '/ x g) (%quotient '/ y g)))]
       [FLO (fp/ (%big->flo x) y)]
       [BIG (let ((g (%gcd-0 '/ x y)))
              (ratnum (%quotient '/ x g) (%quotient '/ y g)))]
       [RAT (%nonrat/rat x y)]
       [COMP (%comp/comp (%make-complex x 0) y)]
       [else (bad-number '/ y)] ) ]
    [RAT
     (switchq (%check-number y)
       ;; a/b / c/d = a*d / b*c  [generic]
       ;;   = ((a / g1) * (d / g2) * sign(a)) / abs((b / g2) * (c / g1))
       ;; With   g1 = gcd(a, c)   and    g2 = gcd(b, d) [Knuth, 4.5.1 ex. 4]
       [RAT (let* ((a (rat-numerator x)) (b (rat-denominator x))
                   (c (rat-numerator y)) (d (rat-denominator y))
                   (g1 (%gcd-0 '/ a c)) (g2 (%gcd-0 '/ b d)))
              (%/ (%* (%quotient '/ a g1) (%quotient '/ d g2))
                  (%* (%quotient '/ b g2) (%quotient '/ c g1))))]
       [COMP (%comp/comp (%make-complex x 0) y)]
       ;; TODO: This can be incorrect when the ratnum consists of bignums
       [FLO (fp/ (%exact->inexact x) y)]
       [NONE (bad-number '/ y)]
       ;; a/b / c/d = a*d / b*c  [with d = 1]
       ;;   = ((a / g) * sign(a)) / abs(b * (c / g))
       ;; With   g = gcd(a, c)   and  c = y  [Knuth, 4.5.1 ex. 4]
       [else (let* ((a (rat-numerator x))
                    (g (%gcd-0 '/ a y))) ;; TODO: Improve error message if /0
               (%/ (%quotient '/ a g)
                   (%* (rat-denominator x) (%quotient '/ y g))))] ) ]
    [COMP
     (switchq (%check-number y)
       [COMP (%comp/comp x y)]
       [NONE (bad-number '/ y)] 
       [else (%comp/comp x (%make-complex y 0))] ) ]
    [else (bad-number '/ x)] ) )

(define (%comp/comp p q)
  (let* ([a (complex-real p)]
	 [b (complex-imag p)]
	 [c (complex-real q)]
	 [d (complex-imag q)]
	 [r (%+ (%* c c) (%* d d))]
	 [x (%/ (%+ (%* a c) (%* b d)) r)]
	 [y (%/ (%- (%* b c) (%* a d)) r)] )
    (make-complex x y) ) )


;;; Comparisons:

(define (%= x y)
  (switchq (%check-number x)
    [FIX 
     (switchq (%check-number y)
       [FIX (fx= x y)]
       [FLO (and (finite? y)
                 (%= x (%flo->rat '= y)))] ; Compare as ratnums (overflow)
       [BIG #f] ;; Needs bignum representation?  Can't be equal to a fixnum!
       [RAT #f] ;; Rats are never x/1, because those are normalised to just x
       [COMP #f] ;; Comps are only ever equal to other comps
       [else (bad-number '= y)] ) ]
    [FLO 
     (switchq (%check-number y)
       [FIX (and (finite? x)
                 (%= (%flo->rat '= x) y))] ; Compare as ratnums (overflow)
       [FLO (fp= x y)]
       [BIG (and (%flo-integer? x) (= (%flo->integer x) y))]
       [RAT (and (not (or (fp= x +inf.0) (fp= x -inf.0)))
                 (%= (%flo->rat '= x) y))] ; Compare as ratnums
       [COMP #f] ;; Comps are only ever equal to other comps
       [else (bad-number '= y)] ) ]
    [BIG
     (switchq (%check-number y)
       [FIX #f]  ;; Needs bignum representation?  Can't be equal to a fixnum!
       [FLO (and (%flo-integer? y) (= x (%flo->integer y)))]
       [BIG (fx= (%big-comp-big x y) 0)]
       [RAT #f] ;; Rats are never x/1, because those are normalised to just x
       [COMP #f] ;; Comps are only ever equal to other comps
       [else (bad-number '= y)] ) ]
    [RAT
     (switchq (%check-number y)
       [FIX #f] ;; Rats are never x/1, because those are normalised to just x
       [FLO (and (not (or (fp= y +inf.0) (fp= y -inf.0)))
                 (%= x (%flo->rat '= y)))] ; Compare as ratnums
       [BIG #f] ;; Rats are never x/1, because those are normalised to just x
       ;; TODO: Use integer= here, when we write it
       [RAT (and (%= (rat-numerator x) (rat-numerator y))
                 (%= (rat-denominator x) (rat-denominator y)))]
       [COMP #f] ;; Comps are only ever equal to other comps
       [else (bad-number '= y)] ) ]
    [COMP
     (switchq (%check-number y)
       [COMP (and (%= (complex-real x) (complex-real y))
                  (%= (complex-imag x) (complex-imag y)))]
       [NONE (bad-number '= y)]
       [else #f] ) ]
    [else (bad-number '= x)] ))

(define (= x1 x2 . xs)
  (and (%= x1 x2)
       (let loop ([x x2] [xs xs])
	 (or (null? xs)
	     (let ([h (##sys#slot xs 0)])
	       (and (%= x h)
		    (loop h (##sys#slot xs 1)) ) ) ) ) ))

(define (eqv? a b)
  (let ((ta (%check-number a))
        (tb (%check-number b)))
    ;; If both are numbers of the same type and exactness, compare.
    ;; Otherwise use eq? Characters are already compared correctly by eq?
    (and (eq? ta tb)
         (switchq ta
           (NONE (eq? a b))
           (FLO  (fp= a b))
           (FIX  (fx= a b))
           (BIG  (fx= (%big-comp-big a b) 0))
           ;; TODO: Use integer= here, when we write it
           (RAT  (and (%= (rat-numerator a) (rat-numerator b))
                      (%= (rat-denominator a) (rat-denominator b))))
           ;; We use eqv? here because exactness of components needs to match
           (COMP (and (eqv? (complex-real a) (complex-real b))
                      (eqv? (complex-imag a) (complex-imag b))))
           (else (error "This should not happen"))))))

(define (> x1 x2 . xs)
  (and (%> x1 x2 '>)
       (let loop ([x x2] [xs xs])
	 (or (null? xs)
	     (let ([h (##sys#slot xs 0)])
	       (and (%> x h '>)
		    (loop h (##sys#slot xs 1)) ) ) ) ) ) )

(define (%> x y loc)
  (switchq (%check-number x)
    (FIX 
     (switchq (%check-number y)
       (FIX (fx> x y))
       ;; Compare as ratnum, to prevent overflows
       (FLO (or (fp= y -inf.0)
                (and (not (fp= y +inf.0)) (fp= y y)
                     (%> x (%flo->rat loc y) loc))))
       ;;   x neg?   y neg?   x > y?   reason
       ;;  ---------------------------------------------------------------
       ;;     no       no       no     abs(y) > abs(x), both positive
       ;;     no      yes      yes     (a > b)  true if  (a > 0) & (b < 0)
       ;;    yes       no       no     (a > b)  false if (a < 0) & (b > 0)
       ;;    yes      yes      yes     abs(y) > abs(x), both negative
       ;;
       ;; It follows that x is only bigger than y when y is negative
       (BIG (%big-negative? y))
       ;; a/b > c/d  when  a*d > b*c  [with b = 1]
       (RAT (%> (%* x (rat-denominator y))
                (rat-numerator y) loc))
       (COMP (bad-complex/o loc y))
       (else (bad-number loc y)) ) )
    (FLO
     (switchq (%check-number y)
       (FLO (fp> x y))
       (COMP (bad-complex/o loc y))
       (NONE (bad-number loc y))
       ;; Compare as ratnums, to avoid errors when overflowing
       ;; (this can happen for bignums, but also for fixnums on 64-bit)
       (else (or (fp= x +inf.0)
                 (and (not (fp= x -inf.0)) (fp= x x)
                      (%> (%flo->rat loc x) y loc)))) ) )
    (BIG 
     (switchq (%check-number y)
       ;;   x neg?   y neg?   x > y?   reason
       ;;  ---------------------------------------------------------------
       ;;     no       no      yes     abs(x) > abs(y), both positive
       ;;     no      yes      yes     (a > b)  true if  (a > 0) & (b < 0)
       ;;    yes       no       no     (a > b)  false if (a < 0) & (b > 0)
       ;;    yes      yes       no     abs(x) > abs(y), both negative
       ;;
       ;; It follows that x is only bigger than y when x is not negative
       (FIX (not (%big-negative? x)))
       (FLO (or (fp= y -inf.0)
                (and (not (fp= y +inf.0)) (fp= y y)
                     (%> x (%flo->rat loc y) loc)))) ; Compare as ratnums
       (BIG (fx> (%big-comp-big x y) 0))
       ;; a/b > c/d  when  a*d > b*c  [with b = 1]
       (RAT (%> (%* x (rat-denominator y))
                (rat-numerator y) loc))
       (COMP (bad-complex/o loc y))
       (else (bad-number loc y)) ) )
    (RAT
     (switchq (%check-number y)
       ;; a/b > c/d  when  a*d > b*c  [generic]
       (RAT (%> (%* (rat-numerator x) (rat-denominator y))
                (%* (rat-denominator x) (rat-numerator y)) loc))
       (FLO (or (fp= y -inf.0)
                (and (not (fp= y +inf.0)) (fp= y y)
                     (%> x (%flo->rat loc y) loc)))) ; Compare as ratnums
       (COMP (bad-complex/o loc y))
       (NONE (bad-number loc y))
       ;; a/b > c/d  when  a*d > b*c  [with d = 1]
       (else (%> (rat-numerator x)
                 (%* (rat-denominator x) y) loc)) ) )
    (COMP (bad-complex/o loc x))
    (else (bad-number loc x)) ) )

(define (< x1 x2 . xs)
  (and (%< x1 x2 '<)
       (let loop ([x x2] [xs xs])
	 (or (null? xs)
	     (let ([h (##sys#slot xs 0)])
	       (and (%< x h '<)
		    (loop h (##sys#slot xs 1)) ) ) ) ) ) )

(define (%< x y loc)
  (switchq (%check-number x)
    (FIX 
     (switchq (%check-number y)
       (FIX (fx< x y))
       ;; Compare as ratnum, to prevent overflows
       (FLO (or (fp= y +inf.0)
                (and (not (fp= y -inf.0)) (fp= y y)
                     (%< x (%flo->rat loc y) loc))))
       ;;   x neg?   y neg?   x < y?   reason
       ;;  ---------------------------------------------------------------
       ;;     no       no      yes     abs(x) < abs(y), both positive
       ;;     no      yes       no     (a < b)  false if  (a > 0) & (b < 0)
       ;;    yes       no      yes     (a < b)  true if (a < 0) & (b > 0)
       ;;    yes      yes       no     abs(x) < abs(y), both negative
       ;;
       ;; It follows that x is only smaller than y when y is not negative
       (BIG (not (%big-negative? y)))
       ;; a/b < c/d  when  a*d < b*c  [with b = 1]
       (RAT (%< (%* x (rat-denominator y))
                (rat-numerator y) loc))
       (COMP (bad-complex/o loc y))
       (else (bad-number loc y)) ) )
    (FLO
     (switchq (%check-number y)
       (FLO (fp< x y))
       (COMP (bad-complex/o loc y))
       (NONE (bad-number loc y))
       ;; Compare as ratnums, to avoid errors when overflowing
       ;; (this can happen for bignums, but also for fixnums on 64-bit)
       (else (or (fp= x -inf.0)
                (and (not (fp= x +inf.0)) (fp= x x)
                     (%< (%flo->rat loc x) y loc))))) )
    (BIG 
     (switchq (%check-number y)
       ;;   x neg?   y neg?   x < y?   reason
       ;;  ---------------------------------------------------------------
       ;;     no       no       no     abs(y) < abs(x), both positive
       ;;     no      yes       no     (a < b)  false if  (a > 0) & (b < 0)
       ;;    yes       no      yes     (a < b)  true if (a < 0) & (b > 0)
       ;;    yes      yes      yes     abs(y) < abs(x), both negative
       ;;
       ;; It follows that x is only smaller than y when x is negative
       (FIX (%big-negative? x))
       (FLO (or (fp= y +inf.0)
                (and (not (fp= y -inf.0)) (fp= y y)
                     (%< x (%flo->rat loc y) loc)))) ; Compare as ratnums
       (BIG (fx< (%big-comp-big x y) 0))
       ;; a/b < c/d  when  a*d < b*c  [with b = 1]
       (RAT (%< (%* x (rat-denominator y))
                (rat-numerator y) loc))
       (COMP (bad-complex/o loc y))
       (else (bad-number loc y)) ) )
    (RAT
     (switchq (%check-number y)
       ;; a/b < c/d  when  a*d < b*c  [generic]
       (RAT (%< (%* (rat-numerator x) (rat-denominator y))
                (%* (rat-denominator x) (rat-numerator y)) loc))
       (COMP (bad-complex/o loc y))
       (FLO (or (fp= y +inf.0)
                (and (not (fp= y -inf.0)) (fp= y y)
                     (%< x (%flo->rat loc y) loc)))) ; Compare as ratnums
       (NONE (bad-number loc y))
       ;; a/b < c/d  when  a*d < b*c  [with d = 1]
       (else (%< (rat-numerator x)
                 (%* (rat-denominator x) y) loc)) ) )
    (COMP (bad-complex/o loc x))
    (else (bad-number loc x)) ) )

(define (>= x1 x2 . xs)
  (and (not (nan? x1)) (not (nan? x2))
       (not (%< x1 x2 '>=))
       (let loop ([x x2] [xs xs])
	 (or (null? xs)
	     (let ([h (##sys#slot xs 0)])
	       (and (not (nan? h))
                    (not (%< x h '>=))
		    (loop h (##sys#slot xs 1)) ) ) ) ) ) )

(define (<= x1 x2 . xs)
  (and (not (nan? x1)) (not (nan? x2))
       (not (%> x1 x2 '<=))
       (let loop ([x x2] [xs xs])
	 (or (null? xs)
	     (let ([h (##sys#slot xs 0)])
	       (and (not (nan? h))
                    (not (%> x h '<=))
		    (loop h (##sys#slot xs 1)) ) ) ) ) ) )


;;; Complex numbers

(define (make-complex r i)
  (if (or (eq? i 0) (and (%flonum? i) (fp= i 0.0)))
      r
      (%make-complex (if (inexact? i) (exact->inexact r) r)
                     (if (inexact? r) (exact->inexact i) i)) ) )

(define (make-rectangular r i)
  (switchq (%check-number r)
    (COMP (bad-real 'make-rectangular r))
    (NONE (bad-number 'make-rectangular r)) )
  (switchq (%check-number i)
    (COMP (bad-real 'make-rectangular i))
    (NONE (bad-number 'make-rectangular i)) )
  (make-complex r i) )

(define (%make-polar r phi)
  (switchq (%check-number r)
    (COMP (bad-real 'make-polar r))
    (NONE (bad-number 'make-polar r)) )
  (switchq (%check-number phi)
    (COMP (bad-real 'make-polar phi))
    (NONE (bad-number 'make-polar phi)) )
  (let ((fphi (exact->inexact phi)))
    (make-complex (%* r (##core#inline_allocate ("C_a_i_cos" 4) fphi))
                  (%* r (##core#inline_allocate ("C_a_i_sin" 4) fphi)))))

(define make-polar %make-polar)

(define (real-part x)
  (switchq (%check-number x)
    (COMP (complex-real x))
    (NONE (bad-number 'real-part x))
    (else x) ) )

(define (imag-part x)
  (switchq (%check-number x)
    (NONE (bad-number 'imag-part x))
    (COMP (complex-imag x))
    (FLO 0.0)
    (else 0) ) )

(define (%magnitude x)
  (switchq (%check-number x)
    (COMP (let ((r (complex-real x))
                (i (complex-imag x)) )
            (%sqrt 'magnitude (%+ (%* r r) (%* i i))) ) )
    (NONE (bad-number 'magnitude x))
    (else (%abs x)) ) )

(define magnitude %magnitude)

(define (%angle x)
  (switchq (%check-number x)
    (NONE (bad-number 'angle x))
    (COMP (##core#inline_allocate ("C_a_i_atan2" 4)
                                  (%exact->inexact (complex-imag x))
                                  (%exact->inexact (complex-real x))))
    (else (##core#inline_allocate ("C_a_i_atan2" 4) 0.0 (%exact->inexact x))) ) )

(define angle %angle)


;;; Rationals

(define (ratnum m n)
  (cond
   ((eq? n 1) m)
   ((eq? n -1) (- m))
   ((negative? n) (%make-rat (- m) (- n)))
   (else (%make-rat m n))))

;; Knuth, 4.5.1
(define (rat+/- loc op x y)
  (let ((a (rat-numerator x)) (b (rat-denominator x))
        (c (rat-numerator y)) (d (rat-denominator y)))
    (let ((g1 (%gcd-0 loc b d)))
      (cond
       ((eq? g1 1) (%make-rat (op (%* a d) (%* b c)) (%* b d)))
       ;; Save a quotient and multiplication if the gcd is equal
       ;; to one of the denominators since quotient of b or d and g1 = 1
       ;; TODO: Check properties of the gcd to see if g2 and t are needed
       ((%= g1 b) (let* ((t (op (%* a (%quotient loc d g1)) c))
                         (g2 (%gcd-0 loc t g1)))
                    (ratnum (%quotient loc t g2) (%quotient loc d g2))))
       ((%= g1 d) (let* ((b/g1 (%quotient loc b g1))
                         (t (op a (%* c b/g1))) ;; Is this worth it?
                         (g2 (%gcd-0 loc t g1)))
                    (ratnum (%quotient loc t g2)
                            (%* b/g1 (%quotient loc d g2)))))
       (else (let* ((b/g1 (%quotient loc b g1))
                    (t (op (%* a (%quotient loc d g1))
                           (%* c b/g1)))
                    (g2 (%gcd-0 loc t g1)))
               (%make-rat (%quotient loc t g2)
                          (%* b/g1 (%quotient loc d g2)))))))))

(define (numerator x)
  (switchq (%check-number x)
    (FIX x)
    (FLO (if (%flo-integer? x) x (bad-ratnum 'numerator x)) )
    (BIG x)
    (RAT (rat-numerator x))
    (COMP (bad-ratnum 'numerator x))
    (else (bad-number 'numerator x)) ) )

(define (denominator x)
  (switchq (%check-number x)
    (FIX 1)
    (FLO (if (%flo-integer? x) 1 (bad-ratnum 'denominator x)) )
    (BIG 1)
    (RAT (rat-denominator x))
    (COMP (bad-ratnum 'denominator x))
    (else (bad-number 'denominator x)) ) )


;;; Enhanced versions of other standard procedures

(define (%abs x)
  (switchq (%check-number x)
    (FIX (if (fx< x 0) (%fix-neg x) x))
    (FLO (##core#inline_allocate ("C_a_i_abs" 4) x))
    (BIG (%big-abs x))
    (RAT (%make-rat (%abs (rat-numerator x)) (rat-denominator x)))
    (COMP (##sys#signal-hook #:type-error 'abs "can not compute absolute value of complex number" x))
    (NONE (bad-number 'abs x)) ) )

(define abs %abs)

(define (number? x)
  (switchq (%check-number x)
    (NONE #f)
    (else #t) ) )

(set! ##sys#number? number?)
(define complex? number?)

(define (real? x)
  (switchq (%check-number x)
    (COMP #f)
    (NONE #f)
    (else #t) ) )

(define (rational? x) (and (real? x) (finite? x)))

(define (%integer? x)
  (switchq (%check-number x)
    (FIX #t)
    (FLO (%flo-integer? x))
    (BIG #t)
    (else #f) ) )

(set! ##sys#integer? %integer?)
(define integer? %integer?)

(define (exact-integer? x)
  (switchq (%check-number x)
    (FIX #t)
    (BIG #t)
    (else #f) ) )

(define (%exact? x)
  (switchq (%check-number x)
    (FLO #f)
    (COMP (and (%exact? (complex-real x)) (%exact? (complex-imag x))))
    (NONE (bad-number 'exact? x))
    (else #t) ) )

(define exact? %exact?)
(define ##sys#exact? %exact?)

(define (%inexact? x)
  (switchq (%check-number x)
    (FLO #t)
    (COMP (and (%inexact? (complex-real x)) (%inexact? (complex-imag x))))
    (NONE (bad-number 'inexact? x))
    (else #f) ) )

(define inexact? %inexact?)
(define ##sys#inexact? %inexact?)

(define (positive? x) (%> x 0 'positive?))
(define (negative? x) (%< x 0 'negative?))

(define (%zero? x)
  (switchq (%check-number x)
    (FIX (eq? x 0))
    (FLO (fp= x 0.0))
    (NONE (bad-number 'zero? x))
    (else #f) ) )

(define zero? %zero?)

(define (odd? x)
  (switchq (%check-number x)
    (FIX (##core#inline "C_i_oddp" x))
    (FLO (##core#inline "C_i_oddp" x))
    (BIG (%big-odd? x))
    (else (bad-integer 'odd? x)) ) )

(define (even? x)
  (switchq (%check-number x)
    (FIX (##core#inline "C_i_evenp" x))
    (FLO (##core#inline "C_i_evenp" x))
    (BIG (not (%big-odd? x)))
    (else (bad-integer 'even? x)) ) )

(define (max x1 . xs)
  (let ((i (%flonum? x1)))
    (let loop ((m x1) (xs xs))
      (if (null? xs)
	  (if i (%exact->inexact m) m)
	  (let ((h (##sys#slot xs 0)))
	    (switchq (%check-number h)
	      (FLO (set! i #t))
	      (COMP (bad-complex/o 'max h)) )
	    (loop (if (%> h m 'max) h m) (##sys#slot xs 1)) ) ) ) ) )

(define (min x1 . xs)
  (let ((i (%flonum? x1)))
    (let loop ((m x1) (xs xs))
      (if (null? xs)
	  (if i (%exact->inexact m) m)
	  (let ((h (##sys#slot xs 0)))
	    (switchq (%check-number h)
	      (FLO (set! i #t))
	      (COMP (bad-complex/o 'min h)) )
	    (loop (if (%< h m 'min) h m) (##sys#slot xs 1)) ) ) ) ) )

(define (%quotient loc x y)
  (switchq (%check-number x)
    (FIX (switchq (%check-number y)
           ;; fix-quotient-big always returns 0 since abs(x) < abs(y)
           ;; But take care of MOST_NEGATIVE_FIXNUM (grrr!)
           (BIG (if (bignum? (- x)) -1 0))
           (RAT (bad-integer loc y)) ; Perhaps convert to flonum?
           (NONE (bad-number loc y))
           (else (%quotient-0 x y))))
    (BIG (switchq (%check-number y)
           (FIX (%big-quotient-fix x (fix-div/0 x y loc)))
           (BIG (%big-quotient-big x y))
           (FLO (if (not (%flo-integer? y))
                    (%quotient-0 (%big->flo x) y) ; Could overflow
                    (%exact->inexact
                     (%quotient loc x (%flo->integer y)))))
           (NONE (bad-number loc y))
           (else (bad-integer loc y))))
    (FLO (switchq (%check-number y)
           (BIG (if (%flo-integer? x)
                    (%exact->inexact (%quotient loc (%flo->integer x) y))
                    (%quotient-0 x (%big->flo y)))) ; Will probably overflow
           (RAT (bad-integer loc y))
           (NONE (bad-number loc y))
           (else (%quotient-0 x y))))
    (NONE (bad-number loc x))
    (else (bad-integer loc x))))

(define (quotient x y) (%quotient 'quotient x y))

(define (%remainder loc x y)
  (switchq (%check-number x)
    [FIX (switchq (%check-number y)
           [FIX (fx- x (fx* (fx/ x y) y))]
           [FLO (let ((flx (%fix->flo x)))
                  (if (%flo-integer? y)
                      (fp- flx (fp* (##sys#truncate (fp/ flx y)) y))
                      (bad-integer loc y)))]
           ;; If abs(x) < abs(y), then remainder is always just x
           ;; But again, take care of MOST_NEGATIVE_FIXNUM
           [BIG (if (bignum? (- x)) 0 x)]
           [else (bad-integer loc y)])]
    [FLO (unless (%flo-integer? x)
           (bad-integer loc x))
         (switchq (%check-number y)
           [FLO (if (%flo-integer? y)
                    (fp- x (fp* (##sys#truncate (fp/ x y)) y))
                    (bad-integer loc y))]
           [FIX (let ((fly (%fix->flo (fix-div/0 x y loc))))
                  (fp- x (fp* (##sys#truncate (fp/ x fly)) fly)))]
           [BIG (%exact->inexact (%remainder loc (%flo->integer x) y))]
           [else (bad-integer loc y)])]
    [BIG (switchq (%check-number y)
           [FIX (%big-remainder-fix x (fix-div/0 x y loc))]
           [FLO (if (%flo-integer? y)
                    (%exact->inexact (%remainder loc x (%flo->integer y)))
                    (bad-integer loc y))]
           [BIG (%big-remainder-big x y)]
           [else (bad-integer loc y)])]
    [else (bad-integer loc x)]) )

(define (remainder x y) (%remainder 'remainder x y))

;; Modulo's sign follows y  (whereas remainder's sign follows x)
(define (modulo x y)
   (let ((r (%remainder 'modulo x y)))
      (if (%> y 0 'modulo)
	  (if (%< r 0 'modulo)
	      (%+ r y)
	      r)
	  (if (%> r 0 'modulo)
	      (%+ r y)
	      r))))

(define (quotient&remainder x y)
  (switchq (%check-number x)
    [FIX (switchq (%check-number y)
           [FIX (values (fx/ x y) (%remainder 'quotient&remainder x y))]
           [FLO (values (quotient x y) (%remainder 'quotient&remainder x y))]
           ;; If abs(x) < abs(y), then remainder is always just x
           ;; But again, take care of MOST_NEGATIVE_FIXNUM
           [BIG (if (bignum? (- x)) (values -1 0) (values 0 x))]
           [else (bad-integer 'quotient&remainder y)])]
    [FLO (switchq (%check-number y)
           [FLO (values (%quotient 'quotient&remainder x y)
                        (%remainder 'quotient&remainder x y))]
           [FIX (values (%quotient 'quotient&remainder x y)
                        (%remainder 'quotient&remainder x y))]
           [BIG (if (%flo-integer? x)
                    (receive (div rem)
                      (quotient&remainder (%flo->integer x) y)
                      (values (%exact->inexact div) (%exact->inexact rem)))
                    (bad-integer 'quotient&remainder x))]
           [else (bad-integer 'quotient&remainder y)])]
    [BIG (switchq (%check-number y)
           [FIX (%big-divrem-fix x (fix-div/0 x y 'quotient&remainder))]
           [FLO (if (%flo-integer? y)
                    (receive (div rem)
                      (quotient&remainder x (%flo->integer y))
                      (values (%exact->inexact div) (%exact->inexact rem)))
                    (bad-integer 'quotient&remainder y))]
           [BIG (%big-divrem-big x y)]
           [else (bad-integer 'quotient&remainder y)])]
    [else (bad-integer 'quotient&remainder x)]))

;; Modulo's sign follows y  (whereas remainder's sign follows x)
(define (quotient&modulo x y)
   (receive (div rem)
     (quotient&remainder x y)
     (if (%> y 0 'modulo)
         (if (%< rem 0 'modulo)
             (values div (%+ rem y))
             (values div rem))
         (if (%> rem 0 'modulo)
             (values div (%+ rem y))
             (values div rem)))))

;; Try to multiply by two until we reach an integer
(define (%float-fraction-length x)
  (do ((x x (fp* x 2.0))
       (i 0 (+ i 1)))
      ((%flo-integer? x) i)
    ;; 3000 -> %float-precision?
    (if (> i 3000) (error "I'm bored." x))))

(define (%flo->rat loc x)
  (define (deliver y d)
    (let ((q (expt 2 (%float-fraction-length y))))
      (if (%exact? q) ; XXX Ever untrue? float-fraction-length returns natnums
          (let ((e (%/ (%/ (%flo->integer
                            (%* y (%exact->inexact q)))
                           q)
                       d)))
             (if (%exact? e)
                e
                (bad-inexact loc x)))
          (bad-inexact loc x))))
  (if (and (< x (%fix->flo 1))    ; watch out for denormalized numbers
           (> x (%fix->flo -1)))
      (deliver (%* x (expt (%fix->flo 2) flonum-precision))
               ;; Can be bignum (is on 32-bit), so must wait until after init.
               ;; We shouldn't need to calculate this every single time, tho..
               (expt 2 flonum-precision))
      (deliver x 1)))

(define (%inexact->exact x)
  (switchq (%check-number x)
    (FIX x)
    (FLO (cond
          ((not (finite? x)) (bad-inexact 'inexact->exact x))
          ((%flo-integer? x) (%flo->integer x))
          (else (%flo->rat 'inexact->exact x))))
    (BIG x)
    (RAT x)
    (COMP (make-complex (%inexact->exact (complex-real x))
                        (%inexact->exact (complex-imag x))))
    (NONE (bad-number 'inexact->exact x)) ) )

(define inexact->exact %inexact->exact)
(define ##sys#inexact->exact %inexact->exact)

;; Exponent of the lowest allowed flonum; if we get any lower we get zero.
;; In other words, this is the first (smallest) flonum after 0.
;; Equal to (expt 2.0 (- flonum-minimum-exponent flonum-precision))
(define minimum-denorm-flonum-expt (fx- flonum-minimum-exponent flonum-precision))

;; This tries to keep the numbers within representable ranges and tries
;; to drop as few significant digits as possible by bringing the two numbers
;; to within the same powers of two.  See algorithms M & N in Knuth, 4.2.1
;;
;; TODO: Use (fp/ n d) if both are finite after conversion to flonums
(define (%rat->flo x)
  (let* ((n1 (rat-numerator x))
         (an (%abs n1))
         (d1 (rat-denominator x))
         ;; Approximate distance between the numbers in powers of 2
         ;; ie,  2^e-1 < n/d < 2^e+1  (e is the *un*biased value of e_w in M2)
         ;; XXX: What if b != 2 (ie, flonum-radix is not 2)
         (e (fx- (integer-length an) (integer-length d1)))
         (rnd (lambda (n d e)           ; Here, 1 <= n/d < 2  (normalized) [N5]
                ;; Cannot shift above the available precision, and can't have
                ;; an exponent that's below the minimum flonum exponent.
                (let* ((s (min (fx- flonum-precision 1)
                               (fx- e minimum-denorm-flonum-expt)))
                       (normalized (%/ (arithmetic-shift n s) d))
                       (r (round normalized))
                       (fraction (%exact->inexact r))
                       (exp (fx- e s)))
                  (let ((res (fp* fraction (expt 2.0 exp))))
                    (if (negative? n1) (%- 0 res) res)))))
         (scale (lambda (n d)                      ; Here, 1/2 <= n/d < 2   [N3]
                  (if (%< n d 'exact->inexact)     ; n/d < 1?
                      ;; Scale left [N3]; only needed once (see note in M3)
                      (rnd (arithmetic-shift n 1) d (fx- e 1))
                      ;; Already normalized
                      (rnd n d e)))))
    ;; After this step, which shifts the smaller number to align with the
    ;; larger, "f" in algorithm N is represented in the procedures above by n/d.
    (if (negative? e)
        (scale (arithmetic-shift an (%- 0 e)) d1)
        (scale an (arithmetic-shift d1 e)))))

(define (%exact->inexact x)
  (switchq (%check-number x)
    (FIX (%fix->flo x))
    (FLO x)
    (BIG (%big->flo x))
    (RAT (%rat->flo x))
    (COMP (make-complex (%exact->inexact (complex-real x)) (%exact->inexact (complex-imag x))))
    (NONE (bad-number 'exact->inexact x)) ) )

(define exact->inexact %exact->inexact)
(define ##sys#exact->inexact %exact->inexact)

(define (%gcd-0 loc x y)
  (switchq (%check-number x)
    [FIX (switchq (%check-number y)
           [FIX (fxgcd x y)]
           [FLO (if (%flo-integer? y)
                    (fpgcd (%fix->flo x) y)
                    (bad-integer loc y))]
           [BIG (if (eq? x 0) y (fxgcd x (%remainder loc y x)))]
           [else (bad-integer loc y)])]
    [FLO (switchq (%check-number y)
           [FIX (if (%flo-integer? x)
                    (fpgcd x (%fix->flo y))
                    (bad-integer loc x))]
           [FLO (if (%flo-integer? x)
                    (if (%flo-integer? y)
                        (fpgcd x y)
                        (bad-integer loc x))
                    (bad-integer loc x))]
           [BIG (if (fp= x 0.0) y (fpgcd x (%remainder loc y x)))]
           [else (bad-integer loc y)])]
    [BIG (switchq (%check-number y)
           [FIX (if (eq? y 0) x (fxgcd y (%remainder loc x y)))]
           [FLO (if (fp= y 0.0) x (fpgcd y (%remainder loc x y)))]
           [BIG (biggcd x y)]
           [else (bad-integer loc y)])]
    [else (bad-integer loc x)]) )

(define (gcd . ns)
  (if (eq? ns '())
      0
      (let loop ([ns ns] [f #t])
	(let ([head (##sys#slot ns 0)]
	      [next (##sys#slot ns 1)] )
	  (if (null? next)
	      (if f (%abs (%->integer 'gcd head)) (%abs head))
	      (let ([n2 (##sys#slot next 0)])
		(loop (cons (%gcd-0 'gcd head n2) (##sys#slot next 1)) #f) ) ) ) ) ) )

(define (%lcm-0 loc x y)
  (%quotient loc (%* x y) (%gcd-0 loc x y)) )

(define (lcm . ns)
  (if (null? ns)
      1
      (let loop ([ns ns] [f #t])
	(let ([head (##sys#slot ns 0)]
	      [next (##sys#slot ns 1)] )
	  (if (null? next)
	      (if f (%abs (%->integer 'lcm head)) (%abs head))
	      (let ([n2 (##sys#slot next 0)])
		(loop (cons (%lcm-0 'lcm head (##sys#slot next 0)) (##sys#slot next 1)) #f) ) ) ) ) ) )

(define (%floor x)
  (switchq (%check-number x)
    (FIX x)
    (FLO (##sys#floor x))
    (BIG x)
    ;; (floor x) = greatest integer <= x
    (RAT (let* ((n (rat-numerator x))
                (q (quotient n (rat-denominator x))))
           (if (>= n 0)
               q
               (%- q 1))))
    (else (bad-real 'floor x))) )

(define floor %floor)

(define (ceiling x)
  (switchq (%check-number x)
    (FIX x)
    (FLO (##sys#ceiling x))
    (BIG x)
    ;; (ceiling x) = smallest integer >= x
    (RAT (let* ((n (rat-numerator x))
                (q (quotient n (rat-denominator x))))
           (if (>= n 0)
               (%+ q 1)
               q)))
    (else (bad-real 'ceiling x))) )

(define (truncate x)
  (switchq (%check-number x)
    (FIX x)
    (FLO (##sys#truncate x))
    (BIG x)
    ;; (rational-truncate x) = integer of largest magnitude <= (abs x)
    (RAT (%quotient 'truncate (rat-numerator x) (rat-denominator x)))
    (else (bad-real 'truncate x))) )

(define (round x)
  (switchq (%check-number x)
    (FIX x)
    (FLO (##core#inline_allocate ("C_a_i_flonum_round_proper" 4) x))
    (BIG x)
    (RAT (let* ((x+1/2 (%+ x (%make-rat 1 2)))
                (r (%floor x+1/2)))
           (if (and (%= r x+1/2)
                    (odd? r))
               (%- r 1)
               r)))
    (else (bad-real 'round x)) ) )

(define (find-ratio-between x y)
  (define (sr x y)
    (let ((fx (%inexact->exact (%floor x))) 
	  (fy (%inexact->exact (%floor y))))
      (cond ((not (%< fx x 'rationalize)) (list fx 1))
	    ((%= fx fy) 
	     (let ((rat (sr (%/ 1 (%- y fy)) (%/ 1 (%- x fx)))))
	       (list (%+ (cadr rat) (%* fx (car rat))) (car rat))))
	    (else (list (%+ 1 fx) 1)))))
  (cond ((%< y x 'rationalize)
	 (find-ratio-between y x))
	((not (%< x y 'rationalize))
	 (list x 1))
	((%> x 0 'rationalize)
	 (sr x y))
	((%< y 0 'rationalize) 
	 (let ((rat (sr (%- 0 y) (%- 0 x))))
	   (list (%- 0 (car rat)) (cadr rat))))
	(else '(0 1))))

(define (find-ratio x e) (find-ratio-between (%- x e) (%+ x e)))

(define (rationalize x e)
  (let ((result (apply %/ (find-ratio x e))))
    (if (or (inexact? x) (inexact? e))
        (exact->inexact result)
        result)))

(define (%exp n)
  (switchq (%check-number n)
    (NONE (bad-number 'exp n))
    (COMP (%* (##core#inline_allocate ("C_a_i_exp" 4) (complex-real n))
	      (let ((p (complex-imag n)))
		(make-complex
		 (##core#inline_allocate ("C_a_i_cos" 4) p)
		 (##core#inline_allocate ("C_a_i_sin" 4) p) ) ) ) )
    (else (##core#inline_allocate ("C_a_i_exp" 4) (%exact->inexact n)) ) ))

(define exp %exp)

(define (%log x)
  (let ((type (%check-number x)))
    (cond
     ;; avoid calling inexact->exact on X here (to avoid overflow?)
     ((or (eq? type COMP) (%< x 0.0 'log)) ; General case
      (%+ (%log (%magnitude x)) (* (make-complex 0 1) (%angle x))))
     ((eq? x 0)                        ; Exact zero?  That's undefined
      (log0 'log x))
     ((eq? type NONE)
      (bad-number 'log x))
     (else                             ; Simple real number case
      (##core#inline_allocate ("C_a_i_log" 4) (%exact->inexact x))))))

(define log %log)

(define %i (%make-complex 0 1))
(define %-i (%make-complex 0 -1))
(define %i2 (%make-complex 0 2))

(define (%sin n)
  (switchq (%check-number n)
    (NONE (bad-number 'sin n))
    (COMP (let ((in (%* %i n)))
	    (%/ (%- (%exp in) (%exp (%- 0 in))) %i2)))
    (else (##core#inline_allocate ("C_a_i_sin" 4) (%exact->inexact n)) ) ))

(define sin %sin)

(define (%cos n)
  (switchq (%check-number n)
    (NONE (bad-number 'cos n))
    (COMP (let ((in (%* %i n)))
	    (%/ (%+ (%exp in) (%exp (%- 0 in))) 2) ) )
    (else (##core#inline_allocate ("C_a_i_cos" 4) (%exact->inexact n)) ) ) )

(define cos %cos)

(define (tan n)
  (switchq (%check-number n)
    (NONE (bad-number 'tan n))
    (COMP (%/ (%sin n) (%cos n)))
    (else (##core#inline_allocate ("C_a_i_tan" 4) (%exact->inexact n)) ) ))

;; General case: sin^{-1}(z) = -i\ln(iz + \sqrt{1-z^2})
(define (%asin n)
  (let ((t (%check-number n)))
    (cond ((eq? t NONE) (bad-number 'asin n))
          ((and (eq? t FLO) (fp>= n -1.0) (fp<= n 1.0))
           (##core#inline_allocate ("C_a_i_asin" 4) n))
          ((and (eq? t FIX) (fx>= n -1) (fx<= n 1))
           (##core#inline_allocate ("C_a_i_asin" 4) (%fix->flo n)))
          ;; General definition can return compnums
          (else (%* %-i (%log (%+ (%* %i n) (%sqrt 'asin (%- 1 (%* n n))))))))))

(define asin %asin)

;; General case:
;; cos^{-1}(z) = 1/2\pi + i\ln(iz + \sqrt{1-z^2}) = 1/2\pi - sin^{-1}(z) = sin(1) - sin(z)
(define %acos
  (let ((asin1 (##core#inline_allocate ("C_a_i_asin" 4) 1)))
    (lambda (n)
      (let ((t (%check-number n)))
        (cond ((eq? t NONE) (bad-number 'acos n))
              ((and (eq? t FLO) (fp>= n -1.0) (fp<= n 1.0))
               (##core#inline_allocate ("C_a_i_acos" 4) n))
              ((and (eq? t FIX) (fx>= n -1) (fx<= n 1))
               (##core#inline_allocate ("C_a_i_acos" 4) (%fix->flo n)))
              ;; General definition can return compnums
              (else (%- asin1 (%asin n)))) ) ) ) )

(define acos %acos)

(define (atan n #!optional b)
  (switchq (%check-number n)
    (NONE (bad-number 'atan n))
    (COMP (if b
	      (bad-real 'atan n)
	      (let ((in (%* %i n)))
		(%/ (%- (%log (%+ 1 in)) (%log (%- 1 in))) %i2) ) ) )
    (else (if b
              (##core#inline_allocate ("C_a_i_atan2" 4) (%exact->inexact n) (%exact->inexact b))
              (##core#inline_allocate ("C_a_i_atan" 4) (%exact->inexact n))))) )

(define (%exact-integer-sqrt loc k)
  (if (or (eq? 0 k) (eq? 1 k))
      (values k 0)
      ;; Hacker's Delight, figure 11-1 (Newton's method - see also SICP 1.1.7)
      (let* ((len (integer-length k))
             (g0 (arithmetic-shift 1 len)))
        (let lp ((g0 g0)
                 (g1 (arithmetic-shift
                      (%+ g0 (arithmetic-shift k (fxneg len))) -1)))
          (if (%< g1 g0 loc)
              (lp g1 (arithmetic-shift (%+ g1 (quotient k g1)) -1))
              (values g0 (%- k (%* g0 g0))))))))

(define (exact-integer-sqrt x)
  (switchq (%check-number x)
    (NONE (bad-number 'exact-integer-sqrt x))
    (FIX (if (fx< x 0)
             (bad-natural 'exact-integer-sqrt x)
             (%exact-integer-sqrt 'exact-integer-sqrt x)))
    (BIG (if (%big-negative? x)
             (bad-natural 'exact-integer-sqrt x)
             (%exact-integer-sqrt 'exact-integer-sqrt x)))
    (else (bad-natural 'exact-integer-sqrt x))))

(define (%sqrt loc n)
  (switchq (%check-number n)
    (NONE (bad-number 'sqrt n))
    (COMP (let ((p (%/ (%angle n) 2))
		(m (##core#inline_allocate ("C_a_i_sqrt" 4) (%magnitude n))) )
	    (make-complex (%* m (%cos p)) (%* m (%sin p)) ) ) )
    (RAT (let ((num (rat-numerator n))
               (den (rat-denominator n)))
           (if (and (>= num 0) (>= den 0))
               (receive (ns^2 nr)
                 (%exact-integer-sqrt loc num)
                 (if (eq? nr 0)
                     (receive (ds^2 dr)
                       (%exact-integer-sqrt loc den)
                       (if (eq? dr 0)
                           (%/ ns^2 ds^2)
                           (%sqrt loc (%exact->inexact n))))
                     (%sqrt loc (%exact->inexact n))))
               (%sqrt loc (%exact->inexact n)))))
    (else
     (cond
      ((negative? n)
       (make-complex
        0.0
        (##core#inline_allocate ("C_a_i_sqrt" 4) (%exact->inexact (- n)))))
      ((integer? n)
       (receive (s^2 r)
         (%exact-integer-sqrt loc (%->integer loc n))
         (if (eq? 0 r)
             (if (exact? n) s^2 (%exact->inexact s^2))
             (##core#inline_allocate ("C_a_i_sqrt" 4) (%exact->inexact n)))))
      (else (##core#inline_allocate ("C_a_i_sqrt" 4) (%exact->inexact n))) ) )))

(define (sqrt x) (%sqrt 'sqrt x))

(define (square x) (%* x x))

;; Generalized Newton's algorithm for positive integers, with a little help
;; from Wikipedia ;)  https://en.wikipedia.org/wiki/Nth_root_algorithm
(define (%exact-integer-nth-root loc k n)
  (if (or (eq? 0 k) (eq? 1 k) (eq? 1 n))       ; Maybe call exact-integer-sqrt on n=2?
      (values k 0)
      (let ((len (integer-length k)))
        (if (fx< len n)        ; Idea from Gambit: 2^{len-1} <= k < 2^{len}
            (values 1 (- k 1)) ; Since we know x >= 2, we know x^{n} can't exist
            (let ((g0 (arithmetic-shift 1 len))
                  (n-1 (%- n 1)))
              (let lp ((g0 g0)
                       (g1 (%quotient loc (%+ (%* n-1 g0) (%quotient loc k (%integer-power g0 n-1))) n)))
                (if (%< g1 g0 loc)
                    (lp g1 (%quotient loc (%+ (%* n-1 g1) (%quotient loc k (%integer-power g1 n-1))) n))
                    (values g0 (%- k (%integer-power g0 n))))))))))

(define (exact-integer-nth-root k n)
  (unless (exact-integer? n)
    (bad-natural 'exact-integer-nth-root n))
  (switchq (%check-number k)
    (NONE (bad-number 'exact-integer-nth-root k))
    (FIX (if (fx< k 0)
             (bad-natural 'exact-integer-nth-root k)
             (%exact-integer-nth-root 'exact-integer-nth-root k n)))
    (BIG (if (%big-negative? k)
             (bad-natural 'exact-integer-nth-root k)
             (%exact-integer-nth-root 'exact-integer-nth-root k n)))
    (else (bad-natural 'exact-integer-nth-root k))))

(define (%integer-power base e)
  (if (negative? e)
      (%/ 1 (%integer-power base (- e)))
      (let lp ((res 1) (e2 e))
        (cond
          ((eq? e2 0) res)
          ((even? e2) ; recursion is faster than iteration here
           (%* res (square (lp 1 (arithmetic-shift e2 -1)))))
          (else
           (lp (%* res base) (%- e2 1)))))))

(define (expt a b)
  (define (slow-expt a b)
    (if (eq? 0 a)
        (expt0 'expt a b)
        (%exp (%* b (%log a)))))
  (let ((ta (%check-number a))
	(tb (%check-number b)) )
    (cond ((eq? NONE ta) (bad-number 'expt a))
	  ((eq? NONE tb) (bad-number 'expt b))
          ((and (eq? RAT ta) (not (inexact? b)))
           ;; (n*d)^b = n^b * d^b = n^b * x^{-b}  | x = 1/b
           ;; Hopefully faster than integer-power
           (%* (expt (rat-numerator a) b) (expt (rat-denominator a) (- b))))
          ;; x^{a/b} = (x^{1/b})^a
	  ((eq? RAT tb)
           (switchq ta
             (FIX (if (fx< a 0)
                      (%expt-0 (%fix->flo a) (%rat->flo b))
                      (receive (ds^n r)
                        (%exact-integer-nth-root 'expt a (rat-denominator b))
                        (if (eq? r 0)
                            (expt ds^n (rat-numerator b))
                            (%expt-0 (%fix->flo a) (%rat->flo b))))))
             (BIG (if (%big-negative? a)
                      (%expt-0 (%big->flo a) (%rat->flo b))
                      (receive (ds^n r)
                        (%exact-integer-nth-root 'expt a (rat-denominator b))
                        (if (eq? r 0)
                            (expt ds^n (rat-numerator b))
                            (%expt-0 (%big->flo a) (%rat->flo b))))))
             (FLO (%expt-0 a (%rat->flo b)))
             (else (slow-expt a b))))
	  ((or (eq? COMP tb) (and (eq? COMP ta) (not (integer? b))))
           (slow-expt a b))
          ((or (eq? FLO ta) (and (eq? FLO tb) (not (%flo-integer? b))))
           (%expt-0 (%exact->inexact a) (%exact->inexact b)))
	  ;; this doesn't work that well, yet...
          ;; (XXX: What does this mean? why not? I do know this is ugly... :P)
	  (else (if (or (inexact? a) (inexact? b))
                    (%exact->inexact (%integer-power a b))
                    (%integer-power a b))) ) ) )

(define (conj n)
  (switchq (%check-number n)
    (NONE (bad-number 'conj n))
    (COMP (make-complex (complex-real n) (%- 0 (complex-imag n))))
    (else n) ) )

(define (add1 n) (%+ n 1))
(define (sub1 n) (%- n 1))

(define (signum n)
  (switchq (%check-number n)
    (FIX (cond ((eq? 0 n) 0)
	       ((fx< n 0) -1)
	       (else 1) ) )
    (FLO (cond ((fp= n 0.0) 0.0)
	       ((fp< n 0.0) -1.0)
	       (else 1.0) ) )
    (BIG (if (%big-negative? n) -1 1)) ; Can't be 0; it would be a fixnum then
    (RAT (signum (rat-numerator n)))
    (COMP (make-polar 1 (angle n)))     ; Definition from CLHS signum
    (else (bad-number 'signum n)) ) )

(define (%->integer loc n)
  (switchq (%check-number n)
    (FIX n)
    (FLO (if (%integer? n)
             (%flo->integer n)
	     (bad-integer loc n)))
    (BIG n)
    (else (bad-integer loc n)) ) )

;; From SRFI-60 (oddly not in Chicken core)
(define (integer-length x)
  (%int-length (%->integer 'integer-length x)))

(define (bitwise-and . xs)
  (let loop ((x -1) (xs xs))
    (if (null? xs)
	x
	(let ((xi (##sys#slot xs 0)))
	  (loop
	   (%int-bitwise-int bignum_and_op x (%->integer 'bitwise-and xi))
	   (##sys#slot xs 1) ) ) ) ) )

(define (bitwise-ior . xs)
  (let loop ((x 0) (xs xs))
    (if (null? xs)
	x
	(let ((xi (##sys#slot xs 0)))
	  (loop
	   (%int-bitwise-int bignum_ior_op x (%->integer 'bitwise-ior xi))
	   (##sys#slot xs 1) ) ) ) ) )

(define (bitwise-xor . xs)
  (let loop ((x 0) (xs xs))
    (if (null? xs)
	x
	(let ((xi (##sys#slot xs 0)))
	  (loop
	   (%int-bitwise-int bignum_xor_op x (%->integer 'bitwise-xor xi))
	   (##sys#slot xs 1) ) ) ) ) )

(define (bitwise-not n)
  (%int-not (%->integer 'bitwise-not n)) )

(define (arithmetic-shift n m)
  (let ((n (%->integer 'arithmetic-shift n)))
    (switchq (%check-number m)
      (FIX (%int-shift-fix n m))
      (BIG (##sys#signal-hook #:type-error 'arithmetic-shift
                              "can not shift by bignum amounts" n m))
      (else (bad-exact 'arithmetic-shift m)))) )

(define %number->string
  (let ((string-append string-append))
    (lambda (n #!optional (base 10))
      (unless (memq base '(2 8 10 16)) (bad-base 'number->string base))
      (let numstr ((n n))
	(switchq (%check-number n)
          (FIX (number->string-0 n base))
	  (FLO (cond
                ((fp= n -inf.0) "-inf.0") ; Core does not handle these right
                ((fp= n +inf.0) "+inf.0")
                ((not (fp= n n)) "+nan.0")
                (else (number->string-0 n base))))
	  (BIG (%big->string n base))
	  (RAT (string-append (numstr (rat-numerator n))
                              "/"
                              (numstr (rat-denominator n))))
	  (COMP (let ((r (complex-real n))
		      (i (complex-imag n)) )
		  (string-append
                   (numstr r)
                   ;; The infinities and NaN always print their sign
                   (if (and (finite? i) (%> i 0 'number->string)) "+" "")
                   (numstr i) "i") ) )
	  (else (bad-number 'number->string n)) ) ) ) ) )

(define number->string %number->string)
(define ##sys#number->string %number->string) ; for printer

;; We try to prevent memory exhaustion attacks by limiting the
;; maximum exponent value.
;; TODO: Make this a parameter?  Would probably slow things down even more...
(define-constant +maximum-allowed-exponent+ 10000)

(define (%string->compnum radix str offset exactness)
  (define (go-inexact!)
    ;; Go inexact unless exact was requested (with #e prefix)
    (unless (eq? exactness 'e) (set! exactness 'i)))
  (define (safe-exponent value e)
    (and e (if (not value)
               0
               (cond
		 ((> e +maximum-allowed-exponent+)
                  (and (eq? exactness 'i)
                       (cond ((zero? value) 0.0)
			     ((> value 0.0) +inf.0)
			     (else -inf.0))))
		 ((< e (- +maximum-allowed-exponent+))
                  (and (eq? exactness 'i) +0.0))
                 (else (%* value (expt 10 e)))))))
  (let* ((len (##sys#size str))
         (r..9 (integer->char (fx+ (char->integer #\0) radix)))
         (r..a (integer->char (fx+ (char->integer #\a) (fx- radix 10))))
         (r..A (integer->char (fx+ (char->integer #\A) (fx- radix 10))))
         ;; Ugly flag which we need (note that "exactness" is mutated too!)
         ;; Since there is (almost) no backtracking we can do this.
         (seen-hashes? #f)
         ;; All these procedures return #f or an object consed onto an end
         ;; position.  If the cdr is false, that's the end of the string.
         ;; If just #f is returned, the string contains invalid number syntax.
         (scan-digits
          (lambda (start)
            (let lp ((i start))
              (if (fx= i len)
                  (and (fx> i start) (cons i #f))
                  (let ((c (%subchar str i)))
                    (if (fx<= radix 10)
                        (if (and (char>=? c #\0) (char<=? c r..9))
                            (lp (fx+ i 1))
                            (and (fx> i start) (cons i i)))
                        (if (or (and (char>=? c #\0) (char<=? c #\9))
                                (and (char>=? c #\a) (char<=? c r..a))
                                (and (char>=? c #\A) (char<=? c r..A)))
                            (lp (fx+ i 1))
                            (and (fx> i start) (cons i i)))))))))
         (scan-hashes
          (lambda (start)
            (let lp ((i start))
              (if (fx= i len)
                  (and (fx> i start) (cons i #f))
                  (let ((c (%subchar str i)))
                    (if (eq? c #\#)
                        (lp (fx+ i 1))
                        (and (fx> i start) (cons i i))))))))
         (scan-digits+hashes
          (lambda (start neg? all-hashes-ok?)
            (let* ((digits (and (not seen-hashes?) (scan-digits start)))
                   (hashes (if digits
                               (and (cdr digits) (scan-hashes (cdr digits)))
                               (and all-hashes-ok? (scan-hashes start))))
                   (end (or hashes digits)))
              (and-let* ((end)
                         (num (%digits->number str start (car end) radix neg?)))
                (when hashes            ; Eeewww. Feeling dirty yet?
                  (set! seen-hashes? #t)
                  (go-inexact!))
                (cons num (cdr end))))))
         (scan-exponent
          (lambda (start)
            (and (fx< start len)
                 (let ((sign (case (%subchar str start)
                               ((#\+) 'pos) ((#\-) 'neg) (else #f))))
                   (and-let* ((start (if sign (fx+ start 1) start))
                              (end (scan-digits start)))
                     (go-inexact!)
                     (cons (%digits->number
                            str start (car end) radix (eq? sign 'neg))
                           (cdr end)))))))
         (scan-decimal-tail
          (lambda (start neg? decimal-head)
            (and (fx< start len)
                 (let* ((tail (scan-digits+hashes start neg? decimal-head))
                        (next (if tail (cdr tail) start)))
                   (and (or decimal-head (not next)
                            (fx> next start)) ; Don't allow empty "."
                        (case (and next (%subchar str next))
                          ((#\e #\s #\f #\d #\l
                            #\E #\S #\F #\D #\L)
                           (and-let* (((fx> len next))
                                      (ee (scan-exponent (fx+ next 1)))
                                      (e (car ee))
                                      (h (safe-exponent decimal-head e)))
                             (let* ((te (and tail (fx- e (fx- (cdr tail) start))))
                                    (num (and tail (car tail)))
                                    (t (safe-exponent num te)))
                               (cons (if t (%+ h t) h) (cdr ee)))))
                          (else (let* ((last (or next len))
                                       (te (and tail (fx- start last)))
                                       (num (and tail (car tail)))
                                       (t (safe-exponent num te))
                                       (h (or decimal-head 0)))
                                  (cons (if t (%+ h t) h) next)))))))))
         (scan-ureal
          (lambda (start neg?)
            (if (and (fx> len (fx+ start 1)) (eq? radix 10)
                     (eq? (%subchar str start) #\.))
                (begin
                  (go-inexact!)
                  (scan-decimal-tail (fx+ start 1) neg? #f))
                (and-let* ((end (scan-digits+hashes start neg? #f)))
                  (case (and (cdr end) (%subchar str (cdr end)))
                    ((#\.)
                     (go-inexact!)
                     (and (eq? radix 10)
                          (if (fx> len (fx+ (cdr end) 1))
                              (scan-decimal-tail (fx+ (cdr end) 1) neg? (car end))
                              (cons (car end) #f))))
                    ((#\e #\s #\f #\d #\l
                      #\E #\S #\F #\D #\L)
                     (and-let* (((eq? radix 10))
                                ((fx> len (cdr end)))
                                (ee (scan-exponent (fx+ (cdr end) 1)))
                                (num (car end))
                                (val (safe-exponent num (car ee))))
                       (cons val (cdr ee))))
                    ((#\/)
                     (set! seen-hashes? #f) ; Reset flag for denominator
                     (and-let* (((fx> len (cdr end)))
                                (d (scan-digits+hashes (fx+ (cdr end) 1) #f #f))
                                (num (car end))
                                (denom (car d)))
                       (if (not (eq? denom 0))
                           (cons (%/ num denom) (cdr d))
                           ;; Hacky: keep around an inexact until we decide we
                           ;; *really* need exact values, then fail at the end.
                           (and (not (eq? exactness 'e))
                            (case (signum num)
                              ((-1) (cons -inf.0 (cdr d)))
                              ((0)  (cons +nan.0 (cdr d)))
                              ((+1) (cons +inf.0 (cdr d))))))))
                    (else end))))))
         (scan-real
          (lambda (start)
            (and (fx< start len)
                 (let* ((sign (case (%subchar str start)
                                ((#\+) 'pos) ((#\-) 'neg) (else #f)))
                        (next (if sign (fx+ start 1) start)))
                   (and (fx< next len)
                        (case (%subchar str next)
                          ((#\i #\I)
                           (or (and sign
                                    (cond
                                     ((fx= (fx+ next 1) len) ; [+-]i
                                      (cons (if (eq? sign 'neg) -1 1) next))
                                     ((and (fx<= (fx+ next 5) len)
                                           (string-ci=? (substring str next (fx+ next 5)) "inf.0"))
                                      (go-inexact!)
                                      (cons (fp/ (if (eq? sign 'neg) -1.0 1.0) 0.0)
                                            (and (fx< (fx+ next 5) len)
                                                 (fx+ next 5))))
                                     (else #f)))
                               (scan-ureal next (eq? sign 'neg))))
                          ((#\n #\N)
                           (or (and sign
                                    (fx<= (fx+ next 5) len)
                                    (string-ci=? (substring str next (fx+ next 5)) "nan.0")
                                    (begin (go-inexact!)
                                           (cons (fp/ 0.0 0.0)
                                                 (and (fx< (fx+ next 5) len)
                                                      (fx+ next 5)))))
                               (scan-ureal next (eq? sign 'neg))))
                          (else (scan-ureal next (eq? sign 'neg)))))))))
         (number (and-let* ((r1 (scan-real offset)))
                   (case (and (cdr r1) (%subchar str (cdr r1)))
                     ((#f) (car r1))
                     ((#\i #\I) (and (fx= len (fx+ (cdr r1) 1))
                                     (or (eq? (%subchar str offset) #\+) ; ugh
                                         (eq? (%subchar str offset) #\-))
                                     (make-rectangular 0 (car r1))))
                     ((#\+ #\-)
                      (set! seen-hashes? #f) ; Reset flag for imaginary part
                      (and-let* ((r2 (scan-real (cdr r1)))
                                 ((cdr r2))
                                 ((fx= len (fx+ (cdr r2) 1)))
                                 ((or (eq? (%subchar str (cdr r2)) #\i)
                                      (eq? (%subchar str (cdr r2)) #\I))))
                        (make-rectangular (car r1) (car r2))))
                     ((#\@)
                      (set! seen-hashes? #f) ; Reset flag for angle
                      (and-let* ((r2 (scan-real (fx+ (cdr r1) 1)))
                                 ((not (cdr r2))))
                        (make-polar (car r1) (car r2))))
                     (else #f)))))
    (and number (if (eq? exactness 'i)
                    (exact->inexact number)
                    ;; Ensure we didn't encounter +inf.0 or +nan.0 with #e
                    (and (finite? number) number)))))

(define (%string->number str #!optional (base 10))
  (##sys#check-string str 'string->number)
  (##sys#check-exact base 'string->number)
  (unless (< 1 base 37)           ; We only have 0-9 and the alphabet!
    (bad-base 'string->number base))
  (let scan-prefix ((i 0)
                    (exness #f)
                    (radix #f)
                    (len (##sys#size str)))
    (if (and (fx< (fx+ i 2) len) (eq? (%subchar str i) #\#))
        (case (%subchar str (fx+ i 1))
          ((#\i #\I) (and (not exness) (scan-prefix (fx+ i 2) 'i radix len)))
          ((#\e #\E) (and (not exness) (scan-prefix (fx+ i 2) 'e radix len)))
          ((#\b #\B) (and (not radix) (scan-prefix (fx+ i 2) exness 2 len)))
          ((#\o #\O) (and (not radix) (scan-prefix (fx+ i 2) exness 8 len)))
          ((#\d #\D) (and (not radix) (scan-prefix (fx+ i 2) exness 10 len)))
          ((#\x #\X) (and (not radix) (scan-prefix (fx+ i 2) exness 16 len)))
          (else #f))
        (%string->compnum (or radix base) str i exness))))

(define (randomize #!optional (seed (##sys#fudge 2)))
  (switchq (%check-number seed)
    (FIX (%fix-randomize seed))
    (BIG (%big-randomize seed))
    (else (bad-integer 'randomize seed)) ) )

(define (random n)
  (switchq (%check-number n)
    (FIX (%fix-random n))
    (BIG (%big-random n))
    (else (bad-integer 'random n)) ) )

(define string->number %string->number)

;;; Reader hook
(define (##sys#string->number str #!optional (radix 10) exactness)
  (%string->compnum radix str 0 exactness))


;;; Non-standard type procedures

(define (bignum? x) (eq? (%check-number x) BIG)) ; big number
(define (ratnum? x) (eq? (%check-number x) RAT)) ; rational number
(define (cplxnum? x) (eq? (%check-number x) COMP)) ; complex number

(define (nan? x)
  (switchq (%check-number x)
    (NONE (bad-number x 'nan?))
    (FLO (not (fp= x x)))
    (COMP (or (nan? (complex-real x)) (nan? (complex-imag x))))
    (else #f)))
(define (infinite? x)
  (switchq (%check-number x)
    (NONE (bad-number x 'infinite?))
    (FLO (or (fp= x +inf.0) (fp= x -inf.0)))
    (COMP (or (infinite? (complex-real x)) (infinite? (complex-imag x))))
    (else #f)))
(define (finite? x)
  (switchq (%check-number x)
    (NONE (bad-number x 'finite?))
    (FLO (and (fp= x x) (not (fp= x +inf.0)) (not (fp= x -inf.0))))
    (COMP (and (finite? (complex-real x)) (finite? (complex-imag x))))
    (else #t)))

(define (rectnum? x)    ; "exact" complex number
  (and (eq? (%check-number x) COMP)
       (%integer? (complex-real x))
       (%integer? (complex-imag x))))

(define (compnum? x)    ; inexact complex number
  (and (eq? (%check-number x) COMP)
       (%inexact? (complex-real x))
       (%inexact? (complex-imag x))))

(define (cintnum? x)    ; integer number
  (switchq (%check-number x)
    (FIX #t)
    (BIG #t)
    (FLO (%flo-integer? x))
    (COMP (and (%integer? (complex-real x)) (%integer? (complex-imag x))))
    (else #f) ) )

(define (cflonum? x)    ; floatingpoint number
  (switchq (%check-number x)
    (FLO #t)
    (COMP (and (%flonum? (complex-real x)) (%flonum? (complex-imag x))))
    (else #f) ) )

;;; What we provide

(register-feature! #:full-numeric-tower)

)
