;;;; bootstrap.scm - compile "scheme" program using CHICKEN
;
; usage: csi -s bootstrap.scm
;    or: <GRASS> bootstrap.scm


(cond-expand
  (grass
   (load "grass/base.scm"))
  (else))

(load "lib/syntax.scm")
(load "lib/match.scm")
(load "lib/misc.scm")
(load "dtype.scm")
(load "read.scm")
(load "alexpand.scm")
(load "program.scm")
(load "lib/normalize.scm")
(load "pp.scm")

(define spec (call-with-input-file "grass.scm" %read))
(display "expanding program\n")
(define prg (expand-program spec))
(display "expanding syntax\n")

(define version (call-with-input-file ".bky/HEAD" %read))

(let ((out (open-output-file "build/core.scm")))
  (pp `(define *grass-version* ,version) out)
  (pp (car (normalize (%%expand prg))) out)
  (close-output-port out))
