;;;; grass.scm - specification for "scheme" interpreter


; "base.scm" and the appropriate implementation-specific "*/base.scm" needs to be
; made available in addition to the produced code.

(program
 (files "lib/syntax.scm"
	"lib/match.scm"
	"lib/misc.scm"
	"dtype.scm"
	"alexpand.scm"
	"promise.scm"
	"read.scm"
	"write.scm"
	"pp.scm"
	"program.scm"
	"eval.scm"
	"env.scm"
	"top.scm"))
