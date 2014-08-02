;;;; prefix.scm - module prefix for Racket


(#%require (only racket
		 current-error-port
		 terminal-port?
		 current-seconds
		 parameterize
		 with-handlers*
		 void
		 getenv
		 struct
		 error
		 exit
		 exn:fail? exn:break? exn-message
		 current-command-line-arguments
		 directory-exists?
		 flush-output
		 file-exists?
		 delete-file delete-directory/files))
(#%require (only racket/system system))
