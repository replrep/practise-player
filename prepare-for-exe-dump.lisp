(setq *load-verbose* nil)
(setq *compile-verbose* nil)
(setf *invoke-debugger-hook*
          (lambda (condition hook)
            (declare (ignore hook))
            (format *error-output* "Error: ~A~%" condition)
            (sb-ext:exit)))
(sb-ext:disable-debugger)

(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(require 'asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system :practise-player)

