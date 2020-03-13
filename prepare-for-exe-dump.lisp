(load (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname)))
(require 'asdf)
(push (truename ".") asdf:*central-registry*)
(asdf:load-system :practise-player)

(sb-ext:disable-debugger)
