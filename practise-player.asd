(in-package :cl-user)

(eval-when (:load-toplevel :execute)
  (asdf:operate 'asdf:load-op '#:cffi-grovel))

(defpackage :practise-player-asd
  (:use :cl :asdf))

(in-package :practise-player-asd)

(defsystem :practise-player
  :description "Practise music player."
  :author "Claus Brunzema <mail@cbrunzema.de>"
  :license "GPLv3"
  :version (:read-file-form "version.txt")
  :perform (load-op :after (op system)
                    (pushnew :practise-player *features*))
  :depends-on (:cffi :bordeaux-threads :unix-opts :cl-ppcre)
  :serial t
  :components ((:file "packages")
               (cffi-grovel:grovel-file "jack-grovel")
               (:file "jack-cffi")
               (cffi-grovel:grovel-file "sndfile-grovel")
               (:file "sndfile-cffi")
               (cffi-grovel:grovel-file "rubberband-grovel")
               (:file "rubberband-cffi")

               (:file "buffer")
               (:file "practise-player")
               (:file "practise-player-cli")))
