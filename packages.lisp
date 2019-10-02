(in-package :cl-user)

(defpackage :jack
  (:use :cl :cffi)
  (:export
   :jack-client-open
   :jack-client-close

   :jack-port-register
   :jack-port-unregister

   :jack-nframes
   :jack-set-process-callback
   :jack-port-get-buffer

   :jack-activate
   :jack-deactivate

   :+jack-default-audio-type+
   :+jack-port-is-input+
   :+jack-port-is-output+
   :+jack-port-is-physical+
   :+jack-port-can-monitor+
   :+jack-port-is-terminal+
   ))

(defpackage :sndfile
  (:use :cl :cffi)
  (:export
   :sndfile-open
   :sndfile-close
   :get-frame-position
   :goto-frame-abs
   :goto-frame-rel
   :read-items
   ))

(defpackage :rubberband
  (:use :cl :cffi)
  (:export
   :make-rubberband
   :destroy-rubberband
   :rubberband-set-time-ratio
   :rubberband-set-pitch-scale
   :rubberband-available
   :rubberband-get-samples-required
   :rubberband-process
   :rubberband-retrieve
   ))

(defpackage :buffer
  (:use :cl :bordeaux-threads)
  (:export :make-buffer
           :perform-read-transaction
           :run-buffer-source-thread
           :stop-buffer-source-thread
           ))

(defpackage :practise-player
  (:use :cl :cffi :jack :sndfile :buffer :rubberband)
  (:export :play
           :stop
           :set-speed
           :set-pitch
           :set-volume-left
           :set-volume-right
           ))
