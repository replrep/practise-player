(in-package :sndfile)

(define-foreign-library sndfile
  ((:and :unix (:not :darwin))
   (:or "libsndfile.so.1.0.28" "libsndfile.so.1" "libsndfile.so"))
  (t (:default "libsndfile")))

(unless (foreign-library-loaded-p 'sndfile)
 (use-foreign-library sndfile))


(defcfun "sf_open" :pointer
  (path :string)
  (mode :int)
  (sfinfo :pointer))

(defcfun "sf_close" :int
  (sndfile :pointer))

(defcfun "sf_read_float" sf-count-t
  (sndfile :pointer)
  (ptr :pointer)
  (items sf-count-t))

(defcfun "sf_seek" sf-count-t
  (sndfile :pointer)
  (frames sf-count-t)
  (whence :int))


(defparameter +default-samplerate+ 44100)

(defun clear-sf-info (sf-info)
  (setf (foreign-slot-value sf-info '(:struct sf-info) 'frames) 0)
  (setf (foreign-slot-value sf-info '(:struct sf-info) 'samplerate) 0)
  (setf (foreign-slot-value sf-info '(:struct sf-info) 'channels) 0)
  (setf (foreign-slot-value sf-info '(:struct sf-info) 'format) 0)
  (setf (foreign-slot-value sf-info '(:struct sf-info) 'sections) 0)
  (setf (foreign-slot-value sf-info '(:struct sf-info) 'seekable) 0))

(defun dump-sf-info (sf-info)
  (format nil "fr:~d r:~d c:~d f:~d s:~d se:~d"
          (foreign-slot-value sf-info '(:struct sf-info) 'frames)
          (foreign-slot-value sf-info '(:struct sf-info) 'samplerate)
          (foreign-slot-value sf-info '(:struct sf-info) 'channels)
          (foreign-slot-value sf-info '(:struct sf-info) 'format)
          (foreign-slot-value sf-info '(:struct sf-info) 'sections)
          (foreign-slot-value sf-info '(:struct sf-info) 'seekable)))

(defun sndfile-open (name begin-pos)
  (with-foreign-object (sf-info '(:struct sf-info))
    (clear-sf-info sf-info)
    (let ((sndfile (sf-open name (foreign-enum-value 'mode :sfm-read) sf-info)))
      (when (or
             (/= +default-samplerate+ 
                 (foreign-slot-value sf-info '(:struct sf-info) 'samplerate))
             (/= 2
                 (foreign-slot-value sf-info '(:struct sf-info) 'channels))
             (not
              (foreign-slot-value sf-info '(:struct sf-info) 'seekable)))
        (sndfile-close sndfile)
        (error "soundfile format error"))
      (sf-seek sndfile begin-pos (foreign-enum-value 'whence :sf-seek-set))
      sndfile)))

(defun sndfile-close (sndfile)
  (sf-close sndfile))

(defun get-frame-position (sndfile)
  (sf-seek sndfile 0 (foreign-enum-value 'whence :sf-seek-cur)))

(defun goto-frame-abs (sndfile frame)
  (sf-seek sndfile frame (foreign-enum-value 'whence :sf-seek-set)))

(defun goto-frame-rel (sndfile frame-offset)
  (sf-seek sndfile frame-offset (foreign-enum-value 'whence :sf-seek-cur)))

(let ((current-gap 0))
(defun provide-next-samples (sndfile
                             loop-begin loop-end gap
                             requested-items
                             callback)
  (let ((pos (get-frame-position sndfile)))
    (when (and loop-end (>= pos loop-end))
      (goto-frame-abs sndfile loop-begin)
      (setf pos loop-begin)
      (setf current-gap gap))
    ;; TODO: actually do something with the gap here...
    (let ((request-size (min requested-items
                             (if loop-end
                                 (* 2 (- loop-end pos))
                                 most-positive-fixnum))))
      (with-foreign-object (buffer :float request-size)
        (let ((actual-size (sf-read-float sndfile buffer request-size)))
          (loop for i below actual-size do
               (funcall callback (mem-aref buffer :float i)))
          actual-size))))))
