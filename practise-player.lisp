(in-package :practise-player)


(defparameter +read-buffer-size+ (* 2 8192))
(defparameter +jack-client-name+ "practise-player")

(defvar *sndfile* nil)
(defvar *sndfile-rubberband-buffer* nil)
(defvar *rubberband* nil)
(defvar *rubberband-jack-buffer* nil)
(defvar *client* nil)
(defvar *output-port-l* nil)
(defvar *output-port-r* nil)


;;;------------------------------------------------------------------------
(defun make-sine-generator (frequency &optional sample-rate)
  (let* ((2pi (* 2 pi))
         (step (/ 2pi (or sample-rate 44100)))
         (x 0))
    (lambda ()
      (prog1
          (coerce (sin (* x frequency)) 'single-float)
        (incf x step)
        (when (> x 2pi)
          (decf x 2pi))))))

(defun make-stream-averager ()
  (let ((avg 0)
        (n 0))
    (lambda (x)
      (if (null x)
          avg
          (prog1
              (setq avg (/ (+ (* avg n) x) (+ n 1)))
            (incf n))))))
(defvar *jack-size-average* nil) ; TEMP
(defvar *rubberband-required-average* nil) ;TEMP


;;;------------------------------------------------------------------------
(defun drain-rubberband (rubberband consumer max-len)
  (let* ((avail (rubberband-available rubberband))
         (retrieve (min avail (floor max-len 2))))
    (if (zerop retrieve)
        0
        (with-foreign-objects ((channels :pointer 2)
                               (left :float retrieve)
                               (right :float retrieve))
          (setf (mem-aref channels :pointer 0) left)
          (setf (mem-aref channels :pointer 1) right)
          (let ((result-len (rubberband-retrieve rubberband channels retrieve)))
            (loop for i below result-len do
                 (funcall consumer (mem-aref left :float i))
                 (funcall consumer (mem-aref right :float i)))
            (* 2 result-len))))))

(defun fill-rubberband (rubberband buffer)
  (let ((samples-required (rubberband-get-samples-required rubberband)))

    (funcall *rubberband-required-average* samples-required)

    (if (zerop samples-required)
        0
        (with-foreign-objects ((channels :pointer 2)
                               (left :float samples-required)
                               (right :float samples-required))
          (let ((index 0))
            (perform-read-transaction
             buffer (* 2 samples-required)
             (lambda (val)
               (if (evenp index)
                   (setf (mem-aref left :float (floor index 2)) val)
                   (setf (mem-aref right :float (floor (- index 1) 2)) val))
               (incf index)))
            (setf (mem-aref channels :pointer 0) left)
            (setf (mem-aref channels :pointer 1) right)
            (rubberband-process rubberband channels (floor index 2) 0)
            index)))))

(defcallback jack-callback :int ((nframes jack-nframes) (arg :pointer))
  (declare (ignore arg)
           ;; (optimize (speed 3))
           )
  (let ((jack-buffer-l (jack-port-get-buffer *output-port-l* nframes))
        (jack-buffer-r (jack-port-get-buffer *output-port-r* nframes))
        (index 0))

    (funcall *jack-size-average* nframes)

    (perform-read-transaction
     *rubberband-jack-buffer*
     (* 2 nframes)
     (lambda (val)
       (if (evenp index)
           (setf (mem-aref jack-buffer-l :float (floor index 2)) val)
           (setf (mem-aref jack-buffer-r :float (floor (- index 1) 2)) val))
       (incf index)))
    (loop for i from (floor index 2) below nframes do
         (setf (mem-aref jack-buffer-l :float i) 0.0)
         (setf (mem-aref jack-buffer-r :float i) 0.0)))
  0)

(defun init ()
  (when *client*
    (return-from init))
  
  (setf *jack-size-average* (make-stream-averager))
  (setf *rubberband-required-average* (make-stream-averager))

  (setf *client* (jack-client-open +jack-client-name+
                                   (null-pointer)
                                   (null-pointer)))
  (when (null-pointer-p *client*)
    (error "Couldn't open jack client, jack server not running?"))

  (setf *output-port-l* (jack-port-register *client* "out L"
                                            +jack-default-audio-type+
                                            (+ +jack-port-is-output+
                                               +jack-port-is-terminal+)
                                            0))
  (setf *output-port-r* (jack-port-register *client* "out R"
                                            +jack-default-audio-type+
                                            (+ +jack-port-is-output+
                                               +jack-port-is-terminal+)
                                            0))

  (jack-set-process-callback *client* (callback jack-callback) (null-pointer))

  (setf *sndfile-rubberband-buffer*
        (make-buffer
         (* 2 +read-buffer-size+)
         (lambda (len consumer)
           (provide-next-samples *sndfile* len consumer))))

  (setf *rubberband* (make-rubberband))

  (setf *rubberband-jack-buffer*
        (make-buffer
         +read-buffer-size+
         (lambda (len consumer)
           (let ((count (drain-rubberband *rubberband* consumer len)))
             (if (plusp count)
                 count
                 (fill-rubberband *rubberband*
                                  *sndfile-rubberband-buffer*)))))))

(defun shutdown ()
  )

(defun play (filename &key (begin 0) (end nil) (gap 0) (speed 1.0) (pitch 1.0))
  (unless *client*
    (init))
  (setf *sndfile* (sndfile-open filename))
  (when (null-pointer-p *sndfile*)
    (error "Couldn't open soundfile ~S" filename))
  (rubberband-set-time-ratio *rubberband* (coerce speed 'double-float))
  (rubberband-set-pitch-scale *rubberband* (coerce pitch 'double-float))
  (run-buffer-source-thread *sndfile-rubberband-buffer*)
  (run-buffer-source-thread *rubberband-jack-buffer*)
  (jack-activate *client*))

(defun stop ()
  (unless (null-pointer-p *client*)
    (jack-deactivate *client*)
    (jack-client-close *client*)
    (setf *client* nil))
  (when *rubberband-jack-buffer*
    (stop-buffer-source-thread *rubberband-jack-buffer*))
  (when *sndfile-rubberband-buffer*
    (stop-buffer-source-thread *sndfile-rubberband-buffer*))
  (unless (null-pointer-p *sndfile*)
    (sndfile-close *sndfile*))
  
  (format t "jack request size average: ~g~%rubberband required average: ~g~%"
          (funcall *jack-size-average* nil)
          (funcall *rubberband-required-average* nil)))

;;; (play "/home/chb/tmp/batum.wav" :speed 1.2)
;;; (play "/hdd/home/chb/musicstore/claus/flac/Musik/Rage_Against_The_Machine/Rage_Against_The_Machine/02-Killing_In_The_Name.flac" :pitch 1.1)