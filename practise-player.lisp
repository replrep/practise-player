(in-package :practise-player)


(defparameter +sample-rate+ 44100)
(defparameter +read-buffer-size+ 8192)
(defparameter +jack-client-name+ "practise-player")

(defvar *sndfile* nil)
(defvar *sndfile-rubberband-buffer* nil)
(defvar *rubberband* nil)
(defvar *rubberband-jack-buffer* nil)
(defvar *client* nil)
(defvar *output-port-l* nil)
(defvar *output-port-r* nil)

;;; written by ui thread / read by sndfile read thread
(defvar *cmd-goto-abs* nil)
(defvar *cmd-goto-rel* nil)
(defvar *loop-begin* 0)
(defvar *loop-end* nil)
(defvar *gap* 0)

;;; written by ui thread / read by rubberband read thread
(defvar *speed* 1.0d0)
(defvar *pitch* 1.0d0)
(defvar *volume-left* 1.0)
(defvar *volume-right* 1.0)

;;; written by sndfile read thread, read by ui thread
(defvar *current-frame-position* nil)


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


;;;------------------------------------------------------------------------
(defun pos-from-frames (frames)
  (multiple-value-bind (hours frames-sans-hours)
      (floor frames (* +sample-rate+ 60 60))
    (multiple-value-bind (minutes frames-sans-minutes)
        (floor frames-sans-hours (* +sample-rate+ 60))
      (multiple-value-bind (seconds frames-sans-seconds)
          (floor frames-sans-minutes +sample-rate+)
        (format nil "~2,'0D:~2,'0D:~2,'0D.~3,'0D"
                hours minutes seconds
                (floor (* 1000 frames-sans-seconds (/ 1 +sample-rate+))))))))


;;;------------------------------------------------------------------------
(defun set-loop (begin end &optional gap)
  (when (and end (< (- end begin) 1000))
    (error "loop too short"))
  (setf *loop-begin* begin)
  (setf *loop-end* end)
  (setf *gap* (or gap 0))
  (values))

(defun set-speed (speed)
  (setf *speed* (coerce speed 'double-float))
  (values))

(defun set-pitch (pitch)
  (setf *pitch* (coerce pitch 'double-float))
  (values))

(defun set-volume-left (volume)
  (setf *volume-left* (coerce volume 'single-float))
  (values))

(defun set-volume-right (volume)
  (setf *volume-right* (coerce volume 'single-float))
  (values))

(defun goto-abs (frame)
  (setf *cmd-goto-abs* frame)
  (values))

(defun goto-rel (offset)
  (setf *cmd-goto-rel* offset)
  (values))

(defun get-position ()
  *current-frame-position*)


;;;------------------------------------------------------------------------
(defun drain-rubberband (rubberband item-consumer nitems)
  (let* ((frames-available (rubberband-available rubberband))
         (retrieve (min frames-available (floor nitems 2))))
    (if (zerop retrieve)
        0
        (with-foreign-objects ((channels :pointer 2)
                               (left :float retrieve)
                               (right :float retrieve))
          (setf (mem-aref channels :pointer 0) left)
          (setf (mem-aref channels :pointer 1) right)
          (let ((result-frames
                 (rubberband-retrieve rubberband channels retrieve)))
            (loop for i below result-frames do
                 (funcall item-consumer
                          (* (mem-aref left :float i) *volume-left*))
                 (funcall item-consumer
                          (* (mem-aref right :float i) *volume-right*)))
            (* 2 result-frames))))))

(defun fill-rubberband (rubberband buffer)
  (let ((frames-required (rubberband-get-samples-required rubberband)))
    (if (zerop frames-required)
        0
        (with-foreign-objects ((channels :pointer 2)
                               (left :float frames-required)
                               (right :float frames-required))
          (let ((index 0))
            (read-chunk
             buffer (* 2 frames-required)
             (lambda (val)
               (if (evenp index)
                   (setf (mem-aref left :float (floor index 2)) val)
                   (setf (mem-aref right :float (floor (- index 1) 2)) val))
               (incf index)))
            (setf (mem-aref channels :pointer 0) left)
            (setf (mem-aref channels :pointer 1) right)
            (rubberband-set-time-ratio rubberband (- 2.0d0 *speed*))
            (rubberband-set-pitch-scale rubberband *pitch*)
            (rubberband-process rubberband channels (floor index 2) 0)
            index)))))

(defcallback jack-callback :int ((nframes jack-nframes) (arg :pointer))
  (declare  (fixnum nframes)
            (ignore arg)
            (optimize (speed 3)))
  (let ((jack-buffer-l (jack-port-get-buffer *output-port-l* nframes))
        (jack-buffer-r (jack-port-get-buffer *output-port-r* nframes))
        (index 0))
    (declare (fixnum index))
    (read-chunk
     *rubberband-jack-buffer*
     (the fixnum (* 2 nframes))
     (lambda (val)
       (if (evenp index)
           (setf (mem-aref jack-buffer-l :float (floor index 2)) val)
           (setf (mem-aref jack-buffer-r :float (floor (- index 1) 2)) val))
       (incf index)))
    (loop for i from (floor index 2) below nframes do
         (setf (mem-aref jack-buffer-l :float i) 0.0)
         (setf (mem-aref jack-buffer-r :float i) 0.0)))
  0)

(let ((remaining-gap 0))
(defun sndfile-handler (nitems item-consumer)
  (when *cmd-goto-abs*
    (goto-frame-abs *sndfile* *cmd-goto-abs*)
    (setf *cmd-goto-abs* nil))
  (when *cmd-goto-rel*
    (goto-frame-rel *sndfile* *cmd-goto-rel*)
    (setf *cmd-goto-rel* nil))
  (setf *current-frame-position* (get-frame-position *sndfile*))
  (when (and *loop-end*
             (>= *current-frame-position* *loop-end*))
      (goto-frame-abs *sndfile* *loop-begin*)
      (setf *current-frame-position* *loop-begin*)
      (setf remaining-gap *gap*))
  (if (plusp remaining-gap)
      (let ((actual-item-count (min nitems (* 2 remaining-gap))))
        (loop repeat actual-item-count do
             (funcall item-consumer 0.0))
        (decf remaining-gap (floor actual-item-count 2))
        actual-item-count)
      (prog1
          (read-items *sndfile*
                      (min nitems
                           (if *loop-end*
                               (* 2 (- *loop-end* *current-frame-position*))
                               most-positive-fixnum))
                      item-consumer)
        (setf *current-frame-position* (get-frame-position *sndfile*))))))

(defun rubberband-handler (nitems item-consumer)
  (let ((count (drain-rubberband *rubberband* item-consumer nitems)))
    (if (plusp count)
        count
        (fill-rubberband *rubberband* *sndfile-rubberband-buffer*))))

(defun init ()
  (when *client*
    (return-from init))
  
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
        (make-buffer +read-buffer-size+ #'sndfile-handler))

  (setf *rubberband* (make-rubberband))

  (setf *rubberband-jack-buffer*
        (make-buffer +read-buffer-size+ #'rubberband-handler)))

(defun play (filename &key (begin 0) (end nil) (gap 0)
                        (speed 1.0) (pitch 1.0)
                        (volume-left 1.0) (volume-right 1.0))
  (unless *client*
    (init))

  (setf *sndfile* (sndfile-open filename))
  (when (null-pointer-p *sndfile*)
    (error "Couldn't open soundfile ~S" filename))

  (set-loop begin
            (or end (get-end-frame-position *sndfile*))
            gap)
  (set-speed speed)
  (set-pitch pitch)
  (set-volume-left volume-left)
  (set-volume-right volume-right)

  (goto-frame-abs *sndfile* begin)

  (rubberband-set-time-ratio *rubberband* (coerce speed 'double-float))
  (rubberband-set-pitch-scale *rubberband* (coerce pitch 'double-float))
  (run-buffer-source-thread *sndfile-rubberband-buffer*)
  (run-buffer-source-thread *rubberband-jack-buffer*)
  (jack-activate *client*)

  (make-condition-variable))

(defun stop ()
  (unless (or (null *client*) (null-pointer-p *client*))
    (jack-deactivate *client*)
    (jack-client-close *client*)
    (setf *client* nil))
  (when *rubberband-jack-buffer*
    (stop-buffer-source-thread *rubberband-jack-buffer*))
  (when *sndfile-rubberband-buffer*
    (stop-buffer-source-thread *sndfile-rubberband-buffer*))
  (unless (or (null *sndfile*) (null-pointer-p *sndfile*))
    (sndfile-close *sndfile*))
  (values))
