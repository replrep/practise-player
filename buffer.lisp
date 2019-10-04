(in-package :buffer)

(defparameter +read-chunk-size+ 2048)


(defstruct (buffer (:constructor %make-buffer))
  lock
  condition-var
  (read-index 0)
  (write-index 0)
  size
  data
  source-callback
  thread
  thread-keep-running
  )

(defun make-buffer (size source-callback)
  (let ((buffer (%make-buffer)))
    (setf (buffer-lock buffer) (make-lock))
    (setf (buffer-condition-var buffer) (make-condition-variable))
    (setf (buffer-size buffer) (1+ size))
    (setf (buffer-data buffer) (make-array (list (1+ size))
                                           :element-type 'single-float))
    (setf (buffer-source-callback buffer) source-callback)
    buffer))

(defun buffer-read-avail (buffer)
  (let ((avail (- (buffer-write-index buffer)
                  (buffer-read-index buffer))))
    (if
      (minusp avail) (+ avail (buffer-size buffer))
      avail)))

(defun buffer-write-avail (buffer)
  (1- (let ((avail (- (buffer-read-index buffer)
                      (buffer-write-index buffer))))
        (cond
          ((zerop avail) (buffer-size buffer))
          ((minusp avail) (+ avail (buffer-size buffer)))
          (t avail)))))

(defun read-chunk (buffer len callback)
  (declare  (fixnum len)
            (function callback)
            (optimize (speed 3)))
  (let* ((read-index (buffer-read-index buffer))
         (data (buffer-data buffer))
         (size (buffer-size buffer))
         (deliver (min len (the fixnum (buffer-read-avail buffer)))))
    (declare (fixnum read-index size deliver)
             (simple-vector data))
    (loop for i below deliver do
         (funcall callback (svref data read-index))
         (setf read-index (mod (1+ read-index) size)))
    (setf (buffer-read-index buffer) read-index)
    (condition-notify (buffer-condition-var buffer))
    deliver))

(defun run-buffer-source-thread (buffer)
  (setf (buffer-thread-keep-running buffer) t)
  (setf (buffer-thread buffer)
        (make-thread
         (lambda ()
           (let* ((data (buffer-data buffer))
                  (size (buffer-size buffer))
                  (callback (buffer-source-callback buffer))
                  (lock (buffer-lock buffer))
                  (condition-var (buffer-condition-var buffer)))
             (loop while (buffer-thread-keep-running buffer) do
                  (loop while (plusp (buffer-write-avail buffer)) do
                       (let* ((write-index (buffer-write-index buffer))
                              (setter (lambda (value)
                                        (setf (svref data write-index) value)
                                        (setf write-index (mod (1+ write-index)
                                                               size)))))
                         (when (zerop
                                (funcall callback
                                         (min (buffer-write-avail buffer)
                                              +read-chunk-size+)
                                         setter))
                           (return))
                         (setf (buffer-write-index buffer) write-index)))
                  (when (buffer-thread-keep-running buffer)
                    (with-lock-held (lock)
                      (condition-wait condition-var lock))))))
         :name "buffer source thread")))

(defun stop-buffer-source-thread (buffer)
  (setf (buffer-thread-keep-running buffer) nil)
  (condition-notify (buffer-condition-var buffer))
  (when (buffer-thread buffer)
    (join-thread (buffer-thread buffer))))
