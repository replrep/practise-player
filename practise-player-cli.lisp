(in-package :practise-player-cli)

(defun parse-time (str)
  (flet ((calc-frame (frac-with-dot second &optional minute hour)
           (let ((frac
                  (if frac-with-dot
                      (let* ((frac-sans-dot (subseq frac-with-dot 1))
                             (frac-expanded
                              (concatenate 'string
                                           frac-sans-dot
                                           (subseq "000"
                                                   (length frac-sans-dot)))))
                        (/ (parse-integer frac-expanded) 1000.0))
                      0))
                 (second (if second (parse-integer second) 0))
                 (minute (if minute (parse-integer minute) 0))
                 (hour (if hour (parse-integer hour) 0)))
             (+ (* hour 60 60 +sample-rate+)
                (* minute 60 +sample-rate+)
                (floor (* (+ second frac) +sample-rate+))))))
  (or
   (register-groups-bind (hour minute second frac)
       ("(\\d?\\d):(\\d?\\d):(\\d?\\d)(\\.\\d\\d?\\d?)?" str)
     (calc-frame frac second minute hour))
   (register-groups-bind (minute second frac)
       ("(\\d?\\d):(\\d?\\d)(\\.\\d\\d?\\d?)?" str)
     (calc-frame frac second minute))
   (register-groups-bind (second frac)
       ("(\\d+)(\\.\\d\\d?\\d?)?" str)
     (calc-frame frac second)))))

(defun parse-percent (str)
  (coerce
   (/ (parse-integer str) 100.0)
   'double-float))

(unix-opts:define-opts
  (:name :begin
         :short #\b
         :long "begin"
         :arg-parser #'parse-time)
  (:name :end
         :short #\e
         :long "end"
         :arg-parser #'parse-time)
  (:name :gap
         :short #\g
         :long "gap"
         :arg-parser #'parse-time)
  (:name :speed
         :short #\s
         :long "speed"
         :arg-parser #'parse-percent)
  (:name :pitch
         :short #\p
         :long "pitch"
         :arg-parser #'parse-integer)
  (:name :tune
         :short #\t
         :long "tune"
         :arg-parser #'parse-integer)
  (:name :volume-left
         :short #\l
         :long "volume-left"
         :arg-parser #'parse-percent)
  (:name :volume-right
         :short #\r
         :long "volume-right"
         :arg-parser #'parse-percent))

(defparameter +halftone-up-factor+ 1.059463094352953d0)
(defparameter +halftone-down-factor+ 0.9438743126816935d0)

(defun run ()
  (multiple-value-bind (options free-args) (unix-opts:get-opts)
    (let ((lock (make-lock)))
      (with-lock-held (lock)
        (condition-wait
         (play (first free-args)
               :begin (or (getf options :begin) 0)
               :end (or (getf options :end) nil)
               :speed (or (getf options :speed) 1.0)
               :pitch (let ((pitch (or (getf options :pitch) 0.0)))
                        (if (plusp pitch)
                            (expt +halftone-up-factor+ pitch)
                            (expt +halftone-down-factor+ (- pitch)))))
         lock)))))
