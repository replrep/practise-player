(in-package :practise-player-cli)

(defun parse-time-value (str)
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

(defun parse-percent-value (str)
  (coerce
   (/ (parse-integer str) 100.0)
   'double-float))

(defun parse-option-and-value (argv)
  (let* ((arg (first argv))
         (results
          (or
           (register-groups-bind (option value)
               ("^--([^=]+)=(.*)" arg)
             (list option value (cdr argv)))
           (register-groups-bind (option)
               ("^--(.*)" arg)
             (list option (cadr argv) (cddr argv)))
           (register-groups-bind (option value)
               ("^-(.)(.+)" arg)
             (list option value (cdr argv)))
           (register-groups-bind (option)
               ("^-(.)" arg)
             (list option (cadr argv) (cddr argv)))
           (list nil arg (cdr argv)))))
    (values-list results)))


(defparameter +halftone-up-factor+ 1.059463094352953d0)
(defparameter +cent-up-factor+ (expt +halftone-up-factor+ (/ 1 50.0d0)))
(defparameter +halftone-down-factor+ 0.9438743126816935d0)
(defparameter +cent-down-factor+ (expt +halftone-down-factor+ (/ 1 50.0d0)))

(defun run ()
  (let ((begin 0)
        (end nil)
        (gap 0)
        (speed 1.0)
        (pitch 0)
        (tune 0)
        (volume-left 1.0)
        (volume-right 1.0)
        (file-name "")
        (argv (uiop:command-line-arguments)))
    (loop while argv do
         (multiple-value-bind (option value new-argv)
             (parse-option-and-value argv)
           (cond
             ((or (string= "b" option) (string= "begin" option))
              (setf begin (parse-time-value value)))
             ((or (string= "e" option) (string= "end" option))
              (setf end (parse-time-value value)))
             ((or (string= "g" option) (string= "gap" option))
              (setf gap (parse-time-value value)))
             ((or (string= "s" option) (string= "speed" option))
              (setf speed (parse-percent-value value)))
             ((or (string= "p" option) (string= "pitch" option))
              (setf pitch (parse-integer value)))
             ((or (string= "t" option) (string= "tune" option))
              (setf tune (parse-integer value)))
             ((or (string= "l" option) (string= "volume-left" option))
              (setf volume-left (parse-percent-value value)))
             ((or (string= "r" option) (string= "volume-right" option))
              (setf volume-right (parse-percent-value value)))
             ((null option)
              (setf file-name value))
             (t
              (error "unknown option ~S" option)))
           (setf argv new-argv)))

    (let ((effective-pitch (* (if (plusp pitch)
                                  (expt +halftone-up-factor+ pitch)
                                  (expt +halftone-down-factor+ (- pitch)))
                              (if (plusp tune)
                                  (expt +cent-up-factor+ tune)
                                  (expt +cent-down-factor+ (- tune)))))
          (lock (make-lock)))
      (with-lock-held (lock)
        (condition-wait
         (play file-name
               :begin begin :end end :gap gap
               :speed speed
               :pitch effective-pitch
               :volume-left volume-left
               :volume-right volume-right)
         lock)))))
