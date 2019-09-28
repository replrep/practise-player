(in-package :rubberband)

(define-foreign-library rubberband
  ((:and :unix (:not :darwin))
   (:or "librubberband.so.2.1.0" "librubberband.so.2" "librubberband.so"))
  (t (:default "librubberband")))

(unless (foreign-library-loaded-p 'rubberband)
 (use-foreign-library rubberband))


(defcfun "rubberband_new" :pointer
  (samplerate :uint)
  (channels :uint)
  (options rubberband-options)
  (initial-time-ratio :double)
  (initial-pitch-scale :double))

(defcfun "rubberband_delete" :void
  (rubberband-state :pointer))

(defcfun "rubberband_set_time_ratio" :void
  (rubberband-state :pointer)
  (ratio :double))

(defcfun "rubberband_set_pitch_scale" :void
  (rubberband-state :pointer)
  (scale :double))

(defcfun "rubberband_get_samples_required" :int
  (rubberband-state :pointer))

(defcfun "rubberband_process" :void
  (rubberband-state :pointer)
  (input :pointer)  ;const float *const *input
  (samples :int)
  (final :int))

(defcfun "rubberband_available" :int
  (rubberband-state :pointer))

(defcfun "rubberband_retrieve" :uint
  (rubberband-state :pointer)
  (output :pointer)  ;float *const *output
  (samples :uint))

(defun make-rubberband ()
  (rubberband-new 44100 2
                  (foreign-enum-value 'rubberband-option
                                      :RubberBandOptionProcessRealTime)
                  1.0d0
                  1.0d0))

(defun destroy-rubberband (rubberband)
  (rubberband-delete rubberband))
