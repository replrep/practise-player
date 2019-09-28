(in-package :jack)

(define-foreign-library jack
  ((:and :unix (:not :darwin))
   (:or "libjack.so.0.1.0" "libjack.so.0" "libjack.so"))
  (t (:default "libjack")))

(unless (foreign-library-loaded-p 'jack)
 (use-foreign-library jack))


(defparameter +jack-default-audio-type+ "32 bit float mono audio")
(defparameter +jack-port-is-input+
  (foreign-enum-value 'jack-port-flags :jackportisinput))
(defparameter +jack-port-is-output+
  (foreign-enum-value 'jack-port-flags :jackportisoutput))
(defparameter +jack-port-is-physical+
  (foreign-enum-value 'jack-port-flags :jackportisphysical))
(defparameter +jack-port-can-monitor+
  (foreign-enum-value 'jack-port-flags :jackportcanmonitor))
(defparameter +jack-port-is-terminal+
  (foreign-enum-value 'jack-port-flags :jackportisterminal))


(defcfun "jack_client_open" :pointer
  (client_name :string)
  (options :pointer)
  (status :pointer))

(defcfun "jack_client_close" :int
  (client :pointer))


(defcfun "jack_port_register" :pointer
  (client :pointer)
  (port_name :string)
  (port_type :string)
  (flags :unsigned-long)
  (buffer_size :unsigned-long))

(defcfun "jack_port_unregister" :int
  (client :pointer)
  (port :pointer))


(defcfun "jack_set_process_callback" :int
  (client :pointer)
  (process_callback :pointer)
  (arg :pointer))

(defcfun "jack_port_get_buffer" :pointer
  (port :pointer)
  (arg jack-nframes))


(defcfun "jack_activate" :int
  (client :pointer))

(defcfun "jack_deactivate" :int
  (client :pointer))
