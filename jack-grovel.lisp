(include "jack/types.h")
(include "jack/jack.h")

(in-package :jack)

(cenum jack-port-flags
       ((:jackportisinput "JackPortIsInput"))
       ((:jackportisoutput "JackPortIsOutput"))
       ((:jackportisphysical "JackPortIsPhysical"))
       ((:jackportcanmonitor "JackPortCanMonitor"))
       ((:jackportisterminal "JackPortIsTerminal")))

(ctype jack-nframes "jack_nframes_t")
