(include "sndfile.h")

(in-package :sndfile)

(ctype sf-count-t "sf_count_t")

(cenum mode
       ((:sfm-read "SFM_READ")))


(cstruct sf-info "SF_INFO"
         (frames "frames" :type sf-count-t)
         (samplerate "samplerate" :type :int)
         (channels "channels" :type :int)
         (format "format" :type :int)
         (sections "sections" :type :int)
         (seekable "seekable" :type :int))
