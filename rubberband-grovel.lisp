(include "rubberband/rubberband-c.h")

(in-package :rubberband)

(ctype rubberband-options "RubberBandOptions")

(cenum rubberband-option
       ((:RubberBandOptionProcessOffline "RubberBandOptionProcessOffline"))
       ((:RubberBandOptionProcessRealTime "RubberBandOptionProcessRealTime"))
       ((:RubberBandOptionStretchElastic "RubberBandOptionStretchElastic"))
       ((:RubberBandOptionStretchPrecise "RubberBandOptionStretchPrecise"))
       ((:RubberBandOptionTransientsCrisp "RubberBandOptionTransientsCrisp"))
       ((:RubberBandOptionTransientsMixed "RubberBandOptionTransientsMixed"))
       ((:RubberBandOptionTransientsSmooth "RubberBandOptionTransientsSmooth"))
       ((:RubberBandOptionDetectorCompound "RubberBandOptionDetectorCompound"))
       ((:RubberBandOptionDetectorPercussive "RubberBandOptionDetectorPercussive"))
       ((:RubberBandOptionDetectorSoft "RubberBandOptionDetectorSoft"))
       ((:RubberBandOptionPhaseLaminar "RubberBandOptionPhaseLaminar"))
       ((:RubberBandOptionPhaseIndependent "RubberBandOptionPhaseIndependent"))
       ((:RubberBandOptionThreadingAuto "RubberBandOptionThreadingAuto"))
       ((:RubberBandOptionThreadingNever "RubberBandOptionThreadingNever"))
       ((:RubberBandOptionThreadingAlways "RubberBandOptionThreadingAlways"))
       ((:RubberBandOptionWindowStandard "RubberBandOptionWindowStandard"))
       ((:RubberBandOptionWindowShort "RubberBandOptionWindowShort"))
       ((:RubberBandOptionWindowLong "RubberBandOptionWindowLong"))
       ((:RubberBandOptionSmoothingOff "RubberBandOptionSmoothingOff"))
       ((:RubberBandOptionSmoothingOn "RubberBandOptionSmoothingOn"))
       ((:RubberBandOptionFormantShifted "RubberBandOptionFormantShifted"))
       ((:RubberBandOptionFormantPreserved "RubberBandOptionFormantPreserved"))
       ((:RubberBandOptionPitchHighQuality "RubberBandOptionPitchHighQuality"))
       ((:RubberBandOptionPitchHighSpeed "RubberBandOptionPitchHighSpeed"))
       ((:RubberBandOptionPitchHighConsistency "RubberBandOptionPitchHighConsistency"))
       ((:RubberBandOptionChannelsApart "RubberBandOptionChannelsApart"))
       ((:RubberBandOptionChannelsTogether "RubberBandOptionChannelsTogether")))