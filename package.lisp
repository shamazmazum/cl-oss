(defpackage cl-oss
  (:use #:cl #:trivial-gray-streams)
  (:nicknames #:oss)
  (:export #:dsp-error
           #:format-supported-p
           #:dsp-device
           #:dsp-device-input
           #:dsp-device-output

           #:dsp-device-channels
           #:dsp-device-sample-rate
           #:dsp-device-sample-format

           #:+afmt-mu-law+
           #:+afmt-a-law+
           #:+afmt-ima-adpcm+
           #:+afmt-u8+
           #:+afmt-s16-le+
           #:+afmt-s16-be+
           #:+afmt-s16-ne+
           #:+afmt-s8+
           #:+afmt-s32-le+
           #:+afmt-s32-be+
           #:+afmt-u16-le+
           #:+afmt-u16-be+
           #:+afmt-mpeg+))
