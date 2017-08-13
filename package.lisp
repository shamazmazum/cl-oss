(defpackage cl-oss
  (:use #:cl #:trivial-gray-streams)
  (:nicknames #:oss)
  (:export #:dsp-error
           #:format-supported-p
           #:dsp-device
           #:dsp-device-input
           #:dsp-device-output
           #:with-dsp-device

           #:dsp-device-channels
           #:dsp-device-sample-rate
           #:dsp-device-sample-format

           #:+afmt-mu-law+
           #:+afmt-a-law+
           #:+afmt-ima-adpcm+
           #:+afmt-mpeg+

           #:+afmt-u8+
           #:+afmt-s8+

           #:+afmt-s16-le+
           #:+afmt-s16-be+
           #:+afmt-s16-ne+
           #:+afmt-u16-le+
           #:+afmt-u16-be+
           #:+afmt-u16-ne+

           #:+afmt-s24-le+
           #:+afmt-s24-be+
           #:+afmt-s24-ne+
           #:+afmt-u24-le+
           #:+afmt-u24-be+
           #:+afmt-u24-ne+

           #:+afmt-s32-le+
           #:+afmt-s32-be+
           #:+afmt-s32-ne+
           #:+afmt-u32-le+
           #:+afmt-u32-be+
           #:+afmt-u32-ne+))
