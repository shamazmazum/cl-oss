(defpackage cl-oss
  (:use #:cl #:trivial-gray-streams)
  (:nicknames #:oss)
  (:export #:dsp-error
           #:format-supported-p
           #:dsp-device
           #:dsp-device-input
           #:dsp-device-output))
