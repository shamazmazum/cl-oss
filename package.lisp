(defpackage cl-oss
  (:use #:cl #:trivial-gray-streams #:cffi)
  (:nicknames #:oss)
  (:export #:dsp-error
           #:dsp-device
           #:dsp-device-input
           #:dsp-device-output
           #:with-dsp-device

           #:dsp-device-channels
           #:dsp-device-sample-rate
           #:dsp-device-sample-format

           #:device-native-formats))
