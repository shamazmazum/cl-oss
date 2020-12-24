(in-package :oss)

#-sbcl ; other implementations later (or never)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This library cannot be compiled on this implementation"))

(define-condition dsp-conf-error (error)
  ((message :reader   dsp-error-message
            :initarg  :message
            :initform "")
   (stream  :reader   dsp-error-stream
            :initarg  :stream
            :initform nil))
  (:report (lambda (c s)
             (format s "DSP error ~S on device ~A"
                     (dsp-error-message c)
                     (dsp-error-stream c))))
  (:documentation "DSP error"))

(defcfun ioctl :int
  (fd      :int)
  (request :ulong)
  (data    :pointer))

(defun stream-descriptor (stream)
  (sb-sys:fd-stream-fd stream))

(defun oss-set-fmt (stream format)
  (let ((format-value (foreign-bitfield-value 'audio-format
                                              (list format))))
    (with-foreign-object (ptr :int)
      (setf (mem-ref ptr :int) format-value)
      (if (or (= (ioctl (stream-descriptor stream)
                        +sndctl-dsp-setfmt+ ptr)
                 -1)
              (/= (mem-ref ptr :int) format-value))
          (error 'dsp-conf-error
                 :message "Cannot set audio format"
                 :stream stream))))
  format)

(defun oss-set-channels (stream channels)
  (with-foreign-object (ptr :int)
    (setf (mem-ref ptr :int) channels)
    (if (or (= (ioctl (stream-descriptor stream)
                      +sndctl-dsp-channels+ ptr)
               -1)
            (/= (mem-ref ptr :int) channels))
        (error 'dsp-conf-error
               :message "Cannot set number of channels"
               :stream stream)))
  channels)

(defun oss-set-sample-rate (stream sample-rate)
  (with-foreign-object (ptr :int)
    (setf (mem-ref ptr :int) sample-rate)
    (if (or (= (ioctl (stream-descriptor stream)
                      +sndctl-dsp-speed+ ptr)
               -1)
            (/= (mem-ref ptr :int) sample-rate))
        (error 'dsp-conf-error
               :message "Cannot set sample rate"
               :stream stream)))
  sample-rate)

(defun oss-get-fmts (stream)
  (with-foreign-object (ptr :int)
    (if (= (ioctl (stream-descriptor stream)
                  +sndctl-dsp-getfmts+ ptr)
           -1)
        (error 'dsp-conf-error
               :message "Cannot query native formats"
               :stream stream))
    (foreign-bitfield-symbols 'audio-format
                              (mem-ref ptr :int))))
