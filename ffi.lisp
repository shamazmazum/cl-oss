(in-package :oss)

#-sbcl ; other implementations later (or never)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This library cannot be compiled on this implementation"))

(define-condition dsp-condition ()
  ((message :reader   dsp-error-message
            :initarg  :message
            :initform "")
   (stream  :reader   dsp-error-stream
            :initarg  :stream
            :initform nil))
  (:report (lambda (c s)
             (format s "DSP ~a ~a on device ~a"
                     (if (typep c 'error)
                         "error" "warning")
                     (dsp-error-message c)
                     (dsp-error-stream c))))
  (:documentation "Generic DSP condition"))

(define-condition dsp-conf-error (error dsp-condition)
  ()
  (:documentation "DSP configuration error"))

(define-condition dsp-conf-warning (warning dsp-condition)
  ()
  (:documentation "DSP configuration warning"))

(defcfun (ioctl% "ioctl")  :int
  (fd      :int)
  (request :ulong)
  (data    :pointer))

(defmacro ioctl (fd request data)
  "Issue ioctl system call for the file descriptor @c(fd). If
@c(request) symbol has no value (e.g. this request is not defined on
your system), simply do nothing."
  (if (boundp request)
      `(/= (ioctl% ,fd ,request ,data) -1)))

(defmacro when-not (conditional &body body)
  `(when (not ,conditional) ,@body))

(defun stream-descriptor (stream)
  "Return underlying file descriptor associated with the stream."
  #+sbcl
  (sb-sys:fd-stream-fd stream))

(defun oss-set-fmt (stream format)
  (let ((format-value
         (foreign-bitfield-value 'audio-format (list format))))
    (with-foreign-object (ptr :int)
      (setf (mem-ref ptr :int) format-value)
      (when-not
          (and (ioctl (stream-descriptor stream)
                      +sndctl-dsp-setfmt+ ptr)
               (= (mem-ref ptr :int) format-value))
        (error 'dsp-conf-error
               :message "Cannot set audio format"
               :stream stream))))
  format)

(defun oss-set-channels (stream channels)
  (with-foreign-object (ptr :int)
    (setf (mem-ref ptr :int) channels)
    (when-not
        (and (ioctl (stream-descriptor stream)
                    +sndctl-dsp-channels+ ptr)
             (= (mem-ref ptr :int) channels))
      (error 'dsp-conf-error
             :message "Cannot set number of channels"
             :stream stream)))
  channels)

(defun oss-set-sample-rate (stream sample-rate)
  (with-foreign-object (ptr :int)
    (setf (mem-ref ptr :int) sample-rate)
    (when-not
        (and (ioctl (stream-descriptor stream)
                    +sndctl-dsp-speed+ ptr)
             (= (mem-ref ptr :int) sample-rate))
      (error 'dsp-conf-error
             :message "Cannot set sample rate"
             :stream stream)))
  sample-rate)

(defun oss-get-fmts (stream)
  (with-foreign-object (ptr :int)
    (when-not
        (ioctl (stream-descriptor stream)
               +sndctl-dsp-getfmts+ ptr)
      (error 'dsp-conf-error
             :message "Cannot query native formats"
             :stream stream))
    (foreign-bitfield-symbols 'audio-format
                              (mem-ref ptr :int))))

(defun oss-set-cooked (stream cooked)
  (let ((cooked-int
         (ecase cooked
           (:enabled  1)
           (:disabled 0))))
    (with-foreign-object (ptr :int)
      (setf (mem-ref ptr :int) cooked-int)
      (when-not
          (and (ioctl (stream-descriptor stream)
                      +sndctl-dsp-cookedmode+ ptr)
               (= (mem-ref ptr :int) cooked-int))
        (warn 'dsp-conf-warning
              :message "Cannot set cooked mode"
              :stream stream))))
  cooked)

(defun oss-set-policy (stream policy)
  (declare (type (integer 0 10) policy))
  (with-foreign-object (ptr :int)
    (setf (mem-ref ptr :int) policy)
    (when-not
        (and (ioctl (stream-descriptor stream)
                    +sndctl-dsp-policy+ ptr)
             (= (mem-ref ptr :int) policy))
      (warn 'dsp-conf-warning
            :message "Cannot set latency policy"
            :stream stream)))
  policy)
