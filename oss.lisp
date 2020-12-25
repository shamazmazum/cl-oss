(in-package :oss)

(defparameter *format-description*
  '((:afmt-mu-law    . "Mu-law encoding")
    (:afmt-a-law     . "A-law encoding")
    (:afmt-ima-adpcm . "IMA ADPCM encoding")
    (:afmt-mpeg      . "MPEG MP2/MP3 encoding")

    (:afmt-u8        . "Unsigned 8 bit")
    (:afmt-s8        . "Signed 8 bit")

    (:afmt-s16-le    . "Signed 16 bit, little endian")
    (:afmt-s16-be    . "Signed 16 bit, big endian")
    (:afmt-s16-ne    . "Signed 16 bit, native endianness")
    (:afmt-u16-le    . "Unsigned 16 bit, little endian")
    (:afmt-u16-be    . "Unsigned 16 bit, big endian")
    (:afmt-u16-ne    . "Unsigned 16 bit, native endianness")

    (:afmt-s24-le    . "Signed 24 bit, little endian")
    (:afmt-s24-be    . "Signed 24 bit, big endian")
    (:afmt-s24-ne    . "Signed 24 bit, native endianness")
    (:afmt-u24-le    . "Unsigned 24 bit, little endian")
    (:afmt-u24-be    . "Unsigned 24 bit, big endian")
    (:afmt-u24-ne    . "Unsigned 24 bit, native endianness")

    (:afmt-s32-le    . "Signed 32 bit, little endian")
    (:afmt-s32-be    . "Signed 32 bit, big endian")
    (:afmt-s32-ne    . "Signed 32 bit, native endianness")
    (:afmt-u32-le    . "Unsigned 32 bit, little endian")
    (:afmt-u32-be    . "Unsigned 32 bit, big endian")
    (:afmt-u32-ne    . "Unsigned 32 bit, native endianness"))
  "String description of format codes")

(defclass dsp-device (fundamental-binary-stream)
  ((name             :initarg :name
                     :initform "/dev/dsp"
                     :reader dsp-device-name
                     :documentation "dsp device filename")
   (sample-format    :reader   dsp-device-sample-format
                     :documentation "Sample format understood by OSS"
                     :initarg  :sample-format
                     :initform (error "Specify sample format"))
   (channels         :reader   dsp-device-channels
                     :documentation "Number of audio channels"
                     :initarg  :channels
                     :initform (error "Specify number of channels"))
   (sample-rate      :reader   dsp-device-sample-rate
                     :documentation "Sample rate"
                     :initarg  :sample-rate
                     :initform (error "Specify sample rate"))
   (stream           :accessor dsp-device-stream
                     :documentation "Underlaying stream"))
  (:documentation "DSP device. Not to be instaniated"))

(defun choose-element-type (format)
  (case format
    ;; 8 bits
    (:afmt-u8 '(unsigned-byte 8))
    (:afmt-s8 '(signed-byte 8))
    ;; 16 bits
    ((:afmt-s16-le
      :afmt-s16-be
      :afmt-s16-ne)
     '(signed-byte 16))
    ((:afmt-u16-le
      :afmt-u16-be
      :afmt-u16-ne)
     '(unsigned-byte 16))
    ;; 24 bits
    ((:afmt-u24-le
      :afmt-u24-be
      :afmt-u24-ne)
     '(unsigned-byte 24))
    ((:afmt-s24-le
      :afmt-s24-be
      :afmt-s24-ne)
     '(signed-byte 24))
    ;; 32 bits
    ((:afmt-s32-le
      :afmt-s32-be
      :afmt-s32-ne)
     '(signed-byte 32))
    ((:afmt-u32-le
      :afmt-u32-be
      :afmt-u32-ne)
     '(unsigned-byte 32))
    (t (error 'dsp-conf-error
              :message "Unsupported sample format"))))

(defun configure-device (device)
  (let ((stream (dsp-device-stream device)))
    (oss-set-fmt stream (dsp-device-sample-format device))
    (oss-set-channels stream (dsp-device-channels device))
    (oss-set-sample-rate stream (dsp-device-sample-rate device))))

(defmethod initialize-instance :after ((device dsp-device) &rest initargs)
  (declare (ignore initargs))
  (let ((direction (cond
                     ((and (input-stream-p device)
                           (output-stream-p device))
                      :io)
                     ((input-stream-p device)  :input)
                     ((output-stream-p device) :output)
                     (t (error "Unreachable")))))
    (setf (dsp-device-stream device)
          (open (dsp-device-name device)
                :direction direction
                :if-exists :supersede
                :element-type (choose-element-type
                               (dsp-device-sample-format device))))
    (configure-device device)))

(defmethod close ((device dsp-device) &rest args)
  (declare (ignore args))
  (close (dsp-device-stream device))
  (call-next-method))

(defmethod print-object ((device dsp-device) stream)
  (print-unreadable-object (device stream :type t :identity t)
    (format stream "format: ~a, channels: ~d, speed: ~d Hz"
            (cdr (assoc (dsp-device-sample-format device)
                        *format-description*))
            (dsp-device-channels device)
            (dsp-device-sample-rate device))))

;; Output
(defclass dsp-device-output (dsp-device fundamental-binary-output-stream)
  ()
  (:documentation "Class for output to DSP device"))

(defmethod stream-write-sequence ((device dsp-device-output) sequence start end &key)
  (write-sequence sequence (dsp-device-stream device)
                  :start start :end end))

(defmethod stream-write-byte ((device dsp-device-output) byte)
  (write-byte byte (dsp-device-stream device)))

(defmethod stream-force-output ((device dsp-device-output))
  (force-output (dsp-device-stream device)))

;; Input
(defclass dsp-device-input (dsp-device fundamental-binary-input-stream)
  ()
  (:documentation "Class for input from DSP device"))

(defmethod stream-read-sequence ((device dsp-device-input) sequence start end &key)
  (read-sequence sequence (dsp-device-stream device)
                 :start start :end end))

(defmethod stream-read-byte ((device dsp-device-input))
  (read-byte (dsp-device-stream device)))

(defmacro with-dsp-device ((device class &rest args) &body body)
  "Bounds @cl:param(device) to open dsp device of class @cl:param(class).
Closes the device when control leaves the @cl:param(body). @cl:param(args)
are passed to @c(make-instance) on creation of the device."
  `(let ((,device (make-instance ',class ,@args)))
     (unwind-protect
          (progn ,@body)
       (close ,device))))

(defun device-native-formats (device-name &optional (direction :output))
  "Return a list of sample formats natively supported by a
device. Direction can be either @c(:input) or @c(:output).

NB: This function may return incorrect results, for example on FreeBSD
when @c(hw.snd.report_soft_formats) sysctl is set to 1."
  (with-open-file (device device-name
                          :direction direction
                          :if-exists :supersede)
    (oss-get-fmts device)))
