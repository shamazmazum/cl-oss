(in-package :oss)

(defparameter +format-description+
  (list (cons +afmt-mu-law+    "Mu-law encoding")
        (cons +afmt-a-law+     "A-law encoding")
        (cons +afmt-ima-adpcm+ "IMA ADPCM encoding")
        (cons +afmt-u8+        "Unsigned byte encoding")
        (cons +afmt-s16-le+    "Signed 16 bit, little endian")
        (cons +afmt-s16-be+    "Signed 16 bit, big endian")
        (cons +afmt-s16-ne+    "Signed 16 bit, native endianness")
        (cons +afmt-s8+        "Signed 8 bit")
        (cons +afmt-s32-le+    "Signed 32 bit, little endian")
        (cons +afmt-s32-be+    "Signed 32 bit, big endian")
        (cons +afmt-u16-le+    "Unsigned 16 bit, little endian")
        (cons +afmt-u16-be+    "Unsigned 16 bit, big endian")
        (cons +afmt-mpeg+      "MPEG MP2/MP3 encoding"))
  "String description of format codes")

(define-condition dsp-conf-error ()
  ((message :reader   dsp-error-message
            :initarg  :message
            :initform "")
   (device  :reader   dsp-error-device
            :initarg  :device
            :initform nil))
  (:report (lambda (c s)
             (format s "DSP error ~S on device ~A"
                     (dsp-error-message c)
                     (dsp-error-device  c))))
  (:documentation "DSP error"))
  
(defclass dsp-device (fundamental-binary-stream)
  ((stream           :accessor dsp-device-stream
                     :documentation "Underlaying stream")
   (file-desc        :accessor dsp-device-file-desc
                     :documentation "File descriptor of the underlaying stream")
   (sample-format    :reader   dsp-device-sample-format
                     :documentation "Sample format understood by OSS"
                     :initarg  :sample-format
                     :initform +afmt-s16-le+)
   (channels         :reader   dsp-device-channels
                     :documentation "Number of audio channels"
                     :initarg  :channels
                     :initform 2)
   (sample-rate      :reader   dsp-device-sample-rate
                     :documentation "Sample rate"
                     :initarg  :sample-rate
                     :initform 44100))
  (:documentation "DSP device. Not to be instaniated"))

(defun choose-element-type (format)
  (cond
    ((= format +afmt-u8+) '(unsigned-byte 8))
    ((= format +afmt-s8+) '(signed-byte 8))
    
    ((or (= format +afmt-s16-le+)
         (= format +afmt-s16-be+)
         (= format +afmt-s16-ne+))
     '(signed-byte 16))
    
    ((or (= format +afmt-u16-le+)
         (= format +afmt-u16-be+))
     '(unsigned-byte 16))

    ((or (= format +afmt-s16-le+)
         (= format +afmt-s16-be+)
         (= format +afmt-s16-ne+))
     '(signed-byte 16))
    
    ((or (= format +afmt-s32-le+)
         (= format +afmt-s32-be+))
     '(signed-byte 32))
    
    (t (error 'dsp-conf-error :message "Unsupported sample format"))))

(defun configure-device (device)
  "Call needed ioctls on device"
  (declare (type dsp-device device))
  (let ((fd (dsp-device-file-desc device)))
    (or (oss-set-fmt fd (dsp-device-sample-format device))
        (error 'dsp-conf-error
               :message "Cannot set audio format"
               :device device))
    (or (oss-set-channels fd (dsp-device-channels device))
        (error 'dsp-conf-error
               :message "Cannot set number of channels"
               :device device))
    (or (oss-set-sample-rate fd (dsp-device-sample-rate device))
        (error 'dsp-conf-error
               :message "Cannot set sample rate"
               :device device))))

(defun format-supported-p (mask fmt)
  "Returns T if format FMT is supported"
  (/= (logand fmt mask) 0))

(defmethod close ((device dsp-device) &rest args)
  (declare (ignore args))
  (close (dsp-device-stream device))
  (call-next-method))

(defmethod print-object ((device dsp-device) stream)
  (pprint-logical-block (stream nil :prefix "#<DSP device: " :suffix ">")
    (format stream "Supported formats: ")
    (let ((supported-formats
           (if (open-stream-p device)
               (mapcar #'cdr
                       (remove-if-not #'(lambda (format-desc)
                                          (format-supported-p
                                           (oss-get-fmts (dsp-device-file-desc device))
                                           (car format-desc)))
                                      +format-description+)))))
      (pprint-logical-block (stream supported-formats)
        (pprint-linear stream supported-formats nil)))
    (pprint-newline :mandatory stream)
    (pprint-indent :block 0 stream)
    (format stream "Current format: ~A"
            (cdr (find (dsp-device-sample-format device)
                       +format-description+
                       :test #'(lambda (format format-desc)
                                 (= format (car format-desc))))))
      
    (pprint-newline :mandatory stream)
    (pprint-indent :block 0 stream)
    (format stream "Number of channels: ~D" (dsp-device-channels device))
      
    (pprint-newline :mandatory stream)
    (pprint-indent :block 0 stream)
    (format stream "Sample rate: ~D" (dsp-device-sample-rate device))))


(defclass dsp-device-output (dsp-device fundamental-binary-output-stream)
  ()
  (:documentation "Class for output to DSP device"))

(defmethod initialize-instance :after ((device dsp-device-output) &rest initargs)
  (declare (ignore initargs))
  (setf (dsp-device-stream device)
        (open "/dev/dsp"
              :element-type (choose-element-type
                             (dsp-device-sample-format device))
              :direction :output
              :if-exists :supersede)
        (dsp-device-file-desc device)
        (get-file-descriptor (dsp-device-stream device) :output))
  (configure-device device))

(defmethod stream-write-sequence ((device dsp-device-output) sequence start end &key)
  (write-sequence sequence (dsp-device-stream device)
                  :start start :end end))

(defmethod stream-write-byte ((device dsp-device-output) byte)
  (write-byte byte (dsp-device-stream device)))

(defclass dsp-device-input (dsp-device fundamental-binary-input-stream)
  ()
  (:documentation "Class for input from DSP device"))

(defmethod initialize-instance :after ((device dsp-device-input) &rest initargs)
  (declare (ignore initargs))
  (setf (dsp-device-stream device)
        (open "/dev/dsp"
              :element-type (choose-element-type
                             (dsp-device-sample-format device))
              :direction :input)
        (dsp-device-file-desc device)
        (get-file-descriptor (dsp-device-stream device) :input))
  (configure-device device))

(defmethod stream-read-sequence ((device dsp-device-input) sequence start end &key)
  (read-sequence sequence (dsp-device-stream device)
                 :start start :end end))

(defmethod stream-read-byte ((device dsp-device-input))
  (read-byte (dsp-device-stream device)))
