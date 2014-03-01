(in-package :oss)

#-(or sbcl clisp)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This library cannot be compiled on this implementation"))

#+sbcl
(defun get-file-descriptor (stream direction)
  (declare (ignore direction))
  (sb-sys:fd-stream-fd stream))
