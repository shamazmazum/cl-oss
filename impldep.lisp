(in-package :oss)

#-(or sbcl clisp)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This library cannot be compiled on this implementation"))

#+sbcl
(defun get-file-descriptor (stream direction)
  (declare (ignore direction))
  (sb-sys:fd-stream-fd stream))

;; Broken for some reason
#+clisp
(defun get-file-descriptor (stream direction)
  (declare (type (member :input :output) direction))
  (multiple-value-bind (in-fd out-fd)
      (ext:stream-handles stream)
    (cond
      ((eq :input direction) in-fd)
      ((eq :output direction) out-fd))))
