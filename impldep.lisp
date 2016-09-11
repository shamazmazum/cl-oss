(in-package :oss)

#-(or sbcl) ; other implementations later (or never)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "This library cannot be compiled on this implementation"))

#+sbcl
(defun open/return-descriptor (name direction)
  (declare (type (member :input :output) direction))
  (sb-posix:open name
                 (case direction
                   (:input sb-posix:o-rdonly)
                   (:output sb-posix:o-wronly)
                   (t 0)))) ; to satisfy compiler

#+sbcl
(defun make-stream-from-descriptor (fd direction element-type)
  (declare (type (member :input :output) direction))
  (let ((inputp (eq direction :input)))
    (sb-sys:make-fd-stream fd
                           :input inputp
                           :output (not inputp)
                           :element-type element-type
                           :buffering :full)))
