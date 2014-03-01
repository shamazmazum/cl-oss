(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:oos 'asdf:load-op :cffi-grovel))

(defsystem :cl-oss
    :description "OSS support for Common Lisp"
    :maintainer "Vasily Postnicov <shamaz.mazum at gmail.com>"
    :version "0.0"
    :depends-on (:cffi :trivial-gray-streams)
    :components ((:file "package")
                 (:file "impldep" :depends-on ("package"))
                 (cffi-grovel:grovel-file "oss-cmacro")
                 (cffi-grovel:wrapper-file "oss-wrapper" :soname "libosswrap")
                 (:file "oss" :depends-on ("package"))))
