(defsystem :cl-oss
    :description "OSS support for Common Lisp"
    :maintainer "Vasily Postnicov <shamaz.mazum@gmail.com>"
    :licence "2-clause BSD"
    :version "0.0"
    :defsystem-depends-on (:cffi-grovel)
    :depends-on (:cffi :trivial-gray-streams)
    :serial t
    :components ((:file "package")
                 (:file "impldep")
                 (:cffi-grovel-file "oss-cmacro" )
                 (:cffi-wrapper-file "oss-wrapper" :soname "libosswrap")
                 (:file "oss")))
