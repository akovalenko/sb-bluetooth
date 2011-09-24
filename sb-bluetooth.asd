(defsystem :sb-bluetooth
  :version "1.0"
  :serial t
  :components ((:file "package")
               (:file "early-uuid")
               (:file "uuid-data")
               (:file "rfcomm"))
  :depends-on (:sb-bsd-sockets)
  :maintainer "Anton Kovalenko <anton.kovalenko@siftsoft.com>"
  :licence "MIT-style"
  :description "Bluetooth stream socket support for SBCL")
