(defpackage #:sb-bluetooth
  (:use #:cl #:sb-bsd-sockets #:sb-alien)
  (:import-from #:sb-bsd-sockets
                #:make-sockaddr-for
                #:free-sockaddr-for
                #:size-of-sockaddr
                #:family)
  (:export #:make-bluetooth-socket
           #:open-rfcomm-channel
           #:def-bluetooth-service-class
           #:service-class-uuid-octets))
