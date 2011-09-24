(in-package #:sb-bluetooth)

;;; UUID definitions for popular services.  If automatic SDP lookup is supported
;;; (now #+win32), keywords from those definitions may be used as CHANNEL
;;; arguments to MAKE-BLUETOOTH-SOCKET.

(def-bluetooth-service-class (:dialup-networking :dun)
    "00001103,0000,1000,80,00,00,80,5F,9B,34,FB")

(def-bluetooth-service-class (:serial-port)
    "00001101,0000,1000,80,00,00,80,5F,9B,34,FB")

(def-bluetooth-service-class (:obex-object-push)
    "00001105,0000,1000,80,00,00,80,5F,9B,34,FB")

(def-bluetooth-service-class (:obex-file-transfer)
    "00001106,0000,1000,80,00,00,80,5F,9B,34,FB")

(def-bluetooth-service-class (:irmc-sync-command)
    "00001107,0000,1000,80,00,00,80,5F,9B,34,FB")

