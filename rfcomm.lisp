(cl:in-package #:sb-bluetooth)
;;; Some things should definitely be split out of here.

;;; Both AF_BTH and address structure should better be grovelled; however,
;;; bluetooth API is different enough between linux and windows (TODO look at
;;; other Unixes), so grovelling won't help that much here.

(defconstant af-bluetooth
  #.(or #+linux 31 
        #+(or win32 windows) 32 ; or is it unix in general?
        (error "Can't determine bluetooth PF/AF value on this platform")))

;;; Just happens to be the same on Linux and Windows,
;;; or is it a widely-accepted convention?
(defconstant btproto-rfcomm 3)

(defclass bluetooth-rfcomm-socket (socket)
  ((family :initform af-bluetooth))
  (:documentation "Class representing bluetooth sockets"))

;;; Windows BT stacks may get the channel automatically by querying SDP, when
;;; the target service UUID is specified.
(define-alien-type sockaddr-rc
    (struct sockaddr-rc
            (family unsigned-short)
            (bdaddr (array unsigned-char 6))
            #+win32 (nil (array char 2))
            #+win32 (uuid (array unsigned-char 16))
            (channel #+win32 long
                     #-win32 char
                     :alignment 1)))

(defmacro octets-to-foreign (lisp-array sap)
  `(multiple-value-bind (from to) (values ,lisp-array (alien-sap ,sap))
    (sb-kernel:%byte-blt from 0 to 0 (length from))))

(defun make-bluetooth-address (source-data)
  "Turn sequence into a bluetooth address"
  (let ((result (make-array 6 :element-type '(unsigned-byte 8))))
    (prog1 result
      (etypecase source-data
        (string
	     (loop for index downfrom 5 to 0
               for start = 0 then (1+ end)
               for end = (+ start 2)
               do (setf (aref result index)
                        (parse-integer source-data :start start :end end
                                                   :radix 16))
                  (unless (zerop index)
                    (assert (eql #\: (aref source-data end))))))
        (sequence
	     (map-into result 'identity (reverse source-data)))))))

(defmethod make-sockaddr-for
    ((socket bluetooth-rfcomm-socket) &optional sockaddr &rest address
     &aux (device (first address)) (channel (second address)))
  (when (stringp (car address)) ;; auto-parse device address if it's a string
    (push (make-bluetooth-address (pop address)) address))
  (let ((sockaddr (or sockaddr (make-alien sockaddr-rc))))
    (loop for index from 0 below (alien-size sockaddr-rc :bytes)
          with addr = (sap-alien (alien-sap sockaddr)
                                 (array unsigned-char
                                        #.(alien-size sockaddr-rc :bytes)))
          do (setf (deref addr index) 0))
    (when (and device channel)
      (setf (slot sockaddr 'family) af-bluetooth
            (slot sockaddr 'channel)
            (typecase channel (integer channel)
              #+win32
              (otherwise (octets-to-foreign
                          (service-class-uuid-octets channel)
                          (slot sockaddr 'uuid)) 0)))
      (octets-to-foreign device (slot sockaddr 'bdaddr))
      sockaddr)))

(defmethod size-of-sockaddr ((socket bluetooth-rfcomm-socket))
  (alien-size sockaddr-rc :bytes))

(defmethod free-sockaddr-for ((socket bluetooth-rfcomm-socket)
                              sockaddr)
  (when sockaddr (free-alien sockaddr)))

(defun make-bluetooth-socket ()
  (make-instance 'bluetooth-rfcomm-socket
                 :type :stream :protocol btproto-rfcomm))

(defun open-rfcomm-channel (device-address channel-id
                            &rest socket-stream-options
                            &key (input t) (output t))
  "Connect remote RFCOMM endpoint and return a stream.
Stream lifetime management may be arranged using WITH-OPEN-STREAM,
without ever dealing with sockets directly.

Defaults for INPUT and OUTPUT are different from SOCKET-MAKE-STREAM, not only
because I like them to be true, but also because the socket is known to be
bidirectional here -- and we don't pass it to outer code."
  (let ((socket (make-bluetooth-socket)) (nlx t))
    (unwind-protect (prog2 (socket-connect socket device-address channel-id)
                        (apply #'socket-make-stream socket
                               :input input :output output
                               socket-stream-options)
                      (setf nlx nil))
      (when nlx (socket-close socket)))))
