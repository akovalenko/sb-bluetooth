(in-package #:sb-bluetooth)
;;; Early definitions for name-based service class UUID lookup

(defparameter *uuid-octet-map*
  '(3 2 1 0 5 4 7 6 . #.(loop for i from 8 repeat 8 collect i))
  "List representing the octet transposition

that makes a final UUID vector from a wrong/big-endian vector, which is the
result of parsing each pair of hex digits from uuid string into octet.")

(defparameter *uuid-hex-map*
  (mapcar #'+ *uuid-octet-map* *uuid-octet-map*)
  "A doubled version of *uuid-octet-map*")

(defun uuid-string-to-octets (uuid-string)
  "Convert UUID-STRING into uuid/uuid.

All non-alphanumeric characters are removed first; remaining string should
contain 16 hexadecimal digit pairs, whose values are transposed into result
array according to *uuid-hex-map*."
  (let ((uuid-string (remove-if-not #'alphanumericp uuid-string)))
    (do ((index 0 (1+ index))
         (start *uuid-hex-map*)
         (result (make-array 16 :element-type '(unsigned-byte 8))))
        ((null start) result)
      (setf (aref result index)
            (parse-integer uuid-string :start (car start)
                                       :end (+ 2 (pop start))
                                       :radix 16)))))

(defgeneric convert-uuid (designator)
  (:documentation "Normalize UUID representation into an octet vector.")
  (:method ((string string))
    "Convert uuid-string to octet vector.
The string should contain hex digits of uuid interspersed by arbitrary
non-alphanumeric punctuation."
    (uuid-string-to-octets string))
  (:method ((sequence sequence))
    "Convert non-string sequence representing an uuid, by mere coercion."
    (coerce sequence '(simple-array (unsigned-byte 8) (16)))))

(defgeneric service-class-uuid-octets (designator)
  (:documentation "Look up service class uuid by a DESIGNATOR.")
  (:method (uuid)
    "Unless more specific method exists, designator is assumed
to be a direct representation of service class uuid."
    (convert-uuid uuid)))

(defmacro def-bluetooth-service-class (names expansion)
  "Arrange #'SERVICE-CLASS-UUID-OCTETS to return uuid 
represented by EXPANSION if any symbol of flattened NAMES
is used as a service class designator."
  (typecase names
    (list
     `(progn ,@(loop for item in names collect
                     `(def-bluetooth-service-class ,item
                          ,(convert-uuid expansion)))))
    (t
     `(defmethod service-class-uuid-octets ((#1=#:designator (eql ,names)))
        (declare (ignore #1#)) ,(convert-uuid expansion)))))
