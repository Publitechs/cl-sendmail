(defpackage :sendmail-system (:use #:cl #:asdf))
(in-package :sendmail-system)

(defsystem :sendmail
    :version "0.2"
    :depends-on (:sb-bsd-sockets :mime :base64)
    :components ((:file "sendmail")))
