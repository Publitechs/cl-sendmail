;;;; sendmail.lisp
;;;;
;;;; This code was written by Christophe Rhodes <csr21@cam.ac.uk>, and
;;;; is in the public domain.

;;; Class mail-output-stream.
;;;
;;; FIXME: This information belongs in documentation strings
;;; somewhere. But where? Possibly distributed in various
;;; :documentation slots.
;;;
;;; The mail-output-stream is a class of stream (therefore depending
;;; on the Gray Stream extension) that, on close, sends its
;;; accumulated content by mail.
;;;
;;; Slots:
;;;
;;; * real-stream: contains the stream that the print- functions
;;; actually print to;
;;;
;;; * string-strm: contains the string-strm that is used on
;;; close. This is the same as the real-stream if no stream is
;;; initially supplied; if one is supplied, real-stream is a
;;; broadcast-stream to both the supplied stream and the
;;; string-strm;
;;;
;;; * to, cc, bcc: lists of strings, one per e-mail address;
;;;
;;; * other-headers: A list of strings, one per header.

(defpackage :sendmail
  (:use :cl :sb-gray :sb-unix :sb-ext :sb-bsd-sockets :mime :base64)
  (:export :mailer-program-error
	   :mail-output-stream
	   :to
	   :cc
	   :bcc
	   :subject
	   :content-type
	   :attachments
	   :other-headers
	   :*smtp-server*
	   :*smtp-port*
	   :*mail-domain*
	   :make-mail-stream))


(in-package :sendmail)

(defparameter *sendmail* "/usr/lib/sendmail")
(defparameter *mail-domain* "piscescom.com")
(defparameter *cr-lf* (format nil "~A~A" #\return #\linefeed))


(defclass mail-output-stream (fundamental-character-output-stream)
  ((real-stream :initarg :stream :initform nil :accessor mail-output-stream-stream)
   (string-strm :accessor string-strm :initform (make-string-output-stream))
   (subject 
    :initarg :subject
    :accessor subject 
    :initform "")
   (to 
    :initarg :to
    :accessor to 
    :initform nil)
   (from
    :initarg :from
    :accessor from
    :initform nil)
   (reply-to
    :initarg :reply-to
    :accessor reply-to
    :initform nil)
   (cc
    :initarg :cc
    :accessor cc 
    :initform nil)
   (bcc 
    :initarg :bcc
    :accessor bcc 
    :initform nil)
   (content-type
    :initarg :type
    :accessor content-type
    :initform "text")
   (content-subtype
    :initarg :subtype
    :accessor content-subtype
    :initform "plain")
   (attachments
    :initarg :attachments
    :accessor attachments
    :initform nil)
   (other-headers 
    :initarg :other-headers
    :accessor other-headers 
    :initform nil)))

(defmethod initialize-instance :after ((object mail-output-stream) &key)
  (if (null (mail-output-stream-stream object))
      (setf (mail-output-stream-stream object) (string-strm object))
    (setf (mail-output-stream-stream object)
	  (make-broadcast-stream (mail-output-stream-stream object)
				 (string-strm object)))))



(defmethod print-object ((object mail-output-stream) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "to ~a regarding ~a" (to object) (subject object))))

(defmethod stream-write-string ((stream mail-output-stream) string
				&optional (start 0) (end (length string)))
  (princ (subseq string start end) (mail-output-stream-stream stream)))

(defmethod stream-write-char ((stream mail-output-stream) character)
  (princ character (mail-output-stream-stream stream)))

(defmethod stream-line-column ((stream mail-output-stream))
  (stream-line-column (mail-output-stream-stream stream)))

(defmethod stream-finish-output ((stream mail-output-stream))
  (finish-output (mail-output-stream-stream stream)))

(defmethod stream-force-output ((stream mail-output-stream))
  (force-output (mail-output-stream-stream stream)))

(defmethod stream-clear-output ((stream mail-output-stream))
  (clear-output (mail-output-stream-stream stream)))

(defmethod stream-line-column ((stream mail-output-stream))
  nil)

(defmethod close ((stream mail-output-stream) &key abort)
  (close (mail-output-stream-stream stream) :abort abort)
  (close (string-strm stream) :abort abort))

(define-condition mailer-program-error (error)
  ((error-code :initarg :error-code :accessor error-code))
  (:report (lambda (condition stream)
	     (format stream "Mailer program returned non-zero exit code: ~d."
		     (error-code condition)))))

(defun portable-run-program (&rest args)
  #+cmu (apply 'ext:run-program args)
  #+sbcl (apply 'sb-ext:run-program args)
  #-(or cmu sbcl) (error "Functionality is missing in this implementation."))

(defmethod close :before ((stream mail-output-stream) &key &allow-other-keys)
    (restart-case
	(send-email stream)
      (retry ()
	     :report "Retry sending mail."
	(close stream))
      (save (pathname)
	    :report "Save mail body to file."
	    :interactive (lambda ()
			   (format *query-io* "~&Please enter a pathname: ")
			   (list (pathname (read-line *query-io*))))
	(with-open-file (s pathname :direction :output :if-exists :error)
	  (write (get-output-stream-string (mail-output-stream-stream stream))
		 :stream s)))));)
    

;(defparameter *smtp-server* "lizzyb.piscescom.com")
;(defparameter *smtp-port* 25)


(defun send-email (mail-output-stream)
  (unless (listp (to mail-output-stream))
    (setf (to mail-output-stream) (list (to mail-output-stream))))
  (unless (listp (cc mail-output-stream))
    (setf (cc mail-output-stream) (list (cc mail-output-stream))))
  (unless (listp (bcc mail-output-stream))
    (setf (bcc mail-output-stream) (list (bcc mail-output-stream))))
  (let ((sendmail (process-input
		   (run-program "/usr/lib/sendmail"
				`("-f" ,(or (from mail-output-stream)
					    (sb-unix:uid-username 
					     (sb-unix:unix-getuid)))
				  ,@(to mail-output-stream)
				  ,@(cc mail-output-stream)
				  ,@(bcc mail-output-stream))
				:input :stream
				:wait nil)))
	(mime (when (attachments mail-output-stream)
		(make-instance 
		 'multipart-mime
		 :subtype "mixed"
		 :content
		 
		 ;; Firstly the text input
		 (cons (make-instance
			(if (string-equal (content-type mail-output-stream)
					  "text")
			    'text-mime
			    'mime)
			:type (content-type mail-output-stream)
			:subtype (content-subtype mail-output-stream)
			:content 
			(get-output-stream-string
			 (mail-output-stream-stream
			  mail-output-stream))
			:disposition "inline")

		       ;; The attachments themselves
		       (mapcar
			(lambda (attachment)
			(multiple-value-bind 
			      (type subtype)
			    (lookup-mime attachment)
			  (make-instance 
			   'mime
			   :type (or type "application")
			   :subtype (or subtype "octet-stream")
			   :content
			   (encode-attachment
			    (read-file attachment))
			   :encoding "base64"
			   :disposition
			   "attachment"
			   :disposition-parameters
			   `((:filename
			      ,(format 
				nil "~A~@[.~A~]"
				(pathname-name 
				 (pathname attachment))
				(pathname-type
				 (pathname attachment))))))))
			(attachments mail-output-stream)))))))

    (mapc (lambda (header value)
	    (when value
	      (format sendmail "~A: ~A~{,~A~}~%" 
		      header
		      (if (listp value)
			  (first value)
			  value)
		      (if (listp value)
			  (rest value)
			  nil))))
	  (list "To" "Cc" "From" "Reply-To")
	  (list (to mail-output-stream)
		(cc mail-output-stream)
		(from mail-output-stream)
		(reply-to mail-output-stream)))
    
    (when (subject mail-output-stream)
      (format sendmail "Subject: ~A~%" (subject mail-output-stream)))

    (if mime
	(print-mime sendmail mime t t)
	(progn
	  (format sendmail "MIME-Version: 1.0~%Content-Type: ~A/~A~%~%" 
		  (content-type mail-output-stream)
		  (content-subtype mail-output-stream))
	  (princ (get-output-stream-string
		  (mail-output-stream-stream mail-output-stream))
		 sendmail)))
    
    (close sendmail)))
      

(defun encode-attachment (data)
  (split-string (format nil "~A~A" #\return #\newline)
                (base64-encode data)
                75))


(defun split-string (splitter string segment-size)
  (let* ((string-length (length string))
         (out-string
          (make-string (+ string-length
                          (* (floor (/ string-length segment-size))
                             (length splitter))))))
    (do* ((i 0 (1+ i))
          (j 0 (1+ j))
          (newline-p nil (= (mod i segment-size) 0)))
         ((>= i string-length))
      (when newline-p
        (do ((k 0 (1+ k)))
            ((>= k (length splitter)))
          (setf (char out-string j) (char splitter k))
          (incf j)))
      (setf (char out-string j) (char string i)))
    out-string))


;;; Deprecated
(defun make-mail-stream (to &key subject cc bcc reply-to mailer)
  (declare (ignore mailer))
  (make-instance 'mail-output-stream
		 :subject subject
		 :to to
		 :cc cc
		 :bcc bcc
		 :other-headers 
		 `(,(when reply-to `("Reply-To" ,reply-to)))))


(defun read-file (pathname)
  (let ((out (make-string-output-stream)))
    (with-open-file (in pathname)
      (do ((char (read-char in nil 'eof)
		 (read-char in nil 'eof)))
	  ((eq char 'eof) (get-output-stream-string out))
	(princ char out)))))

