(quicklisp:quickload "usocket")
(quicklisp:quickload "trivial-utf-8")
(load "/Users/rogersm/dev/lisp-scrapbook/mp.lisp")

(defvar *master-socket* nil) ;; main socket listener
(defvar *sockets* '())       ;; all sockets used by the tcp server, including *master-socket*
(defvar *connections*  (make-hash-table))

(defclass client ()
    ((socket :accessor client-socket
	     :initarg :socket)
     (buffer :initform (make-array 64 :element-type 'unsigned-byte :adjustable t :fill-pointer t)
	     :accessor client-buffer)
     (waiting :initform nil
	      :accessor waitingp)))

(defmethod print-object ((object client) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (socket buffer waiting) object
      (format stream "~a :socket ~a :buffer ~a :waiting" socket buffer waiting))))

(defun collect-input (socket buffer &optional (end-char 13))
  (loop :with stream = (usocket:socket-stream socket)
     :with byte
     :while (listen stream)
     :doing
     (setf byte (read-byte stream))
     (when (= byte end-char)
       (return t))
     (vector-push-extend byte buffer)))

(defmethod client-read ((c client))
    (when (collect-input (client-socket c) (client-buffer c))
      (prog1
	  (trivial-utf-8:utf-8-bytes-to-string (client-buffer c))
	(setf (fill-pointer (client-buffer c)) 0)))) ; reset buffer to 0

(defmacro awhen (cond &body body)
  `(let ((it ,cond))
     (when it
       ,@body)))

(defun handle-client-input (socket)
  (let ((client (gethash socket *connections*)))
    (handler-case
	(awhen (client-read client)
	       (send-text socket "~a~%" (reverse it)))
	(condition (c)
	  (progn
	    (format t "Connection ~a from ~a:~a closed by condition ~a~%" (usocket:get-peer-name socket) (usocket:get-peer-address socket) (usocket:get-peer-port socket) c)
	    (setf *sockets* (remove socket *sockets* :test #'eq))
	    (remhash socket *connections*)
	    (usocket:socket-close socket))))))
					;          (send-to-workers server (curry #'client-on-command client it)))))))

(defun send-text (socket fmt &rest args)
  (let* ((applied-text (apply #'format nil fmt args))
	 (applied-text-utf-8 (trivial-utf-8:string-to-utf-8-bytes applied-text)))
    (loop :with stream = (usocket:socket-stream socket)
       :for octet :across applied-text-utf-8
       :doing (write-byte octet stream)
       :finally (force-output stream))))


(defun curry (fun &rest args1)
  (lambda (&rest args2)
    (apply fun (append args1 args2))))

(defun tcp-server (host port)
  "Create a listener at host:port and manage the new connections incoming
to *master-socket* as well as data reception from the remaining sockets"
  (setf *master-socket* (usocket:socket-listen host port
					       :reuse-address t
					       :element-type 'unsigned-byte)) ;; :element-type 'unsigned-byte
  (setf *sockets* (list *master-socket*))
  (format t "Ready to serve at ~a:~a~%" host port)
  (loop
       (loop :for socket :in (usocket:wait-for-input *sockets* :ready-only t) :do
	   (handler-case
	       (if (and (eq socket *master-socket*)
			(not (null (slot-value socket 'usocket::state)))) ; may happen to wake up the server socket and have status null?
		   ;; the input is from *master-socket* => a new socket
	      (let ((new-socket (usocket:socket-accept socket)))
		(setf *sockets* (nconc *sockets* `(,new-socket)))
		(handle-client-connection new-socket))
	      ;; else: data from an existing socket
	      (if (listen (usocket:socket-stream socket))
		  (handle-client-input socket)
		  (progn
		    (format t "Connection ~a from ~a:~a closed when listening to new socket~%" (usocket:get-peer-name socket) (usocket:get-peer-address socket) (usocket:get-peer-port socket))
		    (setf *sockets* (remove socket *sockets* :test #'eq))
		    (remhash socket *connections*)
		    (usocket:socket-close socket))))))))

(defun handle-client-connection (socket)
  "Add the socket to *the connections*"
  (setf (gethash socket *connections*) (make-instance 'client :socket socket))
  (format t "Accepted incoming connection ~a from ~a:~a~%" (usocket:get-peer-name socket) (usocket:get-peer-address socket) (usocket:get-peer-port socket)))




(defun clean-resources ()
  "Just for debugging, close all the sockets and init global variables to nil"
  (mapcar #'usocket:socket-close *sockets*)
  (setf  *sockets* '()
	 *master-socket* nil
	 *connections* nil))

; (clean-resources)

(tcp-server "127.0.0.1" 8081)
