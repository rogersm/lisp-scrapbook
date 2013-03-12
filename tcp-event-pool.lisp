(quicklisp:quickload "usocket")
(load "/Users/rogersm/dev/lisp-scrapbook/mp.lisp")

(defvar *master-socket* nil) ;; main socket listener
(defvar *sockets* '())       ;; all sockets used by the tcp server, including *master-socket*
(defvar *socket-to-client*  (make-hash-table))

(defclass tcp-client
    ((buffer 

      ((buffer-len

	((waiting-status


(defun tcp-server (host port)
  "Create a listener at host:port and manage the new connections incoming 
to *master-socket* as well as data reception from the remaining sockets"
  (setf *master-socket* (usocket:socket-listen host port
					       :reuse-address t
					       :element-type 'character)) ;; :element-type 'unsigned-byte
  (setf *sockets* (list *master-socket*))
  (format t "Ready to serve at ~a:~a~%" host port)
  (loop
       (loop :for socket :in (usocket:wait-for-input *sockets* :ready-only t) :do
	  (if (eq socket *master-socket*)
	      ;; the input is from *master-socket* => a new socket
	      (let ((new-socket (usocket:socket-accept socket)))
		(setf *sockets* (nconc *sockets* `(,new-socket)))
		(handle-client-connection new-socket))
	      ;; else: data from an existing socket
	      (handle-client-input socket)))))

(defun handle-client-connection (socket)
  "Log the reception of a new client"
  (format t "Accepted incoming connection ~a from ~a:~a~%" (usocket:get-peer-name socket) (usocket:get-peer-address socket) (usocket:get-peer-port socket)))

(defun handle-client-input (socket)
  "Read data from incoming socket and return it reversed. If the connection is unwrittable
close socket and remove socket from *sockets*"
  (let ((stream (usocket:socket-stream socket)))
    (handler-case
	(read-and-reverse (stream))	  
      (condition () 
	(progn
	  (format t "Connection ~a from ~a:~a closed~%" (usocket:get-peer-name socket) (usocket:get-peer-address socket) (usocket:get-peer-port socket)) 
	  (setf *sockets* (remove socket *sockets* :test #'eq))
	  (usocket:socket-close socket))))))

(defun read-and-reverse (stream)
  (format stream "~a~%" (reverse (read-line stream)))
  (force-output stream))


(defun clean-resources ()
  "Just for debugging, close all the sockets and init global variables to nil"
  (mapcar #'usocket:socket-close *sockets*) 
  (usocket:socket-close *master-socket*)
  (setf  *sockets* '()
	 *master-socket* nil))

; (clean-resources)

(tcp-server "127.0.0.1" 8081)
       