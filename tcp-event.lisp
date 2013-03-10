(quicklisp:quickload "usocket")

(defvar *master-socket* nil)
(defvar *sockets* '())

(defun tcp-server (host port)
  (setf *master-socket* (usocket:socket-listen host port
					       :reuse-address t
					       :element-type 'character))
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
  (format t "Accepted incoming connection ~a from ~a:~a~%" (usocket:get-peer-name socket) (usocket:get-peer-address socket) (usocket:get-peer-port socket)))

(defun handle-client-input (socket)
  (let ((stream (usocket:socket-stream socket)))
    (handler-case
	 (progn
	   (format stream "~a~%" (reverse (read-line stream)))
	   (force-output stream))
      (condition () (progn 
			(usocket:socket-close socket)
			(setf *sockets* (remove  socket *sockets* :test #'eq)))))))

(defun clean-resources ()
  (mapcar #'usocket:socket-close *sockets*) 
  (usocket:socket-close *master-socket*)
  (setf  *sockets* '()))

; (clean-resources)

(tcp-server "127.0.0.1" 8081)
       