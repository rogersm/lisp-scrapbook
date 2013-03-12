(quicklisp:quickload "bordeaux-threads")

(defclass sync-stack () 
  ((storage :accessor storage-value
	    :initform '()
	    :initarg :storage)
   (length  :accessor length-value
	    :initform 0)
   (lock    :accessor lock-value
	    :initform (bt:make-lock)))
  (:documentation "A lock syncronised stack"))


(defun §stack (&optional (l '()))
  "Create a new stack with initial content l"
  (make-instance 'sync-stack :storage l))

(defmethod §push (data (x sync-stack))
  "Push data to the stack with the lock taken"
  (bt:with-lock-held ((lock-value x))
    (incf (length-value x))
    (push data (storage-value x))))

(defmethod §pop ((x sync-stack))
  "Pop data from the stach with the lock taken"
  (bt:with-lock-held ((lock-value x))
    (decf (length-value x))
    (pop (storage-value x))))

(defmethod §top ((x sync-stack))
  "View what is on the top of the stack"
  (bt:with-lock-held ((slot-value x 'lock))
    (first (storage-value x))))

(defmethod §isempty ((x sync-stack))
  "Check if the stack is empty"
  (bt:with-lock-held ((lock-value x))
    (zerop (length-value x))))

(defclass thread-notifier (sync-stack)
  ()
  (:documentation "Model a thread sync process"))

(defun §thread-notifier ()
   (make-instance 'thread-notifier)) 

(defmethod §block-until-waken ((x thread-notifier) condition-var lock)
  (declare (ignore x))
  (bt:with-lock-held (lock)
    (bt:condition-wait condition-var lock)))

(defmethod §available-for-work (condition-var (x thread-notifier))
   (§push condition-var x))

(defmethod §remove-ourselves-from-work ((x thread-notifier))
   (§pop x))

(defmethod §wake-up-first-available((x thread-notifier))
  (bt:with-lock-held ((lock-value x))
    (when (> (length-value x) 0)
      (bt:condition-notify (first (storage-value x))))))
