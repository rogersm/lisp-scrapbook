;;; -*- Mode: Lisp; Package: USER -*-
;;;
;;; PPMX - pretty prints a macro expansion
;;;
;;; From the book "Common Lisp:  A Gentle Introduction to
;;;      Symbolic Computation" by David S. Touretzky.
;;; The Benjamin/Cummings Publishing Co., 1990.
;;;
;;; Example of use:  (ppmx (incf a))
;;; http://www.cs.cmu.edu/~dst/Lisp/ppmx.lisp (2013-03-21)

(defmacro ppmx (form)
  "Pretty prints the macro expansion of FORM."
  `(let* ((exp1 (macroexpand-1 ',form))
	  (exp (macroexpand exp1))
	  (*print-circle* nil))
     (cond ((equal exp exp1)
	    (format t "~&Macro expansion:")
	    (pprint exp))
	   (t (format t "~&First step of expansion:")
	      (pprint exp1)
	      (format t "~%~%Final expansion:")
	      (pprint exp)))
     (format t "~%~%")
     (values)))


;;;
;;; WITH-GEMSYS - this macro is useful for writing other macros
;;;
;;; From the book "Practical Common Lisp" by Peter Seibel
;;; Similar to its namesake from Paul Graham's book “On Lisp”.
;;;
;;; Example of use (with-gensyms (start mid end) ...)
;;; http://www.gigamonkeys.com/book/macros-defining-your-own.html (2013-03-21)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

;;;
;;; ONCE-ONLY - this macro is used to generate code that evaluates
;;; certain macro arguments once only and in a particular order
;;;
;;; From the book "Practical Common Lisp" by Peter Seibel
;;;
;;;
;;; Example of use (once-only (start end) ... )
;;; http://www.gigamonkeys.com/book/macros-defining-your-own.html (2013-03-21)

(defmacro once-only ((&rest names) &body body)
  (let ((gensyms (loop for n in names collect (gensym))))
    `(let (,@(loop for g in gensyms collect `(,g (gensym))))
      `(let (,,@(loop for g in gensyms for n in names collect ``(,,g ,,n)))
        ,(let (,@(loop for n in names for g in gensyms collect `(,n ,g)))
           ,@body)))))