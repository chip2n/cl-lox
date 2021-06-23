;;;; * Parser for the Lox language.

(in-package #:lox)

(defclass expr () ())

;;; * Generating the classes

;;; We're using lisp macros instead of coding a separate application to generate
;;; all required classes as it's done in the book.

(defmacro defexpr (name slots)
  "Define a class (with name `<NAME>-EXPR')."
  (let ((sym (alexandria:symbolicate name "-EXPR")))
    `(progn
       (defclass ,sym (expr)
         ,(mapcar (lambda (slot) `(,(car slot)
                              ,@(when (cadr slot) (list :type (caddr slot)))
                              :initarg
                              ,(alexandria:make-keyword (car slot))))
           slots))
       (defun ,sym (&key ,@(mapcar #'car slots))
         (make-instance ',sym
                        ,@(alexandria:mappend
                           (lambda (slot)
                             `(,(alexandria:make-keyword (car slot)) ,(car slot)))
                           slots))))))

(defexpr binary ((left :type expr) (operator :type token) (right :type expr)))
(defexpr grouping ((expression :type expr)))
(defexpr literal ((value)))
(defexpr unary ((operator :type token) (right :type expr)))
