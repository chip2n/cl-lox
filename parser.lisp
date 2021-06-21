;;;; * Parser for the Lox language.

(in-package #:lox)

(defclass expr () ())
(defclass binary-expr (expr)
  ((left :type expr :initarg :left)
   (operator :type token :initarg :token)
   (right :type expr :initarg :right)))

;;; * Generating the classes

;;; We're using lisp macros instead of coding a separate application to generate
;;; all required classes as it's done in the book.

(defmacro defexpr (name slots)
  "Define a class (with name `<NAME>-EXPR')."
  `(defclass ,(alexandria:symbolicate name "-EXPR") (expr)
     ,(mapcar (lambda (slot) `(,(car slot)
                               ,@(when (cadr slot) (list :type (caddr slot)))
                               :initarg
                               ,(alexandria:make-keyword (car slot)))) slots)))

(defexpr binary ((left :type expr) (operator :type token) (right :type expr)))
(defexpr grouping ((expression :type expr)))
(defexpr literal ((value)))
(defexpr unary ((operator :type token) (right :type expr)))
