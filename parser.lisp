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

;;; * Pretty printer

;;; We're using generic functions instead of the visitor pattern described in the book.

(defgeneric pretty-print (expr))

(defmethod pretty-print ((expr literal-expr))
  (with-slots (value) expr
    (if value
        (write-to-string value)
        "nil")))

(defmethod pretty-print ((expr unary-expr))
  (with-slots (operator right) expr
    (parenthesize (token-lexeme operator) right)))

(defmethod pretty-print ((expr binary-expr))
  (with-slots (operator left right) expr
    (parenthesize (token-lexeme operator) left right)))

(defmethod pretty-print ((expr grouping-expr))
  (with-slots (expression) expr
    (parenthesize "group" expression)))

(defun parenthesize (name &rest exprs)
  (apply #'str:concat `("("
                        ,name
                        ,@(alexandria:mappend (lambda (e) (list " " (pretty-print e))) exprs)
                        ")")))

;;; * Test cases

(define-test pretty-print)

(define-test pretty-print-literal-number
  :parent pretty-print
  (is string= "1"
      (pretty-print (make-instance 'literal-expr :value 1))))
(define-test pretty-print-literal-string
  :parent pretty-print
  (is string= "\"hello\""
      (pretty-print (make-instance 'literal-expr :value "hello"))))

(define-test pretty-print-binary
  :parent pretty-print
  (is string= "(+ 1 2)"
      (pretty-print
       (make-instance 'binary-expr
                      :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
                      :left (make-instance 'literal-expr :value 1)
                      :right (make-instance 'literal-expr :value 2)))))

(define-test pretty-print-unary
  (is string= "(- 1)"
      (pretty-print
       (make-instance 'unary-expr
                      :operator (make-instance 'token :lexeme "-" :type :minus :literal nil)
                      :right (make-instance 'literal-expr :value 1)))))

(define-test pretty-print-grouping
  (is string= "(group 1)"
      (pretty-print
       (make-instance 'grouping-expr :expression (make-instance 'literal-expr :value 1)))))

(define-test pretty-print-nested
  (is string= "(+ (- 1) 2)"
      (pretty-print
       (make-instance 'binary-expr
                      :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
                      :left (make-instance 'unary-expr
                                           :operator (make-instance 'token :lexeme "-" :type :minus :literal nil)
                                           :right (make-instance 'literal-expr :value 1))
                      :right (make-instance 'literal-expr :value 2)))))
