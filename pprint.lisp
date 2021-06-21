;;;; * AST pretty printer

(in-package #:lox)

;;; * Pretty printer

;;; We're using generic functions instead of the visitor pattern described in the book.

(defgeneric pretty-print (expr)
  (:documentation "Print AST using Lisp syntax."))

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

;;; * Reverse polish notation printer

(defgeneric rpn-print (expr)
  (:documentation "Print AST using Reverse Polish notation."))

(defmethod rpn-print ((expr literal-expr))
  (with-slots (value) expr
    (if value
        (write-to-string value)
        "nil")))

(defmethod rpn-print ((expr unary-expr))
  (with-slots (operator right) expr
    (stackify (rpn-print right) (token-lexeme operator))
    ;; (apply #'str:concat `(,(rpn-print right) " " ,(token-lexeme operator)))
    ))

(defmethod rpn-print ((expr binary-expr))
  (with-slots (operator left right) expr
    (stackify (rpn-print left) (rpn-print right) (token-lexeme operator))))

(defmethod rpn-print ((expr grouping-expr))
  (with-slots (expression) expr
    (stackify (rpn-print expression))))

(defun stackify (&rest items)
  (str:join " " items)
  ;; (apply #'str:concat `(,@(alexandria:mappend (lambda (e) (list (rpn-print e) " ")) exprs)
  ;;                       ,(rpn-print right) " " ,(token-lexeme operator)))
  )


(define-test rpn-print)

(define-test rpn-print-literal
  :parent rpn-print
  (is string= "1"
      (rpn-print (make-instance 'literal-expr :value 1))))

(define-test rpn-print-unary
  :parent rpn-print
  (is string= "1 -"
      (rpn-print (make-instance 'unary-expr
                                :operator (make-instance 'token :lexeme "-" :type :minus :literal nil)
                                :right (make-instance 'literal-expr :value 1)))))

(define-test rpn-print-binary
  :parent rpn-print
  (is string= "1 2 +"
      (rpn-print
       (make-instance 'binary-expr
                      :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
                      :left (make-instance 'literal-expr :value 1)
                      :right (make-instance 'literal-expr :value 2)))))

(define-test rpn-print-group
  :parent rpn-print
  (is string= "1"
      (rpn-print
       (make-instance 'grouping-expr :expression (make-instance 'literal-expr :value 1)))))

(define-test rpn-print-nested
  :parent rpn-print
  (is string= "1 2 + 4 3 - *"
      (rpn-print
       (make-instance 'binary-expr
                      :operator (make-instance 'token :lexeme "*" :type :star :literal nil)
                      :left (make-instance 'grouping-expr
                                           :expression (make-instance 'binary-expr
                                                                      :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
                                                                      :left (make-instance 'literal-expr :value 1)
                                                                      :right (make-instance 'literal-expr :value 2)))
                      :right (make-instance 'grouping-expr
                                            :expression (make-instance 'binary-expr
                                                                       :operator (make-instance 'token :lexeme "-" :type :plus :literal nil)
                                                                       :left (make-instance 'literal-expr :value 4)
                                                                       :right (make-instance 'literal-expr :value 3)))))))
