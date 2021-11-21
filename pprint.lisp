;;;; * AST pretty printer

(in-package #:lox)

;;; * Pretty printer

;;; We're using generic functions instead of the visitor pattern described in the book.

(defgeneric pretty-print (expr)
  (:documentation "Print AST using Lisp syntax."))

(defmethod pretty-print :around (expr)
  (if (valid? expr)
      (call-next-method)
      "<<INVALID EXPR>>"))

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

(defmethod pretty-print ((expr variable-expr))
  (with-slots (name) expr
    (token-lexeme name)))

(defun parenthesize (name &rest exprs)
  (apply #'str:concat `("("
                        ,name
                        ,@(alexandria:mappend (lambda (e) (list " " (pretty-print e))) exprs)
                        ")")))

(fiveam:def-suite pprint
  :description "Tests for the lox pretty printer")

(fiveam:in-suite pprint)

(test pretty-print-literal-number
  (is (string= "1" (pretty-print (literal-expr :value 1)))))

(test pretty-print-literal-string
  (is (string= "\"hello\"" (pretty-print (literal-expr :value "hello")))))

(test pretty-print-binary
  (is (string= "(+ 1 2)"
               (pretty-print
                (binary-expr
                 :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
                 :left (literal-expr :value 1)
                 :right (literal-expr :value 2))))))

(test pretty-print-unary
  (is (string= "(- 1)"
               (pretty-print
                (unary-expr
                 :operator (make-instance 'token :lexeme "-" :type :minus :literal nil)
                 :right (literal-expr :value 1))))))

(test pretty-print-grouping
  (is (string= "(group 1)"
               (pretty-print
                (grouping-expr :expression (literal-expr :value 1))))))

(test pretty-print-nested
  (is (string= "(+ (- 1) 2)"
               (pretty-print
                (binary-expr
                 :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
                 :left (unary-expr
                        :operator (make-instance 'token :lexeme "-" :type :minus :literal nil)
                        :right (literal-expr :value 1))
                 :right (literal-expr :value 2))))))

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
    (stackify (rpn-print right) (token-lexeme operator))))

(defmethod rpn-print ((expr binary-expr))
  (with-slots (operator left right) expr
    (stackify (rpn-print left) (rpn-print right) (token-lexeme operator))))

(defmethod rpn-print ((expr grouping-expr))
  (with-slots (expression) expr
    (stackify (rpn-print expression))))

(defun stackify (&rest items)
  (str:join " " items))

(test rpn-print-literal
  (is (string= "1" (rpn-print (literal-expr :value 1)))))

(test rpn-print-unary
  (is (string= "1 -"
               (rpn-print
                (unary-expr
                 :operator (make-instance 'token :lexeme "-" :type :minus :literal nil)
                 :right (literal-expr :value 1))))))

(test rpn-print-binary
  (is (string= "1 2 +"
               (rpn-print
                (binary-expr
                 :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
                 :left (literal-expr :value 1)
                 :right (literal-expr :value 2))))))

(test rpn-print-group
  (is (string= "1"
               (rpn-print
                (grouping-expr :expression (literal-expr :value 1))))))

(test rpn-print-nested
  (is (string= "1 2 + 4 3 - *"
               (rpn-print
                (binary-expr
                 :operator (make-instance 'token :lexeme "*" :type :star :literal nil)
                 :left (grouping-expr :expression (binary-expr
                                                   :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
                                                   :left (literal-expr :value 1)
                                                   :right (literal-expr :value 2)))
                 :right (grouping-expr :expression (binary-expr
                                                    :operator (make-instance 'token :lexeme "-" :type :plus :literal nil)
                                                    :left (literal-expr :value 4)
                                                    :right (literal-expr :value 3))))))))
