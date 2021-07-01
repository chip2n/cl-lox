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

;;; * Parser

(defmacro with-grammar (&body body)
  (let ((rules '(expression equality comparison term factor unary primary)))
    `(labels ,(mapcar (lambda (rule) `(,rule () ,(funcall rule))) rules)
       ,@body)))

(define-condition parse-errer (error) ())

;; TODO make a defgrammar macro

(eval-when
    (:compile-toplevel
     :load-toplevel
     :execute)
  (defun expand-parse-binary (rule &rest types)
    `(let ((expr (,rule)))
       (loop :while (match ,@types)
             :do (let ((op (previous))
                       (right (,rule)))
                   (setf expr (binary-expr :left expr :operator op :right right))))
       expr))

  (defun expression ()
    `(equality))

  (defun equality ()
    (expand-parse-binary 'comparison :bang-equal :equal-equal))

  (defun comparison ()
    (expand-parse-binary 'term :greater :greater-equal :less :less-equal))

  (defun term ()
    (expand-parse-binary 'factor :minus :plus))

  (defun factor ()
    (expand-parse-binary 'unary :slash :star))

  (defun unary ()
    `(if (match :bang :minus)
         (let ((op (previous))
               (right (unary)))
           (unary-expr :operator op :right right))
         (primary)))

  (defun primary ()
    `(cond
       ((match :false) (literal-expr :value 'false))
       ((match :true) (literal-expr :value t))
       ((match :nil) (literal-expr :value nil))
       ((match :number :string) (literal-expr :value (token-literal (previous))))
       ((match :left-paren)
        (let ((expr (expression)))
          (consume :right-paren "Expect ')' after expression.")
          (grouping-expr :expression expr)))
       (t
        (throw-error (peek) "Expect expression.")))))

;; expression     → equality ;
;; equality       → comparison ( ( "!=" | "==" ) comparison )* ;
;; comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;
;; term           → factor ( ( "-" | "+" ) factor )* ;
;; factor         → unary ( ( "/" | "*" ) unary )* ;
;; unary          → ( "!" | "-" ) unary
;;                | primary ;
;; primary        → NUMBER | STRING | "true" | "false" | "nil"
;;                | "(" expression ")" ;

;; In C, a block is a statement form that allows you to pack a series of statements
;; where a single one is expected. The comma operator is an analogous syntax for
;; expressions. A comma-separated series of expressions can be given where a single
;; expression is expected (except inside a function call’s argument list). At
;; runtime, the comma operator evaluates the left operand and discards the
;; result. Then it evaluates and returns the right operand.

;; Add support for comma expressions. Give them the same precedence and
;; associativity as in C. Write the grammar, and then implement the necessary
;; parsing code.


;; Below unary?
;; comma         → expression ( "," expression )* ;

(defun parse (tokens)
  (let ((tokens (make-array (length tokens) :initial-contents tokens))
        (current 0))
    (labels ((match (&rest types)
               (loop :for type :in types
                     :do (when (check type)
                           (advance)
                           (return t))))
             (check (type)
               (unless (at-end?)
                 (eq (token-type (peek)) type)))
             (advance ()
               (unless (at-end?)
                 (incf current))
               (previous))
             (at-end? ()
               (eq (token-type (peek)) :eof))
             (peek ()
               (aref tokens current))
             (previous ()
               (aref tokens (- current 1)))
             (throw-error (token msg)
               (report-error token msg)
               (error 'parse-error))
             (consume (type msg)
               (if (check type)
                   (advance)
                   (throw-error (peek) msg)))
             (synchronize ()
               (advance)
               ;; Discard tokens until we think we've found a statement boundary
               ;; TODO Do this check in a better way :D
               (loop :while (not (at-end?))
                     :do (unless (or (eq (token-type (previous)) :semicolon)
                                     (eq (token-type (peek)) :class)
                                     (eq (token-type (peek)) :fun)
                                     (eq (token-type (peek)) :var)
                                     (eq (token-type (peek)) :for)
                                     (eq (token-type (peek)) :if)
                                     (eq (token-type (peek)) :while)
                                     (eq (token-type (peek)) :print)
                                     (eq (token-type (peek)) :return))
                           (advance)))))
      ;; TODO handle parse-error condition
      (with-grammar
        (expression)))))
