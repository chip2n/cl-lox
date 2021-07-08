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

(defgeneric expr= (e1 e2))

(defmethod expr= ((e1 binary-expr) (e2 binary-expr))
  (and
   (expr= (slot-value e1 'left) (slot-value e2 'left))
   (token= (slot-value e1 'operator) (slot-value e2 'operator))
   (expr= (slot-value e1 'right) (slot-value e2 'right))))

(defmethod expr= ((e1 grouping-expr) (e2 grouping-expr))
  (expr= (slot-value e1 'expression) (slot-value e2 'expression)))

(defmethod expr= ((e1 literal-expr) (e2 literal-expr))
  (eql (slot-value e1 'value) (slot-value e2 'value)))

(defmethod expr= ((e1 unary-expr) (e2 unary-expr))
  (and
   (token= (slot-value e1 'token) (slot-value e2 'token))
   (expr= (slot-value e1 'right) (slot-value e2 'right))))

(defmethod print-object ((obj binary-expr) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (pretty-print obj))))

;;; * Parser

(eval-when
    (:compile-toplevel
     :load-toplevel
     :execute)
  (defvar *grammars* nil))

(define-condition lox-parse-error (error) ())

(defmacro with-grammar (&body body)
  (let ((rules (remove-duplicates (reverse (mapcar #'car *grammars*)))))
    `(labels ,(mapcar (lambda (rule) `(,rule () (progn ,@(cdr (assoc rule *grammars*))))) rules)
       ,@body)))

(defmacro defgrammar (name &body body)
  `(eval-when
       (:compile-toplevel
        :load-toplevel
        :execute)
       (push (cons ',name ',body) *grammars*)))

(defmacro expand-parse-binary (rule &rest types)
  `(let ((expr (,rule)))
     (loop :while (match ,@types)
           :do (let ((op (previous))
                     (right (,rule)))
                 (setf expr (binary-expr :left expr :operator op :right right))))
     expr))

(defgrammar comma
  (expand-parse-binary expression :comma))

(defgrammar expression
  (equality))

(defgrammar equality
  (expand-parse-binary comparison :bang-equal :equal-equal))

(defgrammar comparison
  (expand-parse-binary term :greater :greater-equal :less :less-equal))

(defgrammar term
  (expand-parse-binary factor :minus :plus))

(defgrammar factor
  (expand-parse-binary unary :slash :star))

(defgrammar unary
  (if (match :bang :minus)
      (let ((op (previous))
            (right (unary)))
        (unary-expr :operator op :right right))
      (primary)))

(defgrammar primary
  (cond
    ((match :false) (literal-expr :value 'false))
    ((match :true) (literal-expr :value t))
    ((match :nil) (literal-expr :value nil))
    ((match :number :string) (literal-expr :value (token-literal (previous))))
    ((match :left-paren)
     (let ((expr (expression)))
       (consume :right-paren "Expect ')' after expression.")
       (grouping-expr :expression expr)))
    (t
     (throw-error (peek) "Expect expression."))))

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
               (error 'lox-parse-error))
             (consume (type msg)
               (if (check type)
                   (advance)
                   (throw-error (peek) msg)))
             (synchronize ()
               (advance)
               ;; Discard tokens until we think we've found a statement boundary
               ;; TODO Do this check in a better way :D
               (loop :while (not (at-end?))
                     :do (if (or (eq (token-type (previous)) :semicolon)
                                 (eq (token-type (peek)) :class)
                                 (eq (token-type (peek)) :fun)
                                 (eq (token-type (peek)) :var)
                                 (eq (token-type (peek)) :for)
                                 (eq (token-type (peek)) :if)
                                 (eq (token-type (peek)) :while)
                                 (eq (token-type (peek)) :print)
                                 (eq (token-type (peek)) :return))
                             (return)
                             (advance)))))
      ;; TODO handle lox-parse-error condition
      (with-grammar (comma)))))

(define-test parser)

(define-test parser-1
  :parent parser
  (is expr=
      (parse (scan-tokens "1 + 2, 3 + 4"))
      (binary-expr
       :operator (make-instance 'token :lexeme "," :type :comma :literal nil)
        :left (binary-expr
               :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
               :left (literal-expr :value 1)
               :right (literal-expr :value 2))
        :right (binary-expr
               :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
               :left (literal-expr :value 3)
               :right (literal-expr :value 4)))))
