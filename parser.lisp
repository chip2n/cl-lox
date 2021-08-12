;;;; * Parser for the Lox language.

(in-package #:lox)

;;; * Generating the classes

;;; We're using lisp macros instead of coding a separate application to generate
;;; all required classes as it's done in the book.

(defmacro defast (name parent slots)
  "Define a class (with name `<NAME>-<PARENT>')."
  (let ((sym (alexandria:symbolicate name "-" parent)))
    `(progn
       (defclass ,sym (,parent)
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

(defclass expr () ())

(defmacro defexpr (name slots)
  "Define a class (with name `<NAME>-EXPR')."
  `(defast ,name expr ,slots))

(defexpr assign ((name :type token) (value :type expr)))
(defexpr ternary ((condition :type expr) (true-branch :type expr) (false-branch :type expr)))
(defexpr binary ((left :type expr) (operator :type token) (right :type expr)))
(defexpr grouping ((expression :type expr)))
(defexpr literal ((value)))
(defexpr unary ((operator :type token) (right :type expr)))
(defexpr variable ((name :type token)))

(defclass stmt () ())

(defmacro defstmt (name slots)
  "Define a class (with name `<NAME>-STMT')."
  `(defast ,name stmt ,slots))

(defstmt print ((expression :type expr)))
(defstmt expr ((expression :type expr)))
(defstmt var ((name :type token) (initializer :type expr)))

(defgeneric valid? (expr)
  (:documentation "Check if expression is valid.")
  (:method (expr) t))

(defmethod valid? ((expr binary-expr))
  (slot-value expr 'left))

(defgeneric ast= (o1 o2))

(defmethod ast= ((o1 list) (o2 list))
  (or (eql o1 o2)
      (and (ast= (car o1) (car o2))
           (ast= (cdr o1) (cdr o2)))))

(defmethod ast= ((o1 stmt) (o2 stmt))
  (stmt= o1 o2))

(defmethod ast= ((o1 expr) (o2 expr))
  (expr= o1 o2))

(defgeneric stmt= (s1 s2))

(defmethod stmt= ((s1 print-stmt) (s2 print-stmt))
  (expr= (slot-value s1 'expression) (slot-value s2 'expression)))

(defmethod stmt= ((s1 expr-stmt) (s2 expr-stmt))
  (expr= (slot-value s1 'expression) (slot-value s2 'expression)))

(defgeneric expr= (e1 e2)
  (:method (e1 e2) (eql e1 e2)))

(defmethod expr= ((e1 ternary-expr) (e2 ternary-expr))
  (and
   (expr= (slot-value e1 'condition) (slot-value e2 'condition))
   (expr= (slot-value e1 'true-branch) (slot-value e2 'true-branch))
   (expr= (slot-value e1 'false-branch) (slot-value e2 'false-branch))))

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
   (token= (slot-value e1 'operator) (slot-value e2 'operator))
   (expr= (slot-value e1 'right) (slot-value e2 'right))))

(defmethod print-object ((obj expr) out)
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
  (let ((rules (reverse (remove-duplicates (mapcar #'car *grammars*)))))
    `(labels ,(mapcar (lambda (rule) `(,rule () (progn ,@(cdr (assoc rule *grammars*))))) rules)
       ,@body)))

;; TODO Clean up duplicated forms
(defmacro defgrammar (name &body body)
  `(eval-when
       (:compile-toplevel
        :load-toplevel
        :execute)
       (push (cons ',name ',body) *grammars*)))

(defmacro expand-parse-binary (rule &rest types)
  `(let ((expr (handler-case (,rule)
                 (lox-parse-error () nil))))
     (loop :while (match ,@types)
           :do (let ((op (previous))
                     (right (,rule)))
                 (setf expr (binary-expr :left expr :operator op :right right))))
     expr))

(defgrammar declaration
  (handler-case
      (if (match :var)
          (var-declaration)
          (statement))
    (lox-parse-error ()
      (synchronize)
      nil)))

(defgrammar var-declaration
  (let ((name (consume :identifier "Expect variable name."))
        (initializer nil))
    (if (match :equal)
        (setf initializer (expression)))
    (consume :semicolon "Expect ';' after variable declaration.")
    (var-stmt :name name :initializer initializer)))

(defgrammar statement
  (if (match :print)
      (print-statement)
      (expr-statement)))

(defgrammar print-statement
  (let ((value (comma)))
    (consume :semicolon "Expect ';' after value.")
    (print-stmt :expression value)))

(defgrammar expr-statement
  (let ((expr (comma)))
    (consume :semicolon "Expect ';' after value.")
    (expr-stmt :expression expr)))

(defgrammar comma
  (expand-parse-binary expression :comma))

(defgrammar expression
  (assignment))

(defgrammar assignment
  (let ((expr (equality)))
    (if (match :equal)
        (let ((equals (previous))
              (value (assignment)))
          (if (eq (type-of expr) 'variable-expr)
              (assign-expr :name (slot-value expr 'name) :value value)
              (report-error equals "Invalid assignment target.")))
        expr)))

(defgrammar ternary
  (let ((expr (equality)))
    (if (match :question-mark)
        (let ((true-branch (expression)))
          (unless (match :colon)
            (error "No matching : to ? in ternary operator."))
          (let ((false-branch (expression)))
            (ternary-expr :condition expr :true-branch true-branch :false-branch false-branch)))
        expr)))

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
    ((match :identifier)
     (variable-expr :name (previous)))
    (t (throw-error (peek) "Expect expression."))))

;; TODO Can we re-expand this after adding/changing the defgrammars?
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
      (with-grammar
        (loop :until (at-end?) :collect (declaration))))))

(define-test parser)

(define-test parser-comma
  :parent parser
  (is ast=
      (parse (scan-tokens "1 + 2, 3 + 4;"))
      (list
       (expr-stmt :expression (binary-expr
                               :operator (make-instance 'token :lexeme "," :type :comma :literal nil)
                               :left (binary-expr
                                      :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
                                      :left (literal-expr :value 1)
                                      :right (literal-expr :value 2))
                               :right (binary-expr
                                       :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
                                       :left (literal-expr :value 3)
                                       :right (literal-expr :value 4)))))))

(define-test parser-unary
  :parent parser
  (is ast=
      (parse (scan-tokens "-1;"))
      (list
       (expr-stmt :expression
                  (unary-expr :operator (make-instance 'token :lexeme "-" :type :minus :literal nil)
                              :right (literal-expr :value 1))))))

(define-test parser-ternary
  :parent parser
  (is ast=
      (parse (scan-tokens "1 < 2 ? 3 : 4;"))
      (list
       (expr-stmt :expression
                  (ternary-expr
                   :condition (binary-expr
                               :operator (make-instance 'token :lexeme "<" :type :less :literal nil)
                               :left (literal-expr :value 1)
                               :right (literal-expr :value 2))
                   :true-branch (literal-expr :value 3)
                   :false-branch (literal-expr :value 4))))))

(define-test parser-binary-lhs-missing
  :parent parser
  (is ast=
      (parse (scan-tokens "+ 1;"))
      (list
       (expr-stmt :expression (binary-expr
                               :operator (make-instance 'token :lexeme "+" :type :plus :literal nil)
                               :left nil
                               :right (literal-expr :value 1))))))
