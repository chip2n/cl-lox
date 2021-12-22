;;;; * Interpreter for the Lox language.

(in-package #:lox)

(defvar *lox-stdout* *standard-output*)

;;; * Environment

(defvar *lox-env* (list (make-hash-table :test 'equal)))
(defvar *lox-locals* (make-hash-table))

(defmacro with-clean-interpreter (&body body)
  `(let ((*lox-env* (list (make-hash-table :test 'equal)))
         (*lox-locals* (make-hash-table)))
     ,@body))

(defun env-define (lexeme value)
  (setf (gethash lexeme (car *lox-env*)) value))

(defclass lox-callable () ()
  (:documentation "Mixin for callables."))

(defun callable? (obj)
  (typep obj 'lox-callable))

(defgeneric call (callable args)
  (:documentation "Call the given callable."))

(defgeneric arity (callable)
  (:documentation "Get the arity of the given callable."))

(defclass lox-class (lox-callable)
  ((name :type string
         :initarg :name)))

(defmethod call ((callable lox-class) args)
  (make-instance 'lox-instance :class callable))

(defmethod arity ((callable lox-class))
  0)

(defmethod print-object ((obj lox-class) out)
  (format out "~a" (slot-value obj 'name)))

(defclass lox-instance ()
  ((class :type lox-class :initarg :class)
   (fields :type hash-table :initform (make-hash-table :test 'equal))))

(defmethod print-object ((obj lox-instance) out)
  (format out "~a instance" (slot-value obj 'class)))

(defgeneric get-property (obj name)
  (:documentation "Get property of object")
  (:method (obj name)
    (error 'lox-runtime-error
           :token name
           :msd "Only instances have properties.")))

(defmethod get-property ((obj lox-instance) name)
  (let ((lexeme (token-lexeme name)))
    (multiple-value-bind (value exists?) (gethash lexeme (slot-value obj 'fields))
      (if exists?
          value
          (error 'lox-runtime-error :token name :msg (format nil "Undefined property '~a'." lexeme))))))

(defclass lox-function (lox-callable)
  ((arity :type int :initarg :arity)
   (fun :type function :initarg :fun)
   (closure :type list :initarg :closure)))

(defmethod call ((callable lox-function) args)
  (handler-case
      (let ((*lox-env* (slot-value callable 'closure)))
        (funcall (slot-value callable 'fun) args))
    (lox-return (c) (lox-return-value c))))

(defmethod arity ((callable lox-function))
  (slot-value callable 'arity))

(defmacro lox-defun (name args &body body)
  (let ((gargs (gensym)))
    `(let ((*lox-env* (last *lox-env*)))
       (env-define
        ,name
        (make-instance 'lox-function
                       :arity ,(length args)
                       :fun (lambda (,gargs)
                              (destructuring-bind ,args ,gargs
                                ,@body))
                       :closure *lox-env*)))))

(lox-defun "clock" ()
  (coerce (get-universal-time) 'float))

(define-condition lox-return (error)
  ((value :type value
          :type expr
          :initarg :value
          :reader lox-return-value)))

(define-condition lox-runtime-error (error)
  ((token :type token
          :initarg :token
          :reader lox-runtime-error-token)
   (msg :type string
        :initarg :msg
        :reader lox-runtime-error-msg))
  (:report (lambda (condition stream)
             (format stream "Lox runtime error on token ~a: ~a"
                     (lox-runtime-error-token condition)
                     (lox-runtime-error-msg condition)))))

(defun env-get (name)
  (let ((lexeme (token-lexeme name)))
    (labels ((lookup (env)
               (unless env
                 (error 'lox-runtime-error :token name :msg (format nil "Undefined variable '~a'" lexeme)))
               (multiple-value-bind (value exists?) (gethash lexeme (car env))
                 (if exists? value (lookup (cdr env))))))
      (lookup *lox-env*))))

(defun env-push ()
  (let ((env (make-hash-table :test 'equal)))
    (push env *lox-env*)
    env))

(defun env-pop ()
  (when (= (length *lox-env*) 1)
    (error "Cannot pop environment - already at global"))
  (pop *lox-env*))

(defmacro with-new-env (&body body)
  `(let ((*lox-env* (cons (make-hash-table :test 'equal) *lox-env*)))
     ,@body))

;; TODO use reader macro to reuse hashmap place
(defun env-assign (name value)
  (let ((lexeme (token-lexeme name)))
    (if (nth-value 1 (gethash lexeme (car *lox-env*)))
        (setf (gethash lexeme (car *lox-env*)) value)
        (if (cdr *lox-env*)
            (let ((*lox-env* (cdr *lox-env*)))
              (env-assign name value))
            (error 'lox-runtime-error :token name :msg (format nil "Undefined variable '~a'" lexeme))))))

(fiveam:def-suite interpreter
  :description "Tests for the lox interpreter")

(fiveam:in-suite interpreter)

(test interpreter-env
  (let ((*lox-env* nil))
    (env-push)
    (env-define "a" 1)
    (env-push)
    (is (= (env-get (make-instance 'token :lexeme "a")) 1))))

(test interpreter-env-assign-outer-scope
  (let ((*lox-env* nil))
    (env-push)
    (env-define "a" 1)
    (env-push)
    (env-assign (make-instance 'token :lexeme "a") 2)
    (is (= (env-get (make-instance 'token :lexeme "a")) 2))))

;;; * Interpreter

(defun interpret (stmts)
  (handler-case
      (loop :for stmt :in stmts
            :do (evaluate stmt))
    (lox-runtime-error (c) (report-runtime-error c))))

(defgeneric evaluate (expr))

(defmethod evaluate ((stmt class-stmt))
  (with-slots (name) stmt
    (env-define (token-lexeme name) nil)
    (env-assign name (make-instance 'lox-class :name (token-lexeme name)))
    nil))

(defmethod evaluate ((stmt var-stmt))
  (let ((value nil))
    (with-slots (initializer name) stmt
      (if initializer
          (setf value (evaluate initializer)))
      (env-define (token-lexeme name) value))
    nil))

(defmethod evaluate ((stmt expr-stmt))
  (evaluate (slot-value stmt 'expression)))

(defmethod evaluate ((stmt if-stmt))
  (with-slots (condition then-branch else-branch) stmt
    (if (truthy? (evaluate condition))
        (evaluate then-branch)
        (when else-branch (evaluate else-branch)))
    nil))

(defmethod evaluate ((stmt print-stmt))
  (let ((value (evaluate (slot-value stmt 'expression))))
    (princ (stringify value) *lox-stdout*)))

(defmethod evaluate ((stmt return-stmt))
  (with-slots (value) stmt
    (let ((v nil))
      (when value (setf v (evaluate value)))
      (error 'lox-return :value v))))

(defmethod evaluate ((stmt while-stmt))
  (with-slots (condition body) stmt
    (loop :while (truthy? (evaluate condition))
          :do (evaluate body))))

;; TODO We can probably just use dynamic variable with let binding (use with-new-env macro)
(defmethod evaluate ((stmt block-stmt))
  (env-push)
  (unwind-protect
       (loop :for s :in (slot-value stmt 'stmts) :do (evaluate s))
    (env-pop)))

(defmethod evaluate ((stmt fun-stmt))
  (with-slots (name params body) stmt
    (let ((arity (length params))
          (fun (lambda (args)
                 (with-new-env
                   (loop :for i :upto (1- (length params)) :do
                     (env-define (token-lexeme (nth i params))
                                 (nth i args)))
                   (evaluate body)))))
      (env-define (token-lexeme name)
                  (make-instance 'lox-function
                                 :arity arity
                                 :fun fun
                                 :closure *lox-env*)))))

(defmethod evaluate ((expr variable-expr))
  (lookup-var (slot-value expr 'name) expr))

(defmethod evaluate ((expr literal-expr))
  (slot-value expr 'value))

(defmethod evaluate ((expr logical-expr))
  (with-slots (left operator right) expr
    (let ((left-result (evaluate left)))
      (if (eq (token-type operator) :or)
          (if (truthy? left-result) left-result (evaluate right))
          (if (not (truthy? left-result)) left-result (evaluate right))))))

(defmethod evaluate ((expr grouping-expr))
  (evaluate (slot-value expr 'expression)))

(defmethod evaluate ((expr unary-expr))
  (with-slots (operator right) expr
    (let ((r (evaluate right)))
      (ecase (token-type operator)
        (:bang (not (truthy? r)))
        (:minus
         (check-number-operand operator r)
         (- r))))))

(defmethod evaluate ((expr call-expr))
  (let ((callee (evaluate (slot-value expr 'callee)))
        (args (loop :for arg :in (slot-value expr 'arguments)
                    :collect (evaluate arg))))
    (unless (callable? callee)
      (error 'lox-runtime-error :token (slot-value expr 'paren) :msg "Can only call functions and classes."))
    (unless (= #1=(length args) #2=(arity callee))
      (error 'lox-runtime-error :token (slot-value expr 'paren) :msg (format nil "Expected ~a arguments but got ~a." #2# #1#)))
    (call callee args)))

(defmethod evaluate ((expr get-expr))
  (let ((object (evaluate (slot-value expr 'object))))
    (get-property object (slot-value expr 'name))))

(defmethod evaluate ((expr binary-expr))
  (with-slots (left operator right) expr
    (let ((l (evaluate left))
          (r (evaluate right)))
      (ecase (token-type operator)
        (:greater
         (check-number-operands operator l r)
         (> l r))
        (:greater-equal
         (check-number-operands operator l r)
         (>= l r))
        (:less
         (check-number-operands operator l r)
         (< l r))
        (:less-equal
         (check-number-operands operator l r)
         (<= l r))
        (:bang-equal (not (equal l r)))
        (:equal-equal (equal l r))
        (:minus
         (check-number-operands operator l r)
         (- l r))
        (:plus
         (cond
           ((and (numberp l) (numberp r)) (+ l r))
           ((and (stringp l) (stringp r)) (str:concat l r))
           (t (error 'lox-runtime-error :token operator :msg "Operands must be two numbers or two strings."))))
        (:slash
         (check-number-operands operator l r)
         (/ l r))
        (:star
         (check-number-operands operator l r)
         (* l r))))))

(defmethod evaluate ((expr assign-expr))
  (with-slots (name value) expr
    (let ((result (evaluate value))
          (distance (gethash expr *lox-locals*)))
      (if distance
          (env-assign-at distance name result)
          (env-globals-assign name result))
      result)))

(defun truthy? (obj)
  (not (or (null obj) (eq obj 'false))))

(defun check-number-operand (operator operand)
  (unless (numberp operand)
    (error 'lox-runtime-error :token operator :msg "Operand must be a number.")))

(defun check-number-operands (operator left right)
  (unless (and (numberp left) (numberp right))
    (error 'lox-runtime-error :token operator :msg "Operands must be numbers.")))

(defun stringify (obj)
  (cond
    ((null obj) "nil")
    ((eq obj 't) "true")
    (t (format nil "~a" obj))))

(defun interpreter-resolve (expr depth)
  (setf (gethash expr *lox-locals*) depth))

(defun lookup-var (name expr)
  (let ((distance (gethash expr *lox-locals*)))
    (if distance
        (env-get-at distance (slot-value name 'lexeme))
        (env-globals-get name))))

(defun env-globals-get (name)
  (let ((*lox-env* (last *lox-env*)))
    (env-get name)))

(defun env-globals-assign (name value)
  (let ((*lox-env* (last *lox-env*)))
    (env-assign name value)))

(defun env-get-at (distance name)
  (gethash name (ancestor distance)))

(defun ancestor (distance)
  (nth distance *lox-env*)
  ;; (serapeum:drop distance *lox-env*)
  )

(defun env-assign-at (distance name value)
  (setf (gethash (token-lexeme name) (ancestor distance))
        value))
