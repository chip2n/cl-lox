;;;; * Interpreter for the Lox language.

(in-package #:lox)

(defvar *lox-stdout* *standard-output*)

;;; * Environment

(defvar *lox-env* (list (make-hash-table :test 'equal)))

(defun env-define (lexeme value)
  (setf (gethash lexeme (car *lox-env*)) value))

(defclass callable ()
  ((arity :type int :initarg :arity
          :reader callable-arity)
   (fun :type function :initarg :fun)))

(defun callable? (obj)
  (eq (type-of obj) 'callable))

(defmacro lox-defun (name args &body body)
  (let ((gargs (gensym)))
    `(let ((*lox-env* (last *lox-env*)))
       (env-define
        ,name
        (make-instance 'callable :arity ,(length args)
                                 :fun (lambda (,gargs)
                                        (destructuring-bind ,args ,gargs
                                          ,@body)))))))

(lox-defun "clock" ()
  (coerce (get-universal-time) 'float))

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

(define-test interpreter)

(define-test interpreter-env
  :parent interpreter
  (let ((*lox-env* nil))
    (env-push)
    (env-define "a" 1)
    (env-push)
    (is = (env-get (make-instance 'token :lexeme "a")) 1)))

(define-test interpreter-env-assign-outer-scope
  :parent interpreter
  (let ((*lox-env* nil))
    (env-push)
    (env-define "a" 1)
    (env-push)
    (env-assign (make-instance 'token :lexeme "a") 2)
    (is = (env-get (make-instance 'token :lexeme "a")) 2)))

;;; * Interpreter

(defun interpret (stmts)
  (handler-case
      (loop :for stmt :in stmts
            :do (evaluate stmt))
    (lox-runtime-error (c) (report-runtime-error c))))

(defgeneric evaluate (expr))

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

(defmethod evaluate ((stmt while-stmt))
  (with-slots (condition body) stmt
    (loop :while (truthy? (evaluate condition))
          :do
             (format t "doing it")
             (evaluate body))))

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
                   (loop :for i :upto (length params) :do
                     (env-define (token-lexeme (nth i params))
                                 (nth i args)))
                   (evaluate body)))))
      (env-define (token-lexeme name) (make-instance 'callable :arity arity :fun fun)))))

(defmethod evaluate ((expr variable-expr))
  (env-get (slot-value expr 'name)))

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
    (unless (= #1=(length args) #2=(callable-arity callee))
      (error 'lox-runtime-error :token (slot-value expr 'paren) :msg (format nil "Expected ~a arguments but got ~a." #2# #1#)))
    (funcall (slot-value callee 'fun) args)))

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
         (< l r))
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
    (let ((result (evaluate value)))
      (env-assign name result)
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
