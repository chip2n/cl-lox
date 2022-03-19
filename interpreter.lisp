;;;; * Interpreter for the Lox language.

(in-package #:lox)

(defvar *lox-stdout* *standard-output*)

;;; * Environment

(defvar *lox-env* (list (make-hash-table :test 'equal)))
(defvar *lox-locals* (make-hash-table))

(defmacro with-new-env (&body body)
  `(let ((*lox-env* (cons (make-hash-table :test 'equal) *lox-env*)))
     ,@body))

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
  ((name :type string :initarg :name)
   (superclass :type lox-class :initarg :superclass)
   (methods :type hash-table :initarg :methods)))

(defmethod call ((callable lox-class) args)
  (let ((instance (make-instance 'lox-instance :class callable))
        (initializer (lox-find-method callable "init")))
    (when initializer
      (call (bind initializer instance) args))
    instance))

(defmethod arity ((callable lox-class))
  (let ((initializer (lox-find-method callable "init")))
    (if initializer
        (arity initializer)
        0)))

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
  (with-slots (class) obj
    (let ((lexeme (token-lexeme name)))
      (multiple-value-bind (value exists?) (gethash lexeme (slot-value obj 'fields))
        (if exists?
            value
            (let ((method (lox-find-method class lexeme)))
              (if method
                  (bind method obj)
                  (error 'lox-runtime-error :token name :msg (format nil "Undefined property '~a'." lexeme)))))))))

(defun bind (method instance)
  (with-slots (arity fun closure initializer?) method
    (let ((closure
            (let ((*lox-env* closure))
              (with-new-env
                (env-define "this" instance)
                *lox-env*))))
      (make-instance 'lox-function
                     :arity arity
                     :fun (lambda (args)
                            (let () ;; ((*lox-env* closure))
                              (funcall fun args)
                              ;; Return `this' if this function is an initializer
                              (when initializer?
                                (env-get-at 0 "this"))))
                     :closure closure
                     :initializer? initializer?))))

(defun lox-find-method (class name)
  (with-slots (superclass methods) class
    (or (gethash name methods)
        (and superclass (lox-find-method superclass name)))))

(defgeneric set-property (obj name value)
  (:documentation "Set property of object")
  (:method (obj name value)
    (error 'lox-runtime-error
           :token name
           :msd "Only instances have fields.")))

(defmethod set-property ((obj lox-instance) name value)
  (let ((lexeme (token-lexeme name)))
    (setf
     (gethash lexeme (slot-value obj 'fields))
     value)))

(defclass lox-function (lox-callable)
  ((arity :type int :initarg :arity)
   (fun :type function :initarg :fun)
   (closure :type list :initarg :closure)
   (initializer? :type boolean :initarg :initializer? :initform nil)))

(defmethod call ((callable lox-function) args)
  (handler-case
      (let ((*lox-env* (slot-value callable 'closure)))
        (funcall (slot-value callable 'fun) args))
    (lox-return (c)
      (if (slot-value callable 'initializer?)
          (let ((*lox-env* (slot-value callable 'closure)))
            (env-get-at 0 "this"))
          (lox-return-value c)))))

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
  (with-slots (name superclass methods) stmt
    (let ((superclass-result nil))
      (when superclass
        (setf superclass-result (evaluate superclass))
        (unless (typep superclass-result 'lox-class)
          (error 'lox-runtime-error
                 :token (slot-value superclass 'name)
                 :msg "Superclass must be a class.")))
      (env-define (token-lexeme name) nil)

      (flet ((create-methods ()
               (let ((methods-table (make-hash-table :test 'equal)))
                 (loop for method in methods
                       for method-lexeme = (slot-value (slot-value method 'name) 'lexeme)
                       do (setf (gethash method-lexeme methods-table)
                                (lox-function-create method :initializer? (string-equal method-lexeme "init"))))
                 methods-table)))

        (let ((methods-table
                (if superclass
                    (with-new-env
                      (env-define "super" superclass-result)
                      (create-methods))
                    (create-methods))))
          (env-assign name (make-instance 'lox-class
                                          :name (token-lexeme name)
                                          :superclass superclass-result
                                          :methods methods-table)))))
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
  (with-slots (name) stmt
    (env-define (token-lexeme name) (lox-function-create stmt))))

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

(defmethod evaluate ((expr set-expr))
  (let ((object (evaluate (slot-value expr 'object))))
    (if (typep object 'lox-instance)
        (let ((value (evaluate (slot-value expr 'value))))
          (set-property object (slot-value expr 'name) value)
          value)
        (error 'lox-runtime-error :token (slot-value expr 'name) :msg "Only instances have fields."))))

(defmethod evaluate ((expr super-expr))
  (let* ((distance (gethash expr *lox-locals*))
         (superclass (env-get-at distance "super"))
         (object (env-get-at (- distance 1) "this"))
         (method (lox-find-method superclass (token-lexeme (slot-value expr 'method)))))
    (unless method
      (error 'lox-runtime-error (slot-value expr method)
             (format nil "Undefined property '~a'." (token-lexeme (slot-value expr 'method)))))
    (bind method object)))

(defmethod evaluate ((expr this-expr))
  (lookup-var (slot-value expr 'keyword) expr))

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
  (nth distance *lox-env*))

(defun env-assign-at (distance name value)
  (setf (gethash (token-lexeme name) (ancestor distance))
        value))

(defun lox-function-create (fun-stmt &key initializer?)
  (with-slots (name params body) fun-stmt
    (let* ((arity (length params))
           (fun (lambda (args)
                  (with-new-env
                    (loop :for i :upto (1- (length params)) :do
                      (env-define (token-lexeme (nth i params))
                                  (nth i args)))
                    (evaluate body)))))
      (make-instance 'lox-function
                     :arity arity
                     :fun fun
                     :closure *lox-env*
                     :initializer? initializer?))))
