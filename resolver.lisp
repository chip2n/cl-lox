;;;; * Resolver for the Lox language.

(in-package #:lox)

(defvar *scopes* nil)

;; TODO can we statically type this enum?
(defvar *current-function* :none)

(defmacro with-new-scope (&body body)
  `(let ((*scopes* (cons (make-hash-table :test 'equal) *scopes*)))
     ,@body))

(defgeneric resolve (stmt))

(defmethod resolve ((stmt block-stmt))
  (with-new-scope
   (resolve (slot-value stmt 'stmts)))
  nil)

(defmethod resolve ((stmts cons))
  (loop for stmt in stmts do (resolve stmt)))

(defmethod resolve ((stmt var-stmt))
  (with-slots (name initializer) stmt
    (var-declare name)
    (when initializer
      (resolve initializer))
    (var-define name)
    nil))

(defmethod resolve ((stmt class-stmt))
  (with-slots (name) stmt
    (var-declare name)
    (var-define name))
  nil)

(defmethod resolve ((expr variable-expr))
  (with-slots (name) expr
    (when (and *scopes* (var-ready? name))
      (report-error name "Can't read local variable in its own initializer."))
    (resolve-local expr name)
    nil))

(defmethod resolve ((expr assign-expr))
  (with-slots (value name) expr
    ;; We resolve the expression for the assigned value in case it also contains references to other variables
    (resolve value)
    ;; Resolve the variable that's being assigned to
    (resolve-local expr name)))

(defmethod resolve ((stmt fun-stmt))
  (with-slots (name) stmt
    ;; Declaring and defining at the same time allows the function to refer to itself recursively
    (var-declare name)
    (var-define name)
    (resolve-fun stmt :function)
    nil))

(defun resolve-fun (stmt type)
  (with-new-scope
    (let (*current-function* type)
      (with-slots (params body) stmt
        (loop for param in params do
          (var-declare param)
          (var-define param))
        (resolve body)))))

(defmethod resolve ((stmt expr-stmt))
  (resolve (slot-value stmt 'expression))
  nil)

(defmethod resolve ((stmt if-stmt))
  (with-slots (condition then-branch else-branch) stmt
    (resolve condition)
    (resolve then-branch)
    (when else-branch (resolve else-branch))
    nil))

(defmethod resolve ((stmt print-stmt))
  (resolve (slot-value stmt 'expression))
  nil)

(defmethod resolve ((stmt return-stmt))
  (when (eq *current-function* :none)
    (report-error (slot-value stmt 'keyword) "Can't return from top-level code."))
  (with-slots (value) stmt
    (when value (resolve value)))
  nil)

(defmethod resolve ((stmt while-stmt))
  (with-slots (condition body) stmt
    (resolve condition)
    (resolve body))
  nil)

(defmethod resolve ((expr binary-expr))
  (with-slots (left right) expr
    (resolve left)
    (resolve right))
  nil)

(defmethod resolve ((expr call-expr))
  (with-slots (callee arguments) expr
    (resolve callee)
    (loop for argument in arguments do (resolve arguments)))
  nil)

(defmethod resolve ((expr get-expr))
  (resolve (slot-value expr 'object))
  nil)

(defmethod resolve ((expr grouping-expr))
  (resolve (slot-value expr 'expression))
  nil)

(defmethod resolve ((expr literal-expr))
  nil)

(defmethod resolve ((expr logical-expr))
  (with-slots (left right) expr
    (resolve left)
    (resolve right))
  nil)

(defmethod resolve ((expr unary-expr))
  (resolve (slot-value expr 'right))
  nil)

(defun var-declare (name)
  (when *scopes*
    (let ((scope (car *scopes*)))
      ;; It's probably a mistake to redeclare a variable in the local scope
      (when (var-declared? name scope)
        (report-error name "Already a variable with this name in this scope."))
      ;; Mark it as "not ready yet"
      (setf (gethash (token-lexeme name) scope) nil))))

(defun var-declared? (name scope)
  (nth-value 1 (gethash (token-lexeme name) scope)))

(defun var-define (name)
  (if *scopes*
      ;; Mark it as ready
      (setf (gethash (token-lexeme name) (car *scopes*)) t)))

(defun var-ready? (name)
  (multiple-value-bind (ready? present?)
      (gethash (token-lexeme name) (car *scopes*))
    (and present? (not ready?))))

(defun resolve-local (expr name)
  (with-slots (lexeme) name
    (loop for i from 0 to (1- (length *scopes*))
          for scope = (nth i *scopes*)
          do (when (has-key? lexeme scope)
               (interpreter-resolve expr i)
               (return)))))

(defun has-key? (key hashmap)
  (nth-value 1 (gethash key hashmap)))
