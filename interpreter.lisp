;;;; * Interpreter for the Lox language.

(in-package #:lox)

(defvar *lox-stdout* *standard-output*)

(define-condition lox-runtime-error (error)
  ((token :type token)
   (msg :type string)))

(defun interpret (stmts)
  (handler-case
      (loop :for stmt :in stmts
            :do (evaluate stmt))
    (lox-runtime-error (c) (report-runtime-error c))))

(defgeneric evaluate (expr))

(defmethod evaluate ((stmt expr-stmt))
  (evaluate (slot-value stmt 'expression)))

(defmethod evaluate ((stmt print-stmt))
  (let ((value (evaluate (slot-value stmt 'expression))))
    (princ (stringify value) *lox-stdout*)))

(defmethod evaluate ((expr literal-expr))
  (slot-value expr 'value))

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
           (t (error 'lox-runtime-error operator "Operands must be two numbers or two strings."))))
        (:slash
         (check-number-operands operator l r)
         (/ l r))
        (:star
         (check-number-operands operator l r)
         (* l r))))))

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
