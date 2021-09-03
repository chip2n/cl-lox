(in-package #:lox)

(declaim (optimize (safety 3) (debug 3)))

(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (when (> (length args) 1)
      (format t "Usage: cl-lox [script]~%")
      (exit :code 64))
    (if (= (length args) 1)
        (run-file (car args))
        (run-prompt))))

(defun run-file (path)
  (run (uiop:read-file-string path))
  (when *error* (uiop:quit 65))
  (when *runtime-error* (uiop:quit 70)))

(defun run-prompt ()
  (loop :do
    (fresh-line)
    (format t "> ")
    (force-output)
    (let ((line (read-line)))
      (when (str:empty? line)
        (return))
      (run line)
      (setf *error* nil))))

(defun run (source)
  (setf *error* nil)
  (let* ((tokens (scan-tokens source))
         (stmts (parse tokens)))

    (unless *error* (interpret stmts))))

(defvar *error* nil)
(defvar *runtime-error* nil)

(defgeneric report-error (location msg))

(defmethod report-error ((line string) msg)
  (report line "" msg))

(defmethod report-error ((token token) msg)
  (if (eq (token-type token) :eof)
      (report (token-line token) " at end" msg)
      (report (token-line token) (str:concat " at '" (token-lexeme token) "'") msg)))

(defun report (line where msg)
  (format t "[line ~a] Error~a: ~a~%" line where msg)
  (setf *error* t))

(defun report-runtime-error (c)
  (with-slots (token msg) c
    (format t "~a~%[line ~a]" msg (token-line token)))
  (setf *runtime-error* t))

(defun expect-output (expected source)
  (let* ((output (with-output-to-string (s)
                   (let ((*lox-stdout* s)) (run source)))))
    (is string= expected output)))

(define-test run-simple
    (expect-output "onetrue3" "
print \"one\";
print true;
print 2 + 1;"))

(define-test run-scope
    (expect-output "inner aouter bglobal couter aouter bglobal cglobal aglobal bglobal c" "
var a = \"global a\";
var b = \"global b\";
var c = \"global c\";
{
  var a = \"outer a\";
  var b = \"outer b\";
  {
    var a = \"inner a\";
    print a;
    print b;
    print c;
  }
  print a;
  print b;
  print c;
}
print a;
print b;
print c;
"))

(define-test run-if-conditional-true
    (expect-output "true branch" "
if (true) {
  print \"true branch\";
} else {
  print \"false branch\";
}"))

(define-test run-if-conditional-false
    (expect-output "false branch" "
if (false) {
  print \"true branch\";
} else {
  print \"false branch\";
}"))

(define-test run-if-conditional-no-else
    (expect-output "" "
if (false) {
  print \"true branch\";
}"))

(define-test run-logical-or
    (expect-output "1" "
if (false or true) {
  print \"1\";
} else {
  print \"2\";
}"))

(define-test run-logical-and
    (expect-output "2" "
if (false and true) {
  print \"1\";
} else {
  print \"2\";
}"))
