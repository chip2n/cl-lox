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

    (unless *error*
      (resolve stmts)

      ;; Resolver can also report errors
      (unless *error*
        (interpret stmts)))))

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
    (fiveam:is (string= expected output))))

(defmacro test-interpreter (name expected &body body)
  `(test ,name
     (with-clean-interpreter
       (expect-output ,expected (progn ,@body)))))

(fiveam:def-suite run
  :description "Tests for the top level run function")

(fiveam:in-suite run)

(test-interpreter run-simple
    "onetrue3" "
print \"one\";
print true;
print 2 + 1;")

(test-interpreter run-scope
    "inner aouter bglobal couter aouter bglobal cglobal aglobal bglobal c" "
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
")

(test-interpreter run-if-conditional-true
    "true branch" "
if (true) {
  print \"true branch\";
} else {
  print \"false branch\";
}")

(test-interpreter run-if-conditional-false
    "false branch" "
if (false) {
  print \"true branch\";
} else {
  print \"false branch\";
}")

(test-interpreter run-if-conditional-no-else
    "" "
if (false) {
  print \"true branch\";
}")

(test-interpreter run-logical-or
    "1" "
if (false or true) {
  print \"1\";
} else {
  print \"2\";
}")

(test-interpreter run-logical-and
    "2" "
if (false and true) {
  print \"1\";
} else {
  print \"2\";
}")

(test-interpreter run-while
    "012" "
var i = 0;
while (i < 3) {
  print i;
  i = i + 1;
}
")

(test-interpreter run-for
    "012" "
for (var i = 0; i < 3; i = i + 1) {
  print i;
}
")

(test-interpreter run-fun
    "123" "
fun count(n) {
  if (n > 1) count(n - 1);
  print n;
}
count(3);
")

(test-interpreter run-fun2
    "Hi, Dear Reader!" "
fun sayHi(first, last) {
  print \"Hi, \" + first + \" \" + last + \"!\";
}

sayHi(\"Dear\", \"Reader\");
")

(test-interpreter run-fun-return
    "01123581321345589144233377610987159725844181" "
fun fib(n) {
  if (n <= 1) return n;
  return fib(n - 2) + fib(n - 1);
}

for (var i = 0; i < 20; i = i + 1) {
  print fib(i);
}")

(test-interpreter run-fun-nested
    "12" "
fun makeCounter() {
  var i = 0;
  fun count() {
    i = i + 1;
    print i;
  }

  return count;
}
var counter = makeCounter();
counter();
counter();")

(test-interpreter run-fun-scoping
    "globalglobal" "
var a = \"global\";
{
  fun showA() {
    print a;
  }
  showA();
  var a = \"block\";
  showA();
}")

;; TODO I want to be able to check for errors (using *error*? using stderr?)
(test-interpreter run-variable-local-redeclaration
    "" "
fun bad() {
  var a = \"first\";
  var a = \"second\";
}")

(test-interpreter run-class-print
    "DevonshireCream" "
class DevonshireCream {
  serveOn() {
    return \"Scones\";
  }
}

print DevonshireCream;
")

(test-interpreter run-class-instantiate
    "Bagel instance" "
class Bagel {}
var bagel = Bagel();
print bagel;
")
