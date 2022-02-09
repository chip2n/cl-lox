(in-package #:lox)

(declaim (optimize (safety 3) (debug 3)))

(defvar *lox-stderr* *error-output*)

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
  (format *lox-stderr* "[line ~a] Error~a: ~a~%" line where msg)
  (setf *error* t))

(defun report-runtime-error (c)
  (with-slots (token msg) c
    (format t "~a~%[line ~a]" msg (token-line token)))
  (setf *runtime-error* t))

(defun expect-output (expected source)
  (let* ((output (with-output-to-string (s)
                   (let ((*lox-stdout* s)) (run source)))))
    (fiveam:is (string= expected output))))

(defun expect-error (expected source)
  (let* ((output (with-output-to-string (s)
                   (let ((*lox-stderr* s)) (run source)))))
    ;; Ignore newlines at the end for tests
    (fiveam:is (string=
                (str:trim-right expected)
                (str:trim-right output)))))

(defmacro test-interpreter-error (name expected &body body)
  `(test ,name
         (with-clean-interpreter
             (expect-error ,expected (progn ,@body)))))

(defmacro test-interpreter-output (name expected &body body)
  `(test ,name
         (with-clean-interpreter
             (expect-output ,expected (progn ,@body)))))

(fiveam:def-suite run
  :description "Tests for the top level run function")

(fiveam:in-suite run)

(test-interpreter-output run-simple
    "onetrue3" "
print \"one\";
print true;
print 2 + 1;")

(test-interpreter-output run-scope
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

(test-interpreter-output run-if-conditional-true
    "true branch" "
if (true) {
  print \"true branch\";
} else {
  print \"false branch\";
}")

(test-interpreter-output run-if-conditional-false
    "false branch" "
if (false) {
  print \"true branch\";
} else {
  print \"false branch\";
}")

(test-interpreter-output run-if-conditional-no-else
    "" "
if (false) {
  print \"true branch\";
}")

(test-interpreter-output run-logical-or
    "1" "
if (false or true) {
  print \"1\";
} else {
  print \"2\";
}")

(test-interpreter-output run-logical-and
    "2" "
if (false and true) {
  print \"1\";
} else {
  print \"2\";
}")

(test-interpreter-output run-while
    "012" "
var i = 0;
while (i < 3) {
  print i;
  i = i + 1;
}
")

(test-interpreter-output run-for
    "012" "
for (var i = 0; i < 3; i = i + 1) {
  print i;
}
")

(test-interpreter-output run-fun
    "123" "
fun count(n) {
  if (n > 1) count(n - 1);
  print n;
}
count(3);
")

(test-interpreter-output run-fun2
    "Hi, Dear Reader!" "
fun sayHi(first, last) {
  print \"Hi, \" + first + \" \" + last + \"!\";
}

sayHi(\"Dear\", \"Reader\");
")

(test-interpreter-output run-fun-return
    "01123581321345589144233377610987159725844181" "
fun fib(n) {
  if (n <= 1) return n;
  return fib(n - 2) + fib(n - 1);
}

for (var i = 0; i < 20; i = i + 1) {
  print fib(i);
}")

(test-interpreter-output run-fun-nested
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

(test-interpreter-output run-fun-scoping
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

(test-interpreter-error run-variable-local-redeclaration
    "[line 4] Error at 'a': Already a variable with this name in this scope." "
fun bad() {
  var a = \"first\";
  var a = \"second\";
}")

(test-interpreter-output run-class-print
    "DevonshireCream" "
class DevonshireCream {
  serveOn() {
    return \"Scones\";
  }
}

print DevonshireCream;
")

(test-interpreter-output run-class-instantiate
    "Bagel instance" "
class Bagel {}
var bagel = Bagel();
print bagel;
")

(test-interpreter-output run-class-method-call
    "Crunch crunch crunch" "
class Bacon {
  eat() {
    print \"Crunch crunch crunch\";
  }
}

Bacon().eat();
")

(test-interpreter-output run-class-method-this
    "The German chocolate cake is delicious!" "
class Cake {
  taste() {
    var adjective = \"delicious\";
    print \"The \" + this.flavor + \" cake is \" + adjective + \"!\";
  }
}

var cake = Cake();
cake.flavor = \"German chocolate\";
cake.taste();
")

(test-interpreter-output run-class-method-this2
    "Thing instance" "
class Thing {
  getCallback() {
    fun localFunction() {
      print this;
    }

    return localFunction;
  }
}

var callback = Thing().getCallback();
callback();
")

(test-interpreter-error run-print-this-error
    "[line 1] Error at 'this': Can't use 'this' outside of a class."
    "print this;")

(test-interpreter-output run-class-constructor
    "Hello" "
class Person {
  init() {
    print \"Hello\";
  }
}

var person = Person();
")

(test-interpreter-output run-class-constructor2
    "Foo instanceFoo instanceFoo instance" "
class Foo {
  init() {
    print this;
  }
}

var foo = Foo();
print foo.init();
")

(test-interpreter-output run-return-constructor
    "Foo instance" "
class Foo {
  init() {
    return;
  }
}

var foo = Foo();
print foo.init();
")

(test-interpreter-error run-return-value-constructor
    "[line 4] Error at 'return': Can't return a value from an initializer." "
class Foo {
  init() {
    return \"something else\";
  }
}
")
