(in-package #:lox)

(defun make-keywords-table (&rest keywords)
  (let ((m (make-hash-table :test 'equal)))
    (loop :for keyword :in keywords
          :for key := (str:downcase (symbol-name keyword)) :do
            (setf (gethash key m) keyword))
    m))

(defvar *keywords*
  (make-keywords-table
   :and :class :else :false :fun :for :if :nil :or :print :return :super :this :true :var :while))

(deftype token-type ()
  `(member
    ;; Single-character tokens.
    :left-paren :right-paren :left-brace :right-brace
    :comma :dot :minus :plus :semicolon :slash :star

    ;; One or two character tokens.
    :bang :bang-equal
    :equal :equal-equal
    :greater :greater-equal
    :less :less-equal

    ;; Literals.
    :identifier :string :number

    ;; Keywords.
    ,@(alexandria:hash-table-values *keywords*)

    :eof))

(defclass token ()
  ((type :type token-type :initarg :type)
   (lexeme :type string :initarg :lexeme)
   (literal :initarg :literal)
   (line :type integer :initarg :line)))

(defmethod print-object ((obj token) out)
  (with-slots (type lexeme literal) obj
    (print-unreadable-object (obj out :type t)
      (format out "~s ~s ~s" type lexeme literal))))

(defun scan-tokens (source)
  (let ((current 0)
        (start 0)
        (line 1)
        (tokens nil))
    (labels ((current-str () (str:substring start current source))
             (advance-while (pred)
               (loop :for next := (peek)
                     :while (and (funcall pred next) (not (end-p)))
                     :do (when (char= next #\Newline)
                           (incf line))
                         (incf current)))
             (push-token (type &key lexeme literal)
               (let ((lexeme (or lexeme (current-str))))
                 (push (make-instance 'token :type type :lexeme lexeme :literal literal :line line)
                       tokens)))
             (end-p () (>= current (length source)))
             (peek () (if (end-p) #\Null (aref source current)))
             (peek-next ()
               (if (>= (+ current 1) (length source))
                   #\Null
                   (aref source (+ current 1))))
             (char-digit? (c) (digit-char-p c))
             (char-alphanum? (c) (or (char-alpha? c) (char-digit? c)))
             (char-alpha? (c)
               (or (and (char>= c #\a) (char<= c #\z))
                   (and (char>= c #\A) (char<= c #\Z))
                   (char= c #\_)))
             (match (expected)
               (unless (or (end-p) (not (char= (aref source current) expected)))
                 (incf current)
                 t)))
      (loop :while (not (end-p))
            :for c = (aref source current) :do
        (setf start current)
        (incf current)
        (case c
          (#\( (push-token :left-paren))
          (#\) (push-token :right-paren))
          (#\{ (push-token :left-brace))
          (#\} (push-token :right-brace))
          (#\, (push-token :comma))
          (#\. (push-token :dot))
          (#\- (push-token :minus))
          (#\+ (push-token :plus))
          (#\; (push-token :semicolon))
          (#\* (push-token :star))
          (#\! (push-token (if (match #\=) :bang-equal :bang)))
          (#\= (push-token (if (match #\=) :equal-equal :equal)))
          (#\< (push-token (if (match #\=) :less-equal :less)))
          (#\> (push-token (if (match #\=) :greater-equal :greater)))
          (#\/
           (cond
             ((match #\/)
              (advance-while (lambda (c) (and (not (char= c #\Newline))))))
             ((match #\*)
              (let ((depth 1))
                (labels ((at-block-start? (next)
                           (and (char= next #\/) (char= (peek-next) #\*)))
                         (at-block-end? (next)
                           (and (char= next #\*) (char= (peek-next) #\/))))
                  (loop :while (> depth 0)
                        :do (advance-while (lambda (c) (not (or (at-block-start? c) (at-block-end? c)))))

                            (when (end-p)
                              (report-error line "Unterminated block comment."))

                            (if (char= (peek) #\*)
                                (decf depth)
                                (incf depth))

                            (incf current 2)))))
             (t
              ;; A comment goes until the end of the line
              (push-token :slash))))
          (#\Newline (incf line))
          (#\Space)
          (#\Return)
          (#\Tab)

          ;; Strings (supports multiline)
          (#\"
           (advance-while (lambda (c) (and (not (char= c #\")))))
           (when (end-p)
             (report-error line "Unterminated string."))

           ;; The closing "
           (incf current)

           (push-token :string :literal (str:substring (+ start 1) (- current 1) source)))
          (t
           (cond
             ((char-digit? c)
              ;; Digits (always floating point)
              (advance-while #'char-digit?)
              (when (and (char= (peek) #\.) (char-digit? (peek-next)))
                (incf current)
                (advance-while #'char-digit?))
              (push-token :number :literal (parse-number:parse-number (current-str))))
             ((char-alpha? c)
              (advance-while #'char-alphanum?)
              (push-token (or (gethash (current-str) *keywords*) :identifier)))
             (t
              ;; Base case - report error
              (report-error line (format nil "Unexpected character ~a." c)))))))
      (push-token :eof :lexeme ""))
    (nreverse tokens)))