;;;; -- Syntax highlighting --
;;;
;;; Recolors an input line with the standard 16 terminal colors. The
;;; result always has the same display width as the raw line so the
;;; line editor can use it for redraws directly. Command mode colors
;;; the command word by how it resolves and marks expansions inside
;;; arguments; Lisp mode colors known operators in head position, so a
;;; typo like defparam visibly never lights up.

(in-package #:cclsh)

(defparameter *lisp-token-colors*
  '((:string      . :yellow)
    (:comment     . :bright-black)
    (:keyword     . :magenta)
    (:number      . :cyan)
    (:character   . :cyan)
    (:quote       . :magenta)
    (:paren-open  . :bright-black)
    (:paren-close . :bright-black))
  "Colors for Lisp token types; missing types stay uncolored.")


;;; Command mode

(defun highlight-command-name (word lone-p)
  "Return the color keyword for a command WORD by how it resolves:
   builtins cyan, external programs green, unknown commands red. A
   lone word that would evaluate as Lisp (a bound variable or number)
   is magenta."
  (multiple-value-bind (kind target)
      (command-resolve (escape-remove (tilde-expand word)))
    (declare (ignore target))
    (ecase kind
      (:builtin  ':cyan)
      (:external ':green)
      (:unknown  (if (and lone-p (word-evaluates-alone-p word))
                     ':magenta
                     ':red)))))

(defun highlight--number-word-p (text)
  "True when TEXT is a plain integer or decimal literal."
  (let ((digits 0)
        (dots   0)
        (start  (if (and (plusp (length text))
                         (find (char text 0) "+-"))
                    1
                    0)))
    (loop for index from start below (length text)
          for char = (char text index)
          do (cond ((digit-char-p char)
                    (incf digits))
                   ((char= char #\.)
                    (incf dots))
                   (t
                    (return-from highlight--number-word-p nil))))
    (and (plusp digits) (<= dots 1))))

(defun highlight--command-word (text)
  "Recolor a bare argument word: numbers cyan, $references magenta,
   glob wildcards and a leading tilde bright magenta, escapes dim."
  (if (highlight--number-word-p text)
      (ansi-colorize text ':cyan)
      (with-output-to-string (highlighted)
        (let ((index  0)
              (length (length text)))
          (loop while (< index length)
                do (let ((char (char text index)))
                     (cond ((and (char= char #\\) (< (1+ index) length))
                            (write-string
                             (ansi-colorize (subseq text index (+ index 2))
                                            ':bright-black)
                             highlighted)
                            (incf index 2))
                           ((char= char #\$)
                            (multiple-value-bind (expansion next)
                                (variable-reference text index)
                              (cond (expansion
                                     (write-string
                                      (ansi-colorize (subseq text index next)
                                                     ':magenta)
                                      highlighted)
                                     (setf index next))
                                    (t
                                     (write-char char highlighted)
                                     (incf index)))))
                           ((member char '(#\* #\?))
                            (write-string
                             (ansi-colorize (string char) ':bright-magenta)
                             highlighted)
                            (incf index))
                           ((and (zerop index)
                                 (char= char #\~)
                                 (or (= length 1)
                                     (char= (char text 1) #\/)))
                            (write-string (ansi-colorize "~" ':bright-magenta)
                                          highlighted)
                            (incf index))
                           (t
                            (write-char char highlighted)
                            (incf index)))))))))

(defun highlight--double-quote (text)
  "Recolor a double-quoted token yellow with $references magenta."
  (with-output-to-string (highlighted)
    (let ((index  0)
          (length (length text))
          (plain  (make-string-output-stream)))
      (flet ((flush-plain ()
               (let ((run (get-output-stream-string plain)))
                 (when (plusp (length run))
                   (write-string (ansi-colorize run ':yellow) highlighted)))))
        (loop while (< index length)
              do (let ((char (char text index)))
                   (cond ((and (char= char #\\) (< (1+ index) length))
                          (write-string (subseq text index (+ index 2)) plain)
                          (incf index 2))
                         ((char= char #\$)
                          (multiple-value-bind (expansion next)
                              (variable-reference text index)
                            (cond (expansion
                                   (flush-plain)
                                   (write-string
                                    (ansi-colorize (subseq text index next)
                                                   ':magenta)
                                    highlighted)
                                   (setf index next))
                                  (t
                                   (write-char char plain)
                                   (incf index)))))
                         (t
                          (write-char char plain)
                          (incf index)))))
        (flush-plain)))))

(defun highlight--command-lisp (text)
  "Recolor a :lisp substitution token: dim delimiters around Lisp mode
   coloring of the body."
  (let* ((start  (if (char= (char text 0) #\$) 2 1))
         (closed (and (> (length text) start)
                      (char= (char text (1- (length text))) #\))))
         (end    (if closed (1- (length text)) (length text)))
         (inner  (subseq text start end))
         (tokens (lex-lisp-line inner))
         (meaningful (remove ':space tokens :key #'token-type)))
    (concatenate 'string
                 (ansi-colorize (subseq text 0 start) ':bright-black)
                 (highlight--lisp inner tokens
                                  :head-first
                                  (and (rest meaningful)
                                       (eq (token-type (first meaningful))
                                           ':symbol)
                                       t))
                 (if closed
                     (ansi-colorize ")" ':bright-black)
                     ""))))

(defun highlight--command (line tokens)
  "Render LINE with command mode colors."
  (let* ((groups     (token-groups tokens))
         (head       (first groups))
         (head-token (and head
                          (null (rest head))
                          (eq (token-type (first head)) ':word)
                          (first head))))
    (with-output-to-string (highlighted)
      (dolist (token tokens)
        (let ((text (token-text line token)))
          (cond ((eq token head-token)
                 (write-string (ansi-colorize text
                                              (highlight-command-name
                                               text (null (rest groups))))
                               highlighted))
                ((eq (token-type token) ':word)
                 (write-string (highlight--command-word text) highlighted))
                ((eq (token-type token) ':lisp)
                 (write-string (highlight--command-lisp text) highlighted))
                ((eq (token-type token) ':double-quote)
                 (write-string (highlight--double-quote text) highlighted))
                ((eq (token-type token) ':single-quote)
                 (write-string (ansi-colorize text ':yellow) highlighted))
                (t
                 (write-string text highlighted))))))))


;;; Lisp mode

(defun highlight--lisp-symbol-color (name head-p)
  "Color for a Lisp symbol token. In head position, symbols naming a
   function, macro or special operator are blue and anything else
   stays uncolored, so unknown heads never light up. Elsewhere,
   constants are cyan and bound earmuffed specials magenta."
  (multiple-value-bind (symbol found)
      (find-symbol (string-upcase name) *package*)
    (cond (head-p
           (and found
                (or (fboundp symbol)
                    (macro-function symbol)
                    (special-operator-p symbol))
                ':blue))
          ((and found (constantp symbol))
           ':cyan)
          ((and found
                (boundp symbol)
                (> (length name) 2)
                (char= (char name 0) #\*)
                (char= (char name (1- (length name))) #\*))
           ':magenta)
          (t
           nil))))

(defun highlight--lisp (line tokens &key head-first)
  "Render LINE with Lisp mode colors, tracking head position so the
   symbol right after an open paren gets operator coloring. With
   HEAD-FIRST the first symbol counts as a head, which substitution
   bodies use to mirror their several-forms-are-a-call rule."
  (with-output-to-string (highlighted)
    (let ((expect-head head-first))
      (dolist (token tokens)
        (let* ((type  (token-type token))
               (text  (token-text line token))
               (color (case type
                        (:symbol
                         (let ((head-p expect-head))
                           (setf expect-head nil)
                           (highlight--lisp-symbol-color text head-p)))
                        (t
                         (rest (assoc type *lisp-token-colors*))))))
          (case type
            (:paren-open
             (setf expect-head t))
            ((:space :quote)
             nil)
            (:symbol
             nil)
            (t
             (setf expect-head nil)))
          (write-string (if color
                            (ansi-colorize text color)
                            text)
                        highlighted))))))

(defun highlight-line (line)
  "Return LINE with ANSI colors added. The display width of the result
   equals the length of LINE."
  (cond ((zerop (length line))
         line)
        ((line-lisp-p line)
         (highlight--lisp line (lex-lisp-line line)))
        (t
         (highlight--command line (lex-command-line line)))))
