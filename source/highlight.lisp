;;;; -- Syntax highlighting --
;;;
;;; Recolors an input line with the standard 16 terminal colors. The
;;; result always has the same display width as the raw line so the
;;; line editor can use it for redraws directly.

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

(defun line-lisp-p (line)
  "True when LINE should be treated as a Lisp form."
  (let ((start (position-if-not #'whitespace-char-p line)))
    (and start (char= (char line start) #\() t)))

(defun highlight-command-name (word)
  "Return the color keyword for a command WORD by how it resolves:
   builtins cyan, external programs green, unknown commands red."
  (multiple-value-bind (kind target)
      (command-resolve (escape-remove (tilde-expand word)))
    (declare (ignore target))
    (ecase kind
      (:builtin  ':cyan)
      (:external ':green)
      (:unknown  ':red))))

(defun highlight--command (line tokens)
  "Render LINE with command mode colors."
  (let* ((groups (token-groups tokens))
         (head   (first groups))
         (head-token (and head
                          (null (rest head))
                          (eq (token-type (first head)) ':word)
                          (first head))))
    (with-output-to-string (highlighted)
      (dolist (token tokens)
        (let ((text (token-text line token)))
          (cond ((eq token head-token)
                 (write-string (ansi-colorize text (highlight-command-name text))
                               highlighted))
                ((member (token-type token) '(:double-quote :single-quote))
                 (write-string (ansi-colorize text ':yellow) highlighted))
                (t
                 (write-string text highlighted))))))))

(defun highlight--lisp (line tokens)
  "Render LINE with Lisp mode colors."
  (with-output-to-string (highlighted)
    (dolist (token tokens)
      (let* ((text  (token-text line token))
             (color (rest (assoc (token-type token) *lisp-token-colors*))))
        (write-string (if color
                          (ansi-colorize text color)
                          text)
                      highlighted)))))

(defun highlight-line (line)
  "Return LINE with ANSI colors added. The display width of the result
   equals the length of LINE."
  (cond ((zerop (length line))
         line)
        ((line-lisp-p line)
         (highlight--lisp line (lex-lisp-line line)))
        (t
         (highlight--command line (lex-command-line line)))))
