;;;; -- Lexers --
;;;
;;; Two small lexers over raw command lines: one for shell-style word
;;; splitting and one for Lisp forms. Both produce tokens covering the
;;; whole input so highlighting can recolor it span by span.

(in-package #:cclsh)

(defparameter *whitespace-characters* '(#\space #\tab #\newline #\return)
  "Characters treated as whitespace when splitting command lines.")

(defstruct (token (:constructor token-make (type start end)))
  "One lexed span of a command line."
  (type  ':word :type keyword)
  (start 0      :type fixnum)
  (end   0      :type fixnum))

(defun token-text (source token)
  "Return the substring of SOURCE covered by TOKEN."
  (subseq source (token-start token) (token-end token)))

(defun whitespace-char-p (char)
  "True when CHAR is considered whitespace."
  (and (member char *whitespace-characters*) t))


;;; Command mode

(defun lex-command-line (line)
  "Lex LINE into :space, :word, :double-quote and :single-quote tokens.
   Tokens cover the whole line and quote tokens include their quotes."
  (let ((tokens nil)
        (index  0)
        (length (length line)))
    (labels ((emit (type start end)
               (push (token-make type start end) tokens))

             (scan-whitespace (start)
               (loop while (and (< index length)
                                (whitespace-char-p (char line index)))
                     do (incf index))
               (emit ':space start index))

             (scan-quoted (start quote type)
               (incf index)
               (loop while (< index length)
                     do (let ((char (char line index)))
                          (cond ((and (char= quote #\") (char= char #\\))
                                 (incf index (if (< (1+ index) length) 2 1)))
                                ((char= char quote)
                                 (incf index)
                                 (return))
                                (t
                                 (incf index)))))
               (emit type start index))

             (scan-word (start)
               (loop while (< index length)
                     do (let ((char (char line index)))
                          (cond ((char= char #\\)
                                 (incf index (if (< (1+ index) length) 2 1)))
                                ((or (whitespace-char-p char)
                                     (char= char #\")
                                     (char= char #\'))
                                 (return))
                                (t
                                 (incf index)))))
               (emit ':word start index)))
      (loop while (< index length)
            do (let ((start index)
                     (char  (char line index)))
                 (cond ((whitespace-char-p char)
                        (scan-whitespace start))
                       ((char= char #\")
                        (scan-quoted start #\" ':double-quote))
                       ((char= char #\')
                        (scan-quoted start #\' ':single-quote))
                       (t
                        (scan-word start)))))
      (nreverse tokens))))


;;; Lisp mode

(defun lisp-symbol-boundary-p (char)
  "True when CHAR terminates a Lisp atom."
  (or (whitespace-char-p char)
      (and (member char '(#\( #\) #\" #\; #\' #\` #\,)) t)))

(defun lisp-number-text-p (text)
  "Crude check whether TEXT looks like a number literal."
  (and (plusp (length text))
       (find-if #'digit-char-p text)
       (every (lambda (char)
                (or (digit-char-p char)
                    (find char "+-./eEsSdDfFlL")))
              text)))

(defun lex-lisp-line (line)
  "Lex LINE as Lisp source into highlight tokens.
   Returns (values tokens open) where OPEN is NIL, :STRING or :COMMENT
   depending on whether the input ends inside an open construct."
  (let ((tokens nil)
        (index  0)
        (length (length line))
        (open   nil))
    (labels ((emit (type start end)
               (push (token-make type start end) tokens))

             (scan-string (start)
               (incf index)
               (loop while (< index length)
                     do (let ((char (char line index)))
                          (cond ((char= char #\\)
                                 (incf index (if (< (1+ index) length) 2 1)))
                                ((char= char #\")
                                 (incf index)
                                 (emit ':string start index)
                                 (return-from scan-string))
                                (t
                                 (incf index)))))
               (setf open ':string)
               (emit ':string start index))

             (scan-block-comment (start)
               (incf index 2)
               (loop while (< index length)
                     do (if (and (char= (char line index) #\|)
                                 (< (1+ index) length)
                                 (char= (char line (1+ index)) #\#))
                            (progn
                              (incf index 2)
                              (emit ':comment start index)
                              (return-from scan-block-comment))
                            (incf index)))
               (setf open ':comment)
               (emit ':comment start index))

             (scan-character (start)
               (incf index 2)
               (when (< index length)
                 (incf index))
               (loop while (and (< index length)
                                (alphanumericp (char line index)))
                     do (incf index))
               (emit ':character start index))

             (scan-atom (start)
               (loop while (and (< index length)
                                (not (lisp-symbol-boundary-p (char line index))))
                     do (incf index))
               (let ((text (subseq line start index)))
                 (emit (cond ((char= (char text 0) #\:)
                              ':keyword)
                             ((lisp-number-text-p text)
                              ':number)
                             (t
                              ':symbol))
                       start index))))
      (loop while (< index length)
            do (let ((start index)
                     (char  (char line index)))
                 (cond ((whitespace-char-p char)
                        (loop while (and (< index length)
                                         (whitespace-char-p (char line index)))
                              do (incf index))
                        (emit ':space start index))
                       ((char= char #\()
                        (incf index)
                        (emit ':paren-open start index))
                       ((char= char #\))
                        (incf index)
                        (emit ':paren-close start index))
                       ((char= char #\")
                        (scan-string start))
                       ((char= char #\;)
                        (setf index length)
                        (emit ':comment start index))
                       ((and (char= char #\#)
                             (< (1+ index) length)
                             (char= (char line (1+ index)) #\|))
                        (scan-block-comment start))
                       ((and (char= char #\#)
                             (< (1+ index) length)
                             (char= (char line (1+ index)) #\\))
                        (scan-character start))
                       ((member char '(#\' #\` #\,))
                        (incf index)
                        (when (and (char= char #\,)
                                   (< index length)
                                   (char= (char line index) #\@))
                          (incf index))
                        (emit ':quote start index))
                       (t
                        (scan-atom start)))))
      (values (nreverse tokens) open))))

(defun lisp-line-open-p (line)
  "True when LINE is an unfinished Lisp form: unbalanced open parens or
   an open string or block comment."
  (multiple-value-bind (tokens open)
      (lex-lisp-line line)
    (or (not (null open))
        (plusp (loop for token in tokens
                     sum (case (token-type token)
                           (:paren-open  1)
                           (:paren-close -1)
                           (t            0)))))))
