;;;; -- Line editor --
;;;
;;; A small raw mode line editor: cursor movement, kill commands,
;;; history navigation and live syntax highlighting. The editor owns
;;; the redraw so it cooperates with multi-line prompts and wrapped
;;; lines. When the total width is an exact multiple of the terminal
;;; width, a newline is written to materialize the wrap, which keeps
;;; cursor arithmetic exact (the linenoise approach).

(in-package #:cclsh)

;;; Key reading

(defun editor--read-csi ()
  "Read the body of a CSI key sequence and return an editing key."
  (let ((char (read-char *standard-input* nil nil)))
    (case char
      ((nil) ':ignore)
      (#\A   ':up)
      (#\B   ':down)
      (#\C   ':right)
      (#\D   ':left)
      (#\H   ':home)
      (#\F   ':end)
      (t
       (if (digit-char-p char)
           (let ((digits (make-string-output-stream)))
             (write-char char digits)
             (loop for next = (read-char *standard-input* nil nil)
                   while (and next (digit-char-p next))
                   do (write-char next digits)
                   finally
                      (return
                        (if (and next (char= next #\~))
                            (case (parse-integer
                                   (get-output-stream-string digits)
                                   :junk-allowed t)
                              ((1 7) ':home)
                              (3     ':delete)
                              ((4 8) ':end)
                              (t     ':ignore))
                            ':ignore))))
           ':ignore)))))

(defun editor--read-escape ()
  "Read the tail of an escape sequence and return an editing key."
  (let ((first (or (read-char-no-hang *standard-input* nil nil)
                   (progn
                     (sleep 0.002)
                     (read-char-no-hang *standard-input* nil nil)))))
    (case first
      ((nil) ':ignore)
      (#\[   (editor--read-csi))
      (#\O
       (case (read-char *standard-input* nil nil)
         (#\H ':home)
         (#\F ':end)
         (t   ':ignore)))
      (t     ':ignore))))

(defun editor-read-key ()
  "Read one key from the terminal. Returns an editing keyword, or
   (values :char character) for self-inserting input."
  (let ((char (read-char *standard-input* nil nil)))
    (cond ((null char)
           ':eof)
          ((char= char +escape-character+)
           (editor--read-escape))
          (t
           (case (char-code char)
             (1   ':home)          ; C-a
             (2   ':left)          ; C-b
             (3   ':abort)         ; C-c
             (4   ':eof-or-delete) ; C-d
             (5   ':end)           ; C-e
             (6   ':right)         ; C-f
             (8   ':backspace)     ; C-h
             (9   ':complete)      ; Tab
             (10  ':enter)
             (11  ':kill-to-end)   ; C-k
             (12  ':clear-screen)  ; C-l
             (13  ':enter)
             (14  ':down)          ; C-n
             (16  ':up)            ; C-p
             (21  ':kill-line)     ; C-u
             (23  ':kill-word)     ; C-w
             (127 ':backspace)
             (t
              (if (< (char-code char) 32)
                  ':ignore
                  (values ':char char))))))))


;;; Rendering

(defun editor--screen-position (text prompt-width columns
                                &key (end (length text)))
  "Screen position after TEXT through END, following a prompt of
   PROMPT-WIDTH cells in a terminal with COLUMNS columns. Returns row,
   column and whether an exact-width wrap still needs materializing."
  (let ((row          (floor prompt-width columns))
        (column       (mod prompt-width columns))
        (pending-wrap (and (plusp prompt-width)
                           (zerop (mod prompt-width columns)))))
    (loop for index below end
          for char = (char text index)
          do (cond ((char= char #\newline)
                    (if pending-wrap
                        (setf pending-wrap nil)
                        (incf row))
                    (setf column 0))
                   ((char= char #\return)
                    (setf column 0)
                    (setf pending-wrap nil))
                   (t
                    (when pending-wrap
                      (setf pending-wrap nil))
                    (incf column)
                    (when (= column columns)
                      (incf row)
                      (setf column 0)
                      (setf pending-wrap t))))
          finally (return (values row column pending-wrap)))))

(defun editor--write-display (text)
  "Write display TEXT, following every newline with an explicit
   carriage return so redraws do not depend on terminal output flags."
  (loop for char across text
        do (write-char char)
        when (char= char #\newline)
          do (write-char #\return))
  (values))

(defun editor--write-prompt (edit-prompt prompt-width columns)
  "Write the static editable PROMPT once and return its ending row.
   Materialize an exact-width terminal wrap before dynamic text starts."
  (write-string edit-prompt)
  (multiple-value-bind (row column pending-wrap)
      (editor--screen-position "" prompt-width columns)
    (declare (ignore column))
    (when pending-wrap
      (write-char #\linefeed)
      (write-char #\return))
    (force-output)
    row))

(defun editor--render (prompt-width buffer cursor columns previous-row
                       &key suggestion)
  "Redraw only the dynamic input after the static prompt and place the
   cursor. PREVIOUS-ROW is the wrapped row where the prior render left
   it. SUGGESTION is unaccepted text displayed after BUFFER. Returns
   the row where the cursor now sits."
  (let* ((suffix  (or suggestion ""))
         (display (concatenate 'string buffer suffix)))
    (multiple-value-bind (prompt-row prompt-column prompt-wrap)
        (editor--screen-position "" prompt-width columns)
      (declare (ignore prompt-wrap))
      (multiple-value-bind (target-row target-column target-wrap)
          (editor--screen-position buffer prompt-width columns :end cursor)
        (declare (ignore target-wrap))
        (multiple-value-bind (end-row end-column exact-wrap)
            (editor--screen-position display prompt-width columns)
          (declare (ignore end-column))
          (write-string (ansi-cursor-hide))
          (unwind-protect
               (progn
                 (write-string
                  (ansi-cursor-up (- previous-row prompt-row)))
                 (write-string (ansi-cursor-column prompt-column))
                 (write-string (ansi-clear-below))
                 (editor--write-display (highlight-line buffer))
                 (when (plusp (length suffix))
                   (editor--write-display
                    (ansi-colorize suffix ':bright-black)))
                 (when exact-wrap
                   (write-char #\linefeed)
                   (write-char #\return))
                 (write-string (ansi-cursor-up (- end-row target-row)))
                 (write-string (ansi-cursor-column target-column)))
            (write-string (ansi-cursor-show))
            (force-output))
          target-row)))))

(defun editor--finish (prompt-width buffer columns previous-row &key marker)
  "Park the cursor after the buffer, optionally print a MARKER such as
   ^C, and move to a fresh line."
  (editor--render prompt-width buffer (length buffer) columns previous-row)
  (when marker
    (write-string (ansi-colorize marker ':bright-black)))
  (write-char #\Linefeed)
  (force-output))


;;; Editing

(defun editor--word-start (buffer cursor)
  "Index where the word before CURSOR starts."
  (let ((index cursor))
    (loop while (and (plusp index)
                     (whitespace-char-p (char buffer (1- index))))
          do (decf index))
    (loop while (and (plusp index)
                     (not (whitespace-char-p (char buffer (1- index)))))
          do (decf index))
    index))

(defun editor--history-entry (history index)
  "History entry at INDEX, preserving its text exactly."
  (aref history index))

(defun editor--move-right (buffer cursor suggestion)
  "Move right within BUFFER, or accept the full SUGGESTION when CURSOR
   is already at the end. Returns (values buffer cursor)."
  (cond ((< cursor (length buffer))
         (values buffer (1+ cursor)))
        (suggestion
         (values suggestion (length suggestion)))
        (t
         (values buffer cursor))))


;;; Completion presentation

(defun editor--print-candidates (displays columns)
  "Print completion DISPLAYS in columns under the edit line."
  (let ((count (length displays)))
    (if (> count 120)
        (format t "(~d possibilities)~%" count)
        (let* ((width   (+ 2 (loop for display in displays
                                   maximize (length display))))
               (per-row (max 1 (floor columns width))))
          (loop for index from 1
                for display in displays
                do (format t "~va" width display)
                when (zerop (mod index per-row))
                  do (write-char #\Linefeed)
                finally (unless (zerop (mod count per-row))
                          (write-char #\Linefeed))))))
  (force-output))

(defun editor--complete (buffer cursor edit-prompt prompt-width columns
                         previous-row)
  "Apply Tab completion at CURSOR. A unique match inserts itself (plus
   a space unless it is a directory), several matches extend to their
   common prefix, and a repeated Tab prints the candidate list below
   the line. Returns (values buffer cursor previous-row)."
  (multiple-value-bind (start candidates displays)
      (complete-line buffer cursor)
    (cond ((null candidates)
           (write-char (code-char 7))
           (force-output)
           (values buffer cursor previous-row))
          ((null (rest candidates))
           (let* ((candidate   (first candidates))
                  (directory-p (and (plusp (length candidate))
                                    (char= (char candidate
                                                 (1- (length candidate)))
                                           #\/)))
                  (replacement (if directory-p
                                   candidate
                                   (concatenate 'string candidate " "))))
             (values (concatenate 'string
                                  (subseq buffer 0 start)
                                  replacement
                                  (subseq buffer cursor))
                     (+ start (length replacement))
                     previous-row)))
          (t
           (let ((common        (completion--common-prefix candidates))
                 (prefix-length (- cursor start)))
             (if (> (length common) prefix-length)
                 (values (concatenate 'string
                                      (subseq buffer 0 start)
                                      common
                                      (subseq buffer cursor))
                         (+ start (length common))
                         previous-row)
                 (progn
                   (editor--render prompt-width buffer (length buffer)
                                   columns previous-row)
                   (write-char #\Linefeed)
                   (editor--print-candidates displays columns)
                   (values buffer cursor
                           (editor--write-prompt edit-prompt prompt-width
                                                 columns)))))))))

(defun edit-line (prompt &key (history *history*))
  "Edit one line under PROMPT. Returns (values line kind) where KIND is
   :line, :abort or :eof. Falls back to a plain READ-LINE when the
   terminal cannot be switched to raw mode."
  (multiple-value-bind (preamble edit-prompt)
      (prompt-split prompt)
    (multiple-value-bind (rows columns)
        (terminal-size)
      (declare (ignore rows))
      (let ((prompt-width  (ansi-display-width edit-prompt))
            (buffer        "")
            (cursor        0)
            (history-index (fill-pointer history))
            (stash         "")
            (suggestion    nil)
            (previous-row  0))
        (write-string preamble)
        (unwind-protect
            (progn
              (unless (terminal-raw)
                (write-string edit-prompt)
                (force-output)
                (let ((line (read-line *standard-input* nil nil)))
                  (return-from edit-line
                    (if line
                        (values line ':line)
                        (values nil ':eof)))))
              (setf previous-row
                    (editor--write-prompt edit-prompt prompt-width columns))
              (loop
                (setf suggestion
                      (and (= cursor (length buffer))
                           (history-suggestion buffer history)))
                (setf previous-row
                      (editor--render
                       prompt-width buffer cursor columns previous-row
                       :suggestion (and suggestion
                                        (subseq suggestion cursor))))
                (multiple-value-bind (key char)
                    (editor-read-key)
                  (case key
                    (:char
                     (setf buffer (concatenate 'string
                                               (subseq buffer 0 cursor)
                                               (string char)
                                               (subseq buffer cursor)))
                     (incf cursor))
                    (:enter
                     (editor--finish prompt-width buffer columns previous-row)
                     (return (values buffer ':line)))
                    (:abort
                     (editor--finish prompt-width buffer columns previous-row
                                     :marker "^C")
                     (return (values nil ':abort)))
                    (:eof
                     (editor--finish prompt-width buffer columns previous-row)
                     (return (values nil ':eof)))
                    (:eof-or-delete
                     (cond ((zerop (length buffer))
                            (editor--finish prompt-width buffer columns
                                            previous-row)
                            (return (values nil ':eof)))
                           ((< cursor (length buffer))
                            (setf buffer (concatenate 'string
                                                      (subseq buffer 0 cursor)
                                                      (subseq buffer (1+ cursor)))))))
                    (:backspace
                     (when (plusp cursor)
                       (setf buffer (concatenate 'string
                                                 (subseq buffer 0 (1- cursor))
                                                 (subseq buffer cursor)))
                       (decf cursor)))
                    (:delete
                     (when (< cursor (length buffer))
                       (setf buffer (concatenate 'string
                                                 (subseq buffer 0 cursor)
                                                 (subseq buffer (1+ cursor))))))
                    (:left
                     (when (plusp cursor)
                       (decf cursor)))
                    (:right
                     (multiple-value-setq (buffer cursor)
                       (editor--move-right buffer cursor suggestion)))
                    (:home
                     (setf cursor 0))
                    (:end
                     (setf cursor (length buffer)))
                    (:kill-to-end
                     (setf buffer (subseq buffer 0 cursor)))
                    (:kill-line
                     (setf buffer "")
                     (setf cursor 0))
                    (:kill-word
                     (let ((start (editor--word-start buffer cursor)))
                       (setf buffer (concatenate 'string
                                                 (subseq buffer 0 start)
                                                 (subseq buffer cursor)))
                       (setf cursor start)))
                    (:up
                     (when (plusp history-index)
                       (when (= history-index (fill-pointer history))
                         (setf stash buffer))
                       (decf history-index)
                       (setf buffer (editor--history-entry history history-index))
                       (setf cursor (length buffer))))
                    (:down
                     (when (< history-index (fill-pointer history))
                       (incf history-index)
                       (setf buffer
                             (if (= history-index (fill-pointer history))
                                 stash
                                 (editor--history-entry history history-index)))
                       (setf cursor (length buffer))))
                    (:complete
                     (multiple-value-setq (buffer cursor previous-row)
                       (editor--complete buffer cursor edit-prompt
                                         prompt-width columns previous-row)))
                    (:clear-screen
                     (write-string (ansi-clear-screen))
                     (write-string preamble)
                     (setf previous-row
                           (editor--write-prompt edit-prompt prompt-width
                                                 columns)))
                    (:ignore
                     nil)))))
          (terminal-restore))))))
