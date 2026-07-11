;;;; -- Terminal control --
;;;
;;; Raw mode switching through stty and ANSI escape sequence helpers.
;;; No FFI: the terminal is configured by spawning stty against the
;;; shell's controlling terminal.

(in-package #:cclsh)

(defconstant +escape-character+ (code-char 27)
  "The ASCII escape character used in ANSI sequences.")

(defvar *terminal-saved-state* nil
  "Original stty state string, restored after raw line editing.")

(defparameter *ansi-color-codes*
  '((:black          . 30)
    (:red            . 31)
    (:green          . 32)
    (:yellow         . 33)
    (:blue           . 34)
    (:magenta        . 35)
    (:cyan           . 36)
    (:white          . 37)
    (:bright-black   . 90)
    (:bright-red     . 91)
    (:bright-green   . 92)
    (:bright-yellow  . 93)
    (:bright-blue    . 94)
    (:bright-magenta . 95)
    (:bright-cyan    . 96)
    (:bright-white   . 97))
  "Mapping from color keywords to standard SGR color codes.")


;;; Raw mode

(defun terminal-run-stty (arguments)
  "Run stty with ARGUMENTS against the shell's terminal.
   Returns the trimmed standard output, or NIL when stty fails."
  (handler-case
      (let* ((output  (make-string-output-stream))
             (process (run-program "stty" arguments
                                   :input  t
                                   :output output
                                   :error  nil
                                   :wait   t)))
        (multiple-value-bind (status code)
            (external-process-status process)
          (if (and (eq status ':exited) (zerop code))
              (string-trim '(#\space #\newline #\return)
                           (get-output-stream-string output))
              nil)))
    (error () nil)))

(defun terminal-tty-p ()
  "True when standard input is an interactive terminal."
  (not (null (terminal-run-stty '("-g")))))

(defun terminal-raw ()
  "Switch the terminal to character-at-a-time input without echo.
   Returns true when the switch succeeded."
  (let ((saved (terminal-run-stty '("-g"))))
    (when saved
      (setf *terminal-saved-state* saved)
      (terminal-run-stty '("-icanon" "-echo" "-isig" "min" "1" "time" "0"))
      t)))

(defun terminal-restore ()
  "Restore the terminal state saved by TERMINAL-RAW."
  (when *terminal-saved-state*
    (terminal-run-stty (list *terminal-saved-state*))
    (setf *terminal-saved-state* nil))
  (values))

(defun terminal-size ()
  "Return the terminal dimensions as (values rows columns).
   Falls back to 24 by 80 when the size cannot be determined."
  (let* ((size  (terminal-run-stty '("size")))
         (space (and size (position #\space size))))
    (if space
        (values (or (parse-integer size :end space :junk-allowed t) 24)
                (or (parse-integer size :start (1+ space) :junk-allowed t) 80))
        (values 24 80))))


;;; ANSI sequences

(defun ansi-color-code (color)
  "Return the SGR code for the COLOR keyword, defaulting to white."
  (or (rest (assoc color *ansi-color-codes*)) 37))

(defun ansi-colorize (text color &key bold)
  "Wrap TEXT in the SGR sequence for COLOR, optionally BOLD."
  (format nil "~c[~:[~;1;~]~dm~a~c[0m"
          +escape-character+ bold (ansi-color-code color) text
          +escape-character+))

(defun ansi-cursor-up (lines)
  "Return the sequence moving the cursor LINES up, or an empty string."
  (if (plusp lines)
      (format nil "~c[~dA" +escape-character+ lines)
      ""))

(defun ansi-cursor-column (column)
  "Return the sequence moving the cursor to zero-based COLUMN."
  (format nil "~c[~dG" +escape-character+ (1+ column)))

(defun ansi-clear-below ()
  "Return the sequence clearing from the cursor to the screen end."
  (format nil "~c[J" +escape-character+))

(defun ansi-clear-screen ()
  "Return the sequence clearing the whole screen and homing the cursor."
  (format nil "~c[H~c[2J" +escape-character+ +escape-character+))

(defun ansi--skip-csi (string start)
  "Return the index just past a CSI sequence body starting at START."
  (loop for index from start below (length string)
        for code = (char-code (char string index))
        when (<= #x40 code #x7e)
          return (1+ index)
        finally (return (length string))))

(defun ansi--skip-osc (string start)
  "Return the index just past an OSC sequence body starting at START."
  (loop for index from start below (length string)
        for char = (char string index)
        when (char= char (code-char 7))
          return (1+ index)
        when (and (char= char +escape-character+)
                  (< (1+ index) (length string))
                  (char= (char string (1+ index)) #\\))
          return (+ index 2)
        finally (return (length string))))

(defun ansi-strip (string)
  "Remove ANSI escape sequences from STRING."
  (with-output-to-string (clean)
    (let ((index  0)
          (length (length string)))
      (loop while (< index length)
            do (let ((char (char string index)))
                 (cond ((and (char= char +escape-character+)
                             (< (1+ index) length))
                        (let ((kind (char string (1+ index))))
                          (setf index
                                (case kind
                                  (#\[ (ansi--skip-csi string (+ index 2)))
                                  (#\] (ansi--skip-osc string (+ index 2)))
                                  (t   (+ index 2))))))
                       (t
                        (write-char char clean)
                        (incf index))))))))

(defun ansi-display-width (string)
  "Return the number of visible character cells STRING occupies."
  (length (ansi-strip string)))
