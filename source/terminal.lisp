;;;; -- Terminal control --
;;;
;;; In-process terminal control through libc: raw mode with tcgetattr
;;; and tcsetattr, size with the TIOCGWINSZ ioctl, and foreground
;;; process group handling with tcsetpgrp. CCL runs external programs
;;; in their own process groups, so the shell must hand them the
;;; terminal for the duration of a foreground command and take it back
;;; afterwards; the byte offsets below follow the glibc termios layout
;;; on Linux.

(in-package #:cclsh)

(defconstant +escape-character+ (code-char 27)
  "The ASCII escape character used in ANSI sequences.")

(defconstant +termios-size+ 128
  "Buffer size comfortably larger than glibc's struct termios.")

(defconstant +termios-lflag-offset+ 12
  "Byte offset of c_lflag in struct termios.")

(defconstant +termios-vtime-offset+ 22
  "Byte offset of c_cc[VTIME] in struct termios.")

(defconstant +termios-vmin-offset+ 23
  "Byte offset of c_cc[VMIN] in struct termios.")

(defconstant +termios-raw-lflag-mask+ #x0b
  "The ISIG, ICANON and ECHO bits of c_lflag.")

(defconstant +tcsanow+ 0
  "tcsetattr optional action: apply immediately.")

(defconstant +winsize-ioctl+ #x5413
  "The TIOCGWINSZ ioctl request.")

(defconstant +sigcont+ 18
  "Signal number of SIGCONT.")

(defconstant +sigttin+ 21
  "Signal number of SIGTTIN.")

(defconstant +sigttou+ 22
  "Signal number of SIGTTOU.")

(defvar *terminal-saved-termios* nil
  "Saved termios bytes, restored after raw line editing.")

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

(defun terminal-tty-p ()
  "True when standard input is an interactive terminal."
  (= 1 (external-call "isatty" :int 0 :int)))

(defun terminal-output-tty-p ()
  "True when standard output is a terminal."
  (= 1 (external-call "isatty" :int 1 :int)))

(defun terminal--get-termios (pointer)
  "Fill POINTER with the terminal attributes. Returns true on success."
  (zerop (external-call "tcgetattr" :int 0 :address pointer :int)))

(defun terminal--set-termios (pointer)
  "Apply the terminal attributes at POINTER. Returns true on success."
  (zerop (external-call "tcsetattr" :int 0 :int +tcsanow+ :address pointer :int)))

(defun terminal-raw ()
  "Switch the terminal to character-at-a-time input without echo or
   signal generation. Returns true when the switch succeeded."
  (ccl:%stack-block ((pointer +termios-size+))
    (when (terminal--get-termios pointer)
      (let ((saved (make-array +termios-size+ :element-type '(unsigned-byte 8))))
        (dotimes (index +termios-size+)
          (setf (aref saved index) (ccl:%get-unsigned-byte pointer index)))
        (setf *terminal-saved-termios* saved))
      (setf (ccl:%get-unsigned-long pointer +termios-lflag-offset+)
            (logandc2 (ccl:%get-unsigned-long pointer +termios-lflag-offset+)
                      +termios-raw-lflag-mask+))
      (setf (ccl:%get-unsigned-byte pointer +termios-vtime-offset+) 0)
      (setf (ccl:%get-unsigned-byte pointer +termios-vmin-offset+) 1)
      (terminal--set-termios pointer))))

(defun terminal-restore ()
  "Restore the terminal state saved by TERMINAL-RAW."
  (when *terminal-saved-termios*
    (ccl:%stack-block ((pointer +termios-size+))
      (dotimes (index +termios-size+)
        (setf (ccl:%get-unsigned-byte pointer index)
              (aref *terminal-saved-termios* index)))
      (terminal--set-termios pointer))
    (setf *terminal-saved-termios* nil))
  (values))

(defun terminal-size ()
  "Return the terminal dimensions as (values rows columns).
   Falls back to 24 by 80 when the size cannot be determined."
  (ccl:%stack-block ((pointer 8))
    (if (zerop (external-call "ioctl" :int 0 :unsigned-long +winsize-ioctl+
                              :address pointer :int))
        (let ((rows    (ccl:%get-unsigned-word pointer 0))
              (columns (ccl:%get-unsigned-word pointer 2)))
          (values (if (plusp rows) rows 24)
                  (if (plusp columns) columns 80)))
        (values 24 80))))


;;; Foreground process groups

(defun terminal-signals-setup ()
  "Ignore the terminal stop signals so handing the terminal to child
   process groups and taking it back never stops the shell."
  (external-call "signal" :int +sigttou+ :address (ccl:%int-to-ptr 1) :address)
  (external-call "signal" :int +sigttin+ :address (ccl:%int-to-ptr 1) :address)
  (values))

(defun terminal-own-process-group ()
  "Return the shell's own process group id."
  (external-call "getpgrp" :int))

(defun terminal-foreground (process-group)
  "Make PROCESS-GROUP the terminal's foreground process group."
  (external-call "tcsetpgrp" :int 0 :int process-group :int)
  (values))

(defun process-group-continue (process-group)
  "Send SIGCONT to PROCESS-GROUP, unsticking a child that touched the
   terminal in the window before it became the foreground group."
  (external-call "kill" :int (- process-group) :int +sigcont+ :int)
  (values))


;;; ANSI sequences

(defun ansi-color-code (color)
  "Return the SGR code for the COLOR keyword, defaulting to white."
  (or (rest (assoc color *ansi-color-codes*)) 37))

(defun ansi-colorize (text color &key bold)
  "Wrap TEXT in the SGR sequence for COLOR, optionally BOLD."
  (format nil "~c[~:[~;1;~]~dm~a~c[0m"
          +escape-character+ bold (ansi-color-code color) text
          +escape-character+))

(defun ansi-reverse-video (text)
  "Wrap TEXT in the reverse video SGR sequence."
  (format nil "~c[7m~a~c[0m" +escape-character+ text +escape-character+))

(defun terminal-fresh-line ()
  "Ensure the next output starts at column 0 of a fresh line. On a
   terminal this uses the fish trick: print a reverse video return
   marker followed by a line of spaces and a carriage return. When the
   previous output ended mid-line the spaces wrap, leaving the marker
   visible; at column 0 the next write simply overwrites it. Off
   terminals this falls back to FRESH-LINE."
  (if (terminal-output-tty-p)
      (multiple-value-bind (rows columns)
          (terminal-size)
        (declare (ignore rows))
        (write-string (ansi-reverse-video "⏎"))
        (dotimes (fill (max 0 (- columns 2)))
          (declare (ignore fill))
          (write-char #\space))
        (write-char #\return)
        (write-string (ansi-clear-line-right))
        (force-output))
      (fresh-line))
  (values))

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

(defun ansi-clear-line-right ()
  "Return the sequence clearing from the cursor to the line end."
  (format nil "~c[K" +escape-character+))

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
