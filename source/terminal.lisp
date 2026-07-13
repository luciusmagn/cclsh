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

(defconstant +sigpipe+ 13
  "Signal number of SIGPIPE.")

(defconstant +sigcont+ 18
  "Signal number of SIGCONT.")

(defconstant +sigtstp+ 20
  "Signal number of SIGTSTP.")

(defconstant +sigttin+ 21
  "Signal number of SIGTTIN.")

(defconstant +sigttou+ 22
  "Signal number of SIGTTOU.")

(defconstant +terminal-eintr+ 4
  "Linux errno for an interrupted system call.")

(defconstant +terminal-esrch+ 3
  "Linux errno for a vanished process group.")

(defconstant +terminal-eperm+ 1
  "Linux errno used when a process group has left the session.")

(define-condition terminal-control-error (error)
  ((operation
    :initarg :operation
    :reader terminal-control-error-operation)
   (process-group
    :initarg :process-group
    :reader terminal-control-error-process-group)
   (code
    :initarg :code
    :reader terminal-control-error-code))
  (:documentation "Signaled when a foreground terminal handoff fails.")
  (:report
   (lambda (condition stream)
     (format stream "cannot ~a process group ~d: ~a"
             (terminal-control-error-operation condition)
             (terminal-control-error-process-group condition)
             (let ((pointer
                     (external-call
                      "strerror"
                      :int (terminal-control-error-code condition)
                      :address)))
               (if (ccl:%null-ptr-p pointer)
                   (format nil "system error ~d"
                           (terminal-control-error-code condition))
                   (ccl:%get-cstring pointer)))))))

(define-condition terminal-attributes-error (error)
  ((operation
    :initarg :operation
    :reader terminal-attributes-error-operation)
   (code
    :initarg :code
    :reader terminal-attributes-error-code))
  (:documentation "Signaled when terminal attributes cannot be handled.")
  (:report
   (lambda (condition stream)
     (let* ((code (terminal-attributes-error-code condition))
            (pointer (external-call "strerror" :int code :address)))
       (format stream "cannot ~a terminal attributes: ~a"
               (terminal-attributes-error-operation condition)
               (if (ccl:%null-ptr-p pointer)
                   (format nil "system error ~d" code)
                   (ccl:%get-cstring pointer)))))))

(defvar *terminal-saved-termios* nil
  "Saved termios bytes, restored after raw line editing.")

(defvar *terminal-shell-attributes* nil
  "The terminal attributes of an interactive session at startup, the
   known good state reapplied when a stopped job leaves the terminal
   in whatever mode it was using.")

(defvar *terminal-presentation-enabled* t
  "Whether output may contain terminal presentation sequences.
   Dynamically bind this to NIL while capturing or redirecting output.")

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
  "True when terminal presentation is enabled and output is a terminal."
  (and *terminal-presentation-enabled*
       (= 1 (external-call "isatty" :int 1 :int))))

(defun terminal--get-termios (pointer)
  "Fill POINTER with terminal attributes. Return success and errno."
  (loop
    (when (zerop (external-call "tcgetattr"
                                :int 0
                                :address pointer
                                :int))
      (return (values t 0)))
    (let ((code (ccl::get-errno)))
      (unless (= code +terminal-eintr+)
        (return (values nil code))))))

(defun terminal--set-termios (pointer)
  "Apply attributes at POINTER. Return success and final errno."
  (loop
    (when (zerop (external-call "tcsetattr"
                                :int 0
                                :int +tcsanow+
                                :address pointer
                                :int))
      (return (values t 0)))
    (let ((code (ccl::get-errno)))
      (unless (= code +terminal-eintr+)
        (return (values nil code))))))

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
    (unwind-protect
        (ccl:%stack-block ((pointer +termios-size+))
          (dotimes (index +termios-size+)
            (setf (ccl:%get-unsigned-byte pointer index)
                  (aref *terminal-saved-termios* index)))
          (multiple-value-bind (success code)
              (terminal--set-termios pointer)
            (unless success
              (error 'terminal-attributes-error
                     :operation "restore"
                     :code code))))
      (setf *terminal-saved-termios* nil)))
  (values))

(defun terminal-attributes ()
  "Snapshot the current terminal attributes. Returns a byte vector for
   TERMINAL-ATTRIBUTES-APPLY, or NIL when they cannot be read."
  (ccl:%stack-block ((pointer +termios-size+))
    (when (terminal--get-termios pointer)
      (let ((saved (make-array +termios-size+ :element-type '(unsigned-byte 8))))
        (dotimes (index +termios-size+)
          (setf (aref saved index) (ccl:%get-unsigned-byte pointer index)))
        saved))))

(defun terminal-attributes-checked ()
  "Snapshot terminal attributes or signal TERMINAL-ATTRIBUTES-ERROR."
  (ccl:%stack-block ((pointer +termios-size+))
    (multiple-value-bind (success code)
        (terminal--get-termios pointer)
      (unless success
        (error 'terminal-attributes-error
               :operation "read"
               :code code)))
    (let ((saved (make-array +termios-size+
                             :element-type '(unsigned-byte 8))))
      (dotimes (index +termios-size+)
        (setf (aref saved index) (ccl:%get-unsigned-byte pointer index)))
      saved)))

(defun terminal-attributes-apply (attributes)
  "Apply an ATTRIBUTES snapshot taken by TERMINAL-ATTRIBUTES. Does
   nothing when ATTRIBUTES is NIL."
  (when attributes
    (ccl:%stack-block ((pointer +termios-size+))
      (dotimes (index +termios-size+)
        (setf (ccl:%get-unsigned-byte pointer index) (aref attributes index)))
      (multiple-value-bind (success code)
          (terminal--set-termios pointer)
        (unless success
          (error 'terminal-attributes-error
                 :operation "apply"
                 :code code)))))
  (values))

(defun terminal-shell-attributes-save ()
  "Remember the startup terminal attributes of an interactive session
   in *TERMINAL-SHELL-ATTRIBUTES*."
  (setf *terminal-shell-attributes* (terminal-attributes))
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

(defun terminal--signal-disposition (signal disposition)
  "Set the handler of SIGNAL to DISPOSITION: 0 for the default action,
   1 to ignore it."
  (external-call "signal" :int signal :address (ccl:%int-to-ptr disposition)
                 :address)
  (values))

(defun terminal-signals-setup ()
  "Ignore the terminal stop signals so handing the terminal to child
   process groups and taking it back never stops the shell. SIGTSTP
   keeps its default action: children inherit signal dispositions
   across exec, and an inherited ignore would make Ctrl-Z dead in
   every program the shell runs."
  (terminal--signal-disposition +sigttou+ 1)
  (terminal--signal-disposition +sigttin+ 1)
  (values))

(defmacro with-child-signal-defaults (&body body)
  "Run BODY, which spawns a child process, with SIGTTIN, SIGTTOU and
   SIGPIPE back at their default dispositions. Children inherit the
   shell's dispositions across exec: with the inherited ignores a
   child could never be stopped by terminal reads the way job control
   expects, and a pipeline stage would write into a closed pipe
   forever instead of dying of SIGPIPE (CCL ignores it process-wide
   for its own stream code). The ignores are restored afterwards so
   the shell itself survives taking the terminal back from a finished
   job and treats its own broken pipes as stream errors."
  `(unwind-protect
       (progn
         (terminal--signal-disposition +sigttin+ 0)
         (terminal--signal-disposition +sigttou+ 0)
         (terminal--signal-disposition +sigpipe+ 0)
         ,@body)
     (terminal--signal-disposition +sigttin+ 1)
     (terminal--signal-disposition +sigttou+ 1)
     (terminal--signal-disposition +sigpipe+ 1)))

(defun terminal-own-process-group ()
  "Return the shell's own process group id."
  (external-call "getpgrp" :int))

(defun terminal-foreground (process-group)
  "Make PROCESS-GROUP the terminal's foreground process group.
   Retries interrupted calls. Returns success and the final errno."
  (loop
    (when (zerop (external-call "tcsetpgrp"
                                :int 0
                                :int process-group
                                :int))
      (return (values t 0)))
    (let ((code (ccl::get-errno)))
      (unless (= code +terminal-eintr+)
        (return (values nil code))))))

(defun terminal-current-foreground ()
  "Return the terminal's foreground process group and final errno."
  (loop
    (let ((process-group (external-call "tcgetpgrp" :int 0 :int)))
      (unless (minusp process-group)
        (return (values process-group 0))))
    (let ((code (ccl::get-errno)))
      (unless (= code +terminal-eintr+)
        (return (values nil code))))))

(defun terminal-foreground-checked (process-group
                                    &key (operation "foreground"))
  "Make PROCESS-GROUP foreground or signal TERMINAL-CONTROL-ERROR."
  (multiple-value-bind (success code)
      (terminal-foreground process-group)
    (unless success
      (error 'terminal-control-error
             :operation operation
             :process-group process-group
             :code code)))
  (values))

(defun process-group-continue (process-group)
  "Send SIGCONT to PROCESS-GROUP, unsticking a child that touched the
   terminal in the window before it became the foreground group."
  (external-call "kill" :int (- process-group) :int +sigcont+ :int)
  (values))

(defun process-group-stop (process-group)
  "Send SIGTSTP to PROCESS-GROUP, carrying a stop across every process
   group of a multi-stage job."
  (external-call "kill" :int (- process-group) :int +sigtstp+ :int)
  (values))


;;; ANSI sequences

(defun ansi-color-code (color)
  "Return the SGR code for the COLOR keyword, defaulting to white."
  (or (rest (assoc color *ansi-color-codes*)) 37))

(defun ansi-colorize (text color &key bold)
  "Wrap TEXT in the SGR sequence for COLOR, optionally BOLD.
   Return TEXT unchanged when terminal presentation is disabled."
  (if *terminal-presentation-enabled*
      (format nil "~c[~:[~;1;~]~dm~a~c[0m"
              +escape-character+ bold (ansi-color-code color) text
              +escape-character+)
      text))

(defun terminal-colorize (text color &key bold)
  "Colorize TEXT only for terminal presentation output."
  (if (terminal-output-tty-p)
      (ansi-colorize text color :bold bold)
      text))

(defun ansi-reverse-video (text)
  "Wrap TEXT in reverse video, unless terminal presentation is disabled."
  (if *terminal-presentation-enabled*
      (format nil "~c[7m~a~c[0m" +escape-character+ text +escape-character+)
      text))

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
        (loop repeat (max 0 (- columns 2))
              do (write-char #\space))
        (write-char #\return)
        (write-string (ansi-clear-line-right))
        (force-output))
      (fresh-line))
  (values))

(defun ansi-cursor-up (lines)
  "Return the sequence moving the cursor LINES up, or an empty string."
  (if (and *terminal-presentation-enabled* (plusp lines))
      (format nil "~c[~dA" +escape-character+ lines)
      ""))

(defun ansi-cursor-column (column)
  "Return the sequence moving the cursor to zero-based COLUMN."
  (if *terminal-presentation-enabled*
      (format nil "~c[~dG" +escape-character+ (1+ column))
      ""))

(defun ansi-cursor-hide ()
  "Return the sequence that hides the terminal cursor."
  (if *terminal-presentation-enabled*
      (format nil "~c[?25l" +escape-character+)
      ""))

(defun ansi-cursor-show ()
  "Return the sequence that makes the terminal cursor visible."
  (if *terminal-presentation-enabled*
      (format nil "~c[?25h" +escape-character+)
      ""))

(defun ansi-clear-below ()
  "Return the sequence clearing from the cursor to the screen end."
  (if *terminal-presentation-enabled*
      (format nil "~c[J" +escape-character+)
      ""))

(defun ansi-clear-line-right ()
  "Return the sequence clearing from the cursor to the line end."
  (if *terminal-presentation-enabled*
      (format nil "~c[K" +escape-character+)
      ""))

(defun ansi-clear-screen ()
  "Return the sequence clearing the whole screen and homing the cursor."
  (if *terminal-presentation-enabled*
      (format nil "~c[H~c[2J" +escape-character+ +escape-character+)
      ""))

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
