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

(defconstant +terminal-lc-ctype-mask+ 1
  "Glibc newlocale mask selecting the LC_CTYPE category.")

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
                   (ccl::%get-utf-8-cstring pointer)))))))

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
                   (ccl::%get-utf-8-cstring pointer)))))))

(defvar *terminal-saved-termios* nil
  "Saved termios bytes, restored after raw line editing.")

(defvar *terminal-shell-attributes* nil
  "The terminal attributes of an interactive session at startup, the
   known good state reapplied when a stopped job leaves the terminal
   in whatever mode it was using.")

(defvar *terminal-presentation-enabled* t
  "Whether output may contain terminal presentation sequences.
   Dynamically bind this to NIL while capturing or redirecting output.")

(defvar *terminal-cell-locale-active* nil
  "True while the current Lisp thread has a UTF-8 locale for wcwidth.")

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


;;; Unicode terminal geometry

(defun terminal--make-cell-locale ()
  "Create a private UTF-8 LC_CTYPE locale for terminal width queries.
   Return a null pointer only when none of the conventional Linux locale
   names is available. The locale is never retained in a saved image."
  (dolist (name '("C.UTF-8" "C.utf8" "en_US.UTF-8" "")
                (ccl:%null-ptr))
    (ccl::with-utf-8-cstr (encoded name)
      (let ((locale
              (external-call "newlocale"
                             :int +terminal-lc-ctype-mask+
                             :address encoded
                             :address (ccl:%null-ptr)
                             :address)))
        (unless (ccl:%null-ptr-p locale)
          (return locale))))))

(defun terminal--call-with-cell-locale (function)
  "Call FUNCTION with wcwidth using a thread-local UTF-8 locale.

   Glibc's uselocale changes only the calling native thread. A fresh locale
   is freed before returning, so no foreign pointer survives image saving.
   Nested calls reuse the established dynamic extent."
  (if *terminal-cell-locale-active*
      (funcall function)
      (let ((locale (terminal--make-cell-locale)))
        (if (ccl:%null-ptr-p locale)
            (funcall function)
            (let ((previous
                    (external-call "uselocale"
                                   :address locale
                                   :address)))
              (unwind-protect
                  (if (ccl:%null-ptr-p previous)
                      (funcall function)
                      (let ((*terminal-cell-locale-active* t))
                        (funcall function)))
                (unless (ccl:%null-ptr-p previous)
                  (external-call "uselocale"
                                 :address previous
                                 :address))
                (external-call "freelocale"
                               :address locale
                               :void)))))))

(defun terminal--wide-code-p (code)
  "True when CODE is in a broadly stable Unicode wide-character range.
   This is the fallback used only if libc cannot classify a character."
  (or (<= #x1100 code #x115f)
      (= code #x2329)
      (= code #x232a)
      (and (<= #x2e80 code #xa4cf) (/= code #x303f))
      (<= #xac00 code #xd7a3)
      (<= #xf900 code #xfaff)
      (<= #xfe10 code #xfe19)
      (<= #xfe30 code #xfe6f)
      (<= #xff00 code #xff60)
      (<= #xffe0 code #xffe6)
      (<= #x1f300 code #x1faff)
      (<= #x20000 code #x3fffd)))

(defun terminal--zero-width-code-p (code)
  "True when CODE is a control or Unicode formatting code with no cells."
  (or (< code 32)
      (<= 127 code 159)
      (= code #x200b)
      (<= #x200c code #x200f)
      (<= #x202a code #x202e)
      (<= #x2060 code #x206f)
      (<= #xfe00 code #xfe0f)
      (= code #xfeff)
      (<= #xe0000 code #xeffff)))

(defun terminal--character-cell-width (character)
  "Return CHARACTER's terminal cell width under an active cell locale."
  (let* ((code   (char-code character))
         (width  (external-call "wcwidth"
                                :unsigned-int code
                                :int)))
    (cond ((not (minusp width))
           width)
          ((terminal--zero-width-code-p code)
           0)
          ((ccl::is-combinable character)
           0)
          ((terminal--wide-code-p code)
           2)
          (t
           1))))

(defun terminal-character-cell-width (character)
  "Return the number of terminal cells occupied by CHARACTER.
   Controls, combining marks and formatting characters occupy zero cells;
   East Asian wide characters and emoji occupy two."
  (terminal--call-with-cell-locale
   (lambda ()
     (terminal--character-cell-width character))))

(defun terminal--regional-indicator-p (character)
  "True when CHARACTER is a regional-indicator flag component."
  (<= #x1f1e6 (char-code character) #x1f1ff))

(defun terminal--emoji-modifier-p (character)
  "True when CHARACTER is an emoji skin-tone modifier."
  (<= #x1f3fb (char-code character) #x1f3ff))

(defun terminal--variation-selector-16-p (character)
  "True when CHARACTER requests emoji presentation."
  (= (char-code character) #xfe0f))

(defun terminal--zero-width-joiner-p (character)
  "True when CHARACTER joins adjacent emoji or script glyphs."
  (= (char-code character) #x200d))

(defun terminal--grapheme-control-p (character)
  "True when CHARACTER is a C0 or C1 grapheme-breaking control."
  (let ((code (char-code character)))
    (or (< code 32) (<= 127 code 159))))

(defun terminal--grapheme-extend-p (character)
  "True when CHARACTER remains attached to the preceding grapheme.

   wcwidth covers Unicode nonspacing marks, variation selectors, Hangul
   medial and final jamo, and tag characters. CCL's normalization table
   additionally recognizes spacing combining marks in its supported data."
  (and (not (terminal--zero-width-joiner-p character))
       (not (terminal--grapheme-control-p character))
       (or (zerop (terminal--character-cell-width character))
           (ccl::is-combinable character)
           (terminal--emoji-modifier-p character))))

(defun terminal-grapheme-next-boundary
    (string start &optional (end (length string)))
  "Return the first extended-grapheme boundary after START, no later than END.
   Combining characters, emoji modifiers, regional-indicator pairs and
   zero-width-joiner sequences remain indivisible."
  (when (>= start end)
    (return-from terminal-grapheme-next-boundary end))
  (terminal--call-with-cell-locale
   (lambda ()
     (let* ((first (char string start))
            (index (1+ start)))
       (cond ((and (char= first #\return)
                   (< index end)
                   (char= (char string index) #\newline))
              (1+ index))
             ((terminal--regional-indicator-p first)
              (if (and (< index end)
                       (terminal--regional-indicator-p (char string index)))
                  (1+ index)
                  index))
             (t
              (loop
                (loop while (and (< index end)
                                 (terminal--grapheme-extend-p
                                  (char string index)))
                      do (incf index))
                (cond ((and (< index end)
                            (terminal--zero-width-joiner-p
                             (char string index)))
                       ;; GB9 keeps the joiner with the preceding cluster.
                       ;; When another character follows, GB11 keeps that
                       ;; character in the same joined glyph as well.
                       (incf index)
                       (when (< index end)
                         (incf index)))
                      (t
                       (return index))))))))))

(defun terminal-grapheme-previous-boundary (string index)
  "Return the extended-grapheme boundary immediately before INDEX.
   INDEX may itself be inside a grapheme; in that case return its start."
  (setf index (min index (length string)))
  (when (<= index 0)
    (return-from terminal-grapheme-previous-boundary 0))
  (terminal--call-with-cell-locale
   (lambda ()
     (loop with start = 0
           for next = (terminal-grapheme-next-boundary string start)
           when (>= next index)
             return start
           do (setf start next)))))

(defun terminal-grapheme-boundary-at-or-after (string index)
  "Return the first extended-grapheme boundary at or after INDEX."
  (when (<= index 0)
    (return-from terminal-grapheme-boundary-at-or-after 0))
  (terminal--call-with-cell-locale
   (lambda ()
     (loop with boundary = 0
           while (< boundary (length string))
           do (setf boundary
                    (terminal-grapheme-next-boundary string boundary))
           when (>= boundary index)
             return boundary
           finally (return (length string))))))

(defun terminal-grapheme-cell-width (string start end)
  "Return the terminal cell width of one grapheme in STRING from START to END.

   Libc supplies codepoint widths. Emoji presentation selectors and keycaps
   promote a cluster to two cells; skin-tone modifiers and joined emoji do
   not add cells of their own."
  (terminal--call-with-cell-locale
   (lambda ()
     (let ((total              0)
           (widest             0)
           (joiner-p           nil)
           (emoji-presentation nil)
           (keycap-p           nil))
       (loop for index from start below end
             for character = (char string index)
             for code = (char-code character)
             for width = (terminal--character-cell-width character)
             do (setf widest (max widest width))
                (cond ((terminal--zero-width-joiner-p character)
                       (setf joiner-p t))
                      ((and (> index start)
                            (terminal--emoji-modifier-p character))
                       nil)
                      (t
                       (incf total width)))
                (when (terminal--variation-selector-16-p character)
                  (setf emoji-presentation t))
                (when (= code #x20e3)
                  (setf keycap-p t)))
       (cond ((or keycap-p
                  (and emoji-presentation (> end (1+ start))))
              2)
             (joiner-p
              widest)
             (t
              total))))))

(defun terminal-text-cell-width (string)
  "Return the terminal cells occupied by plain Unicode STRING.
   Newlines, returns and other controls occupy no horizontal cells."
  (terminal--call-with-cell-locale
   (lambda ()
     (loop with index = 0
           while (< index (length string))
           for next = (terminal-grapheme-next-boundary string index)
           sum (terminal-grapheme-cell-width string index next)
           do (setf index next)))))


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
  "Return the number of visible terminal cells STRING occupies."
  (terminal-text-cell-width (ansi-strip string)))
