;;;; -- History persistence --
;;;
;;; History entries are stored one PRIN1ed string per entry, so entries
;;; containing newlines or quotes survive round-trips and loading is a
;;; plain READ loop with *READ-EVAL* disabled.

(in-package #:cclsh)

(defvar *history* (make-array 0 :adjustable t :fill-pointer t)
  "In-memory command history, oldest entry first.")

(defvar *history-limit* 10000
  "Maximum number of history entries kept on load.")

(defun config-directory ()
  "Return the cclsh configuration directory as a string with a trailing
   slash, honoring XDG_CONFIG_HOME."
  (let* ((xdg  (getenv "XDG_CONFIG_HOME"))
         (base (if (and xdg (plusp (length xdg)))
                   (if (char= (char xdg (1- (length xdg))) #\/)
                       xdg
                       (concatenate 'string xdg "/"))
                   (namestring (merge-pathnames ".config/"
                                                (user-homedir-pathname))))))
    (concatenate 'string base "cclsh/")))

(defun history-file ()
  "Return the path of the persistent history file."
  (concatenate 'string (config-directory) "history"))

(defun history-suggestion (input &optional (history *history*))
  "Newest entry in HISTORY that starts with nonempty INPUT and has
   more text to suggest. Equal entries are skipped so an older, longer
   command can still match."
  (when (plusp (length input))
    (loop for index downfrom (1- (fill-pointer history)) to 0
          for entry = (aref history index)
          when (and (> (length entry) (length input))
                    (string= input entry :end2 (length input)))
            return entry)))

(defun history-load ()
  "Load persisted history into *HISTORY*. Unreadable content is
   silently ignored; only the newest *HISTORY-LIMIT* entries are kept."
  (setf (fill-pointer *history*) 0)
  (let ((entries nil))
    (handler-case
        (with-open-file (stream (history-file)
                                :direction :input
                                :if-does-not-exist nil)
          (when stream
            (let ((*read-eval* nil))
              (loop for entry = (read stream nil ':cclsh-eof)
                    until (eq entry ':cclsh-eof)
                    when (stringp entry)
                      do (push entry entries)))))
      (error () nil))
    (let ((kept (nreverse (if (> (length entries) *history-limit*)
                              (subseq entries 0 *history-limit*)
                              entries))))
      (dolist (entry kept)
        (vector-push-extend entry *history*))))
  (values))

(defun history-append (entry)
  "Record ENTRY in memory and append it to the history file. Blank
   entries and immediate duplicates are skipped."
  (let ((trimmed (string-trim *whitespace-characters* entry)))
    (when (and (plusp (length trimmed))
               (or (zerop (fill-pointer *history*))
                   (not (string= entry (aref *history*
                                             (1- (fill-pointer *history*)))))))
      (vector-push-extend entry *history*)
      (handler-case
          (progn
            (ensure-directories-exist (config-directory))
            (with-open-file (stream (history-file)
                                    :direction :output
                                    :if-exists :append
                                    :if-does-not-exist :create)
              (prin1 entry stream)
              (terpri stream)))
        (error () nil))))
  (values))
