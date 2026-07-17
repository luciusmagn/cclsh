;;;; -- Line editor adapter --

(in-package #:cclsh)

(defun line-editor--accept-completion (candidate)
  "Return the accepted form of CANDIDATE for insertion into shell input.
   Directory candidates stay open for further path completion. Other unique
   candidates receive the separating space expected by shell input."
  (if (and (plusp (length candidate))
           (char= (char candidate (1- (length candidate))) #\/))
      candidate
      (concatenate 'string candidate " ")))

(defun edit-line (prompt &key (history *history*))
  "Edit one shell input line under PROMPT.
   Return the line and a result kind of :LINE, :ABORT or :EOF."
  (clinedi:edit-line
   prompt
   :history history
   :history-match-function #'history-search-match-p
   :terminal-size-function #'terminal-size
   :raw-mode-function #'terminal-raw
   :restore-function #'terminal-restore
   :highlight-function #'highlight-line
   :completion-function #'complete-line
   :common-prefix-function #'completion--common-prefix
   :completion-accept-function #'line-editor--accept-completion
   :completion-arrangement :grid
   :suggestion-function #'history-suggestion))
