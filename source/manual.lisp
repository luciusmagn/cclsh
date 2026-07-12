;;;; -- Manual --
;;;
;;; The built-in manual behind the HELP command. Content lives here as
;;; data so the installed binary is self-documenting; docs/guide.org
;;; is the long form. Everything not covered here is ordinary Common
;;; Lisp in a live CCL image.

(in-package #:cclsh)

(defparameter *manual-sections*
  '(("dispatch" "how a line becomes Lisp or a command"
     "A line starting with ( evaluates as Lisp in the cclsh-user
package. Anything else is a command line whose first word resolves in
order:

  1. a word containing / runs as a file path directly
  2. a symbol whose value is a COMMAND instance runs as a builtin
  3. the word is looked up in PATH
  4. a lone word naming a bound variable or keyword, or a number
     literal, evaluates REPL style: after (defvar *balls* 'hi),
     typing *balls* prints HI
  5. otherwise: command not found, status 127

PATH lookups are cached for highlighting, but execution retries a
cached miss with a fresh scan, so freshly installed programs work
immediately. rehash drops all caches.")

    ("expansion" "words, quoting, variables and globs"
     "Command lines split on whitespace. In bare words:

  ~ and ~/x      home directory, only at the start of a word
  $VAR ${VAR}    environment variables, empty when unset
  * and ?        globs, matched per path segment
  \\x             escape: \\  joins words, \\* is a literal star

Hidden files only match globs that spell the leading dot. A glob with
no matches passes through literally. Quoting: \"...\" is one word with
$VAR still expanding inside, '...' is fully literal. Adjacent pieces
concatenate: pre\"mid dle\"post is one word. There is no | > or <
syntax; pipelines and redirection are Lisp, see pipelines.")

    ("substitution" "Lisp values inside command lines"
     "Parens inside a command line substitute Lisp values. The outer
parens are shell delimiters: one form inside is an expression, several
forms become one call.

  echo (*balls*)                    the value of *balls*
  echo (+ 1 2)                      3
  mv draft.txt (format nil \"post-~a.txt\" (get-universal-time))

$(form) is the same thing. A standalone substitution splices a proper
list into several arguments and NIL vanishes entirely; inside a larger
word the value concatenates, x(+ 1 2)y is x3y. Results never re-glob.
Quote parens to keep them literal: '(1 2 3)' stays text.")

    ("lisp" "Lisp lines and the REPL side"
     "Lisp lines evaluate in cclsh-user (uses cl, ccl and cclsh) and
print their values one per line. * ** and *** hold recent values,
*last-status* the last exit status. Calling an undefined function is
reported up front with suggestions: (defparam x 1) says did you mean
defparameter? and skips evaluating the doomed arguments. Errors print
red and return to the prompt; the CCL debugger is never entered.
Output missing a final newline gets a reverse video ⏎ marker before
the next thing printed, fish style.")

    ("commands" "defining commands, run and cmd"
     "defcommand defines a shell command that is also an ordinary
function:

  (defcommand gs ()
    \"Short git status.\"
    (run \"git\" \"status\" \"--short\"))

An integer return value becomes the exit status, anything else means
0. run executes one program (name as symbol or string, arguments
stringified). cmd is the macro form, (cmd git \"status\"), whose head
resolves like a command word. Builtins: cd (with -), exit, export,
unset, rehash, commands, help. quicklisp-setup loads or installs
Quicklisp. commands lists everything currently defined.")

    ("pipelines" "pipe, seq, all and any"
     "Process orchestration is spelled in Lisp. A stage is (name
argument...) where NAME resolves like a command word and the arguments
are evaluated expressions:

  (pipe (ls \"-la\") (grep \"lisp\"))     ls -la | grep lisp
  (seq (make \"clean\") (make))          make clean ; make
  (all (make) (make \"install\"))        make && make install
  (any (probe) (echo \"fallback\"))      probe || echo fallback

Each returns the deciding exit status and records *last-status*.
Builtins can appear inside pipe; their output streams to the next
stage. The first stage owns the terminal, so Ctrl-C interrupts the
pipeline from its head.")

    ("editing" "keys, completion and colors"
     "Tab completes command names in command position, file paths
elsewhere (directories end with /, special characters get escaped) and
Lisp symbols in Lisp mode or inside a substitution. Unique matches
insert, several extend to the common prefix, Tab again lists.

  Left/Right C-b/C-f   move          C-w        kill word
  Up/Down C-p/C-n      history       C-k / C-u  kill to end / line
  Home/End C-a/C-e     line ends     C-l        clear screen
  Backspace / Delete   delete        C-c        abort line
  C-d                  delete forward, or exit on an empty line

Colors: external commands green, builtins cyan, unknown red, lone
bound variables magenta, strings yellow, numbers cyan, $VAR magenta,
globs and ~ bright magenta. In Lisp: known operators in head position
blue (a typo never lights up), keywords magenta, constants cyan,
bound *specials* magenta.")

    ("history" "what is remembered and where"
     "History persists in ~/.config/cclsh/history (XDG_CONFIG_HOME is
respected), one printed string per line, capped at 10000 on load.
Blank lines, aborted lines and immediate duplicates are skipped, and
non-interactive sessions never write it. Multi-line entries recall
with newlines folded to spaces.")

    ("environment" "environment variables, the lispy way"
     "From the command line:

  export EDITOR=vim PAGER=less    set, several at a time
  export PATH=$HOME/bin:$PATH     expansions work in values
  export                          print the whole environment
  unset PAGER

From Lisp, names are designators and values stringify:

  (setenv 'editor \"vim\")          (getenv :home)
  (setf (env 'port) 8000)         (env 'port)
  (environment-variables)         live NAME=value list

Lowercase names like http_proxy need strings. ~ does not expand after
=, use $HOME in values.")

    ("startup" "configuration and safe mode"
     "~/.config/cclsh/startup.lisp loads on startup in cclsh-user; put
environment setup and defcommand aliases there. A broken startup file
prints its error and the shell starts anyway. CCLSH_SAFE=1 skips
startup.lisp and history entirely, the escape hatch when user state
misbehaves.")

    ("scripting" "one-shots, scripts and shebangs"
     "Three non-interactive modes, all skipping startup.lisp and
history:

  cclsh -c 'echo one shot'        one command string
  cclsh provision.cclsh           a script file
  cclsh < input                   piped standard input

Script files work as shebang interpreters (#! lines are ignored). The
process exit code is the last status, or the argument of exit. -c is
what ssh, scp, rsync and git invoke, so remote operations work with
cclsh as a login shell; the string uses cclsh syntax, not sh.")

    ("login" "using cclsh as a login shell"
     "Point /etc/passwd (or the Guix user-account shell field) at a
real copy outside the repository, ~/.local/bin/cclsh from
scripts/install. Unknown flags are ignored so odd login invocations
cannot lock you out, builds are atomic, and the binary sets SHELL to
itself. Nothing sources /etc/profile for you: own PATH at the top of
startup.lisp. Keep root on a stock shell. Emergency:
CCLSH_SAFE=1 cclsh skips user state. No job control yet, so avoid
Ctrl-Z."))
  "The built-in manual: (name one-liner body) per section.")

(defun manual--heading (text)
  "Render a manual heading, colored only on a terminal."
  (if (terminal-output-tty-p)
      (ansi-colorize text ':cyan :bold t)
      text))

(defun manual--print-overview ()
  "Print the manual overview and section list."
  (format t "~a~%~%" (manual--heading "cclsh"))
  (format t "A system shell inside a live Clozure CL image. One rule: a line~%~
             starting with ( is Lisp, anything else is a command. The sections~%~
             below cover what is special; the rest is ordinary Common Lisp.~%~%")
  (dolist (section *manual-sections*)
    (destructuring-bind (name one-liner body) section
      (declare (ignore body))
      (format t "  ~a~a~%"
              (if (terminal-output-tty-p)
                  (ansi-colorize (format nil "~14a" name) ':cyan)
                  (format nil "~14a" name))
              one-liner)))
  (format t "~%help SECTION prints details. commands lists what is callable,~%~
             --version identifies the build, docs/guide.org is the long form.~%"))

(defun manual--print-section (name)
  "Print one manual section. Returns true when NAME exists."
  (let ((section (assoc name *manual-sections* :test #'string-equal)))
    (if (null section)
        (progn
          (format *error-output* "~a~%"
                  (ansi-colorize
                   (format nil "help: no section ~a; sections are ~{~a~^, ~}"
                           name
                           (mapcar #'first *manual-sections*))
                   ':red))
          nil)
        (destructuring-bind (section-name one-liner body) section
          (format t "~a  ~a~%~%~a~%"
                  (manual--heading section-name)
                  one-liner
                  body)
          t))))

(defcommand help (&rest sections)
  "Show the built-in manual. help SECTION elaborates on one topic."
  (if (null sections)
      (progn
        (manual--print-overview)
        0)
      (let ((status 0))
        (dolist (section sections status)
          (unless (manual--print-section
                   (string-downcase (princ-to-string section)))
            (setf status 1))))))
