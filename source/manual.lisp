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
     literal, evaluates REPL style
  5. otherwise: command not found, status 127

  ./configure              a path, runs directly
  cd src                   cd is bound to a COMMAND, a builtin
  ls -la                   found in PATH
  (+ 1 2)                  starts with (, evaluates as Lisp
  *last-status*            lone bound variable, prints its value
  pi                       3.141592653589793D0
  42                       numbers print themselves

PATH lookups are cached for highlighting, but execution retries a
cached miss with a fresh scan, so freshly installed programs work
immediately. rehash drops all caches.")

    ("expansion" "words, quoting, variables and globs"
     "Command lines split on whitespace. In bare words:

  ~ and ~/x      home directory, only at the start of a word
  $VAR ${VAR}    environment variables, empty when unset
  * and ?        globs, matched per path segment
  \\x             escape: \\  joins words, \\* is a literal star

  echo ~/notes             /home/mag/notes
  echo $HOME/bin           /home/mag/bin
  echo *.lisp              main.lisp util.lisp
  echo *.zip               *.zip when nothing matches
  echo .h*                 .hidden, dotfiles need the explicit dot
  echo \"a b\" 'c d'         two words: a b and c d
  echo \"v=$HOME\" '$HOME'   variables expand in double quotes only
  echo pre\"mid dle\"post    one word: premid dlepost
  echo up\\ time            one word: up time

There is no | > or < syntax; a literal | is just an argument.
Pipelines are Lisp, see pipelines.")

    ("substitution" "Lisp values inside command lines"
     "Parens inside a command line substitute Lisp values. The outer
parens are shell delimiters: one form inside is an expression, several
forms become one call.

  echo (*balls*)                    HI, the value of *balls*
  echo (+ 1 2)                      3, several forms are a call
  echo (string-downcase *balls*)    hi
  echo (list 1 2 3)                 1 2 3, lists splice
  echo (getenv 'missing)            nothing at all, NIL vanishes
  echo x(+ 1 2)y                    x3y, in-word concatenation
  cd (*project-directory*)          anywhere a word can go
  mv draft.txt (format nil \"post-~a.txt\" (get-universal-time))

$(form) is the same thing. Results never re-glob or re-expand. Quote
parens to keep them literal: '(1 2 3)' stays text, which also means
filenames containing parens need quoting.")

    ("lisp" "Lisp lines and the REPL side"
     "Lisp lines evaluate in cclsh-user (uses cl, ccl and cclsh) and
print their values one per line.

  (+ 1 2)                  3
  (* 2 *)                  6, * holds the last value (** and *** too)
  (defvar *project* \"~/src\")
  cd (*project*)           definitions feed straight back into commands
  (ql:quickload :dexador)  every saved image includes Quicklisp

  (defparam x 1)           cclsh: undefined function defparam,
                           did you mean defparameter?

Undefined calls are caught before their arguments run. Unbalanced
forms, open strings and trailing backslashes continue on the next line
under a ... prompt. *last-status* holds the last exit status. Errors
print red and return to the prompt; the CCL debugger is never entered.
Output missing a final newline gets a reverse video ⏎ marker, fish
style:

  (format t \"hello\")       hello⏎ then NIL on a fresh line")

    ("commands" "defining commands, run and cmd"
     "defcommand defines a shell command that is also an ordinary
function:

  (defcommand gs ()
    \"Short git status.\"
    (run \"git\" \"status\" \"--short\"))

  (defcommand mkcd (directory)
    \"Create DIRECTORY and enter it.\"
    (run \"mkdir\" \"-p\" directory)
    (cd directory))

  gs                       from the command line
  (gs)                     and from Lisp
  (mkcd \"/tmp/scratch\")

An integer return value becomes the exit status, anything else means
0. Three ways to run programs from Lisp:

  (run \"git\" \"status\")     function, name may be computed
  (run 'git \"log\" \"-1\")    symbols work too
  (cmd git \"diff\" file)    macro, head resolves like a command word

Builtins: cd (with -), exit, export, unset, rehash, commands, help,
jobs, fg, bg. Saved cclsh images include Quicklisp; quicklisp-setup
loads or installs it when running from an unsaved development image.
commands lists everything currently defined.")

    ("pipelines" "pipe, seq, all and any"
     "Process orchestration is spelled in Lisp. A stage is (name
argument...) where NAME resolves like a command word and the arguments
are evaluated expressions:

  (pipe (ls \"-la\") (grep \"lisp\"))     ls -la | grep lisp
  (seq (make \"clean\") (make))          make clean ; make
  (all (make) (make \"install\"))        make && make install
  (any (probe) (echo \"fallback\"))      probe || echo fallback

  (let ((pattern \"defun\"))
    (pipe (git \"grep\" pattern) (wc \"-l\")))

  (defcommand emit () (format t \"b~%a~%\") 0)
  (pipe (emit) (sort))                 builtins can sit in pipes

Redirection is spelled as stages, and capture returns output as a
string (sh's $(cmd)), trailing newlines trimmed:

  (pipe (make) (to \"build.log\"))       make > build.log
  (pipe (make) (append-to \"build.log\"))  make >> build.log
  (pipe (from \"in.txt\") (wc \"-l\"))     wc -l < in.txt
  (pipe (from \"a.bin\") (to \"b.bin\"))   byte-exact file copy
  (pipe (make) (error-to \"err.log\"))   make 2> err.log
  (pipe (make) (error-append-to \"err.log\"))  make 2>> err.log
  (capture (make) (merge-error))       both streams, sh's 2>&1
  (capture (git \"rev-parse\" \"HEAD\"))   \"3e45271...\"
  echo (capture (hostname))            capture inside substitution

Error redirection applies to every stage, builtins included, and
merge-error sends standard error wherever the ordinary output goes.
Each pipeline returns the deciding exit status and records
*last-status*. The first stage owns the terminal, so Ctrl-C interrupts
the pipeline from its head, and Ctrl-Z stops the whole pipeline as
one job, see jobs.")

    ("jobs" "background jobs, fg, bg and Ctrl-Z"
     "Background and stopped commands are jobs:

  sleep 300 &              background job, prints [1] 12345
  make build               then Ctrl-Z stops it:
                           [2]+  Stopped                 make build
  jobs                     list jobs: + current, - previous;
                           jobs -l adds process group ids
  fg                       resume the current job in the foreground
  bg %2                    resume a stopped job in the background
  fg %ma                   specs: %2, 2, %+, %-, %prefix

Finished background jobs are announced before the next prompt as
Done, Exit 2 or the ending signal. fg restores the terminal modes a
stopped job was using, so Ctrl-Z out of vim and back just works.
Ctrl-Z stops a whole pipeline as one job; a capture continues through
Ctrl-Z with a notice, since the shell itself is reading its output.

Builtins and Lisp forms run inside the shell process: & reports an
error for them, and Ctrl-Z during a busy Lisp evaluation stops the
shell itself, so avoid that one. exit with stopped jobs warns once,
exit again to leave anyway. jobs, fg and bg are ordinary functions
too: (fg 1) resumes job 1 from Lisp.")

    ("editing" "keys, completion and colors"
     "Tab completes command names in command position, file paths
elsewhere (directories end with /, special characters get escaped) and
Lisp symbols in Lisp mode or inside a substitution:

  git ch<Tab>              checkout and friends
  ls src/ma<Tab>           src/main.lisp
  (princ-to<Tab>           (princ-to-string
  echo (*bal<Tab>          echo (*balls*

Unique matches insert, several extend to the common prefix, Tab again
lists the candidates. The newest history entry beginning with the
current input appears in dim text; Right or C-f at the end accepts it.

  Left C-b             move left     C-w        kill word
  Right C-f            move/accept   C-k        kill to end
  Up/Down C-p/C-n      history       C-u        kill line
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
respected), one printed string per entry:

  \"ls -la\"
  \"(pipe (git \\\"log\\\") (head))\"

Loading keeps the newest 10000. Blank lines, aborted lines and
immediate duplicates are skipped, and non-interactive sessions never
write it. Multi-line entries recall with their original newlines. The
newest entry beginning with current input is offered as a dim
suggestion; Right or C-f accepts it.")

    ("environment" "environment variables, the lispy way"
     "From the command line:

  export EDITOR=vim PAGER=less    set, several at a time
  export PATH=$HOME/bin:$PATH     expansions work in values
  export EDITOR                   print one value
  export                          print the whole environment
  unset PAGER

From Lisp, names are designators and values stringify:

  (setenv 'editor \"vim\")          (getenv :home)
  (setf (env 'port) 8000)         (env 'port) is \"8000\"
  (unset 'port)
  (environment-variables)         live NAME=value list

Lowercase names like http_proxy need strings. ~ does not expand after
=, use $HOME in values.")

    ("startup" "configuration and safe mode"
     "~/.config/cclsh/startup.lisp loads on startup in cclsh-user:

  (setenv 'editor \"vim\")
  (defcommand la ()
    \"Long listing including hidden files.\"
    (run \"ls\" \"-la\"))
  (defvar *project* \"~/common-lisp/cclsh\")

A broken startup file prints its error and the shell starts anyway.
CCLSH_SAFE=1 skips startup.lisp and history entirely, the escape hatch
when user state misbehaves.")

    ("scripting" "one-shots, scripts and shebangs"
     "Three non-interactive modes, all skipping startup.lisp and
history:

  cclsh -c 'echo one shot'        one command string
  cclsh provision.cclsh           a script file
  cclsh < input                   piped standard input

Script files work as shebang interpreters:

  #!/home/mag/.local/bin/cclsh
  (format t \"deploying~%\")
  (all (make \"build\") (make \"deploy\"))

The process exit code is the last status, or the argument of exit. -c
is what ssh, scp, rsync and git invoke, so remote operations work with
cclsh as a login shell; the string uses cclsh syntax, not sh.")

    ("login" "using cclsh as a login shell"
     "Point the login shell at a real copy outside the repository,
~/.local/bin/cclsh from scripts/install. On Guix:

  (user-account
   (name \"mag\")
   (shell \"/home/mag/.local/bin/cclsh\"))

Unknown flags are ignored so odd login invocations cannot lock you
out, builds are atomic, and the binary sets SHELL to itself. Keep ccl
installed in your Guix profile: the binary links glibc from the store
through CCL's closure, and guix gc could otherwise collect it out
from under your login. Nothing sources /etc/profile for you, so own
PATH at the top of startup.lisp:

  (setenv 'path (format nil \"~a/.local/bin:~a\"
                        (getenv 'home) (getenv 'path)))

Keep root on a stock shell. Emergency access past broken user state:

  ssh host -t env CCLSH_SAFE=1 /home/mag/.local/bin/cclsh"))
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
