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

  1. an existing non-directory word containing / runs directly
  2. a symbol whose value is a COMMAND instance runs as a builtin
  3. the word is looked up in PATH
  4. one explicit directory path changes directory as if by cd
  5. a lone word naming a bound variable or keyword, or a number
     literal, evaluates REPL style
  6. otherwise: command not found, status 127

  ./configure              a path, runs directly
  cd src                   cd is bound to a COMMAND, a builtin
  ls -la                   found in PATH
  ..                       changes to the parent directory
  src/                     changes to the src directory
  (+ 1 2)                  starts with (, evaluates as Lisp
  *last-status*            lone bound variable, prints its value
  pi                       3.141592653589793D0
  42                       numbers print themselves

Implicit cd accepts .., absolute paths, paths beginning ./ or ../,
expanded ~/ paths and names ending in /. A bare name remains a command
lookup. The path must be the only word and cannot be backgrounded.
Builtins and executables always win first.

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

There is no | > or < syntax; a literal | is just an argument. # does
not start a command comment either. Pipelines are Lisp, see pipelines.")

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

    ("unicode" "UTF-8 text and byte-transparent files"
     "Terminal I/O, startup and history files, scripts, prompt output,
environment names and values, child arguments and paths, and captured
pipeline text are UTF-8 regardless of the process locale.

The editor measures terminal cells. Wide CJK and emoji glyphs wrap in
two cells; combining marks and joined emoji move and delete as one
grapheme.

Redirect-only pipelines are byte-transparent, so arbitrary binary
files can be copied without UTF-8 decoding:

  (pipe (from \"input.bin\") (to \"output.bin\"))")

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
jobs, fg, bg and zoxide-setup. Saved cclsh images include Quicklisp;
quicklisp-setup loads or installs it when running from an unsaved development
image.
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
*last-status*. The pipeline owns the terminal as one job, so Ctrl-C
interrupts every stage and Ctrl-Z stops the whole pipeline together,
see jobs.")

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

Standalone builtins and Lisp forms run inside the shell process, so &
reports an error for them. Builtin stages inside pipe are controlled
with the rest of their pipeline and can be stopped and resumed. Ctrl-Z
during another busy Lisp evaluation stops the shell itself, so avoid
that one. exit with stopped jobs warns once, exit again to leave anyway.
Every orderly exit sends SIGHUP to live job groups and SIGCONT to stopped
groups so they do not linger after logout.
jobs, fg and bg are ordinary functions too: (fg 1) resumes job 1 from
Lisp.")

    ("editing" "keys, completion and colors"
     "At a slash-free command position, Tab completes command names and
directories. It completes file paths after a slash and in arguments,
and Lisp symbols in Lisp mode or inside a substitution. Directories end
with /, making command-position matches ready for implicit cd, and
special characters get escaped:

  git ch<Tab>              checkout and friends
  ls src/ma<Tab>           src/main.lisp
  (princ-to<Tab>           (princ-to-string
  echo (*bal<Tab>          echo (*balls*

Unique matches insert and several extend to the common prefix. Tab again
opens a candidate grid: arrows navigate, Tab cycles, Escape restores the
prefix, and typing keeps the selected candidate before inserting. The newest
history entry beginning with the current input appears in dim text; Right or
C-f at the end accepts it.

The built-in prompt shows username@hostname (PACKAGE) directory $. Set
*prompt-function* in startup.lisp to a function designator for another prompt.
It receives :status, :duration-milliseconds, :columns and :job-count keyword
arguments. A string is used verbatim; NIL selects prompt-default. Errors and
other values are reported and safely fall back. Use &allow-other-keys in custom
functions so future context additions remain compatible.

  Left/Right C-b/C-f   move/accept   Ctrl-arrows move by word
  Up/Down C-p/C-n      history       C-w/C-h     kill word
  Home/End C-a/C-e     line ends     C-k/C-u     kill rest/line
  Backspace / Delete   delete        C-l/C-c     clear/abort
  C-d                  delete forward, or exit on an empty line
  Alt-Enter            insert a newline without submitting
  Shift-Enter          same when the terminal reports modified Enter

Colors: external commands green, builtins and valid implicit directory
paths cyan, unknown red, lone bound variables magenta, strings yellow,
numbers cyan, $VAR magenta, globs and ~ bright magenta. In Lisp: known
operators in head position blue (a typo never lights up), keywords
magenta, constants cyan, bound *specials* magenta.")

    ("history" "what is remembered and where"
     "History persists in ~/.config/cclsh/history (XDG_CONFIG_HOME is
respected), one printed string per entry:

  \"ls -la\"
  \"(pipe (git \\\"log\\\") (head))\"

Loading keeps the newest 10000. Blank lines, aborted lines and
immediate duplicates are skipped. Non-interactive sessions neither
load nor write history. Multi-line entries recall with their original
newlines. The newest entry beginning with current input is offered as
a dim suggestion; Right or C-f accepts it. Writes keep the cclsh
configuration directory mode at 0700 and the history file at 0600.")

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
=, use $HOME in values.

CCLSH_PACKAGE is maintained automatically as the canonical name of the
current Lisp package. It is refreshed before the configured prompt renderer
and for external commands launched through cclsh, including after
(in-package ...). The built-in prompt displays it directly. CCL:RUN-PROGRAM
calls made elsewhere bypass the refresh performed at those boundaries.")

    ("startup" "configuration and safe mode"
     "~/.config/cclsh/startup.lisp loads for interactive sessions and
configured command strings in cclsh-user:

  (setenv 'editor \"vim\")
  (defcommand la (&rest arguments)
    \"Long listing including hidden files.\"
    (apply #'run \"ls\" \"-la\" arguments))
  (defvar *project* \"~/common-lisp/cclsh\")
  (zoxide-setup)

A broken startup file prints its error and the shell starts anyway.
CCLSH_SAFE=1 skips startup.lisp and history entirely, the escape hatch
when user state misbehaves. The repository's examples/startup.lisp is
a sanitized login-safe starting point.")

    ("directories" "directory hooks and zoxide"
     "Register a function to observe every successful directory change:

  (defun announce-directory (old new)
    (format t \"moved from ~a to ~a~%\" old new))
  (directory-change-hook-add 'announce-directory)
  (directory-change-hook-remove 'announce-directory)

Hooks run after PWD, OLDPWD and CCL's default directory are committed.
A failing hook is reported without undoing cd or skipping later hooks. Use
symbols for named hooks so redefinition retains identity. A hook cannot call
cd again; reentrant changes are rejected to keep later hook state coherent.

Install zoxide, and fzf for interactive selection, then call this once
from startup.lisp:

  (zoxide-setup)

After zoxide records the current directory successfully, setup installs its
change hook, z and zi. z with no arguments goes home, z - goes to OLDPWD, an
existing path is entered directly and other arguments query zoxide. zi runs
zoxide's interactive query. Running setup after zoxide disappears removes
its stale hook and command bindings.")

    ("install" "a reproducible user installation"
     "The recommended Linux x86-64 package is the Nix flake:

  nix run github:luciusmagn/cclsh -- --version
  nix profile install github:luciusmagn/cclsh

From a checkout, use nix run . or nix profile install .#cclsh. The flake
rebuilds CCL 1.13 from the exact downstream CCL commits and Clinedi revision
in dependencies.lock, and includes pinned Quicklisp metadata. nix flake check
builds and tests the installed result.

The package uses an existing ~/quicklisp or initializes a writable tree below
${XDG_DATA_HOME:-$HOME/.local/share}/cclsh/quicklisp. Override it with
CCLSH_QUICKLISP_HOME. Existing overrides must contain setup.lisp; invalid or
unwritable targets are reported and refused. Interactive and configured
sessions load local-init files, while plain commands, scripts and safe mode do
not. The flake does not edit /etc/shells or run chsh.

Source checks can use stock CCL. For a standalone saved image, select the exact
downstream CCL revision:

  git clone git@github.com:luciusmagn/ccl.git ../ccl
  git -C ../ccl checkout --detach 579c87300ee632af99182276f2ad40e1c38c5d0a
  make ccl-kernel CCL_SOURCE=../ccl

The fork is based on https://github.com/Clozure/ccl. Local files under
patches/ are byte-stable attestation mirrors. Nix fetches immutable diffs for
the exact fork commits. Select the rebuilt lx86cl64 and its matching boot image
for make build. README.org has the complete source and login installation
sequences.")

    ("scripting" "one-shots, scripts and shebangs"
     "Three non-interactive modes, all skipping startup.lisp and
history:

  cclsh -c 'echo one shot'        one command string
  cclsh provision.cclsh           a script file
  cclsh < input                   piped standard input

Load startup.lisp, but not history, for a configured one-shot:

  cclsh -lc 'z project'
  cclsh -ic 'echo $EDITOR'

Short flags combine in any order: -lc, -cl and -ilc are equivalent,
and -l -c works too. CCLSH_SAFE=1 suppresses startup.lisp even here.

Script files work as shebang interpreters:

  #!/usr/local/bin/cclsh
  (format t \"deploying~%\")
  (all (make \"build\") (make \"deploy\"))

*argv* is a list containing the script path followed by every argument
after it, and is NIL outside script mode. Arguments beginning with a dash
remain script data. Use -- before a dash-prefixed script path:

  cclsh -- -provision.cclsh alpha

Keep the executable basename cclsh. The patched CCL kernel recognizes that
name when preserving command and script arguments.

The process exit code is the last status, or the argument of exit. -c
is what ssh invokes, so remote commands skip user state. Programs already
visible in sshd's PATH work directly; user-profile-only tools need an
absolute path or a system-visible wrapper. The string uses cclsh syntax,
not sh.")

    ("login" "using cclsh as a login shell"
     "Install and register a root-owned copy outside the repository:

  scripts/check
  make login-build CCL_SOURCE=../ccl CCL=../ccl/lx86cl64 \\
    CCL_IMAGE=/usr/lib/ccl/lx86cl64.image
  make integration-check CCL=../ccl/lx86cl64 \\
    CCL_IMAGE=/usr/lib/ccl/lx86cl64.image
  sudo make install-login-shell LOGIN_USER=USER

Before the configured probe and chsh, copy examples/startup.lisp to
~/.config/cclsh/startup.lisp and make the directory mode 0700 and the file
mode 0600. Then verify:

  sudo -u USER env HOME=/home/USER XDG_CONFIG_HOME=/home/USER/.config \\
    SHELL=/usr/local/bin/cclsh \\
    /usr/local/bin/cclsh --version
  sudo -u USER env HOME=/home/USER XDG_CONFIG_HOME=/home/USER/.config \\
    SHELL=/usr/local/bin/cclsh CCLSH_SAFE=1 \\
    /usr/local/bin/cclsh -c 'exit 0'
  sudo -u USER env HOME=/home/USER XDG_CONFIG_HOME=/home/USER/.config \\
    SHELL=/usr/local/bin/cclsh \\
    /usr/local/bin/cclsh -lc 'echo $PATH'
  sudo chsh -s /usr/local/bin/cclsh USER
  getent passwd USER

Plain -c skips all user state for remote safety. -lc, -cl, -ic and
-l -c load startup.lisp before running their command, so $SHELL -lc
sees the configured login environment. Other flags are ignored so odd
login invocations cannot lock you out. login-build attests the required CCL
patches and exact kernel/image hashes. install-login-shell rejects an absent
or stale attestation, probes the candidate as USER before activation, then
registers its stable path in /etc/shells. Registration failure restores the
previous release. It never changes an account. USER must have a private
primary group, and one stable path can serve only one login account. Use a
different BINDIR for another account. Root is supported at a dedicated path:

  sudo make install-login-shell LOGIN_USER=root BINDIR=/usr/local/sbin

Probe optional startup tools such as zoxide separately after the guaranteed
version, safe command and configured PATH checks pass.

Nothing sources /etc/profile for you. Keep /usr/local/bin, /usr/bin and /bin
in PATH.

The conservative choice is to keep root or another privileged recovery
account on a stock shell. If root uses cclsh, keep an existing privileged
session open while testing and verify boot-loader or live-system recovery.
Emergency access past broken user state:

  env CCLSH_SAFE=1 /usr/local/bin/cclsh
  sudo chsh -s /usr/bin/fish USER"))
  "The built-in manual: (name one-liner body) per section.")

(defun manual--heading (text)
  "Render a manual heading, colored only on a terminal."
  (terminal-colorize text ':cyan :bold t))

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
              (terminal-colorize (format nil "~14a" name) ':cyan)
              one-liner)))
  (format t "~%help SECTION prints details. commands lists what is callable,~%~
             --version identifies the build, docs/guide.org is the long form.~%"))

(defun manual--print-section (name)
  "Print one manual section. Returns true when NAME exists."
  (let ((section (assoc name *manual-sections* :test #'string-equal)))
    (if (null section)
        (progn
          (format *error-output* "~a~%"
                  (terminal-colorize
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
