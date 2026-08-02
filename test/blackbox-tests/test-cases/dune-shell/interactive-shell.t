With no command after the target, `dune shell` opens bash in the prepared
environment.  Piping a script into its stdin keeps this path deterministic in a
cram test: the non-interactive bash still sources the session helpers through
[BASH_ENV], so the piped commands can call them.

  $ cat > dune-project <<'EOF'
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<'EOF'
  > (rule
  >  (target prepared-input)
  >  (deps input)
  >  (action (with-stdout-to %{target} (cat input))))
  > 
  > (rule
  >  (target out)
  >  (deps prepared-input)
  >  (action (with-stdout-to %{target} (cat prepared-input))))
  > EOF

  $ echo data > input

The piped script runs inside the session shell.  A real interactive session
loads the helper commands from its --rcfile automatically; a non-interactive
bash driven by a pipe does not read the rcfile, so the script sources the same
$DUNE_SHELL/init.bash itself.  It checks the exact mapped rule directory and
the state at the intercepted action boundary, confirms the helper commands are
available, and replays the action with [run].  Changing the source after entry
must not start another build system: replay executes only the action against
the dependencies already in the sandbox.

  $ dune shell --sandbox=copy _build/default/out >shell.stdout 2>shell.stderr <<'EOF'
  > . "$DUNE_SHELL/init.bash" >/dev/null 2>&1
  > expected="$(cat "$DUNE_SHELL/sandbox")/default"
  > if test "$PWD" = "$expected"; then
  >   echo "cwd: exact mapped directory"
  > else
  >   echo "cwd: unexpected: $PWD"
  > fi
  > if test -e out; then echo "target-on-entry: present"; else echo "target-on-entry: absent"; fi
  > printf "declared: "; cat prepared-input
  > if type run show_action help >/dev/null 2>&1; then
  >   echo "helpers: sourced"
  > else
  >   echo "helpers: missing"
  > fi
  > if test "$(type -P run)" = "$DUNE_SHELL/run"; then
  >   echo "run-executable: on PATH"
  > else
  >   echo "run-executable: missing"
  > fi
  > show_action | grep -q with-stdout-to && echo "show_action: works"
  > help | grep -qi 'not an isolation boundary' && echo "help-notes: shown"
  > help | grep -qi 'sandbox mode copy' && echo "help-mode: shown"
  > test -n "${TMPDIR:-${TEMP:-}}" && echo "tmpdir: set"
  > printf 'changed after entry\n' > "$PWD/input"
  > env run
  > printf "replay: "; cat out
  > echo "execution-path: $PWD"
  > EOF
  $ grep -v '^execution-path:' shell.stdout
  cwd: exact mapped directory
  target-on-entry: absent
  declared: data
  helpers: sourced
  run-executable: on PATH
  show_action: works
  help-notes: shown
  help-mode: shown
  tmpdir: set
  replay: data

The live build owns the ordinary [Sandbox.with_] scope. Once the shell exits,
that scope destroys the sandbox, and replay has not extracted its target into
the real build directory.

  $ execution_path=$(sed -n 's/^execution-path: //p' shell.stdout)
  $ if test -e "$execution_path"; then
  >   echo "sandbox-after-shell: present"
  > else
  >   echo "sandbox-after-shell: removed"
  > fi
  sandbox-after-shell: removed
  $ if test -e _build/default/out; then
  >   echo "build-target-after-shell: present"
  > else
  >   echo "build-target-after-shell: absent"
  > fi
  build-target-after-shell: absent

On systems with util-linux [script], exercise a real interactive shell through
a pseudo-terminal.  The shell gives each foreground job control of the
terminal, so Ctrl+C and Ctrl-\ stop those jobs without reaching the live Dune
process.  Dune's progress line must also stay out of the shell session.

  $ pty_status=0
  $ if script --version 2>&1 | grep -q util-linux; then
  >   export SHELL="$(command -v bash)"
  >   export PTY_ECHO_OFF=$PWD/pty-echo-off
  >   export PTY_INT_READY=$PWD/pty-int-ready
  >   export PTY_INT_SURVIVED=$PWD/pty-int-survived
  >   export PTY_QUIT_READY=$PWD/pty-quit-ready
  >   export PTY_QUIT_SURVIVED=$PWD/pty-quit-survived
  >   export PTY_SANDBOX=$PWD/pty-sandbox
  >   {
  >     printf '%s\n' 'stty -echo; : > "$PTY_ECHO_OFF"'
  >     with_timeout dune_cmd wait-for-file-to-appear "$PTY_ECHO_OFF"
  >     printf '%s\n' 'PS1=; PS2=; printf "__ENTERED__\n"'
  >     printf '%s\n' 'printf "%s\n" "$PWD" > "$PTY_SANDBOX"'
  >     printf '%s\n' 'sh -c ": > \"$PTY_INT_READY\"; exec sleep 30"'
  >     with_timeout dune_cmd wait-for-file-to-appear "$PTY_INT_READY"
  >     printf '\003'
  >     printf '%s\n' ': > "$PTY_INT_SURVIVED"'
  >     printf '%s\n' 'sh -c ": > \"$PTY_QUIT_READY\"; exec sleep 30"'
  >     with_timeout dune_cmd wait-for-file-to-appear "$PTY_QUIT_READY"
  >     printf '\034'
  >     printf '%s\n' ': > "$PTY_QUIT_SURVIVED"'
  >     printf '%s\n' 'printf "__LEAVING__\n"; exit'
  >   } | with_timeout script -q -e -f -c \
  >       'dune shell --display=progress --sandbox=copy _build/default/out' \
  >       /dev/null >pty.transcript 2>&1
  >   command_status=$?
  >   tr -d '\r' <pty.transcript |
  >     sed -n '/__ENTERED__/,/__LEAVING__/p' >pty-session
  >   if test "$command_status" -eq 0 &&
  >      test -e "$PTY_INT_SURVIVED" &&
  >      test -e "$PTY_QUIT_SURVIVED" &&
  >      test -s "$PTY_SANDBOX" &&
  >      test ! -e "$(cat "$PTY_SANDBOX")" &&
  >      grep -q 'session commands' pty.transcript &&
  >      ! grep -q 'Done:' pty-session; then
  >     :
  >   else
  >     pty_status=1
  >     cat pty.transcript >&2
  >   fi
  > fi
  > echo "pty-job-control: checked when supported"
  > test "$pty_status" -eq 0
  pty-job-control: checked when supported
