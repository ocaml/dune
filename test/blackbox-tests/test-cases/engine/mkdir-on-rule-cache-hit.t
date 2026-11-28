  $ cat > dune-project <<'EOF'
  > (lang dune 3.20)
  > EOF
  $ mkdir sub
  $ cat > sub/dune <<'EOF'
  > (rule
  >  (target a)
  >  (action (write-file %{target} a)))
  > (rule
  >  (target b)
  >  (action (write-file %{target} b)))
  > EOF

Warm the workspace-local rule cache.

  $ dune build sub/a sub/b

A null build does not try to create the common target directory when both
rules hit the workspace-local cache.

  $ strace -e trace=mkdir,mkdirat -o trace dune build sub/a sub/b
  $ grep -c 'mkdir.*"_build/default/sub"' trace || true
  0

A redirection should be closed when the first command in a sequence fails.
Trace just the Dune process: it owns the descriptor shared by the sequence.

  $ cat > dune <<'EOF'
  > (rule
  >  (deps (sandbox none))
  >  (action
  >   (with-stdout-to leaked.txt
  >    (progn (system "printf partial; exit 1") (echo unreachable)))))
  > EOF
  $ strace -y -e trace=openat,close -o io.trace dune build leaked.txt > build.log 2>&1
  [1]
  $ grep -c 'openat(.*"_build/default/leaked.txt"' io.trace
  1

CR: Dune does not close the descriptor after the action fails.

  $ grep -c 'close(.*</.*_build/default/leaked.txt>' io.trace || true
  0

A stdin redirection should be closed when the command fails.

  $ printf 'input\n' > stdin-input.txt
  $ dune build stdin-input.txt
  $ cat > dune <<'EOF'
  > (rule
  >  (target stdin-leaked)
  >  (deps (sandbox none))
  >  (action
  >   (with-stdin-from stdin-input.txt
  >    (system "exit 1"))))
  > EOF
  $ strace -y -e trace=openat,close -o stdin.trace dune build stdin-leaked > stdin.log 2>&1
  [1]
  $ grep -c 'openat(.*"_build/default/stdin-input.txt"' stdin.trace
  1

CR: Dune does not close the stdin descriptor after the action fails.

  $ grep -c 'close(.*</.*_build/default/stdin-input.txt>' stdin.trace || true
  0

A buffered output channel redirection should be closed when a later step fails.

  $ cat > dune <<'EOF'
  > (rule
  >  (deps (sandbox none))
  >  (action
  >   (with-stdout-to channel-leaked.txt
  >    (progn (echo partial) (run nonexistent-prog)))))
  > EOF
  $ strace -y -e trace=openat,close -o channel.trace dune build channel-leaked.txt > channel.log 2>&1
  [1]
  $ grep -c 'openat(.*"_build/default/channel-leaked.txt"' channel.trace
  1

CR: Dune does not close the buffered channel descriptor after the action fails.

  $ grep -c 'close(.*</.*_build/default/channel-leaked.txt>' channel.trace || true
  0

An unopened output redirection must not be opened or created when the action
fails before using it.

  $ cat > dune <<'EOF'
  > (rule
  >  (deps (sandbox none))
  >  (action
  >   (with-stdout-to untouched.txt
  >    (run nonexistent-prog))))
  > EOF
  $ strace -y -e trace=openat,close -o untouched.trace dune build untouched.txt > untouched.log 2>&1
  [1]
  $ grep -c 'openat(.*"_build/default/untouched.txt"' untouched.trace || true
  0
  $ test ! -e _build/default/untouched.txt

Unopened redirections to or from nonexistent paths must not add secondary open
errors when the action fails before using them.

  $ cat > dune <<'EOF'
  > (rule
  >  (target untouched-missing)
  >  (deps (sandbox none))
  >  (action
  >   (no-infer
  >    (with-stdin-from missing-input.txt
  >     (with-stdout-to missing-dir/untouched.txt
  >      (run nonexistent-prog))))))
  > EOF
  $ strace -y -e trace=openat,close -o missing.trace dune build untouched-missing
  File "dune", line 8, characters 10-26:
  8 |      (run nonexistent-prog))))))
                ^^^^^^^^^^^^^^^^
  Error: Program nonexistent-prog not found in the tree or in PATH
   (context: default)
  [1]
  $ grep -c 'openat(.*"missing-input.txt"' missing.trace || true
  0
  $ grep -c 'openat(.*"missing-dir/untouched.txt"' missing.trace || true
  0
