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
