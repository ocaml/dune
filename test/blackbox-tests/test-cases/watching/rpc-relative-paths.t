RPC build requests interpret relative paths from the client's working directory.

Root discovery is disabled inside tests, so add a workspace file and unset
INSIDE_DUNE for commands run from subdirectories.

  $ make_dune_project 3.23
  $ echo '(lang dune 3.23)' > dune-workspace
  $ cat > dune <<'EOF'
  > (rule
  >  (target rpc-build-target)
  >  (action (with-stdout-to %{target} (echo root))))
  > (rule
  >  (alias runtest)
  >  (target rpc-runtest-root)
  >  (action (with-stdout-to %{target} (echo root))))
  > (executable
  >  (name root))
  > EOF
  $ echo 'let ()=print_int (5+4)' > root.ml

  $ mkdir sub
  $ cat > sub/dune <<'EOF'
  > (rule
  >  (target rpc-build-target)
  >  (action (with-stdout-to %{target} (echo sub))))
  > (rule
  >  (alias runtest)
  >  (target rpc-runtest-sub)
  >  (action (with-stdout-to %{target} (echo sub))))
  > (executable
  >  (name nested))
  > EOF
  $ echo 'let ()=print_int (6+5)' > sub/nested.ml

Start the server from a third directory so its working directory cannot affect
how it interprets the paths sent by the client.

  $ mkdir server
  $ cat > server/dune <<'EOF'
  > (rule
  >  (target rpc-build-target)
  >  (action (with-stdout-to %{target} (echo server))))
  > (rule
  >  (alias runtest)
  >  (target rpc-runtest-server)
  >  (action (with-stdout-to %{target} (echo server))))
  > (executable
  >  (name server))
  > EOF
  $ echo 'let ()=print_int (7+6)' > server/server.ml

  $ cd server
  $ unset INSIDE_DUNE
  $ start_dune
  $ cd ..

A forwarded build resolves its target from the client's directory.

  $ (cd sub && unset INSIDE_DUNE; dune build rpc-build-target) 2>/dev/null
  $ find _build/default -name rpc-build-target -type f -print
  _build/default/sub/rpc-build-target

Exec also builds relative programs from the client's directory.

  $ (cd sub && dune exec ./nested.exe) >/dev/null 2>&1
  $ find _build/default -name nested.exe -type f -print
  _build/default/sub/nested.exe

The default target is also relative to the client's directory.

  $ (cd sub && dune build) 2>/dev/null
  $ find _build/default -name '*.exe' -type f -print
  _build/default/sub/nested.exe

A forwarded runtest request also uses the client's directory.

  $ (cd sub && unset INSIDE_DUNE; dune runtest) 2>/dev/null
  $ find _build/default -name 'rpc-runtest-*' -type f -print
  _build/default/sub/rpc-runtest-sub

  $ stop_dune_quiet
