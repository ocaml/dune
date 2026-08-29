RPC build requests currently interpret relative paths from the server's working
directory rather than the client's working directory.

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
  $ touch .ocamlformat

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

Start the server from a third directory so that incorrect resolution from its
working directory is visible.

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

A forwarded build resolves its target from the server's directory.

  $ (cd sub && unset INSIDE_DUNE; dune build rpc-build-target) 2>/dev/null
  $ find _build/default -name rpc-build-target -type f -print
  _build/default/server/rpc-build-target

The build request made by exec also loses the client's directory.

  $ (cd sub && dune exec ./nested.exe) >/dev/null 2>&1
  [1]
  $ find _build/default -name nested.exe -type f -print

The default target also resolves from the server's directory.

  $ (cd sub && dune build) 2>/dev/null
  $ find _build/default -name '*.exe' -type f -print
  _build/default/server/server.exe

A forwarded runtest request does the same.

  $ (cd sub && unset INSIDE_DUNE; dune runtest) 2>/dev/null
  $ find _build/default -name 'rpc-runtest-*' -type f -print
  _build/default/server/rpc-runtest-server

The format RPC has no path argument, so it formats recursively from the
server's directory rather than the client's directory.

  $ (cd sub && unset INSIDE_DUNE; dune fmt) 2>/dev/null
  $ cat root.ml
  let ()=print_int (5+4)
  $ cat sub/nested.ml
  let ()=print_int (6+5)
  $ cat server/server.ml
  (* fake ocamlformat output *)

  $ stop_dune_quiet
