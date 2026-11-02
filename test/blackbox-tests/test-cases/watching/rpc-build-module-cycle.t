Building over RPC when there is a module cycle reports the cycle error.

  $ make_dune_project 3.18
  $ cat > dune <<EOF
  > (executable (name main))
  > EOF
  $ echo 'let x = B.x' > a.ml
  $ echo 'let x = A.x' > b.ml
  $ echo 'let () = print_int A.x' > main.ml

  $ dune build --passive-watch-mode > .#dune-output 2>&1 &
  $ DUNE_PID=$!
  $ wait_for_rpc_server

  $ with_timeout dune rpc build --wait ./main.exe
  Failure
  [1]

  $ with_timeout dune build ./main.exe 2>&1 | sed '/^Raised/,/^$/d'
  Error: Dependency cycle between:
     transitive deps of dune__exe__A.impl in _build/default
  -> transitive deps of dune__exe__B.impl in _build/default
  -> transitive deps of dune__exe__A.impl in _build/default
  Error: Build failed with 1 error.
  [1]

  $ stop_dune > /dev/null
