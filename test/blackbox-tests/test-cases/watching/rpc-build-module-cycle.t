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
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  providing the file _build/trace.csexp, if possible. This includes build
  commands, message logs, and file paths.
  Description:
    ("Build via RPC failed, but the RPC server did not send an error message.",
     {})
  I must not crash.  Uncertainty is the mind-killer. Exceptions are the
  little-death that brings total obliteration.  I will fully express my cases. 
  Execution will pass over me and through me.  And when it has gone past, I
  will unwind the stack along its path.  Where the cases are handled there will
  be nothing.  Only I will remain.
  [1]

  $ stop_dune > /dev/null
