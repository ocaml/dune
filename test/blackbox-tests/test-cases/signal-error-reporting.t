Report signals even when a command produces output (#16575).

  $ make_dune_project 3.24
  $ ulimit -c 0

  $ cat > dune <<'EOF'
  > (test
  >  (name test_segfault)
  >  (libraries unix)
  >  (action (run %{test} %{env:OUTPUT=none})))
  > EOF

  $ cat > test_segfault.ml <<'EOF'
  > let () =
  >   (match Sys.argv.(1) with
  >    | "stdout" -> print_endline "Beginning tests..."
  >    | "stderr" -> prerr_endline "Beginning tests..."
  >    | "newline" -> print_endline ""
  >    | _ -> ());
  >   Unix.kill (Unix.getpid ()) Sys.sigsegv
  > EOF

Without output, the signal is reported.

  $ dune runtest
  File "dune", line 2, characters 7-20:
  2 |  (name test_segfault)
             ^^^^^^^^^^^^^
  Command got signal SEGV.
  [1]

Output on stdout must not hide the signal.

  $ OUTPUT=stdout dune runtest
  File "dune", line 2, characters 7-20:
  2 |  (name test_segfault)
             ^^^^^^^^^^^^^
  Command got signal SEGV.
  Beginning tests...
  [1]

The process trace still records the signal.

  $ dune trace cat | jq_dune \
  >   'processes | select(.args.prog | endswith("test_segfault.exe"))
  >    | .args | {stdout, error}'
  {
    "stdout": "Beginning tests...\n",
    "error": "got signal SEGV"
  }

Output on stderr must not hide the signal either.

  $ OUTPUT=stderr dune runtest
  File "dune", line 2, characters 7-20:
  2 |  (name test_segfault)
             ^^^^^^^^^^^^^
  Command got signal SEGV.
  Beginning tests...
  [1]

A single newline must not hide the signal.

  $ OUTPUT=newline dune runtest
  File "dune", line 2, characters 7-20:
  2 |  (name test_segfault)
             ^^^^^^^^^^^^^
  Command got signal SEGV.
  
  [1]

Quiet display must also report the signal.

  $ OUTPUT=stdout dune runtest --display quiet
  File "dune", line 2, characters 7-20:
  2 |  (name test_segfault)
             ^^^^^^^^^^^^^
  Command got signal SEGV.
  Beginning tests...
  [1]

Short display reports the signal alongside the output.

  $ OUTPUT=stdout dune runtest --display short
  File "dune", line 2, characters 7-20:
  2 |  (name test_segfault)
             ^^^^^^^^^^^^^
  test_segfault (anonymous) (got signal SEGV)
  Beginning tests...
  [1]

Verbose display also reports the signal.

  $ OUTPUT=stdout dune runtest --display verbose >verbose-output 2>&1
  [1]
  $ grep 'got signal' verbose-output \
  >   | dune_cmd subst 'Command \[[0-9]+\]' 'Command [ID]'
  Command [ID] got signal SEGV:
