Testing how dune exec handles errors in the program being run.

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name foo))
  > EOF

If a program exits with a non-zero exit code, we should return that code.

  $ cat > foo.ml <<EOF
  > let () =
  >   Printf.eprintf "oh no!\n";
  >   exit 1
  > ;;
  > EOF

  $ dune exec -- ./foo.exe
  oh no!
  [1]

If a program encounters an exception, we should return a non-zero exit code.

  $ cat > foo.ml <<EOF
  > let () =
  >   raise (Failure "oh no!")
  > ;;
  > EOF

  $ dune exec -- ./foo.exe
  Fatal error: exception Failure("oh no!")
  [2]

Disable shell monitoring (of jobs) so that we do not get spurious messages
about signals.
  $ set +m

If a program segfaults, we should return a non-zero exit code.

  $ cat > foo.ml <<EOF
  > let () =
  >   let f = Obj.magic 0 in
  >   f 1
  >   [@@warning "-20"]
  > ;;
  > EOF

Note that 128 + 11 (SEGV) = 139
  $ { dune exec -- ./foo.exe; } 2> /dev/null
  [139]

If a program is killed by a signal before it terminates, we should return a non-zero exit
code.

  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (libraries unix))
  > EOF

  $ cat > foo.ml <<EOF
  > let () =
  >   let pid = Unix.getpid () in
  >   Printf.printf "Killing self";
  >   Unix.kill pid Sys.sigkill
  > ;;
  > EOF

  $ { dune exec -- ./foo.exe; } 2> /dev/null
  [137]
