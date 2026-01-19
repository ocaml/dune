Here we test what happens if the program we are running with dune exec -w exits with a
non-zero exit code.

  $ DONE_FLAG=_build/done_flag

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (libraries unix))
  > EOF

  $ cat > foo.ml <<EOF
  > let touch path =
  >  let fd = Unix.openfile path [ Unix.O_CREAT; O_CLOEXEC ] 0o777 in
  >  Unix.close fd
  > ;;
  > 
  > let () =
  >   touch "$DONE_FLAG";
  >   Printf.eprintf "oh no!\n";
  >   exit 1
  > ;;
  > EOF

The build will still be considered successful even if the program being run fails. We
output the exit code of the program to be clear to the user. It's not useful anyway since
we are in watch mode.
  $ dune exec -w -- ./foo.exe &
  oh no!
  Program exited with code [1]
  Success, waiting for filesystem changes...
  $ PID=$!
  $ ./wait-for-file.sh $DONE_FLAG

  $ kill $PID
  $ wait $PID
  [130]
