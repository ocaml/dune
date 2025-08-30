Test exec --watch with a program that does not terminate immediately.

File created by the program being execed. In between each experiment we will
wait until the file exists so that dune has enough time to build and run the
program between each change to its code.

  $ export DONE_FLAG=_build/done_flag

  $ cat >foo.ml <<EOF
  > let () =
  >   print_endline "0: before";
  >   Touch.touch "$DONE_FLAG";
  >   Unix.sleep 1;
  >   print_endline "0: after"
  > EOF

Below, 0: after should *not* be appearing.

  $ dune exec --watch ./foo.exe &
  0: before
  0: after
  1: before
  1: after
  Success, waiting for filesystem changes...
  2: before
  $ PID=$!

  $ ../wait-for-file.sh $DONE_FLAG

Change the program so that it terminates immediately.
  $ cat >foo.ml <<EOF
  > let () =
  >   print_endline "1: before";
  >   Unix.sleep 0;
  >   print_endline "1: after";
  >   Touch.touch "$DONE_FLAG"
  > EOF

  $ ../wait-for-file.sh $DONE_FLAG

Change the program so that it no longer terminates immediately.
  $ cat >foo.ml <<EOF
  > let () =
  >   print_endline "2: before";
  >   Touch.touch "$DONE_FLAG";
  >   Unix.sleep 1;
  >   print_endline "2: after"
  > EOF

  $ ../wait-for-file.sh $DONE_FLAG

