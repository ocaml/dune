Test exec --watch with a program that doesn't terminate immediately.

File created by the program being exec'd. In between each experiment we'll wait
until the file exists so that dune has enough time to build and run the program
between each change to its code.
  $ export DONE_FLAG=_build/done_flag

  $ cat >foo.ml <<EOF
  > let () =
  >   print_endline "0: before";
  >   Touch.touch "$DONE_FLAG";
  >   Unix.sleep 1000;
  >   print_endline "0: after"
  > EOF

  $ dune exec --watch ./foo.exe &
  Success, waiting for filesystem changes...
  0: before
  Success, waiting for filesystem changes...
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
  >   Unix.sleep 1000;
  >   print_endline "2: after"
  > EOF

  $ ../wait-for-file.sh $DONE_FLAG

Prevent the test from leaking the dune process.
  $ kill $PID
