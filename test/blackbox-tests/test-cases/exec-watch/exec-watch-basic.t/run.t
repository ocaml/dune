Test exec --watch with a program that terminates immediately.

File created by the program being exec'd. In between each experiment we'll wait
until the file exists so that dune has enough time to build and run the program
between each change to its code.
  $ export DONE_FLAG=_build/done_flag

  $ cat >foo.ml <<EOF
  > let () = print_endline "foo"; Touch.touch "$DONE_FLAG"
  > EOF

  $ dune exec --watch ./foo.exe &
  Success, waiting for filesystem changes...
  foo
  Success, waiting for filesystem changes...
  bar
  File "foo.ml", line 1, characters 23-24:
  1 | let () = print_endline "baz
                             ^
  Error: String literal not terminated
  Had errors, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  baz
  $ PID=$!

Wait for the $DONE_FLAG file to exist, then delete the file. This file is
created by the program being exec'd, so when it exists we know that it's safe to
change the code and proceed with the test.
  $ ../wait-for-file.sh $DONE_FLAG

  $ cat >foo.ml <<EOF
  > let () = print_endline "bar"; Touch.touch "$DONE_FLAG"
  > EOF

  $ ../wait-for-file.sh $DONE_FLAG

  $ cat >foo.ml <<EOF
  > let () = print_endline "baz
  > EOF

Wait until the error shows up in the log
  $ until grep 'print_endline "baz' _build/log > /dev/null; do sleep 0.1; done

  $ cat >foo.ml <<EOF
  > let () = print_endline "baz"; Touch.touch "$DONE_FLAG"
  > EOF

  $ ../wait-for-file.sh $DONE_FLAG

Prevent the test from leaking the dune process.
  $ kill $PID
