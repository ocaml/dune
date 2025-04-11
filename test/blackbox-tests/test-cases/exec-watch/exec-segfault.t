Here we test what happens if there is a segfault in the program we are running with dune
exec. Segfaults are determined and signalled from the operating system. It is usually
impossible for a program to recover after recieving such a signal.

For dune exec -w, we are indifferent to what the process we are running is actually doing
since it shouldn't affect dune's other functions.

TODO: It would be nice for Dune to indicate to the user that the subprocess was terminated
in this way. But for now we fail and stop the program silently.
  $ DONE_FLAG=_build/done_flag

  $ cat > dune-project <<EOF
  > (lang dune 3.18)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (libraries unix))
  > EOF

  $ cat > touch.ml <<EOF
  > let touch path =
  >  let fd = Unix.openfile path [ Unix.O_CREAT ] 0o777 in
  >  Unix.close fd
  > ;;
  > EOF

This first program will cause a segfault by using Obj.magic and calling an integer like it
is a function.
  $ cat > foo.ml <<EOF
  > let () =
  >   let f = Obj.magic 0 in
  >   Touch.touch "$DONE_FLAG";
  >   print_endline "about to segfault";
  >   f 1  (* Segfault: calling an int as if it is a function *)
  >   [@@warning "-20"]
  > ;;
  > EOF

When we start ./foo.exe with dune exec -w we note that we haven't exited, but simply
finished a build.
  $ dune exec -w ./foo.exe &
  about to segfault
  fixed segfault
  Success, waiting for filesystem changes...
  $ PID=$!
  $ ./wait-for-file.sh $DONE_FLAG

We can now start a new build by modifying the original program and removing the segfault.
This rebuilds successfully as indicated by the above output.
  $ cat > foo.ml <<EOF
  > let () =
  >   Touch.touch "$DONE_FLAG";
  >   print_endline "fixed segfault";
  > ;;
  > EOF

  $ ./wait-for-file.sh $DONE_FLAG

  $ kill $PID
