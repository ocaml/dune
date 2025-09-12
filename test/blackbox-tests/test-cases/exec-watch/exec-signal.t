Here we test what happens if there is a signal for the program to terminate
when we are running with dune exec. Signals are dispatched from the operating
system. It is usually impossible for a program to recover after recieving such
a signal.

For dune exec -w, we are indifferent to what the process we are running is
actually doing since it should not affect the other functions of dune.

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

This first program will signal itself with a SEGV signal.:
  $ cat > foo.ml <<EOF
  > let () =
  >   Touch.touch "$DONE_FLAG";
  >   print_endline "about to be killed";
  >   let pid = Unix.getpid () in
  >   Unix.kill pid Sys.sigsegv
  > ;;
  > EOF

  $ LOG_FILE=_build/log_file
  $ mkdir _build

When reaching a signal like SEGV dune exec -w will exit.
  $ dune exec -w ./foo.exe 2> $LOG_FILE &
  about to be killed
  fixed signal
  $ PID=$!
  $ ./wait-for-file.sh $DONE_FLAG

Waiting for SEGV signal...
  $ tail -f "$LOG_FILE" | while read line; do
  >   echo "$line" | grep 'SEGV' && break
  > done
  Command got signal SEGV.

We can now start a new build by modifying the original program and removing the
segfault. This rebuilds successfully as indicated by the above output.
  $ cat > foo.ml <<EOF
  > let () =
  >   Touch.touch "$DONE_FLAG";
  >   print_endline "fixed signal";
  > ;;
  > EOF

  $ ./wait-for-file.sh $DONE_FLAG

Here is what dune exec -w was outputting.
  $ cat $LOG_FILE
  Command got signal SEGV.
  Had 1 error, waiting for filesystem changes...
  Success, waiting for filesystem changes...

