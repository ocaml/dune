Testing stdin for watch mode dune exec
 
We use the done flag as a signal that the program has finished.
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
  >  let fd = Unix.openfile path [ Unix.O_CREAT; O_CLOEXEC ] 0o777 in
  >  Unix.close fd
  > ;;
  > EOF
 
  $ cat > foo.ml <<EOF
  > let () =
  >   print_endline "what is your name?";
  >   let name = read_line () in
  >   print_endline ("hello " ^ name ^ "!");
  >   Touch.touch "$DONE_FLAG"
  > ;;
  > EOF

Our program takes in some input from stdin, so we need to make sure dune passes this along
correctly. To simulate giving this input later we use a pipe.
  $ mkfifo input.pipe
  $ dune exec -w -- ./foo.exe < input.pipe &
  what is your name?
  hello John Doe!
  Success, waiting for filesystem changes...
  (2) what is your name?
  (2) hello Alice Johnson!
  Success, waiting for filesystem changes...
  $ PID=$!

  $ cat > input.pipe <<EOF
  > John Doe
  > EOF
  $ ./wait-for-file.sh $DONE_FLAG

We can trigger arebuild and give another input.
  $ cat > foo.ml <<EOF
  > let () =
  >   print_endline "(2) what is your name?";
  >   let name = read_line () in
  >   print_endline ("(2) hello " ^ name ^ "!");
  >   Touch.touch "$DONE_FLAG"
  > ;;
  > EOF
  $ cat > input.pipe <<EOF
  > Alice Johnson
  > EOF

  $ ./wait-for-file.sh $DONE_FLAG

  $ kill $PID
  $ wait $PID
  [130]
