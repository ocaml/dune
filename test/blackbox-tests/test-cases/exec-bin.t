  $ cat > dune-project << EOF
  > (lang dune 1.1)
  > 
  > (package
  >  (name e))
  > EOF
  $ cat > dune << EOF
  > (executable
  >  (public_name e))
  > EOF

The executable just displays "Hello" and its arguments.

  $ cat > e.ml << EOF
  > let () =
  >   print_endline "Hello";
  >   Array.iteri (fun i s ->
  >     Printf.printf "argv[%d] = %s\n" i s
  >     ) Sys.argv
  > EOF

By default, e is executed with the program name and arguments in argv.

  $ dune exec ./e.exe a b c
  Hello
  argv[0] = _build/default/e.exe
  argv[1] = a
  argv[2] = b
  argv[3] = c

The special form %{bin:public_name} is supported.

  $ dune exec %{bin:e} a b c
  Hello
  argv[0] = _build/install/default/bin/e
  argv[1] = a
  argv[2] = b
  argv[3] = c

This wrapper parses its own arguments and executes the rest.

  $ cat > wrap.sh << 'EOF'
  > #!/bin/sh
  > while getopts "xy" o; do
  >   echo "Got option: $o"
  > done
  > shift $((OPTIND-1))
  > echo Before
  > "$@"
  > echo After
  > EOF
  $ chmod +x wrap.sh

It is possible to put the %{bin:...} pform in arguments rather than first.

  $ dune exec -- ./wrap.sh -x -y %{bin:e} a b c
  Got option: x
  Got option: y
  Before
  Hello
  argv[0] = _build/install/default/bin/e
  argv[1] = a
  argv[2] = b
  argv[3] = c
  After

The first item is still looked up in PATH.

  $ dune exec ls %{bin:e}
  _build/install/default/bin/e

Pforms can appear several times.

  $ dune exec ls %{bin:e} %{bin:e}
  _build/install/default/bin/e
  _build/install/default/bin/e

It should also be possible to call another program that is also supposed to be
built if referenced, for this we create a new binary that calls its first
argument:

  $ cat > call_arg.ml << EOF
  > let () =
  >   let first = Sys.argv.(1) in
  >   Printf.printf "Calling my first arg, %S:\n" first;
  >   let inch, outch = Unix.open_process_args first [|first|] in
  >   print_endline (input_line inch);
  >   let status = Unix.close_process (inch, outch) in
  >   match status with
  >   | Unix.WEXITED 0 -> print_endline "All good"
  >   | _ -> print_endline "Something is Rotten in the State of Dune"
  > EOF
  $ cat > called.ml << EOF
  > let () = print_endline "I was called"
  > EOF
  $ cat > dune << EOF
  > (executables
  >  (public_names e call_arg called)
  >  (libraries unix))
  > EOF

If we then ask it to execute, both `call_arg` and `called` should be compiled
and run, successfully.

  $ dune exec %{bin:call_arg} %{bin:called}
  Calling my first arg, "_build/install/default/bin/called":
  I was called
  All good
