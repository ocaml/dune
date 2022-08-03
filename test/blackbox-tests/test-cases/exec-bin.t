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
  > #!/bin/bash
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
