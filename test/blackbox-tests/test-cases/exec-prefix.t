"dune exec --prefix wrap ./e.exe args" behaves like "dune exec ./e.exe args"
except that it executes "wrap" with the rest passed as arguments instead.

This is useful for "adverbial" commands like time or perf.

  $ cat > dune-project << EOF
  > (lang dune 1.0)
  > EOF
  $ cat > dune << EOF
  > (executable
  >  (name e))
  > EOF

The executable just displays "Hello" and its arguments.

  $ cat > e.ml << EOF
  > let () =
  >   print_endline "Hello";
  >   Array.iteri (fun i s ->
  >     Printf.printf "argv[%d] = %s\n" i s
  >     ) Sys.argv
  > EOF

The wrapper parses its own arguments and executes the rest.

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

With no wrapper, e is executed with the program name and arguments in argv.

  $ dune exec ./e.exe a b c
  Hello
  argv[0] = _build/default/e.exe
  argv[1] = a
  argv[2] = b
  argv[3] = c

With just a string as prefix, the wrapper sees no arguments and executes the
program with its arguments.

  $ dune exec --prefix ./wrap.sh ./e.exe a b c
  Before
  Hello
  argv[0] = _build/default/e.exe
  argv[1] = a
  argv[2] = b
  argv[3] = c
  After

--prefix is actually space-separated: the wrapper gets the ones passed in
--prefix, and the executable gets the rest (this is up to the wrapper, but most
work this way).

  $ dune exec --prefix './wrap.sh -x -y' ./e.exe a b c
  Got option: x
  Got option: y
  Before
  Hello
  argv[0] = _build/default/e.exe
  argv[1] = a
  argv[2] = b
  argv[3] = c
  After

When the prefix is empty, this is equivalent to when no prefix is passed.

  $ dune exec --prefix '' ./e.exe a b c
  Hello
  argv[0] = _build/default/e.exe
  argv[1] = a
  argv[2] = b
  argv[3] = c
