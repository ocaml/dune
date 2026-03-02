Relative paths in %{bin:...} and %{bin-available:...} are resolved from the
current directory. See #9564.

  $ cat > dune-project << EOF
  > (lang dune 3.10)
  > 
  > (package
  >  (name p))
  > EOF

  $ cat > dune << EOF
  > (executable
  >  (public_name e)
  >  (package p))
  > 
  > (executable
  >  (public_name disabled)
  >  (package p)
  >  (modules disabled)
  >  (enabled_if false))
  > 
  > (rule
  >  (alias all)
  >  (action
  >   (progn
  >    (echo "available e: %{bin-available:./e}\n")
  >    (echo "available root foo: %{bin-available:./foo}\n")
  >    (echo "available pathonly: %{bin-available:./pathonly}\n")
  >    (echo "available dironly: %{bin-available:./dironly}\n")
  >    (echo "available disabled: %{bin-available:./disabled}\n")
  >    (run %{bin:./pathonly})
  >    (run %{bin:./e}))))
  > EOF
  $ mkdir path-bin
  $ cat > path-bin/pathonly << EOF
  > #!/bin/sh
  > echo "running pathonly"
  > EOF
  $ chmod +x path-bin/pathonly
  $ mkdir dironly
  $ export PATH=$PWD:$PWD/path-bin:$PATH
  $ mkdir sub
  $ cat > sub/dune << EOF
  > (executable
  >  (public_name nested)
  >  (package p)
  >  (modules nested))
  > 
  > (env
  >  (_ (binaries (nested.exe as foo))))
  > 
  > (rule
  >  (alias all)
  >  (action
  >   (echo "available nested foo: %{bin-available:./foo}\n")))
  > EOF
  $ cat > e.ml << EOF
  > let () = print_endline "running e"
  > EOF
  $ touch sub/nested.ml
  $ touch disabled.ml

  $ dune build @all
  available nested foo: true
  available e: true
  available root foo: false
  available pathonly: true
  available dironly: false
  available disabled: false
  running pathonly
  running e
