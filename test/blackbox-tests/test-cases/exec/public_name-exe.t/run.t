Test case for https://github.com/ocaml/dune/issues/3322
"dune exec needs to add .exe on Windows"

  $ os_type=$(ocamlc -config-var os_type)
  $ echo 'let () = print_endline "Hello, World!"' > example.ml

Test that on Windows `dune exec -- public_name` and
`dune exec -- public_name.exe` have the same effect.

With extension
==============

  $ dune clean
  $ dune build @install
  $ if [ $os_type = Win32 ]; then dune exec -- shaihulud.exe; else echo "Hello, World!"; fi
  Hello, World!

Without extension, prebuild
=================

  $ dune clean
  $ dune build @install
  $ dune exec -- shaihulud
  Hello, World!


Test that `dune exec -- public_name` (omitting the .exe) works from a
clean state.

Without extension, clean state
==============================

  $ dune clean
  $ dune exec -- shaihulud
  Hello, World!


Test that the public name resolves well to the actual executable file
when a dependency changes. On platforms where there are no symlinks,
updating the public_name executable matters.

With extension, prebuild
==============

  $ dune clean
  $ dune build @install

  $ if [ $os_type = Win32 ]; then dune exec -- shaihulud.exe; else echo "Hello, World!"; fi
  Hello, World!
  $ dune exec -- ./example.exe
  Hello, World!
  $ sed -i.bak 's/World/Arrakis/' example.ml
  $ if [ $os_type = Win32 ]; then dune exec -- shaihulud.exe; else echo "Hello, Arrakis!"; fi
  Hello, Arrakis!
  $ dune exec -- ./example.exe
  Hello, Arrakis!

Without extension, prebuild
=================

  $ sed -i.bak 's/Arrakis/World/' example.ml
  $ dune clean
  $ dune build @install

  $ dune exec -- shaihulud
  Hello, World!
  $ dune exec -- ./example.exe
  Hello, World!
  $ sed -i.bak 's/World/Arrakis/' example.ml
  $ dune exec -- shaihulud
  Hello, Arrakis!
  $ dune exec -- ./example.exe
  Hello, Arrakis!

Without extension, clean state
==============================

  $ sed -i.bak 's/Arrakis/World/' example.ml
  $ dune clean

  $ dune exec -- shaihulud
  Hello, World!
  $ dune exec -- ./example.exe
  Hello, World!
  $ sed -i.bak 's/World/Arrakis/' example.ml
  $ dune exec -- shaihulud
  Hello, Arrakis!
  $ dune exec -- ./example.exe
  Hello, Arrakis!
