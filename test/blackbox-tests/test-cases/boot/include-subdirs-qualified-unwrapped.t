Testing the bootstrap of an unwrapped include subdirs qualified.

  $ init_bootstrap

  $ mkdir -p src/lib/b/c
  $ mkdir -p src/lib/a

  $ cat > src/lib/x.ml <<EOF
  > let () = Printf.printf "Hello from unwrapped a/x.ml\n"
  > EOF

  $ cat > src/lib/b/b.ml <<EOF
  > module C = C
  > let () = Printf.printf "Hello from unwrapped a/b/b.ml\n"
  > EOF

  $ cat > src/lib/b/c/c.ml <<EOF
  > let () = Printf.printf "Hello from unwrapped a/b/c/c.ml\n"
  > EOF

  $ make_module src/lib/root.ml

  $ cat > src/lib/dune <<EOF
  > (library
  >  (name lib)
  >  (wrapped false))
  > (include_subdirs qualified)
  > EOF

  $ create_dune lib <<EOF
  > module Root = Root
  > module M1 = X
  > module M2 = B
  > module M3 = B.C
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamllex -ml -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  Hello from unwrapped a/b/c/c.ml
  Hello from unwrapped a/b/b.ml
  Hello from unwrapped a/x.ml
  Hello from bootstrapped binary!

Renaming a directory also changes its descendants' module paths in an
unwrapped library, without changing their source paths.

  $ cat > dune-project <<EOF
  > (lang dune 3.25)
  > (using dune-bootstrap-info 0.1)
  > EOF
  $ cat > src/lib/dune <<EOF
  > (library
  >  (name lib)
  >  (wrapped false))
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (b as public)))
  > EOF

  $ chmod +w boot/libs.ml
  $ create_dune lib <<EOF
  > module Root = Root
  > module M1 = X
  > module M2 = Public
  > module M3 = Public.C
  > let () = Printf.printf "Hello from renamed bootstrapped binary!"
  > EOF
  ocamllex -ml -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  Hello from unwrapped a/b/c/c.ml
  Hello from unwrapped a/b/b.ml
  Hello from unwrapped a/x.ml
  Hello from renamed bootstrapped binary!
