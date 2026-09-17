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

Bootstrap info does not support directory mappings in unwrapped libraries yet.

  $ cat >dune-project <<EOF
  > (lang dune 3.25)
  > (using dune-bootstrap-info 0.1)
  > EOF
  $ cat >src/lib/dune <<EOF
  > (library
  >  (name lib)
  >  (wrapped false))
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (b as public)))
  > EOF
  $ cat >bin/main.ml <<EOF
  > module M = Public
  > EOF
  $ dune build bin/bootstrap-info
  File "src/lib/dune", line 6, characters 8-9:
  6 |  (dirs (b as public)))
              ^
  Error: Directory mappings are not supported in bootstrap info.
  [1]
