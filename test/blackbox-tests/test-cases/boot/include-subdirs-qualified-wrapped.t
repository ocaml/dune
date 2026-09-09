Testing the bootstrap of a wrapped include subdirs qualified.

  $ init_bootstrap

  $ mkdir -p src/a/b/c

  $ make_module src/a/x.ml
  $ cat >> src/a/x.ml <<EOF
  > let () = Printf.printf "Hello from unwrapped a/x.ml\n"
  > EOF

  $ cat > src/a/b/b.ml <<EOF
  > module C = C
  > let () = Printf.printf "Hello from wrapped a/b/b.ml\n"
  > EOF

  $ cat > src/a/b/c/c.ml <<EOF
  > let () = Printf.printf "Hello from wrapped a/b/c/c.ml\n"
  > EOF

  $ cat > src/a/dune <<EOF
  > (library
  >  (name a))
  > (include_subdirs qualified)
  > EOF

  $ create_dune a <<EOF
  > module M1 = A
  > module M2 = A.X
  > module M3 = A.B
  > module M4 = A.B.C
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamllex -ml -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  Hello from wrapped a/b/c/c.ml
  Hello from wrapped a/b/b.ml
  Hello from unwrapped a/x.ml
  Hello from bootstrapped binary!

Renamed directories, including a nested group interface, must use the same
module paths in the bootstrapped binary. Rename variables are expanded by Dune
before writing the bootstrap description.

  $ cat > dune-project <<EOF
  > (lang dune 3.25)
  > (using dune-bootstrap-info 0.1)
  > EOF
  $ export TEST_BOOT_RENAME=public
  $ cat > src/a/dune <<EOF
  > (library
  >  (name a))
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (b as %{env:TEST_BOOT_RENAME=unused})
  >        (b/c as public/exposed)))
  > EOF
  $ cat > src/a/b/b.ml <<EOF
  > module Exposed = Exposed
  > let () = Printf.printf "Hello from wrapped a/b/b.ml\n"
  > EOF
  $ cat > src/a/b/b.mli <<EOF
  > module Exposed : module type of Exposed
  > EOF

  $ chmod +w boot/libs.ml
  $ create_dune a <<EOF
  > module M1 = A
  > module M2 = A.X
  > module M3 = A.Public
  > module M4 = A.Public.Exposed
  > let () = Printf.printf "Hello from renamed bootstrapped binary!"
  > EOF
  ocamllex -ml -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  Hello from wrapped a/b/c/c.ml
  Hello from wrapped a/b/b.ml
  Hello from unwrapped a/x.ml
  Hello from renamed bootstrapped binary!
