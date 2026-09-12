Testing the bootstrap of a wrapped include subdirs qualified.

  $ init_bootstrap

  $ mkdir -p src/a/b

  $ make_module src/a/b/x.ml
  $ cat >> src/a/b/x.ml <<EOF
  > let () = print_endline "Hello from wrapped a/b/x.ml"
  > EOF

  $ cat > src/a/dune <<EOF
  > (library (name a))
  > (include_subdirs qualified)
  > EOF

  $ create_dune a <<EOF
  > module M1 = A.B
  > module M2 = A.B.X
  > let () = Printf.printf "Hello from bootstrapped binary!"
  > EOF
  ocamllex -ml -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  Hello from wrapped a/b/x.ml
  Hello from bootstrapped binary!

Generated group aliases also use the renamed module path.

  $ cat > dune-project <<EOF
  > (lang dune 3.25)
  > (using dune-bootstrap-info 0.1)
  > EOF
  $ cat > src/a/dune <<EOF
  > (library (name a))
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (b as public)))
  > EOF

  $ chmod +w boot/libs.ml
  $ create_dune a <<EOF
  > module M1 = A.Public
  > module M2 = A.Public.X
  > let () = Printf.printf "Hello from renamed bootstrapped binary!"
  > EOF
  ocamllex -ml -q -o boot/pps.ml boot/pps.mll
  ocaml -I +unix unix.cma $DUNEBOOT
  Hello from wrapped a/b/x.ml
  Hello from renamed bootstrapped binary!
