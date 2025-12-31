Test allow_unused_libraries field

The allow_unused_libraries field allows specifying libraries that should not
trigger unused library errors even when they are not used.

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using unreleased 0.1)
  > EOF

Create three libraries - one will be used, two won't be used:

  $ cat > dune <<EOF
  > (library
  >  (name used_lib)
  >  (modules used_lib))
  > 
  > (library
  >  (name unused_allowed)
  >  (modules unused_allowed))
  > 
  > (library
  >  (name unused_not_allowed)
  >  (modules unused_not_allowed))
  > 
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries used_lib unused_allowed unused_not_allowed)
  >  (allow_unused_libraries unused_allowed))
  > EOF

  $ cat > used_lib.ml <<EOF
  > let helper x = x + 1
  > EOF

  $ cat > unused_allowed.ml <<EOF
  > let allowed x = x * 2
  > EOF

  $ cat > unused_not_allowed.ml <<EOF
  > let not_allowed x = x * 3
  > EOF

  $ cat > main.ml <<EOF
  > (* Only use used_lib, not the other two *)
  > let () = print_int (Used_lib.helper 42)
  > EOF

Build the unused-libs alias - should only error on unused_not_allowed:

  $ dune build @unused-libs
  File "dune", line 16, characters 36-54:
  16 |  (libraries used_lib unused_allowed unused_not_allowed)
                                           ^^^^^^^^^^^^^^^^^^
  Error: Unused libraries:
  - unused_not_allowed
  [1]

Now allow both unused libraries:

  $ cat > dune <<EOF
  > (library
  >  (name used_lib)
  >  (modules used_lib))
  > 
  > (library
  >  (name unused_allowed)
  >  (modules unused_allowed))
  > 
  > (library
  >  (name unused_not_allowed)
  >  (modules unused_not_allowed))
  > 
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries used_lib unused_allowed unused_not_allowed)
  >  (allow_unused_libraries unused_allowed unused_not_allowed))
  > EOF

Build should succeed now:

  $ dune build @unused-libs

Test with a library that has dependencies:

  $ cat > dune <<EOF
  > (library
  >  (name base_lib)
  >  (modules base_lib))
  > 
  > (library
  >  (name dep_lib)
  >  (modules dep_lib)
  >  (libraries base_lib)
  >  (allow_unused_libraries base_lib))
  > 
  > (library
  >  (name unused_dep)
  >  (modules unused_dep))
  > 
  > (library
  >  (name top_lib)
  >  (modules top_lib)
  >  (libraries dep_lib unused_dep))
  > EOF

  $ cat > base_lib.ml <<EOF
  > let x = 1
  > EOF

  $ cat > dep_lib.ml <<EOF
  > (* Intentionally not using base_lib *)
  > let y = 2
  > EOF

  $ cat > unused_dep.ml <<EOF
  > let z = 3
  > EOF

  $ cat > top_lib.ml <<EOF
  > (* Only use dep_lib *)
  > let result = Dep_lib.y
  > EOF

Build - dep_lib should not error on base_lib, but top_lib should error on
unused_dep:

  $ dune build @unused-libs
  File "dune", line 18, characters 20-30:
  18 |  (libraries dep_lib unused_dep))
                           ^^^^^^^^^^
  Error: Unused libraries:
  - unused_dep
  [1]
