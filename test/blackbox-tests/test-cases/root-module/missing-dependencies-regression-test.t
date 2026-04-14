When a library has a root module and shadows a stdlib module (like Seq), the
compiler needs to see the library's .cmi to resolve type equalities. Without
sandboxing, the .cmi happens to be in the build directory. With sandboxing, it
must be a declared dependency.

  $ make_dune_project 3.23

  $ cat > dune << EOF
  > (library
  >  (name mylib)
  >  (root_module root))
  > EOF

  $ cat > root.ml << EOF
  > module Seq = Seq
  > module List = List
  > EOF

  $ cat > seq.ml << EOF
  > include Stdlib.Seq
  > EOF

  $ cat > seq.mli << EOF
  > include module type of struct include Stdlib.Seq end
  > EOF

  $ cat > list.ml << EOF
  > include ListLabels
  > EOF

  $ cat > list.mli << EOF
  > include module type of struct include ListLabels end
  > val of_seq : 'a Seq.t -> 'a list
  > EOF

This should build successfully both with and without sandboxing:

  $ dune build --sandbox symlink

  $ dune build
