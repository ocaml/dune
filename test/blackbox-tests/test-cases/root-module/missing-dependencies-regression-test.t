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
  File "list.ml", line 1:
  Error: The implementation list.ml does not match the interface list.mli: 
         Values do not match:
           val of_seq : 'a Seq.t -> 'a t
         is not included in
           val of_seq : 'a Mylib.Seq.t -> 'a t
         The type 'a Seq.t -> 'a t is not compatible with the type
           'b Mylib.Seq.t -> 'b t
         Type 'a Seq.t = unit -> 'a Seq.node is not compatible with type
           'b Mylib.Seq.t
         File "list.mli", line 2, characters 0-32: Expected declaration
         File "listLabels.mli", line 562, characters 0-32: Actual declaration
  File "list.ml", line 1:
  Error: The implementation list.ml does not match the interface list.mli: 
         Values do not match:
           val of_seq : 'a Seq.t -> 'a t
         is not included in
           val of_seq : 'a Mylib.Seq.t -> 'a t
         The type 'a Seq.t -> 'a t is not compatible with the type
           'b Mylib.Seq.t -> 'b t
         Type 'a Seq.t = unit -> 'a Seq.node is not compatible with type
           'b Mylib.Seq.t
         File "list.mli", line 2, characters 0-32: Expected declaration
         File "listLabels.mli", line 562, characters 0-32: Actual declaration
  [1]

  $ dune build
