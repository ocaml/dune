Building a single .odoc file should also build the .odoc dependencies added
by preprocessing. This test also records the current failure to document a
library that depends on a virtual library implementation.

  $ make_dune_project_with_package 3.0 my_pkg

A preprocessed library, where A refers to B only after preprocessing:

  $ mkdir pp
  $ cat > pp/dune <<EOF
  > (library
  >  (public_name my_pkg.pp)
  >  (name pp)
  >  (preprocess
  >   (action (with-stdin-from %{input-file} (run dune_cmd subst TYPE B.t)))))
  > EOF
  $ cat > pp/a.mli <<EOF
  > val x : TYPE
  > EOF
  $ cat > pp/a.ml <<EOF
  > let x : TYPE = B.v
  > EOF
  $ cat > pp/b.mli <<EOF
  > type t
  > val v : t
  > EOF
  $ cat > pp/b.ml <<EOF
  > type t = int
  > let v = 0
  > EOF

Building the odoc file of A alone also builds the one of B:

  $ dune build _build/default/pp/.pp.objs/byte/pp__A.odoc
  $ find _build/default/pp -name '*.odoc' | LC_ALL=C sort
  _build/default/pp/.pp.objs/byte/pp.odoc
  _build/default/pp/.pp.objs/byte/pp__A.odoc
  _build/default/pp/.pp.objs/byte/pp__B.odoc

A virtual library and an implementation whose own (private) module refers to a
module of the virtual library:

  $ mkdir vlib impl
  $ cat > vlib/dune <<EOF
  > (library
  >  (public_name my_pkg.vlib)
  >  (name vlib)
  >  (virtual_modules v))
  > EOF
  $ cat > vlib/u.mli <<EOF
  > type t
  > val u : t
  > EOF
  $ cat > vlib/u.ml <<EOF
  > type t = int
  > let u = 0
  > EOF
  $ cat > vlib/v.mli <<EOF
  > val v : U.t
  > EOF
  $ cat > impl/dune <<EOF
  > (library
  >  (public_name my_pkg.impl)
  >  (name impl)
  >  (implements vlib)
  >  (private_modules extra))
  > EOF
  $ cat > impl/v.ml <<EOF
  > let v = U.u
  > EOF
  $ cat > impl/extra.mli <<EOF
  > val e : U.t
  > EOF
  $ cat > impl/extra.ml <<EOF
  > let e = U.u
  > EOF

The virtual library itself can be documented:

  $ dune build @doc

When another library depends on the implementation, Dune looks for the
inherited module U's odoc file in the implementation's object directory.
This currently fails:

  $ mkdir user
  $ cat > user/dune <<EOF
  > (library
  >  (public_name my_pkg.user)
  >  (name user)
  >  (libraries my_pkg.impl))
  > EOF
  $ cat > user/user.mli <<EOF
  > val w : Vlib.U.t
  > EOF
  $ cat > user/user.ml <<EOF
  > let w = Vlib.U.u
  > EOF

  $ dune build @doc
  File "impl/.impl.objs/byte/_unknown_", line 1, characters 0-0:
  Error: No rule found for impl/.impl.objs/byte/vlib__U.odoc
  [1]
