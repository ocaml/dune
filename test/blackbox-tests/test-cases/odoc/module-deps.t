The odoc file of a module depends on the odoc files of the modules its
interface refers to. These dependencies are computed without the compilation
context of the library, so check that they are still found when the library is
preprocessed, and when it implements a virtual library.

  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (package (name my_pkg))
  > EOF

A preprocessed library, where A refers to B only after preprocessing:

  $ mkdir pp
  $ cat > pp/dune <<EOF
  > (library
  >  (public_name my_pkg.pp)
  >  (name pp)
  >  (preprocess (action (run sed s/TYPE/B.t/ %{input-file}))))
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

  $ dune build _build/default/_doc/_odoc/my_pkg/my_pkg.pp/pp__A.odoc
  $ find _build/default/_doc/_odoc -name '*.odoc' | sort
  _build/default/_doc/_odoc/my_pkg/my_pkg.pp/pp.odoc
  _build/default/_doc/_odoc/my_pkg/my_pkg.pp/pp__A.odoc
  _build/default/_doc/_odoc/my_pkg/my_pkg.pp/pp__B.odoc
  _build/default/_doc/_odoc/my_pkg/page-index.odoc

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

Implementations of virtual libraries are not documented, so they have no odoc
rules. A documented library can still depend on one:

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
  $ find _build/default/_doc/_odoc -name '*.odoc' | sort
  _build/default/_doc/_odoc/my_pkg/my_pkg.pp/pp.odoc
  _build/default/_doc/_odoc/my_pkg/my_pkg.pp/pp__A.odoc
  _build/default/_doc/_odoc/my_pkg/my_pkg.pp/pp__B.odoc
  _build/default/_doc/_odoc/my_pkg/my_pkg.user/user.odoc
  _build/default/_doc/_odoc/my_pkg/my_pkg.vlib/vlib.odoc
  _build/default/_doc/_odoc/my_pkg/my_pkg.vlib/vlib__U.odoc
  _build/default/_doc/_odoc/my_pkg/my_pkg.vlib/vlib__V.odoc
  _build/default/_doc/_odoc/my_pkg/page-index.odoc
