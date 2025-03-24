Repro for issue #7034.

This example is made up of two packages. The package "inner" defines a type with
an unused field. The package "outer" vendors the package "inner". It will be
demonstrated that "inner" builds with a warning on lang dune 3.2 and an error on
3.3 (this isn't the problem - it's just to establish that the warning was
changed to an error in 3.3). Then when "inner" is vendored inside of "outer", it
will be shown that when the dune lang version of "outer" is 3.3, the vendored
"inner" no longer builds, even though it's using lang dune 3.2 internally (this
is the problem).

The "inner" package enables a warning (69: unused-field) in its source code.
It builds on its own (with a warning) when lang dune is 3.2 or below:
  $ ./make-inner .
  $ ./gen-dune-project inner 3.2 > inner/dune-project
  $ dune printenv --root=inner --field flags
  Entering directory 'inner'
  (flags
   (-w
    @1..3@5..28@30..39@43@46..47@49..57@61..62-40
    -strict-sequence
    -strict-formats
    -short-paths
    -keep-locs))
  Leaving directory 'inner'
  $ dune build --root=inner
  Entering directory 'inner'
  File "inner.ml", line 6, characters 11-18:
  6 | type t = { x : int }
                 ^^^^^^^
  Warning 69 [unused-field]: unused record field x.
  File "inner.ml", line 6, characters 11-18:
  6 | type t = { x : int }
                 ^^^^^^^
  Warning 69 [unused-field]: unused record field x.
  Leaving directory 'inner'


Note that in versions of lang dune above 3.2 this warning becomes an error:
  $ ./gen-dune-project inner 3.3 > inner/dune-project
  $ dune printenv --root=inner --field flags
  Entering directory 'inner'
  (flags
   (-w
    @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40
    -strict-sequence
    -strict-formats
    -short-paths
    -keep-locs))
  Leaving directory 'inner'
  $ dune build --root=inner
  Entering directory 'inner'
  File "inner.ml", line 6, characters 11-18:
  6 | type t = { x : int }
                 ^^^^^^^
  Error (warning 69 [unused-field]): unused record field x.
  Leaving directory 'inner'
  [1]

Create another version of the inner package in outer/vendored:
  $ ./make-inner outer/vendored
Change the version back to 3.2 so the inner package builds again:
  $ ./gen-dune-project inner 3.2 > outer/vendored/inner/dune-project

Building the outer project works when lang dune is 3.2 or below:

  $ ./gen-dune-project outer 3.2 > outer/dune-project
  $ dune printenv --root=outer --field flags
  Entering directory 'outer'
  (flags
   (-w
    @1..3@5..28@30..39@43@46..47@49..57@61..62-40
    -strict-sequence
    -strict-formats
    -short-paths
    -keep-locs))
  Leaving directory 'outer'

  $ dune build --root=outer
  Entering directory 'outer'
  File "vendored/inner/inner.ml", line 6, characters 11-18:
  6 | type t = { x : int }
                 ^^^^^^^
  Warning 69 [unused-field]: unused record field x.
  Leaving directory 'outer'

But when lang dune is 3.3 or higher the warning becomes an error:
  $ ./gen-dune-project outer 3.3 > outer/dune-project
  $ dune printenv --root=outer --field flags
  Entering directory 'outer'
  (flags
   (-w
    @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40
    -strict-sequence
    -strict-formats
    -short-paths
    -keep-locs))
  Leaving directory 'outer'
  $ dune printenv outer/vendored/inner --root=outer --field flags
  Entering directory 'outer'
  (flags
   (-w
    @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40
    -strict-sequence
    -strict-formats
    -short-paths
    -keep-locs))
  Leaving directory 'outer'

  $ dune build --root=outer
  Entering directory 'outer'
  Leaving directory 'outer'
  $ pat="\-o [./a-zA-Z_]\{1,\}.cmx"
  $ log=outer/_build/log
  $ grep -o "$pat" $log | sort
  -o .outer.eobjs/native/dune__exe__Outer.cmx
  $ grep "$pat" $log | sort | grep -n -E -o "\-w [^ ]+"
  1:-w @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40

This is unexpected as vendored projects should be built according to their
declared dune-project rather than the dune-project of the outer project.

The problem seems related to the fact that the vendored project enables a
warning in its source. Other warnings introduced into the vendored project are
not visible when building the outer project (this is expected as vendored
projects are supposed to be build with `-w -a` according to the docs).
