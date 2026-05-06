Regression test for jsoo shared-runtime lib ordering.

When two libraries on the link path each contribute a JS runtime file,
jsoo concatenates them in the runtime and later files override earlier
ones on colliding symbols. The shared runtime must therefore link them
in topological order (deps before dependents) — the same order each
consumer would have produced unshared. Sorting by name (the previous
behaviour) flipped the order whenever alphabetical disagreed with
topological, silently changing which conflicting binding wins.

Here [lib_a] depends on [lib_z], so topological order is [lib_z] first
then [lib_a]. Alphabetical would invert this.

  $ make_dune_project 3.23

  $ mkdir lib_a lib_z

  $ cat > lib_z/dune <<EOF
  > (library
  >  (name lib_z)
  >  (js_of_ocaml (javascript_files runtime.js)))
  > EOF
  $ touch lib_z/lib_z.ml lib_z/runtime.js

  $ cat > lib_a/dune <<EOF
  > (library
  >  (name lib_a)
  >  (libraries lib_z)
  >  (js_of_ocaml (javascript_files runtime.js)))
  > EOF
  $ touch lib_a/lib_a.ml lib_a/runtime.js

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries lib_a)
  >  (modes js))
  > EOF
  $ touch main.ml

  $ dune build --display verbose main.bc.js 2>&1 | grep -oE 'lib_[az]/runtime\.js'
  lib_z/runtime.js
  lib_a/runtime.js
