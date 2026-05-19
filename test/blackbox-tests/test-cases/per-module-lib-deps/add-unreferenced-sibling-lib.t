Observational baseline: adding a library to a stanza's
[(libraries ...)] list currently rebuilds every module in the
stanza, including modules that reference nothing from the new
library. The [-I]/[-H] flags on each module's compiler invocation
are derived from the cctx-wide library list rather than from the
module's own ocamldep reference set, so adding an unreferenced
library still changes every consumer's compile-action hash and
invalidates the rule cache.

[consumer_lib] starts with [(libraries existing_dep)] and two
modules: [consumes_dep] references [Existing_dep_module];
[unrelated_module] references nothing from [existing_dep] or any
other library. After the initial build, [added_lib] is added to
the stanza's [(libraries ...)] list. Neither [consumes_dep] nor
[unrelated_module] uses anything from [added_lib], yet both
rebuild. Records the rebuild counts on each so a future change
can flip them to 0.

  $ cat > dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name existing_dep)
  >  (wrapped false)
  >  (modules existing_dep_module))
  > (library (name added_lib) (wrapped false) (modules added_lib_module))
  > (library
  >  (name consumer_lib)
  >  (wrapped false)
  >  (modules consumes_dep unrelated_module)
  >  (libraries existing_dep))
  > EOF

  $ cat > existing_dep_module.ml <<EOF
  > let x = 42
  > EOF
  $ cat > existing_dep_module.mli <<EOF
  > val x : int
  > EOF
  $ cat > added_lib_module.ml <<EOF
  > let y = "C"
  > EOF
  $ cat > added_lib_module.mli <<EOF
  > val y : string
  > EOF
  $ cat > consumes_dep.ml <<EOF
  > let x = Existing_dep_module.x
  > EOF
  $ cat > unrelated_module.ml <<EOF
  > let x = 12
  > EOF

  $ dune build @check

Add [added_lib] to [consumer_lib]'s [(libraries ...)]. Neither
[consumes_dep] nor [unrelated_module] references [added_lib]:

  $ cat > dune <<EOF
  > (library
  >  (name existing_dep)
  >  (wrapped false)
  >  (modules existing_dep_module))
  > (library (name added_lib) (wrapped false) (modules added_lib_module))
  > (library
  >  (name consumer_lib)
  >  (wrapped false)
  >  (modules consumes_dep unrelated_module)
  >  (libraries existing_dep added_lib))
  > EOF

  $ dune build @check
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("consumes_dep"))]'
  [
    {
      "target_files": [
        "_build/default/.consumer_lib.objs/byte/consumes_dep.cmi",
        "_build/default/.consumer_lib.objs/byte/consumes_dep.cmo",
        "_build/default/.consumer_lib.objs/byte/consumes_dep.cmt"
      ]
    }
  ]
  $ dune trace cat | jq -s 'include "dune"; [.[] | targetsMatchingFilter(test("unrelated_module"))]'
  [
    {
      "target_files": [
        "_build/default/.consumer_lib.objs/byte/unrelated_module.cmi",
        "_build/default/.consumer_lib.objs/byte/unrelated_module.cmo",
        "_build/default/.consumer_lib.objs/byte/unrelated_module.cmt"
      ]
    }
  ]
