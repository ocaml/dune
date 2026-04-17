Test cms_cmt_dependency setting in context definition.

This setting controls whether compilation depends on .cms or .cmt files of
dependencies:
- none: No additional dependencies
- cms: Depend on .cms/.cmsi files
- cmt: Depend on .cmt/.cmti files

Note: We use release profile because in dev profile, -opaque is enabled by default,
which disables .cmx, .cms, and .cmt dependencies.

Create a project with oxcaml extension (required for cms_cmt_dependency):

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (using oxcaml 0.1)
  > EOF

Probe whether the current compiler is actually OxCaml. The language extension
can be available even when the compiler does not support the `.cms` dependency
machinery itself.

  $ cat >probe_oxcaml.ml <<EOF
  > let fst_local ((x, _) @ local) = x
  > let () = ignore (fst_local (stack_ (1, 2)))
  > EOF

  $ if ocamlc -c probe_oxcaml.ml >/dev/null 2>&1; then IS_OX=true; else IS_OX=false; fi

Create a library with two modules where one depends on the other:

  $ mkdir -p mylib

  $ cat >mylib/dep.ml <<EOF
  > type t = int
  > let make () = 42
  > EOF

  $ cat >mylib/dep.mli <<EOF
  > type t
  > val make : unit -> t
  > EOF

  $ cat >mylib/user.ml <<EOF
  > let x = Dep.make ()
  > let () = ignore x
  > EOF

  $ cat >mylib/dune <<EOF
  > (env (_ (bin_annot_cms true)))
  > (library (name mylib))
  > EOF

Test 1: cms_cmt_dependency = none (default)
===========================================

With the default setting, neither .cms nor .cmt files should appear as
dependencies of compilation rules.

  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > (using oxcaml 0.1)
  > (context (default
  >   (name default)
  >   (profile release)
  >   (cms_cmt_dependency none)))
  > EOF

Build everything first so rules are generated:

  $ dune build

Check that User.cmx does NOT depend on Dep's .cms/.cmsi files:

  $ dune rules --root . --format=json mylib/.mylib.objs/native/mylib__User.cmx |
  > jq 'include "dune"; [ .[] | ruleDepFilePaths | select(test("mylib__Dep\\.cms")) ] | length'
  0

Check that User.cmx does NOT depend on Dep's .cmt/.cmti files:

  $ dune rules --root . --format=json mylib/.mylib.objs/native/mylib__User.cmx |
  > jq 'include "dune"; [ .[] | ruleDepFilePaths | select(test("mylib__Dep\\.cmt")) ] | length'
  0

Test 2: cms_cmt_dependency = cms
================================

With this setting enabled, .cms files SHOULD appear as dependencies of
compilation rules, but .cmt files should NOT.

  $ dune clean

  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > (using oxcaml 0.1)
  > (context (default
  >   (name default)
  >   (profile release)
  >   (cms_cmt_dependency cms)))
  > EOF

Build everything:

  $ dune build

Check that User.cmx DOES depend on Dep's .cms/.cmsi files:

  $ if [ "$IS_OX" = true ]; then
  >   test "$(dune rules --root . --format=sexp mylib/.mylib.objs/native/mylib__User.cmx | grep -c 'mylib__Dep\.cms')" = 2
  > else
  >   test "$(dune rules --root . --format=sexp mylib/.mylib.objs/native/mylib__User.cmx | grep -c 'mylib__Dep\.cms')" = 0
  > fi

Check that User.cmx does NOT depend on Dep's .cmt/.cmti files:

  $ test "$(dune rules --root . --format=sexp mylib/.mylib.objs/native/mylib__User.cmx | grep -c 'mylib__Dep\.cmt')" = 0

Test 3: cms_cmt_dependency = cmt
================================

With this setting enabled, .cmt files SHOULD appear as dependencies of
compilation rules, but .cms files should NOT.
  $ dune clean

  $ cat >dune-workspace <<EOF
  > (lang dune 3.21)
  > (using oxcaml 0.1)
  > (context (default
  >   (name default)
  >   (profile release)
  >   (cms_cmt_dependency cmt)))
  > EOF

Build everything:

  $ dune build

Check that User.cmx DOES depend on Dep's .cmt/.cmti files:

  $ if [ "$IS_OX" = true ]; then
  >   test "$(dune rules --root . --format=sexp mylib/.mylib.objs/native/mylib__User.cmx | grep -c 'mylib__Dep\.cmt')" = 2
  > else
  >   test "$(dune rules --root . --format=sexp mylib/.mylib.objs/native/mylib__User.cmx | grep -c 'mylib__Dep\.cmt')" = 0
  > fi

Check that User.cmx does NOT depend on Dep's .cms/.cmsi files:

  $ test "$(dune rules --root . --format=sexp mylib/.mylib.objs/native/mylib__User.cmx | grep -c 'mylib__Dep\.cms')" = 0
