The same library can be consumed through native rules or an opaque recipe.
Both cases use the same source and consumer, in independent build roots.

  $ make_external_mypkg_lib_source 'let x = 42'
  $ sed -i 's/(name test_lib)/& (modules test_lib)/' external_sources/dune
  $ echo '(executable (name tool) (modules tool))' >> external_sources/dune
  $ echo 'let () = ()' > external_sources/tool.ml
  $ mkdir native
  $ (cd native && make_dune_project 3.11)
  $ cat > native/dune <<'EOF'
  > (executable (name main) (modules main) (libraries foo))
  > (library
  >  (name foo)
  >  (modules foo)
  >  (libraries mypkg.lib))
  > EOF
  $ echo 'let x = Test_lib.x' > native/foo.ml
  $ echo 'let () = Printf.printf "%d\n" Foo.x' > native/main.ml
  $ cp -R native opaque
  $ make_backend_lockdirs mypkg "$PWD/external_sources"

Describe the cold workspace before compilation. The consumer's executable and
library, and the native dependency's library/modules, are included; the
provider's unrelated executable is not part of the workspace description.

  $ (cd native && dune describe workspace) | grep -E 'names|\(name '
    ((names (main))
      (((name Main)
    ((name foo)
      (((name Foo)
    ((name mypkg.lib)
      (((name Test_lib)

The native recipe deliberately fails. Successful compilation and execution
therefore require native loading, without an opaque installation.

  $ (cd native && dune build foo.cma main.exe)
  $ native/_build/default/main.exe
  42
  $ test ! -e native/_build/default/.pkg/mypkg/_install/cookie

The opaque recipe performs a harmless substitution before building the same
Dune project. Its marker and cookie prove this really exercised installation,
not another native build.

  $ (cd opaque && dune describe workspace) | grep -E 'names|\(name '
    ((names (main))
      (((name Main)
    ((name foo)
      (((name Foo)
    ((name mypkg.lib)
  $ (cd opaque && dune build foo.cma main.exe)
  $ opaque/_build/default/main.exe
  42
  $ cat opaque/_build/default/.pkg/mypkg/_install/opaque-recipe
  opaque
  $ test -f opaque/_build/default/.pkg/mypkg/_install/cookie
