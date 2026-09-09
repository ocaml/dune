When --only-packages masks workspace b, workspace a must use installed b and
track its artifacts. Exercise this fallback through both metadata readers.

Installed a returns 2. Workspace a returns B.value + 10, workspace b returns
100, and installed b returns 1. The expected result 11 distinguishes both
workspace-root precedence and fallback to the installed dependency.

  $ mkdir -p source/b/unrelated prefix masked-dune/a-src masked-dune/b-src
  $ mkdir masked-meta
  $ cat >source/dune-project <<'EOF'
  > (lang dune 3.24)
  > (package (name a))
  > (package (name b))
  > EOF
  $ cat >source/dune <<'EOF'
  > (library
  >  (public_name a)
  >  (modes byte)
  >  (libraries b))
  > EOF
  $ echo 'let value = B.value + 1' >source/a.ml
  $ echo '(library (public_name b) (modes byte))' >source/b/dune
  $ echo 'let value = 1' >source/b/b.ml
  $ cat >source/b/unrelated/dune <<'EOF'
  > (library
  >  (name unrelated)
  >  (public_name b.unrelated)
  >  (modes byte))
  > EOF
  $ touch source/b/unrelated/unrelated.ml
  $ dune build --root source @install
  $ dune install --root source --prefix "$PWD/prefix" 2>/dev/null
  $ export OCAMLPATH="$PWD/prefix/lib"
  $ b_lib="$PWD/prefix/lib/b"
  $ test -f prefix/lib/a/dune-package
  $ test -f "$b_lib/dune-package"
  $ test -f prefix/lib/a/META
  $ test -f "$b_lib/META"
  $ test -f "$b_lib/b.cmi"
  $ test -f "$b_lib/b.cma"
  $ test -f "$b_lib/unrelated/unrelated.cmi"
  $ test -f "$b_lib/unrelated/unrelated.cma"

Make independent workspace consumers so each starts with the intended reader.

  $ cat >masked-dune/dune-project <<'EOF'
  > (lang dune 3.24)
  > (package (name a))
  > (package (name b))
  > EOF
  $ cat >masked-dune/a-src/dune <<'EOF'
  > (library
  >  (public_name a)
  >  (modes byte)
  >  (libraries b))
  > EOF
  $ echo 'let value = B.value + 10' >masked-dune/a-src/a.ml
  $ echo '(library (public_name b) (modes byte))' >masked-dune/b-src/dune
  $ echo 'let value = 100' >masked-dune/b-src/b.ml
  $ echo 'let () = Printf.printf "%d\n" A.value' >masked-dune/main.ml
  $ cat >masked-dune/dune <<'EOF'
  > (rule
  >  (target main.exe)
  >  (deps main.ml (package a))
  >  (action
  >   (run %{bin:ocamlfind} ocamlc -package a -linkpkg -o %{target} main.ml)))
  > EOF
  $ cp -R masked-dune/. masked-meta/
  $ dune build --root masked-dune --only-packages a main.exe
  $ masked-dune/_build/default/main.exe
  11

The installed dependency is tracked, not just visible to the compiler. Its
interface, archive and metadata are dependencies, but its sibling is not.

  $ dune rules --root masked-dune --only-packages a --format=json main.exe \
  > >dune-rules.json
  $ jq_dune --arg b "$b_lib" --arg metadata dune-package '
  >   rulesMatchingTarget("main.exe") | {
  >     reader: $metadata,
  >     required_interface:
  >       ruleHasDepFileOrMatchingGlob($b + "/b.cmi"; $b; "*.cmi"),
  >     required_archive:
  >       ruleHasDepFileOrMatchingGlob($b + "/b.cma"; $b; "*.cma"),
  >     required_metadata: ruleHasDepFile($b + "/" + $metadata),
  >     unrelated_interface:
  >       ruleHasDepFileOrMatchingGlob(
  >         $b + "/unrelated/unrelated.cmi"; $b + "/unrelated"; "*.cmi"),
  >     unrelated_archive:
  >       ruleHasDepFileOrMatchingGlob(
  >         $b + "/unrelated/unrelated.cma"; $b + "/unrelated"; "*.cma")
  >   }' dune-rules.json
  {
    "reader": "dune-package",
    "required_interface": true,
    "required_archive": true,
    "required_metadata": true,
    "unrelated_interface": false,
    "unrelated_archive": false
  }

Repeat the same precedence and dependency checks with META files.

  $ rm prefix/lib/a/dune-package "$b_lib/dune-package"
  $ dune build --root masked-meta --only-packages a main.exe
  $ masked-meta/_build/default/main.exe
  11
  $ dune rules --root masked-meta --only-packages a --format=json main.exe \
  > >meta-rules.json
  $ jq_dune --arg b "$b_lib" --arg metadata META '
  >   rulesMatchingTarget("main.exe") | {
  >     reader: $metadata,
  >     required_interface:
  >       ruleHasDepFileOrMatchingGlob($b + "/b.cmi"; $b; "*.cmi"),
  >     required_archive:
  >       ruleHasDepFileOrMatchingGlob($b + "/b.cma"; $b; "*.cma"),
  >     required_metadata: ruleHasDepFile($b + "/" + $metadata),
  >     unrelated_interface:
  >       ruleHasDepFileOrMatchingGlob(
  >         $b + "/unrelated/unrelated.cmi"; $b + "/unrelated"; "*.cmi"),
  >     unrelated_archive:
  >       ruleHasDepFileOrMatchingGlob(
  >         $b + "/unrelated/unrelated.cma"; $b + "/unrelated"; "*.cma")
  >   }' meta-rules.json
  {
    "reader": "META",
    "required_interface": true,
    "required_archive": true,
    "required_metadata": true,
    "unrelated_interface": false,
    "unrelated_archive": false
  }
