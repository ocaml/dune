An action depending on installed package a must track its required library b,
not merely find it through OCAMLPATH. Package b also installs b.unrelated,
which requires c. Package-granular closure must include both b and c.

Use bytecode so changing b's implementation does not change a through native
inlining. Test both dune-package and META readers with fresh consumers.

  $ mkdir -p source/b/unrelated source/c prefix dune-consumer meta-consumer
  $ cat >source/dune-project <<'EOF'
  > (lang dune 3.24)
  > (package (name a))
  > (package (name b))
  > (package (name c))
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
  >  (modes byte)
  >  (libraries c))
  > EOF
  $ echo 'let value = C.value' >source/b/unrelated/unrelated.ml
  $ echo '(library (public_name c) (modes byte))' >source/c/dune
  $ echo 'let value = 7' >source/c/c.ml
  $ echo 'package data' >source/b/payload
  $ echo '(install (section share) (package b) (files payload))' >>source/b/dune
  $ dune build --root source @install
  $ dune install --root source --prefix "$PWD/prefix" 2>/dev/null

Check the metadata reader's inputs and the artifacts used in the dependency
observations, including the sibling and its dependency, which should also be tracked.

  $ export OCAMLPATH="$PWD/prefix/lib"
  $ a_lib="$PWD/prefix/lib/a"
  $ b_lib="$PWD/prefix/lib/b"
  $ c_lib="$PWD/prefix/lib/c"
  $ test -f "$a_lib/dune-package"
  $ test -f "$b_lib/dune-package"
  $ test -f "$a_lib/META"
  $ test -f "$b_lib/META"
  $ test -f "$b_lib/b.cmi"
  $ test -f "$b_lib/b.cma"
  $ test -f "$b_lib/unrelated/unrelated.cmi"
  $ test -f "$b_lib/unrelated/unrelated.cma"
  $ test -f "$c_lib/c.cma"
  $ test -f prefix/share/b/payload
  $ cp "$a_lib/a.cmi" a.cmi.before
  $ cp "$a_lib/a.cma" a.cma.before
  $ cp "$b_lib/b.cma" b.cma.before

Both consumers use only (package a), not an explicit dependency on b.

  $ echo '(lang dune 3.24)' >dune-consumer/dune-project
  $ echo 'let () = Printf.printf "%d\n" A.value' >dune-consumer/main.ml
  $ cat >dune-consumer/dune <<'EOF'
  > (rule
  >  (target main.exe)
  >  (deps main.ml (package a))
  >  (action
  >   (run ocamlfind ocamlc -package a -linkpkg -o %{target} main.ml)))
  > EOF
  $ cp dune-consumer/dune dune-consumer/dune-project \
  > dune-consumer/main.ml meta-consumer/
  $ dune build --root dune-consumer main.exe
  $ dune-consumer/_build/default/main.exe
  2

CR-someday alizter: Required packages' contents should be tracked, including
sibling libraries, their requirements, and non-library installed files.

  $ dune rules --root dune-consumer --format=json main.exe >dune-rules.json
  $ jq_dune --arg b "$b_lib" --arg c "$c_lib" --arg metadata dune-package '
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
  >         $b + "/unrelated/unrelated.cma"; $b + "/unrelated"; "*.cma"),
  >     sibling_dependency: ruleHasDepFile($c + "/c.cma"),
  >     package_data: ruleHasDepFile("share/b/payload")
  >   }' dune-rules.json
  {
    "reader": "dune-package",
    "required_interface": false,
    "required_archive": false,
    "required_metadata": false,
    "unrelated_interface": false,
    "unrelated_archive": false,
    "sibling_dependency": false,
    "package_data": false
  }

Replace only b's installed archive. Do not reinstall a or clean the consumer:
its existing executable must be relinked because b changed.

  $ echo 'let value = 10' >source/b/b.ml
  $ dune build --root source @install
  $ cp source/_build/default/b/b.cma "$b_lib/b.cma"
  $ cmp -s b.cma.before "$b_lib/b.cma"
  [1]
  $ dune build --root dune-consumer main.exe
  $ cmp a.cmi.before "$a_lib/a.cmi"
  $ cmp a.cma.before "$a_lib/a.cma"

CR-someday alizter: This should print 11, but the consumer is stale.

  $ dune-consumer/_build/default/main.exe
  2

Restore b's original archive and remove dune-package files. The second
consumer must use META, including after its dependency changes.

  $ cp b.cma.before "$b_lib/b.cma"
  $ rm "$a_lib/dune-package" "$b_lib/dune-package" "$c_lib/dune-package"
  $ dune build --root meta-consumer main.exe
  $ meta-consumer/_build/default/main.exe
  2
  $ dune rules --root meta-consumer --format=json main.exe >meta-rules.json
  $ jq_dune --arg b "$b_lib" --arg c "$c_lib" --arg metadata META '
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
  >         $b + "/unrelated/unrelated.cma"; $b + "/unrelated"; "*.cma"),
  >     sibling_dependency: ruleHasDepFile($c + "/c.cma")
  >   }' meta-rules.json
  {
    "reader": "META",
    "required_interface": false,
    "required_archive": false,
    "required_metadata": false,
    "unrelated_interface": false,
    "unrelated_archive": false,
    "sibling_dependency": false
  }

The rebuilt archive still contains b = 10. Copy only that archive, leaving
both a's artifacts and the metadata reader unchanged.

  $ cp source/_build/default/b/b.cma "$b_lib/b.cma"
  $ cmp -s b.cma.before "$b_lib/b.cma"
  [1]
  $ dune build --root meta-consumer main.exe
  $ cmp a.cmi.before "$a_lib/a.cmi"
  $ cmp a.cma.before "$a_lib/a.cma"
  $ test ! -e "$a_lib/dune-package"
  $ test ! -e "$b_lib/dune-package"

CR-someday alizter: The META consumer should also print 11, not its stale result.

  $ meta-consumer/_build/default/main.exe
  2
