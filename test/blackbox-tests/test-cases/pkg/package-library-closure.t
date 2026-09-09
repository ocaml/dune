A rule depending on a lock-built package must track its required libraries,
not just the requested package's install cookie.

Library dependencies: a -> b.
Package dependencies: a -> b, c.
Package b also installs an unrelated library, b.unrelated.

Keep the package sources outside the workspace so they are built through the
lock directory, not treated as workspace libraries. Use bytecode so changing
b's implementation does not change a's compiled archive through inlining.

  $ make_dune_project 3.24
  $ make_lockdir
  $ sources="$TMPDIR/package_sources"
  $ mkdir -p "$sources/a" "$sources/b/unrelated" "$sources/c"

  $ cat >"$sources/a/dune-project" <<'EOF'
  > (lang dune 3.24)
  > (package
  >  (name a)
  >  (depends b c))
  > EOF
  $ cat >"$sources/a/dune" <<'EOF'
  > (library
  >  (public_name a)
  >  (modes byte)
  >  (libraries b))
  > EOF
  $ echo 'let value = B.value + 1' >"$sources/a/a.ml"

  $ cat >"$sources/b/dune-project" <<'EOF'
  > (lang dune 3.24)
  > (package (name b))
  > EOF
  $ echo '(library (public_name b) (modes byte))' >"$sources/b/dune"
  $ echo 'let value = 1' >"$sources/b/b.ml"
  $ cat >"$sources/b/unrelated/dune" <<'EOF'
  > (library
  >  (name unrelated)
  >  (public_name b.unrelated)
  >  (modes byte))
  > EOF
  $ touch "$sources/b/unrelated/unrelated.ml"

  $ cat >"$sources/c/dune-project" <<'EOF'
  > (lang dune 3.24)
  > (package (name c))
  > EOF
  $ echo '(library (public_name c) (modes byte))' >"$sources/c/dune"
  $ touch "$sources/c/c.ml"

  $ make_lockpkg b <<EOF
  > (version 0.0.1)
  > (source (copy "$sources/b"))
  > (build (run dune build @install --promote-install-files))
  > EOF
  $ make_lockpkg c <<EOF
  > (version 0.0.1)
  > (source (copy "$sources/c"))
  > (build (run dune build @install --promote-install-files))
  > EOF
  $ make_lockpkg a <<EOF
  > (version 0.0.1)
  > (depends b c)
  > (source (copy "$sources/a"))
  > (build (run dune build @install --promote-install-files))
  > EOF

The consumer declares only package a. Findlib selects libraries a and b;
this query checks library requirements, not which other libraries are visible.

  $ echo 'let () = Printf.printf "%d\n" A.value' >main.ml
  $ cat >dune <<'EOF'
  > (rule
  >  (targets main.exe libraries)
  >  (deps main.ml (package a))
  >  (action
  >   (progn
  >    (with-stdout-to libraries
  >     (run ocamlfind query -recursive -format %p a))
  >    (run ocamlfind ocamlc -package a -linkpkg -o main.exe main.ml))))
  > EOF
  $ dune build main.exe
  $ sort _build/default/libraries
  a
  b
  $ _build/default/main.exe
  2

Check that the excluded libraries really were installed, so their absence
from the consumer's dependencies cannot be explained by missing artifacts.

  $ a_target="$(get_build_pkg_dir a)/target"
  $ b_lib="$(get_build_pkg_dir b)/target/lib/b"
  $ c_lib="$(get_build_pkg_dir c)/target/lib/c"
  $ test -f "$b_lib/b.cmi"
  $ test -f "$b_lib/b.cma"
  $ test -f "$b_lib/dune-package"
  $ test -f "$b_lib/unrelated/unrelated.cmi"
  $ test -f "$b_lib/unrelated/unrelated.cma"
  $ test -f "$c_lib/c.cmi"
  $ test -f "$c_lib/c.cma"

The required interface, archive and metadata are tracked, but the unselected
libraries' interfaces and archives are not. Accept dependencies represented
by direct files or matching selectors.

  $ dune rules --format=json main.exe >rules.json
  $ jq_dune --arg b "$b_lib" --arg c "$c_lib" '
  >   rulesMatchingTarget("main.exe") | {
  >     required_interface:
  >       ruleHasDepFileOrMatchingGlob($b + "/b.cmi"; $b; "*.cmi"),
  >     required_archive:
  >       ruleHasDepFileOrMatchingGlob($b + "/b.cma"; $b; "*.cma"),
  >     required_metadata: ruleHasDepFile($b + "/dune-package"),
  >     unrelated_interface:
  >       ruleHasDepFileOrMatchingGlob(
  >         $b + "/unrelated/unrelated.cmi"; $b + "/unrelated"; "*.cmi"),
  >     unrelated_archive:
  >       ruleHasDepFileOrMatchingGlob(
  >         $b + "/unrelated/unrelated.cma"; $b + "/unrelated"; "*.cma"),
  >     package_only_interface:
  >       ruleHasDepFileOrMatchingGlob($c + "/c.cmi"; $c; "*.cmi"),
  >     package_only_archive:
  >       ruleHasDepFileOrMatchingGlob($c + "/c.cma"; $c; "*.cma")
  >   }' rules.json
  {
    "required_interface": true,
    "required_archive": true,
    "required_metadata": true,
    "unrelated_interface": false,
    "unrelated_archive": false,
    "package_only_interface": false,
    "package_only_archive": false
  }

Changing b's implementation must relink the consumer even if a's artifacts
and install cookie remain unchanged.

  $ cp "$a_target/cookie" a.cookie.before
  $ cp "$a_target/lib/a/a.cmi" a.cmi.before
  $ cp "$a_target/lib/a/a.cma" a.cma.before
  $ cp "$b_lib/b.cmi" b.cmi.before
  $ cp "$b_lib/b.cma" b.cma.before
  $ echo 'let value = 10' >"$sources/b/b.ml"
  $ dune build main.exe
  $ cmp a.cookie.before "$a_target/cookie"
  $ cmp a.cmi.before "$a_target/lib/a/a.cmi"
  $ cmp a.cma.before "$a_target/lib/a/a.cma"
  $ cmp b.cmi.before "$b_lib/b.cmi"

The required library's archive really changed.

  $ cmp -s b.cma.before "$b_lib/b.cma"
  [1]

The consumer was relinked against b's updated archive.

  $ _build/default/main.exe
  11
