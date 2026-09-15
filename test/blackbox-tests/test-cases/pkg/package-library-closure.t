A rule depending on a lock-built package must track its package dependency
closure, not just the requested package's install cookie.

Library dependencies: a -> b; b.unrelated -> d.
Lock package dependencies: provider -> b-provider, c; b-provider -> d.
The provider names differ from the library namespaces. Although a's library
closure contains only a and b, the managed package dependency includes the
installation directories of b-provider, c, and d.

The final case requests a workspace package and checks that its library
closure identifies the differently named managed provider b-provider.

Keep the package sources outside the workspace so they are built through the
lock directory, not treated as workspace libraries. Use bytecode so changing
b's implementation does not change a's compiled archive through inlining.

  $ make_dune_project 3.24
  $ make_lockdir
  $ sources="$TMPDIR/package_sources"
  $ mkdir -p "$sources/a" "$sources/b/unrelated" "$sources/c" "$sources/d"

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
  >  (modes byte)
  >  (libraries d))
  > EOF
  $ echo 'let value = D.value' >"$sources/b/unrelated/unrelated.ml"
  $ echo 'package data' >"$sources/b/payload"
  $ cat >"$sources/d/dune-project" <<'EOF'
  > (lang dune 3.24)
  > (package (name d))
  > EOF
  $ echo '(library (public_name d) (modes byte))' >"$sources/d/dune"
  $ echo 'let value = 7' >"$sources/d/d.ml"

  $ cat >"$sources/c/dune-project" <<'EOF'
  > (lang dune 3.24)
  > (package (name c))
  > EOF
  $ echo '(library (public_name c) (modes byte))' >"$sources/c/dune"
  $ touch "$sources/c/c.ml"

Use explicit installation manifests to preserve the findlib namespaces even
though the lock packages have different names.

  $ cat >"$sources/a/provider.install" <<'EOF'
  > lib_root: [
  >   "_build/install/default/lib/a/META" {"a/META"}
  >   "_build/install/default/lib/a/dune-package" {"a/dune-package"}
  >   "_build/install/default/lib/a/a.cmi" {"a/a.cmi"}
  >   "_build/install/default/lib/a/a.cma" {"a/a.cma"}
  > ]
  > EOF
  $ cat >"$sources/b/b-provider.install" <<'EOF'
  > lib_root: [
  >   "_build/install/default/lib/b/META" {"b/META"}
  >   "_build/install/default/lib/b/dune-package" {"b/dune-package"}
  >   "_build/install/default/lib/b/b.cmi" {"b/b.cmi"}
  >   "_build/install/default/lib/b/b.cma" {"b/b.cma"}
  >   "_build/install/default/lib/b/unrelated/unrelated.cmi"
  >     {"b/unrelated/unrelated.cmi"}
  >   "_build/install/default/lib/b/unrelated/unrelated.cma"
  >     {"b/unrelated/unrelated.cma"}
  > ]
  > share: [ "payload" ]
  > EOF
  $ make_lockpkg d <<EOF
  > (version 0.0.1)
  > (source (copy "$sources/d"))
  > (build (run dune build @install --promote-install-files))
  > EOF
  $ make_lockpkg b-provider <<EOF
  > (version 0.0.1)
  > (depends d)
  > (source (copy "$sources/b"))
  > (build (run dune build @install))
  > EOF
  $ make_lockpkg c <<EOF
  > (version 0.0.1)
  > (source (copy "$sources/c"))
  > (build (run dune build @install --promote-install-files))
  > EOF
  $ make_lockpkg provider <<EOF
  > (version 0.0.1)
  > (depends b-provider c)
  > (source (copy "$sources/a"))
  > (build (run dune build @install))
  > EOF

The consumer declares only package provider. Findlib selects libraries a and b;
this query checks library requirements, not which other libraries are visible.

  $ echo 'let () = Printf.printf "%d\n" A.value' >main.ml
  $ cat >dune <<'EOF'
  > (rule
  >  (targets main.exe libraries)
  >  (deps main.ml (package provider))
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

Check that all the libraries and package data were installed.

  $ a_target="$(get_build_pkg_dir provider)/target"
  $ b_target="$(get_build_pkg_dir b-provider)/target"
  $ c_target="$(get_build_pkg_dir c)/target"
  $ d_target="$(get_build_pkg_dir d)/target"
  $ b_lib="$b_target/lib/b"
  $ c_lib="$c_target/lib/c"
  $ d_lib="$d_target/lib/d"
  $ test -f "$b_lib/b.cmi"
  $ test -f "$b_lib/b.cma"
  $ test -f "$b_lib/dune-package"
  $ test -f "$b_lib/unrelated/unrelated.cmi"
  $ test -f "$b_lib/unrelated/unrelated.cma"
  $ test -f "$c_lib/c.cmi"
  $ test -f "$c_lib/c.cma"
  $ test -f "$d_lib/d.cma"
  $ test -f "$b_target/share/b-provider/payload"

The requested package's cookie and files are direct dependencies. Its lock
package dependencies are tracked through their whole installation directories,
including the unrelated library and data file in b-provider, as well as c and d.

  $ dune rules --format=json main.exe >rules.json
  $ jq_dune --arg a "$a_target" --arg b "$b_target" \
  > --arg c "$c_target" --arg d "$d_target" '
  >   rulesMatchingTarget("main.exe") | {
  >     requested_cookie: ruleHasDepFile($a + "/cookie"),
  >     requested_interface: ruleHasDepFile($a + "/lib/a/a.cmi"),
  >     requested_archive: ruleHasDepFile($a + "/lib/a/a.cma"),
  >     requested_metadata: ruleHasDepFile($a + "/lib/a/dune-package"),
  >     required_package: ruleHasDepFile($b),
  >     package_only_dependency: ruleHasDepFile($c),
  >     sibling_dependency: ruleHasDepFile($d)
  >   }' rules.json
  {
    "requested_cookie": true,
    "requested_interface": true,
    "requested_archive": true,
    "requested_metadata": true,
    "required_package": true,
    "package_only_dependency": true,
    "sibling_dependency": true
  }

Changing b's implementation must relink the consumer even if a's installed
artifacts and both providers' install cookies remain unchanged.

  $ cp "$a_target/cookie" a.cookie.before
  $ cp "$b_target/cookie" b.cookie.before
  $ cp "$a_target/lib/a/a.cmi" a.cmi.before
  $ cp "$a_target/lib/a/a.cma" a.cma.before
  $ cp "$b_lib/b.cmi" b.cmi.before
  $ cp "$b_lib/b.cma" b.cma.before
  $ echo 'let value = 10' >"$sources/b/b.ml"
  $ dune build main.exe
  $ cmp a.cookie.before "$a_target/cookie"
  $ cmp b.cookie.before "$b_target/cookie"
  $ cmp a.cmi.before "$a_target/lib/a/a.cmi"
  $ cmp a.cma.before "$a_target/lib/a/a.cma"
  $ cmp b.cmi.before "$b_lib/b.cmi"

The required library's archive really changed.

  $ cmp -s b.cma.before "$b_lib/b.cma"
  [1]

The consumer is relinked against b's updated archive.

  $ _build/default/main.exe
  11

Remove the requested provider's META file, keeping its dune-package metadata
and archives. The same package-level dependencies must remain. This action
does not invoke ocamlfind, so the missing META cannot break the action itself.

  $ dune_cmd delete 'META' "$sources/a/provider.install"
  $ cat >dune <<'EOF'
  > (rule
  >  (target package-result)
  >  (deps (package provider))
  >  (action (with-stdout-to %{target} (echo built))))
  > EOF
  $ dune build package-result
  $ test ! -e "$a_target/lib/a/META"
  $ dune rules --format=json package-result | jq_dune \
  > --arg a "$a_target" --arg b "$b_target" \
  > --arg c "$c_target" --arg d "$d_target" '
  >   rulesMatchingTarget("package-result") | {
  >     requested_metadata: ruleHasDepFile($a + "/lib/a/dune-package"),
  >     required_package: ruleHasDepFile($b),
  >     package_only_dependency: ruleHasDepFile($c),
  >     sibling_dependency: ruleHasDepFile($d)
  >   }'
  {
    "requested_metadata": true,
    "required_package": true,
    "package_only_dependency": true,
    "sibling_dependency": true
  }

Request workspace package a explicitly. Its library depends on installed
library b, whose owning lock package is b-provider, not b. The local package is
materialised and b-provider's cookie and files become direct dependencies;
b-provider's dependency d is tracked through its installation directory.
Package c is not in this closure.

  $ echo '(package (name a))' >>dune-project
  $ mkdir workspace-a
  $ cat >workspace-a/dune <<'EOF'
  > (library
  >  (public_name a)
  >  (modes byte)
  >  (libraries b))
  > EOF
  $ echo 'let value = 100 + B.value' >workspace-a/a.ml
  $ cat >dune <<'EOF'
  > (rule
  >  (target main.exe)
  >  (deps main.ml (package a))
  >  (action
  >   (run ocamlfind ocamlc -package a -linkpkg -o %{target} main.ml)))
  > EOF
  $ dune build main.exe
  $ _build/default/main.exe
  110
  $ dune rules --format=json main.exe | jq_dune \
  > --arg b "$b_target" --arg c "$c_target" --arg d "$d_target" '
  >   rulesMatchingTarget("main.exe") | {
  >     required_cookie: ruleHasDepFile($b + "/cookie"),
  >     required_archive: ruleHasDepFile($b + "/lib/b/b.cma"),
  >     transitive_package: ruleHasDepFile($d),
  >     unrelated_package: ruleHasDepFile($c)
  >   }'
  {
    "required_cookie": true,
    "required_archive": true,
    "transitive_package": true,
    "unrelated_package": false
  }
