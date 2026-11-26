Selected packages sharing a source snapshot have separate output roots. Nested
projects retain their relative paths, and each view hides the other selected
package. Cram test fixtures are data-only before project parsing.

  $ make_lockdir
  $ make_named_package_project main 3.25 left right
  $ echo '(executable (name main) (libraries left right))' > dune
  $ echo 'let () = Printf.printf "%s/%s\n" Left.message Right.message' > main.ml
  $ mkdir -p _shared/sub _shared/invalid.t
  $ (cd _shared && make_dune_project_with_package 3.25 left)
  $ cat > _shared/dune <<'EOF'
  > (library (public_name left))
  > (cram (applies_to :whole_subtree))
  > EOF
  $ echo 'let message = "left"' > _shared/left.ml
  $ echo '(' > _shared/invalid.t/dune-project
  $ (cd _shared/sub && make_dune_project_with_package 3.25 right)
  $ echo '(library (public_name right))' > _shared/sub/dune
  $ echo 'let message = "right"' > _shared/sub/right.ml
  $ for package in left right; do
  >   make_dune_source_lockpkg native "$package" "$PWD/_shared"
  > done

  $ dune build main.exe
  $ _build/default/main.exe
  left/right
  $ test -f _build/default/.pkg/left/left.cmxa
  $ test -f _build/default/.pkg/right/sub/right.cmxa
  $ test ! -e _build/default/.pkg/left/sub/right.cmxa
  $ test ! -e _build/default/.pkg/right/left.cmxa

Other packages in the same snapshot are not selected merely by being present.
In particular, their libraries must not replace a selected workspace provider
or be reassigned to the mounted package during installation.

  $ mkdir workspace-support
  $ (cd workspace-support && make_dune_project_with_package 3.25 support)
  $ echo '(library (public_name support))' > workspace-support/dune
  $ echo 'let message = "workspace"' > workspace-support/support.ml
  $ mkdir -p _shared/other
  $ (cd _shared/other && make_dune_project_with_package 3.25 support)
  $ echo '(library (public_name support))' > _shared/other/dune
  $ echo 'let message = "unselected"' > _shared/other/support.ml
  $ echo '(library (public_name left) (libraries support))' > _shared/dune
  $ echo '(library (public_name right) (libraries support))' > _shared/sub/dune
  $ echo 'let message = "left/" ^ Support.message' > _shared/left.ml
  $ echo 'let message = "right/" ^ Support.message' > _shared/sub/right.ml
  $ cat > dune <<'EOF'
  > (executable (name main_left) (modules main_left) (libraries left))
  > (executable (name main_right) (modules main_right) (libraries right))
  > (executable (name workspace_main) (modules workspace_main) (libraries support))
  > EOF
  $ echo 'let () = print_endline Left.message' > main_left.ml
  $ echo 'let () = print_endline Right.message' > main_right.ml
  $ echo 'let () = print_endline Support.message' > workspace_main.ml

The mounted and workspace consumers all select the same provider.

  $ dune build main_left.exe main_right.exe workspace_main.exe
  $ _build/default/main_left.exe
  left/workspace
  $ _build/default/main_right.exe
  right/workspace
  $ _build/default/workspace_main.exe
  workspace

They can also link together without introducing duplicate compilation units.

  $ cat >> dune <<'EOF'
  > (executable (name main) (modules main) (libraries left right support))
  > EOF
  $ dune build main.exe
  $ _build/default/main.exe
  left/workspace/right/workspace
  $ test ! -e _build/default/.pkg/left/other/support.cmxa
  $ test ! -e _build/default/.pkg/right/other/support.cmxa

Installation preserves the declared dependency, without adopting its library.

  $ dune build @pkg-install
  $ grep '^requires' _build/default/.pkg/left/META.left
  requires = "support"
  $ grep '^requires' _build/default/.pkg/right/sub/META.right
  requires = "support"

Native public libraries have ordinary workspace visibility. Neither lock
package below declares a dependency on the other.

  $ echo '(executable (name main_left) (modules main_left) (libraries left))' > dune
  $ echo '(library (public_name left) (libraries right))' > _shared/dune
  $ echo 'let message = Right.message' > _shared/left.ml
  $ dune build main_left.exe
  $ _build/default/main_left.exe
  right/workspace

A private library of a native package is not visible from the workspace:

  $ echo '(library (name right_private) (modules ()))' >> _shared/sub/dune
  $ echo '(executable (name use_private) (modules use_private) (libraries right_private))' >> dune
  $ touch use_private.ml
  $ dune build use_private.exe 2>&1 | grep Error
  Error: Library "right_private" not found.
  [1]
  $ sed -i '$d' dune _shared/sub/dune && rm use_private.ml

The same undeclared provider is hidden when it is opaque: requesting the
package itself runs its recipe. The recipe must not be run while resolving
left's libraries.

  $ sed -i '3s/(run false)/(progn & (patch missing.patch))/' dune.lock/right.pkg
  $ build_pkg right 2>&1 | grep -v '^ *$'
  File "dune.lock/right.pkg", line 3, characters 19-24:
  3 | (build (progn (run false) (patch missing.patch)))
                         ^^^^^
  Error: Logs for package right
  [1]
  $ dune build main_left.exe
  File "_build/default/.pkg/left/dune", line 1, characters 39-44:
  Error: Library "right" not found.
  -> required by library "left" in _build/default/.pkg/left
  -> required by executable main_left in dune:1
  -> required by
     _build/default/.main_left.eobjs/native/dune__exe__Main_left.cmx
  -> required by _build/default/main_left.exe
  [1]

A declared transitive dependency makes the native provider visible without
building the metadata-only intermediate package.

  $ make_dune_source_lockpkg native right "$PWD/_shared"
  $ make_lockpkg bridge <<'EOF'
  > (version 1.0)
  > (depends right)
  > (build (run false))
  > EOF
  $ echo '(depends bridge)' | append_to_lockpkg left
  $ dune build main_left.exe
  $ _build/default/main_left.exe
  right/workspace

A source that vendors a project defining a package of its own is mounted like
any other, but a mount holds one package, so the vendored library is not
visible. Known limitation (Menhir has this shape): under opam, [dune build -p]
keeps vendored packages and the build succeeds.

  $ mkdir vendored-project
  $ cd vendored-project
  $ make_lockdir
  $ make_named_package_project main 3.25 bundle
  $ mkdir -p _bundle/vendor
  $ (cd _bundle && make_dune_project_with_package 3.25 bundle)
  $ cat > _bundle/dune <<'EOF'
  > (vendored_dirs vendor)
  > (library (public_name bundle) (libraries vendored_support))
  > EOF
  $ touch _bundle/bundle.ml
  $ (cd _bundle/vendor && make_dune_project_with_package 3.25 vendored_support)
  $ echo '(library (public_name vendored_support))' > _bundle/vendor/dune
  $ touch _bundle/vendor/vendored_support.ml
  $ make_dune_source_lockpkg native bundle "$PWD/_bundle"
  $ dune build _build/default/.pkg/bundle/bundle.cma
  File "_build/default/.pkg/bundle/dune", line 2, characters 41-57:
  Error: Library "vendored_support" not found.
  -> required by library "bundle" in _build/default/.pkg/bundle
  -> required by _build/default/.pkg/bundle/.bundle.objs/byte/bundle.cmo
  -> required by _build/default/.pkg/bundle/bundle.cma
  [1]

Without the vendored package the library builds.

  $ rm -r _bundle/vendor
  $ echo '(library (public_name bundle))' > _bundle/dune
  $ dune build _build/default/.pkg/bundle/bundle.cma

A disabled package in a vendored project must not leave a missing package scope.
Start with the package enabled: private libraries can be looked up from both
its exclusive directory and an unscoped directory in the same project.

  $ mkdir -p _bundle/vendor/d
  $ cat > _bundle/dune <<'EOF'
  > (vendored_dirs vendor)
  > (library (public_name bundle))
  > EOF
  $ cat > _bundle/vendor/dune-project <<'EOF'
  > (lang dune 3.25)
  > (using unreleased 0.1)
  > (package
  >  (name disabled)
  >  (dir d)
  >  (allow_empty)
  >  (enabled_if true))
  > EOF
  $ cat > _bundle/vendor/d/dune <<'EOF'
  > (library (name scoped))
  > (rule
  >  (target available)
  >  (action (with-stdout-to %{target} (echo %{lib-available:scoped}))))
  > EOF
  $ echo 'let value = 1' > _bundle/vendor/d/scoped.ml
  $ cat > _bundle/vendor/dune <<'EOF'
  > (library (name helper) (modules helper))
  > (executable (name use) (modules use) (libraries helper))
  > (rule
  >  (target available)
  >  (action
  >   (with-stdout-to %{target}
  >    (echo "%{lib-available:helper}/%{lib-available:scoped}"))))
  > EOF
  $ echo 'let value = 42' > _bundle/vendor/helper.ml
  $ echo 'let () = Printf.printf "%d\n" Helper.value' > _bundle/vendor/use.ml
  $ (cd _bundle && make_lockdir && dune build --root . \
  >   vendor/d/scoped.cma vendor/use.exe vendor/d/available vendor/available)
  $ _bundle/_build/default/vendor/use.exe
  42
  $ cat _bundle/_build/default/vendor/d/available
  true
  $ cat _bundle/_build/default/vendor/available
  true/true

Disabling the package hides its directory without affecting private libraries
outside it. A lookup of the scoped library must still complete without a missing
package scope; its availability result is not the property tested here.

  $ sed -i 's/(enabled_if true)/(enabled_if false)/' _bundle/vendor/dune-project
  $ cat >> _bundle/vendor/dune <<'EOF'
  > (rule
  >  (target disabled-query)
  >  (action (with-stdout-to %{target} (echo %{lib-available:scoped}))))
  > EOF
  $ echo 'let value = 43' > _bundle/vendor/helper.ml
  $ (cd _bundle && dune build --root . vendor/use.exe vendor/disabled-query)
  $ _bundle/_build/default/vendor/use.exe
  43
  $ test -f _bundle/_build/default/vendor/disabled-query
  $ (cd _bundle && dune build --root . vendor/d/available)
  Error: Don't know how to build vendor/d/available
  [1]

Mounting the same tree uses the locked package's scope, not the disabled
vendored package's scope, and retains the private library outside that package.

  $ mount=$pkg_root/bundle
  $ dune build $mount/bundle.cma $mount/vendor/use.exe \
  >   $mount/vendor/disabled-query
  $ $mount/vendor/use.exe
  43
  $ test -f $mount/vendor/disabled-query
  $ dune build $mount/vendor/d/available
  Error: Don't know how to build _build/default/.pkg/bundle/vendor/d/available
  [1]
