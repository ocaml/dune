Selected packages sharing a source snapshot have separate output roots. Nested
projects retain their relative paths, and each view hides the other selected
package. Cram test fixtures are data-only before project parsing.

  $ make_lockdir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (dir .) (depends left right))
  > EOF
  $ echo '(executable (name main) (libraries left right))' > dune
  $ echo 'let () = Printf.printf "%s/%s\n" Left.message Right.message' > main.ml
  $ mkdir -p _shared/sub _shared/invalid.t
  $ cat > _shared/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name left))
  > EOF
  $ cat > _shared/dune <<'EOF'
  > (library (public_name left))
  > (cram (applies_to :whole_subtree))
  > EOF
  $ echo 'let message = "left"' > _shared/left.ml
  $ echo '(' > _shared/invalid.t/dune-project
  $ cat > _shared/sub/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name right))
  > EOF
  $ echo '(library (public_name right))' > _shared/sub/dune
  $ echo 'let message = "right"' > _shared/sub/right.ml
  $ for package in left right; do
  >   make_lockpkg "$package" <<EOF
  > (version 1.0)
  > (source (copy $PWD/_shared))
  > (build (run false))
  > EOF
  > done

  $ dune build main.exe
  $ _build/default/main.exe
  left/right
  $ find _build/_fetch -type d -name dir | wc -l
  1
  $ test -f _build/default/.lockfile/pkg/left/left.cmxa
  $ test -f _build/default/.lockfile/pkg/right/sub/right.cmxa
  $ test ! -e _build/default/.lockfile/pkg/left/sub/right.cmxa
  $ test ! -e _build/default/.lockfile/pkg/right/left.cmxa

Other packages in the same snapshot are not selected merely by being present.
In particular, their libraries must not replace a selected workspace provider
or be reassigned to the mounted package during installation.

  $ mkdir workspace-support
  $ cat > workspace-support/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name support))
  > EOF
  $ echo '(library (public_name support))' > workspace-support/dune
  $ echo 'let message = "workspace"' > workspace-support/support.ml
  $ mkdir -p _shared/other
  $ cat > _shared/other/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name support))
  > (package (name unused))
  > EOF
  $ cat > _shared/other/dune <<'EOF'
  > (library (public_name support) (modules support))
  > (library (public_name unused) (modules unused) (libraries missing))
  > EOF
  $ echo 'let message = "unselected"' > _shared/other/support.ml
  $ echo 'let value = ()' > _shared/other/unused.ml
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
  $ test ! -e _build/default/.lockfile/pkg/left/other/support.cmxa
  $ test ! -e _build/default/.lockfile/pkg/right/other/support.cmxa

Installation preserves the declared dependency, without adopting its library.

  $ dune build @pkg-install
  $ grep '^requires' _build/default/.lockfile/pkg/left/META.left
  requires = "support"
  $ grep '^requires' _build/default/.lockfile/pkg/right/sub/META.right
  requires = "support"
  $ find _build/_private/default/.packages -path '*/__private__/support/support.cmxa' | wc -l
  0

Managed dependencies must have the same visibility regardless of their builder.
Neither lock package below declares a dependency on the other.

  $ echo '(executable (name main_left) (modules main_left) (libraries left))' > dune
  $ echo '(library (public_name left) (libraries right))' > _shared/dune
  $ echo 'let message = Right.message' > _shared/left.ml
  $ dune build main_left.exe
  File "_build/default/.lockfile/pkg/left/dune", line 1, characters 39-44:
  Error: Library "right" not found.
  -> required by library "left" in _build/default/.lockfile/pkg/left
  -> required by executable main_left in dune:1
  -> required by
     _build/default/.main_left.eobjs/native/dune__exe__Main_left.cmx
  -> required by _build/default/main_left.exe
  [1]

The same undeclared provider is hidden when it is opaque. Its recipe
must not be run while resolving left's libraries.

  $ make_lockpkg right <<EOF
  > (version 1.0)
  > (source (copy $PWD/_shared))
  > (build (progn (run false) (patch missing.patch)))
  > EOF
  $ dune build main_left.exe
  File "_build/default/.lockfile/pkg/left/dune", line 1, characters 39-44:
  Error: Library "right" not found.
  -> required by library "left" in _build/default/.lockfile/pkg/left
  -> required by executable main_left in dune:1
  -> required by
     _build/default/.main_left.eobjs/native/dune__exe__Main_left.cmx
  -> required by _build/default/main_left.exe
  [1]

A declared transitive dependency makes the native provider visible without
building the metadata-only intermediate package.

  $ make_lockpkg right <<EOF
  > (version 1.0)
  > (source (copy $PWD/_shared))
  > (build (run false))
  > EOF
  $ make_lockpkg bridge <<'EOF'
  > (version 1.0)
  > (depends right)
  > (build (run false))
  > EOF
  $ make_lockpkg left <<EOF
  > (version 1.0)
  > (depends bridge)
  > (source (copy $PWD/_shared))
  > (build (run false))
  > EOF
  $ dune build main_left.exe
  $ _build/default/main_left.exe
  right/workspace
  $ test ! -e _build/default/.lockfile/pkg/bridge/.opam/bridge/target

Native eligibility belongs to the selected package's project, not its snapshot.
A package with no Dune rules initially uses its recipe.

  $ mkdir _eligibility
  $ printf '(lang dune 3.25)\n(package (name probe))\n' > _eligibility/dune-project
  $ make_lockpkg probe <<EOF
  > (version 1.0)
  > (source (copy $PWD/_eligibility))
  > (build (system "mkdir -p %{prefix}/share/probe; echo opaque > %{prefix}/share/probe/marker"))
  > EOF
  $ dune build _build/default/.lockfile/pkg/probe/.opam/probe/target/share/probe/marker
  $ cat _build/default/.lockfile/pkg/probe/.opam/probe/target/share/probe/marker
  opaque

Adding an unrelated nested project must not change that choice.

  $ mkdir _eligibility/unrelated
  $ printf '(lang dune 3.25)\n(package (name unrelated))\n' > _eligibility/unrelated/dune-project
  $ echo '(rule (target output) (action (write-file %{target} unrelated)))' > _eligibility/unrelated/dune
  $ dune build _build/default/.lockfile/pkg/probe/.opam/probe/target/share/probe/marker
  $ cat _build/default/.lockfile/pkg/probe/.opam/probe/target/share/probe/marker
  opaque

Rules in a subdirectory of the selected project do make it eligible.

  $ mkdir _eligibility/own
  $ echo '(rule (target output) (action (write-file %{target} native)))' > _eligibility/own/dune
  $ dune build _build/default/.lockfile/pkg/probe/own/output
  $ cat _build/default/.lockfile/pkg/probe/own/output
  native

Restore the prototype's authored-vendoring regression. Unlike the unselected
sibling packages above, this nested project is explicitly in vendored_dirs.
The selected package needs its public library even though the vendored package
is not selected in the lockdir, as with Menhir's vendored_pprint.

  $ mkdir vendored-project
  $ cd vendored-project
  $ make_lockdir
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (depends bundle))
  > EOF
  $ echo '(executable (name main) (libraries bundle))' > dune
  $ echo 'let () = print_endline Bundle.message' > main.ml
  $ mkdir -p _bundle/vendor/src
  $ cat > _bundle/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name bundle))
  > EOF
  $ cat > _bundle/dune <<'EOF'
  > (vendored_dirs vendor)
  > (library (public_name bundle) (libraries vendored_support))
  > EOF
  $ echo 'let message = Vendored_support.message' > _bundle/bundle.ml
  $ cat > _bundle/vendor/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name vendored_support))
  > EOF
  $ cat > _bundle/vendor/src/dune <<'EOF'
  > (library (public_name vendored_support))
  > EOF
  $ cat > _bundle/vendor/src/vendored_support.ml <<'EOF'
  > let message = "mounted vendored project"
  > EOF
  $ make_lockpkg bundle <<EOF
  > (version 1.0)
  > (source (copy $PWD/_bundle))
  > (build (run false))
  > EOF

The same authored projects work without mounting, including package selection.

  $ cp -R _bundle _workspace
  $ (cd _workspace && dune build -p bundle bundle.cma)

BUG: the mounted package mask drops the explicitly vendored library as well.

  $ dune build main.exe 2>error
  [1]
  $ grep '^Error:' error
  Error: Library "vendored_support" not found.

Vendoring does not change package ownership or public names. A library-file
pform can refer to the vendored package from outside its project. Within that
project, lib-private still recognizes its original project identity.

  $ cat >> _bundle/dune <<'EOF'
  > (rule
  >  (target public-file)
  >  (action (copy %{lib:vendored_support:payload} %{target})))
  > EOF
  $ cat >> _bundle/vendor/src/dune <<'EOF'
  > (install (section lib) (files payload))
  > (rule
  >  (target private-file)
  >  (action (copy %{lib-private:vendored_support:payload} %{target})))
  > EOF
  $ echo 'vendored payload' > _bundle/vendor/src/payload
  $ cp _bundle/dune _workspace/dune
  $ cp _bundle/vendor/src/dune _bundle/vendor/src/payload _workspace/vendor/src
  $ (cd _workspace && dune build --only-packages bundle \
  >   public-file vendor/src/private-file META.bundle \
  >   vendor/vendored_support.dune-package)
  $ cat _workspace/_build/default/public-file
  vendored payload
  $ cat _workspace/_build/default/vendor/src/private-file
  vendored payload
  $ grep '^requires' _workspace/_build/default/META.bundle
  requires = "vendored_support"
  $ grep '^(name ' \
  >   _workspace/_build/default/vendor/vendored_support.dune-package
  (name vendored_support)

BUG: the mounted view drops the library and its own package metadata.

  $ mounted=_build/default/.lockfile/pkg/bundle
  $ dune build "$mounted/public-file" 2>error
  [1]
  $ grep '^Error:' error
  Error: Library "vendored_support" not found.
  $ dune build "$mounted/vendor/src/private-file" 2>error
  [1]
  $ grep '^Error:' error
  Error: Library "vendored_support" not found.
  $ dune build "$mounted/vendor/vendored_support.dune-package" 2>error
  [1]
  $ grep '^Error:' error
  Error: Don't know how to build

Two selected packages from a common source see the same vendored project, just
as they do when that source folder is included once in the workspace.

  $ mkdir _bundle/peer
  $ cat > _bundle/peer/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name peer))
  > EOF
  $ cat > _bundle/peer/dune <<'EOF'
  > (library (public_name peer) (libraries vendored_support))
  > EOF
  $ echo 'let message = Vendored_support.message' > _bundle/peer/peer.ml
  $ cp -R _bundle/peer _workspace/peer
  $ (cd _workspace && dune build --only-packages bundle,peer \
  >   bundle.cma peer/peer.cma)
  $ make_lockpkg peer <<EOF
  > (version 1.0)
  > (source (copy $PWD/_bundle))
  > (build (run false))
  > EOF
  $ cat > dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name main) (depends bundle peer))
  > EOF
  $ echo '(executable (name main) (libraries bundle peer))' > dune
  $ cat > main.ml <<'EOF'
  > let () = Printf.printf "%s/%s\n" Bundle.message Peer.message
  > EOF
  $ dune build main.exe 2>error
  [1]
  $ grep '^Error:' error | sort -u
  Error: Library "vendored_support" not found.
