Selected lock sources have package-owned output roots. Recipes whose source
preparation needs build-produced files remain opaque. Their incidental Dune
configuration is recipe input, not configuration for the outer build.

  $ make_lockdir
  $ mkdir -p _sources/empty
  $ echo '(' > _sources/dune-project
  $ echo '(include missing)' > _sources/dune
  $ echo original > _sources/input
  $ cat > _sources/build.sh <<'EOF'
  > #!/bin/sh
  > set -eu
  > test -d empty
  > cat input > result
  > echo modified > input
  > printf 'share: [ "result" ]\n' > "$1.install"
  > EOF
  $ for package in first second; do
  >   make_lockpkg "$package" <<EOF
  > (version 1.0)
  > (source (copy $PWD/_sources))
  > (build
  >  (progn
  >   (run sh build.sh %{pkg-self:name})
  >   (substitute result prepared)))
  > EOF
  > done

Mounting the selected packages does not execute unrelated recipes.

  $ make_lockpkg unused <<'EOF'
  > (version 1.0)
  > (build (run false))
  > EOF
  $ dune build _build/default/.lockfile/pkg/first/.opam/first/target
  $ cat _build/default/.lockfile/pkg/first/.opam/first/target/share/first/result
  original
  $ test ! -e _build/default/.lockfile/pkg/unused/.opam/unused/target

Recipes receive writable copies. The source snapshot and the other package
using that snapshot are not modified.

  $ dune build _build/default/.lockfile/pkg/second/.opam/second/target
  $ cat _build/default/.lockfile/pkg/second/.opam/second/target/share/second/result
  original
  $ cat _sources/input
  original
  $ find _build/_fetch -path '*/dir/input' -exec cat {} \;
  original

Dune packages with supported source preparation now load into the outer graph
instead of executing their recorded recipe.

  $ mkdir _native
  $ cat > _native/dune-project <<'EOF'
  > (lang dune 3.25)
  > (package (name native) (allow_empty))
  > EOF
  $ cat > _native/dune <<'EOF'
  > (rule (target data) (action (write-file %{target} built-by-recipe)))
  > (install (package native) (section share) (files data))
  > EOF
  $ make_lockpkg native <<EOF
  > (version 1.0)
  > (source (copy $PWD/_native))
  > (dune)
  > EOF
  $ dune build _build/default/.lockfile/pkg/native/data
  $ dune trace cat | jq_dune -s '[.[] | processes | select(.args.process_args == ["build", "-p", "native"])] | length'
  0
  $ cat _build/default/.lockfile/pkg/native/data
  built-by-recipe

Opaque recipes see transitive tools but obtain installed-file capabilities from
their direct dependencies. All providers use the shared install-layout rules.

  $ make_lockpkg leaf <<'EOF'
  > (version 1.0)
  > (install
  >  (progn
  >   (run sh -c "printf '#!/bin/sh\necho leaf-tool\n' > '%{bin}/leaf-tool'")
  >   (run chmod +x %{bin}/leaf-tool)))
  > EOF
  $ make_lockpkg middle <<'EOF'
  > (version 1.0)
  > (depends leaf)
  > (build
  >  (progn
  >   (run sh -c "leaf-tool > result")
  >   (run sh -c "printf 'share: [ \"result\" ]\n' > middle.install")))
  > EOF
  $ make_lockpkg consumer <<'EOF'
  > (version 1.0)
  > (depends middle)
  > (build
  >  (progn
  >   (run leaf-tool)
  >   (system "leaf-tool")
  >   (run cat %{pkg:middle:share:result})))
  > EOF
  $ build_pkg consumer
  leaf-tool
  leaf-tool
  leaf-tool
  $ find _build/_private/default/.packages -path '*/bin/leaf-tool' | wc -l
  2
