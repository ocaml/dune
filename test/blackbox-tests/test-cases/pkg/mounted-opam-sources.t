Selected lock sources have package-owned output roots. Recipes whose source
preparation needs build-produced files remain opaque. Their incidental Dune
configuration is recipe input, not configuration for the outer build.

  $ make_lockdir
  $ mkdir _sources
  $ echo '(' > _sources/dune-project
  $ echo '(include missing)' > _sources/dune
  $ echo original > _sources/input
  $ for package in first second; do
  >   make_lockpkg "$package" <<EOF
  > (version 1.0)
  > (source (copy $PWD/_sources))
  > (build
  >  (progn
  >   (system "cat input > result && echo modified > input")
  >   (run sh -c "printf 'share: [ \"result\" ]\n' > %{pkg-self:name}.install")
  >   (substitute result prepared)))
  > EOF
  > done

  $ dune build _build/default/.pkg/first/_install
  $ cat _build/default/.pkg/first/_install/share/first/result
  original

Opaque recipes see transitive tools and read the installed files of their
direct dependencies through section variables. All providers use the shared
install-layout rules.

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
  >   (run cat %{pkg:middle:share}/result)))
  > EOF
  $ build_pkg consumer
  leaf-tool
  leaf-tool
  leaf-tool

A recipe-built package's source is in its mount, including directories that
Dune would not treat as sources, and its recipe runs there.

  $ mkdir -p _raw/_underscore _raw/.dot _raw/sub
  $ echo a > _raw/_underscore/a
  $ echo b > _raw/.dot/b
  $ echo c > _raw/sub/c
  $ echo '(dirs)' > _raw/dune
  $ make_lockpkg raw <<EOF2
  > (version 1.0)
  > (source (copy $PWD/_raw))
  > (build (system "find . -type f | sort"))
  > EOF2
  $ build_pkg raw
  ./.dot/b
  ./_underscore/a
  ./dune
  ./sub/c
  $ test -f _build/default/.pkg/raw/.dot/b
