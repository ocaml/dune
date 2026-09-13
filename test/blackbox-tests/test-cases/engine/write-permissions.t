Even legacy projects remove all write permissions from generated files without
caching, while preserving executable bits.

  $ export DUNE_CACHE=disabled
  $ umask 022
  $ mkdir 2.3 2.4
  $ cat > 2.3/dune-project <<EOF
  > (lang dune 2.3)
  > EOF
  $ cat > 2.3/dune <<EOF
  > (rule
  >   (deps source)
  >   (targets target tool)
  >   (action
  >    (bash "cat source source > target; cp target tool;\n\
  >           chmod 666 target; chmod 777 tool")))
  > EOF
  $ cat > 2.3/source <<EOF
  > \_o< COIN
  > EOF
  $ dune build --root 2.3 target tool
  $ dune_cmd stat permissions 2.3/_build/default/target
  444
  $ dune_cmd stat permissions 2.3/_build/default/tool
  555

Both cache storage modes remove write permissions on a miss and on a hit.
Executable bits are preserved.

  $ export DUNE_TRACE=cache
  $ for mode in copy hardlink; do
  >   export DUNE_CACHE_ROOT="$PWD/cache-$mode"
  >   for workspace in first second; do
  >     dir="$mode-$workspace"
  >     mkdir "$dir"
  >     cp 2.3/dune-project 2.3/dune 2.3/source "$dir/"
  >     (
  >       cd "$dir"
  >       echo "$dir"
  >       dune build --cache=enabled --cache-storage-mode="$mode" target tool
  >       dune trace cat | jq -r '
  >         select(.cat == "cache" and .args.head == "_build/default/target")
  >         | .name'
  >       dune_cmd stat permissions _build/default/target
  >       dune_cmd stat permissions _build/default/tool
  >     )
  >   done
  > done
  copy-first
  miss
  444
  555
  copy-second
  hit
  444
  555
  hardlink-first
  miss
  444
  555
  hardlink-second
  hit
  444
  555

Check that dune >= 2.4 removes target write permissions.

  $ cat > 2.4/dune-project <<EOF
  > (lang dune 2.4)
  > (package
  >   (name foo))
  > EOF
  $ touch 2.4/foo.ml
  $ cat > 2.4/dune <<EOF
  > (executable
  >   (public_name foo))
  > (rule
  >   (deps source)
  >   (targets target)
  >   (action (bash "cat source source > target")))
  > (install
  >   (section bin)
  >   (package foo)
  >   (files foo.exe))
  > (install
  >   (section share)
  >   (package foo)
  >   (files target))
  > EOF
  $ cat > 2.4/source <<EOF
  > \_o< COIN
  > EOF
  $ dune build --root 2.4 foo.exe @install
  $ dune_cmd stat permissions 2.4/_build/default/foo.exe | head -c1
  5
  $ dune install --root 2.4 --prefix ./ --display short
  Installing lib/foo/META
  Installing lib/foo/dune-package
  Installing bin/foo
  Installing bin/foo.exe
  Installing share/foo/target
  $ dune_cmd stat permissions 2.4/bin/foo.exe | head -c1
  7
  $ dune_cmd stat permissions 2.4/share/foo/target | head -c1
  6
