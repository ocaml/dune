Test for packages with an extra-source file with the same name as a
file in the package's source.

  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (source
  >  (copy $PWD/foo-source))
  > (extra_sources
  >  (foo.txt
  >   (fetch
  >    (url file://$PWD/foo.txt))))
  > EOF

  $ mkdir -p foo-source
  $ echo "from source" > foo-source/foo.txt

  $ echo "from extra source" > foo.txt

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (allow_empty)
  >  (name a)
  >  (depends foo))
  > EOF

  $ export DUNE_CACHE=disabled
  $ echo 'share: [ "foo.txt" ]' >foo-source/foo.install
  $ build_pkg foo
  $ cat "$(get_build_pkg_dir foo)/target/share/foo/foo.txt"
  from extra source
  $ cat foo-source/foo.txt
  from source

The overlay currently overwrites the staged copy, not the original file.

  $ cat "$(get_build_pkg_dir foo)/source/foo.txt"
  from extra source

The first package had no explicit build or install command. Dropping the
overlay must recover the original source for the next recipe, even when its
target is reused. Compare the installed file to a fresh build.

  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build (run cp foo.txt observed))
  > (source (copy $PWD/foo-source))
  > EOF
  $ echo 'share: [ "observed" ]' >foo-source/foo.install
  $ target=$(get_build_pkg_dir foo)/target
  $ dune build "$target"
  $ fresh=_fresh/${target#_build/}
  $ dune build --build-dir _fresh "$fresh"
  $ cat "$fresh/share/foo/observed"
  from source
  $ cmp "$target/share/foo/observed" "$fresh/share/foo/observed"

Restoring the overlay must also work for an explicit build command.

  $ make_lockpkg foo <<EOF
  > (version 1)
  > (build (run cp foo.txt observed))
  > (source (copy $PWD/foo-source))
  > (extra_sources
  >  (foo.txt (fetch (url file://$PWD/foo.txt))))
  > EOF
  $ build_pkg foo
  $ cat "$(get_build_pkg_dir foo)/target/share/foo/observed"
  from extra source
  $ cat foo-source/foo.txt
  from source

A lockdir .files overlay should install its contents without changing the
staged copy. With no explicit commands, the package rule tries to overwrite
that copy directly. This can fail if the staged file is read-only.

  $ mkdir bar-source
  $ echo 'from source' >bar-source/foo.txt
  $ echo 'share: [ "foo.txt" ]' >bar-source/bar.install
  $ make_lockpkg bar <<EOF
  > (version 1)
  > (source (copy $PWD/bar-source))
  > EOF
  $ make_lockpkg_file bar foo.txt <<'EOF'
  > from .files
  > EOF
  $ bar=$(get_build_pkg_dir bar)
  $ overlay="${source_lock_dir}/bar.files/foo.txt"
  $ dune build "$bar/target" >overlay.log 2>&1 &&
  >   cmp -s "$bar/target/share/bar/foo.txt" "$overlay" &&
  >   cmp -s "$bar/source/foo.txt" bar-source/foo.txt
  [1]
