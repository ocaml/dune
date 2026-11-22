Empty directories are currently omitted when staging package inputs. Regular
files are copied, but empty directories disappear from both local sources and
lockfile-adjacent overlays.

  $ make_dune_project 3.26
  $ make_lockdir
  $ mkdir -p src/source-empty
  $ echo source > src/source-file

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build (system "find . | sort"))
  > (source (copy $PWD/src))
  > EOF

  $ build_pkg foo
  .
  ./source-file

The .files overlay has the same omission: the regular file reaches the action,
but the empty directory does not.

  $ make_lockpkg_file foo overlay-file <<EOF
  > overlay
  > EOF
  $ mkdir dune.lock/foo.files/overlay-empty
  $ build_pkg foo
  .
  ./overlay-file
  ./source-file
