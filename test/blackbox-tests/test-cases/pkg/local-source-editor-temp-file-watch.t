The watch server must not keep a stale listing of a package sourced from a
local directory when an editor temporary file, whose events are ignored by the
file watcher, is created and then deleted in that directory.

  $ mkdir -p dep/lib
  $ echo "let x = 1" > dep/lib/a.ml

  $ mkdir project
  $ cd project
  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (source
  >  (fetch
  >   (url
  >    file://$PWD/../dep)))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends foo))
  > EOF

  $ target="$(get_build_pkg_dir foo)/target"

  $ start_dune

  $ build "$target"
  Success

Emacs creates an autosave file, and a change to the directory makes dune
re-read its listing while the autosave file exists:

  $ touch "../dep/lib/#a.ml#"
  $ touch ../dep/lib/b.ml
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build "$target"
  Success
  $ ls -a $(dirname "$target")/source/lib
  #a.ml#
  .
  ..
  a.ml
  b.ml

Emacs deletes the autosave file on save.

BUG: A change elsewhere in the package makes dune regenerate the package rules,
reusing the listing of lib with a stale file:

  $ rm "../dep/lib/#a.ml#"
  $ touch ../dep/c.ml
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build "$target"
  Failure
  [1]

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Error:
  lstat($TESTCASE_ROOT/project/../dep/lib/#a.ml#): No such file or directory
  Had 1 error, waiting for filesystem changes...
