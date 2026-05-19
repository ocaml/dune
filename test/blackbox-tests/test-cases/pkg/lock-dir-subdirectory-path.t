Lock directories in subdirectories should work.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.24)
  > (lock_dir (path sub/dune.lock))
  > (context
  >  (default
  >   (lock_dir sub/dune.lock)))
  > EOF
  $ source_lock_dir=sub/dune.lock make_lockdir

  $ dune build

Also with deeper nesting:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.24)
  > (lock_dir (path a/b/c.lock))
  > (context
  >  (default
  >   (lock_dir a/b/c.lock)))
  > EOF
  $ source_lock_dir=a/b/c.lock make_lockdir

  $ dune build
