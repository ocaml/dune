Lock directories in subdirectories should work.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.24)
  > (lock_dir (path sub/dune.lock))
  > (context
  >  (default
  >   (lock_dir sub/dune.lock)))
  > EOF
  $ source_lock_dir=sub/dune.lock make_lockdir

BUG: no rule is generated for the lock directory at the subdirectory path.

  $ dune build
  Error: No rule found for default/.lock/sub/dune.lock (context _private)
  -> required by read lock directory
     _build/_private/default/.lock/sub/dune.lock
  [1]
