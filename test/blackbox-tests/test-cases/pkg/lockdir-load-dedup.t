Test that loading lock directories is not duplicated across contexts sharing
the same lock dir.

  $ cat >dune-workspace <<EOF
  > (lang dune 3.23)
  > (context
  >  (default))
  > (context
  >  (default
  >   (name other)))
  > EOF

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF

  $ cat >dune-project <<EOF
  > (lang dune 3.23)
  > EOF

  $ dune build @check

Both contexts share the default lock dir. Check how many times it is loaded:

  $ dune trace cat | jq -s '[ .[] | select(.name == "load_lock_dir") ] | length'
  1
