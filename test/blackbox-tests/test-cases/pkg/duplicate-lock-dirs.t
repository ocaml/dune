We configure the same lock directory twice:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.11)
  > (lock_dir (path foo))
  > (lock_dir (path foo))
  > EOF

  $ dune build

Another way to define a duplicate is by omitting the path. Since it defaults to
dune.lock:

  $ cat >dune-workspace <<EOF
  > (lang dune 3.11)
  > (lock_dir)
  > (lock_dir (path dune.lock))
  > EOF

  $ dune build
