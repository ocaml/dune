Fetch from more than one source

  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF

  $ mkdir foo
  $ cat >foo/bar <<EOF
  > this is bar
  > EOF

  $ cat >baz <<EOF
  > this is baz
  > EOF

  $ cat >dune.lock/test.pkg <<EOF
  > (source (copy $PWD/foo))
  > (extra_sources (mybaz (copy $PWD/baz)))
  > (build
  >  (system "find . | sort -u"))
  > EOF

  $ dune build .pkg/test/target/
  .
  ./bar
  ./mybaz
