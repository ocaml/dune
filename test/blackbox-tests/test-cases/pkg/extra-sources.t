Fetch from more than one source

  $ . ./helpers.sh

  $ make_lockdir
  $ mkdir foo
  $ cat >foo/bar <<EOF
  > this is bar
  > EOF

  $ cat >baz <<EOF
  > this is baz
  > EOF

  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (source (copy $PWD/foo))
  > (extra_sources (mybaz (copy $PWD/baz)))
  > (build
  >  (system "find . | sort -u"))
  > EOF

  $ build_pkg test
  .
  ./bar
  ./mybaz
