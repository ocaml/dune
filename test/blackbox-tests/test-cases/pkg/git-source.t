Test fetching from git

  $ mkdir somerepo
  $ cd somerepo
  $ git init --quiet
  $ echo "hello world" > foo
  $ git add foo
  $ git commit -am _ --quiet
  $ cd ..

  $ MYGITREPO=$PWD/somerepo

  $ mkdir foo && cd foo
  $ make_lockdir
  $ make_lockpkg test <<EOF
  > (version 0.0.1)
  > (source (fetch (url "git+file://$MYGITREPO")))
  > (build (run cat foo))
  > EOF

Loading fetch rules must not resolve Git sources of unrelated packages.

  $ make_lockpkg unused <<EOF
  > (version 0.0.1)
  > (source (fetch (url "git+file://$PWD/missing-repo")))
  > EOF

  $ build_pkg test
  hello world
