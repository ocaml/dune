Test fetching from git

  $ . ./helpers.sh

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
  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (source (fetch (url "git+file://$MYGITREPO")))
  > (build (run cat foo))
  > EOF

  $ build_pkg test
  hello world
