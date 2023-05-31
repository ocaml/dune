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
  $ mkdir dune.lock
  $ cat >dune.lock/lock.dune <<EOF
  > (lang package 0.1)
  > EOF
  $ cat >dune.lock/test <<EOF
  > (source (fetch (url "git+file://$MYGITREPO")))
  > (build (run cat foo))
  > EOF

  $ dune build _build/default/.pkg/test/target 2>&1 | tail -n +4
  Error: is a directory
         is a directory
         
