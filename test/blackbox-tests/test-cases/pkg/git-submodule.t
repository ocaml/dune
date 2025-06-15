  $ . ./helpers.sh

When we fetch a package source we should also fetch any submodules. Since we
will use the file protocol for git submodules we will need to explicitly enable
it as it is disabled for security purposes.

  $ export GIT_ALLOW_PROTOCOL=file

This repository will be a submodule in our main repository.
  $ mkdir someotherrepo
  $ cd someotherrepo
  $ git init --quiet
  $ echo "world" > bar
  $ git add bar
  $ git commit -am _ --quiet
  $ cd ..
  $ SOMEOTHERREPO=$PWD/someotherrepo

We create a repository for the package that we wish to build that has
someotherrepo as a submodule.
  $ mkdir somerepo
  $ cd somerepo
  $ git init --quiet
  $ echo "hello" > foo
  $ git submodule add --quiet $SOMEOTHERREPO mysubmodule
  $ git add foo mysubmodule .gitmodules
  $ git commit -am _ --quiet
  $ cd ..
  $ SOMEREPO=$PWD/somerepo

  $ mkdir foo && cd foo
  $ make_lockdir
  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (source (fetch (url "git+file://$SOMEREPO")))
  > (build (progn (run cat foo) (run cat mysubmodule/bar)))
  > EOF

Building this package should pull in both repositories. At the moment this is
not the case and only somerepo is pulled.

  $ build_pkg test 2>&1 | sed -E 's|.*/cat|cat|'
  hello
  File "dune.lock/test.pkg", line 3, characters 33-36:
  3 | (build (progn (run cat foo) (run cat mysubmodule/bar)))
                                       ^^^
  Error: Logs for package test
  cat: mysubmodule/bar: No such file or directory
  
When the above works it should act like:

  $ make_lockdir
  $ cat >dune.lock/test.pkg <<EOF
  > (version 0.0.1)
  > (source (fetch (url "git+file://$SOMEREPO")))
  > (build 
  >  (progn
  >   (run cp -r $SOMEOTHERREPO/bar mysubmodule)
  >   (run cat foo)
  >   (run cat mysubmodule/bar)))
  > EOF

  $ build_pkg test
  hello
  world

