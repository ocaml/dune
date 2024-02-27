We want to make sure locking works even with submodules. Submodules can
contains submodules on their own which should also work.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo <<EOF
  > EOF
  $ mkpkg bar <<EOF
  > depends: [ "foo" ]
  > EOF

We move `packages` into a different folder

  $ mkdir remote-submodule
  $ mv mock-opam-repository/packages/* remote-submodule
  $ rm -r mock-opam-repository/packages

We also move `bar` to a different subrepo:

  $ mv remote-submodule/bar remote-bar

And create a tree of repos with each of them referencing each other. First we
create a regular repo containing `bar`

  $ cd remote-bar
  $ git init --quiet
  $ git add -A
  $ git commit -m "Initial commit of bar package" --quiet
  $ SUBMODULE_BAR_LOCATION=$(pwd)
  $ cd ..

Then we add the `bar` repo as submodule of the `packages` repo:

  $ cd remote-submodule
  $ git init --quiet
  $ git add -A
  $ GIT_ALLOW_PROTOCOL=file git submodule add ${SUBMODULE_BAR_LOCATION} bar
  Cloning into '$TESTCASE_ROOT/remote-submodule/bar'...
  done.
  $ git commit -m "Initial subrepo commit" --quiet
  $ SUBMODULE_REVISION=$(git rev-parse HEAD)
  $ SUBMODULE_LOCATION=$(pwd)
  $ cd ..

In our mock repository, we make sure to add the package submodule as
`packages` folder:

  $ cd mock-opam-repository
  $ git init --quiet
  $ GIT_ALLOW_PROTOCOL=file git submodule add ${SUBMODULE_LOCATION} packages
  Cloning into '$TESTCASE_ROOT/mock-opam-repository/packages'...
  done.
  $ git commit -m "Initial opam-repo commit" --quiet
  $ git submodule init
  $ git ls-tree -r HEAD | grep "commit ${SUBMODULE_REVISION}" > /dev/null && echo "Submodule exists at expected revision"
  Submodule exists at expected revision
  $ cd ..

We'll use the mock repository as source and depend on `bar`:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (repository
  >  (name mock)
  >  (source "git+file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock))
  > EOF
  $ cat > dune-project <<EOF
  > (lang dune 3.10)
  > (package
  >   (name baz)
  >   (depends bar))
  > EOF

We should be able to successfully solve the project with `foo` and `bar`:

  $ dune pkg lock
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1
