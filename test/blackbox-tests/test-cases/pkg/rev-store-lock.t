Testing whether the revision store locks properly.

To start with we create a repository in with a `foo` package.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo 1.0 <<EOF
  > EOF
  $ cd mock-opam-repository
  $ git init --quiet
  $ git add -A
  $ git commit --quiet -m "Initial commit"
  $ cd ..

We set this repository as sole source for opam repositories.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (repository
  >  (name mock)
  >  (source "git+file://$(pwd)/mock-opam-repository"))
  > (context
  >  (default
  >   (name default)
  >   (repositories mock)))
  > EOF

We set the project up to depend on `foo`

  $ cat > dune-project <<EOF
  > (lang dune 3.10)
  > 
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF
  $ cat > dune <<EOF
  > EOF

Creating a lock should thus work.

  $ mkdir dune-workspace-cache
  $ XDG_CACHE_HOME=$(pwd)/fake-xdg-cache dune pkg lock
  Solution for dune.lock:
  - foo.1.0

There should also be some kind of error message if getting the revision store
lock fails (simulated here with a failing flock(2) call):

  $ XDG_CACHE_HOME=$(pwd)/dune-workspace-cache strace -e inject=flock:error=EBADFD -o /dev/null dune pkg lock
  Error: Failed to get a lock for the revision store at
  $TESTCASE_ROOT/dune-workspace-cache/dune/rev-store.lock:
  File descriptor in bad state
  [1]
