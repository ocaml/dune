We want to test that a failing flock(2) shows an error.

Thus we first create a repo:

  $ . ../git-helpers.sh
  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo 1.0 <<EOF
  > EOF
  $ cd mock-opam-repository
  $ git init --quiet
  $ git add -A
  $ git commit --quiet -m "Initial commit"
  $ cd ..
  $ add_mock_repo_if_needed "git+file://$(pwd)/mock-opam-repository"

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

There should be some kind of error message if getting the revision store lock
fails (simulated here with a failing flock(2) call):

  $ XDG_CACHE_HOME=$(pwd)/dune-workspace-cache strace -e inject=flock:error=EBADFD -o /dev/null dune pkg lock
  Error: Failed to get a lock for the revision store at
  $TESTCASE_ROOT/dune-workspace-cache/dune/rev-store.lock:
  File descriptor in bad state
  [1]
