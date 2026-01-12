Test that we get a clear error when referencing a non-existent branch in a
repository URL.

Create a git repository with a single branch:

  $ mkrepo
  $ mkpkg foo 1.0
  $ cd mock-opam-repository
  $ git init --quiet
  $ git add -A
  $ git commit --quiet -m "Initial commit"
  $ cd ..

Reference a branch that does not exist:

  $ add_mock_repo_if_needed "git+file://$PWD/mock-opam-repository#nonexistent-branch"

  $ cat > dune-project <<EOF
  > (lang dune 3.10)
  > 
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

  $ dune pkg lock 2>&1 | dune_cmd subst '/[^ ]*/bin/git' 'git'
  Error: Command returned nothing: cd
  $TESTCASE_ROOT/.cache/dune/git-repo
  && git rev-parse
  --verify --quiet nonexistent-branch^{commit}
  [1]
