Test that we get a clear error when a pin references a non-existent branch.

  $ mkrepo
  $ add_mock_repo_if_needed

Create a git repo to pin:

  $ mkdir _repo
  $ cd _repo
  $ git init --initial-branch=main --quiet
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF
  $ git add -A
  $ git commit -qm "initial commit"
  $ cd ..

Reference a branch that does not exist in the pin:

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "git+file://$PWD/_repo#nonexistent-branch")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

  $ dune pkg lock 2>&1 | dune_cmd subst '/[^ ]*/bin/git' 'git'
  Error: Command returned nothing: cd
  $TESTCASE_ROOT/.cache/dune/git-repo
  && git rev-parse
  --verify --quiet nonexistent-branch^{commit}
  [1]
