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

  $ export BUILD_PATH_PREFIX_MAP="PWD=//$PWD:$BUILD_PATH_PREFIX_MAP"
  $ dune pkg lock 2>&1 | dune_cmd subst '-\d+' '-eol' | dune_cmd delete '\^+'
  File "dune-workspace", line 6, characters 6-eol:
  6 |  (url "git+file:PWD/mock-opam-repository#nonexistent-branch"))
  revision "nonexistent-branch" not found in
  file:PWD/mock-opam-repository
  [1]

