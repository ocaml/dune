Package sources can be set to git:

  $ . ../helpers.sh

  $ mkrepo
  $ add_mock_repo_if_needed

We create a repo with a fixed name for the default branch.

  $ mkdir _repo
  $ cd _repo
  $ git init --initial-branch=duplicated --quiet
  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (package (name foo))
  > EOF
  $ git add -A
  $ git commit -qm "initial commit"
  $ cd ..

  $ cat >dune-project <<EOF
  > (lang dune 3.13)
  > (pin
  >  (url "git+file://$PWD/_repo")
  >  (package (name foo)))
  > (package
  >  (name main)
  >  (depends foo))
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - foo.dev

We create a tag that clashes with the name of the branch (hence we needed to
fix the name of the branch eariler):

  $ git -C _repo tag duplicated

This should work but it fails at the moment:

  $ dune pkg lock 2>&1 | head -n 3
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("Map.of_list_exn", { key = "duplicated" })
