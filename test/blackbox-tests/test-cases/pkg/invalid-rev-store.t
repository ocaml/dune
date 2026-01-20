Tests behavior when the rev store repo is not a correct git bare repo

  $ mkrepo
  $ add_mock_repo_if_needed

Create a package to lock

  $ mkdir _repo
  $ git -C _repo init --initial-branch=main --quiet
  $ cat > _repo/dune-project <<EOF
  > (lang dune 3.22)
  > (package (name testpkg))
  > EOF
  $ git -C _repo add -A
  $ git -C _repo commit -m "Initial commit" --quiet

Locking the package works

  $ cat >dune-project <<EOF
  > (lang dune 3.22)
  > (pin
  >  (url "git+file://$PWD/_repo")
  >  (package (name testpkg)))
  > (package
  >  (name my)
  >  (allow_empty)
  >  (depends testpkg))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - testpkg.dev

The rev store is set up correctly:

  $ git -C $PWD/.cache/dune/git-repo rev-parse --is-bare-repository
  true

Let's replace the rev-store with one that's not properly initialized:

  $ rm -rf $PWD/.cache/dune/git-repo
  $ mkdir $PWD/.cache/dune/git-repo
  $ git -C $PWD/.cache/dune/git-repo rev-parse --is-bare-repository 2>&1 | grep -o "invalid gitfile format"
  invalid gitfile format
  [128]

Dune does not detect the invalid rev-store and fails with an unhelpful error
message because git is confused:

  $ dune_pkg_lock_normalized 2>&1 | grep -o "invalid gitfile format"
  invalid gitfile format
  [1]
