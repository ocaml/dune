Tests behavior when the rev store repo is not a correct git bare repo

  $ mkrepo

Create a package to lock

  $ mkdir _repo
  $ git -C _repo init --initial-branch=main --quiet
  $ touch _repo/content
  $ git -C _repo add -A
  $ git -C _repo commit -m "Initial commit" --quiet

Locking the package works

  $ mkpkg testpkg <<EOF
  > url {
  >   src: "git+file://$PWD/_repo"
  > }
  > EOF
  $ solve testpkg
  Solution for dune.lock:
  - testpkg.0.0.1

The rev store is set up correctly:

  $ git -C $PWD/.cache/dune/git-repo rev-parse --is-bare-repository
  true

Let's replace the rev-store with one that's not properly initialized:

  $ rm -r $PWD/.cache/dune/git-repo
  $ mkdir $PWD/.cache/dune/git-repo
  $ git -C $PWD/.cache/dune/git-repo rev-parse --is-bare-repository 2>&1 | grep -o "invalid gitfile format"
  invalid gitfile format
  [128]

Dune does not detect the invalid rev-store and fails

  $ solve testpkg 2>&1 | grep -o "invalid gitfile format"
  invalid gitfile format
  [1]
