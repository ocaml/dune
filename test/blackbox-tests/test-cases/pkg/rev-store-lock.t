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
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "git+file://$(pwd)/mock-opam-repository"))
  > (context
  >  (default
  >   (name default)
  >   (lock_dir dune.lock)))
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
