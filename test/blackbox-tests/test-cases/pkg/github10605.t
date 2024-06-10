This reproduces the issue from #10605.
Dune fails when we upstream or overlay as repository name.
It fixed now.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo 1.0 <<EOF
  > EOF
  $ cd mock-opam-repository
  $ git init --quiet
  $ git add -A
  $ git commit --quiet -m "foo 1.0"
  $ cd ..

upstream repository

  $ mv mock-opam-repository upstream-mock-opam-repository

  $ mkrepo
  $ mkpkg bar 1.0 <<EOF
  > EOF
  $ cd mock-opam-repository
  $ git init --quiet
  $ git add -A
  $ git commit --quiet -m "bar 1.0"
  $ cd ..

overlay repository
  $ mv mock-opam-repository overlay-mock-opam-repository

We set up a project that uses upstream and overlay repository

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (repositories upstream overlay))
  > (repository
  >  (name upstream)
  >  (source "git+file://$PWD/upstream-mock-opam-repository"))
  > (repository
  >  (name overlay)
  >  (source "git+file://$PWD/overlay-mock-opam-repository"))
  > (context
  >  (default
  >   (name default)))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.10)
  > 
  > (package
  >  (name baz)
  >  (depends foo bar))
  > EOF
  $ cat > dune <<EOF
  > EOF

  $ XDG_CACHE_HOME=$(pwd)/dune-workspace-cache dune pkg lock
  Solution for dune.lock:
  - bar.1.0
  - foo.1.0
