It's possible to include additional packages or constraints in workspace files:

  $ . ./helpers.sh

  $ cat >dune-workspace <<EOF
  > (lang dune 3.11)
  > (lock_dir
  >  (constraints doesnotexist foo (bar (= 1.0.0)))
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF

  $ mkrepo

  $ mkpkg foo 1.0.0
  $ mkpkg bar 1.0.0
  $ mkpkg bar 1.9.1

Notice that the constraints field doesn't introduce additional packages. The
"doesnotexist" package isn't being pulled.

  $ solve_project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name x)
  >  (depends foo bar))
  > EOF
  Solution for dune.lock:
  - bar.1.0.0
  - foo.1.0.0
