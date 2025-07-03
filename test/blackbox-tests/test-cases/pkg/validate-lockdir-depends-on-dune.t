Reproduce internal error with dune pkg validate-lockdir in #11188.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg a <<EOF
  > depends: [ "dune" ]
  > EOF
  $ mkpkg dune

  $ solve_project<<EOF
  > (lang dune 3.20)
  > (package
  >  (name vscode)
  >  (depends
  >   a))
  > EOF
  Solution for dune.lock:
  - a.0.0.1

  $ dune pkg validate-lockdir 2>&1 | grep "Map.find_exn"
    ("Map.find_exn: failed to find key", { key = "dune"; keys = [ "a" ] })
