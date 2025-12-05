Ensure dune can handle the special string interpolation syntax used by opam for
packages whose names contain a '+' character. This syntax is described in
https://opam.ocaml.org/doc/Manual.html#Variables

  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed

This is based on the build command of mingw-w64-shims.0.2.0
  $ mkpkg foo <<EOF
  > build: [
  >   [ "echo" "i686-gcc-g++-%{?conf-mingw-w64-g++-i686:installed:}%" ]
  > ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.17)
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

  $ dune_pkg_lock_normalized
  Solution for dune.lock:
  - foo.0.0.1
