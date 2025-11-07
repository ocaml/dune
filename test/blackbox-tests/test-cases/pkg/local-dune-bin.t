The dune binary we resolve should be the current one we are running:

  $ . ./helpers.sh

  $ mkdir fake_dune
  $ cat > fake_dune/dune <<EOF
  > #!/usr/bin/env sh
  > echo "Incorrect dune!"
  > EOF
  $ chmod +x fake_dune/dune

  $ REAL_DUNE=$(which dune)

  $ mkdir test && cd test
  $ mkrepo

  $ mkpkg bar <<EOF
  > depends: [ "dune" ]
  > build: [ "echo" "$(which dune)" ]
  > EOF

  $ solve_project <<EOF
  > (lang dune 3.21)
  > (package
  >  (name foo)
  >  (allow_empty)
  >  (depends bar))
  > EOF
  Solution for dune.lock:
  - bar.0.0.1

  $ test() {
  >   dune build @pkg-install 2>&1 | sed "s|$REAL_DUNE|Correct dune!|"
  > }

Everything appears to build correctly.

  $ test
  Correct dune!

However if we put a different dune in PATH, the problems become apparent.

  $ export PATH="$PWD/../fake_dune:$PATH"

  $ test
  Incorrect dune!
