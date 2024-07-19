Test that post dependencies of a package a treated as transitive
dependencies of that package's dependant packages.

  $ . ./helpers.sh
  $ mkrepo

A package with a post dependency on "bar".
  $ mkpkg foo <<EOF
  > depends: [
  >  "bar" {post}
  > ]
  > install: [
  >  [ "echo" "Installing foo..." ]
  >  [ "sh" "-c" "printf '#!/bin/sh\necho This is foo!' > %{bin}%/foo.sh && chmod a+x %{bin}%/foo.sh" ]
  >  [ "echo" "Done installing foo!" ]
  > ]
  > EOF

A package with a regular dependency on "foo". This package is also a post depenency of "foo".
  $ mkpkg bar <<EOF
  > depends: [
  >  "foo"
  > ]
  > install: [
  >  [ "echo" "Installing bar..." ]
  >  [ "echo" "Bar installer running foo.sh:" ]
  >  [ "foo.sh" ]
  >  [ "sh" "-c" "printf '#!/bin/sh\necho This is bar!' > %{bin}%/bar.sh && chmod a+x %{bin}%/bar.sh" ]
  >  [ "echo" "Done installing bar!" ]
  > ]
  > EOF

Test that a dune project that depends on "foo" has access to "bar" in its build environment.
  $ solve foo
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1

  $ cat > dune <<EOF
  > (rule
  >  (target x.txt)
  >  (action
  >   (with-stdout-to x.txt
  >    (run bar.sh))))
  > EOF

  $ dune build x.txt
  Error: Dependency cycle between:
     - package bar
  [1]

  $ cat _build/default/x.txt
  cat: _build/default/x.txt: No such file or directory
  [1]

  $ dune clean

A package with a regular dependency on "foo".
  $ mkpkg baz <<'EOF'
  > depends: [
  >  "foo"
  > ]
  > install: [
  >  [ "echo" "Installing baz..." ]
  >  [ "echo" "Baz installer running foo.sh:" ]
  >  [ "foo.sh" ]
  >  [ "echo" "Baz installer running bar.sh:" ]
  >  [ "bar.sh" ]
  >  [ "echo" "Done installing baz!" ]
  > ]
  > EOF

Test that an opam package that depends on "foo" has access to "bar" in its build environment.
  $ solve baz
  Solution for dune.lock:
  - bar.0.0.1
  - baz.0.0.1
  - foo.0.0.1

  $ dune build
  Error: Dependency cycle between:
     - package bar
  [1]
