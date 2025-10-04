Requesting to build a single package should not build unrelated things:

  $ . ./helpers.sh

  $ add_mock_repo_if_needed
  $ mkrepo

  $ cat >dune-project <<EOF
  > (lang dune 3.12)
  > (package
  >  (name single)
  >  (depends foo bar))
  > EOF

  $ pkg() {
  > mkpkg $1 <<EOF
  > build: ["echo" "building" "$1"]
  > EOF
  > }

These two packages are independent:

  $ pkg foo
  $ pkg bar

We should only see the result of building "foo"

  $ build_pkg foo
  building foo

We should only see the result of building "bar"

  $ build_pkg bar
  building bar
