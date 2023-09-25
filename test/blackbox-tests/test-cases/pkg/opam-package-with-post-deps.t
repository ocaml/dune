Demonstrate the translation of post dependencies

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg foo <<EOF
  > opam-version: "2.0"
  > EOF

  $ mkpkg bar <<EOF
  > opam-version: "2.0"
  > depends: [ "foo" {post} ]
  > EOF

  $ solve_project 2>/dev/null <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends bar))
  > EOF

The package bar should mark post dependencies appropriately:

  $ cat dune.lock/bar.pkg
  (version 0.0.1)
  
  (deps foo)
