Test that available optional dependencies of a package are added to the
"depends" field of that package's lockfile.

  $ . ../helpers.sh
  $ mkrepo

  $ mkpkg a
  $ mkpkg b

Make a package which has a regular dependency and an optional dependency.
  $ mkpkg foo <<EOF
  > depends: [ "a" ]
  > depopts: [ "b" ]
  > EOF

The optional dependency on "b" is not included in foo's dependencies because
"b" is not part of the package solution:
  $ solve foo
  Solution for dune.lock:
  - a.0.0.1
  - foo.0.0.1
  $ cat dune.lock/foo.pkg
  (version 0.0.1)
  
  (depends a)

Another package which has a regular dependency on "b":
  $ mkpkg bar <<EOF
  > depends: [ "b" ]
  > EOF

Solve again, this time depending on both "foo" and "bar". Now "b" is among
the dependencies of "foo", since "b" is part of the package solution:
  $ solve foo bar
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.1
  - bar.0.0.1
  - foo.0.0.1
  $ cat dune.lock/foo.pkg
  (version 0.0.1)
  
  (depends a b)
