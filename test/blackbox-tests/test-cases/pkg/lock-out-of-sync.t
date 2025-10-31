Trying to build a package after updating the dependencies in dune-project but
without running `dune pkg lock` must raise an error in the context of Dune
Package Managemenet. 

  $ . ./helpers.sh

Create a fake project and lock it:

  $ mkrepo
  $ mkpkg foo <<EOF
  > build: [ "echo" "foo" ]
  > EOF
  $ mkpkg bar <<EOF
  > build: [ "echo" "bar" ]
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (allow_empty)
  >  (depends foo))
  > EOF
  $ add_mock_repo_if_needed
  $ dune pkg lock
  Solution for .dune-solution-cache:
  - foo.0.0.1

As the lock file is syncronised with `dune-pkg`, the build succeeds:
  $ build_pkg foo
  foo

We add the bar dependency to the test package
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name test)
  >  (allow_empty)
  >  (depends foo bar))
  > EOF

It fails as we have not regenerated the lock:
  $ dune build
  File ".dune-solution-cache/lock.dune", line 1, characters 0-0:
  Error: The lock dir is not sync with your dune-project
  Hint: run dune pkg lock
  [1]

We fix it and the build succeeds again:
  $ dune pkg lock
  Solution for .dune-solution-cache:
  - bar.0.0.1
  - foo.0.0.1
  $ build_pkg foo
  $ build_pkg bar
  bar
