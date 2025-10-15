A package depending on a package that doesn't exist.
The solver now gives a more sane error message.

A few packages here so the errors could get large.
  $ . ./helpers.sh
  $ mkrepo
  $ add_mock_repo_if_needed
  $ mkpkg a 0.0.1
  $ mkpkg a 0.0.2
  $ mkpkg a 0.1.0
  $ mkpkg b 0.0.1
  $ mkpkg c 0.0.1 << EOF
  > depends: [ "b" "a" {< "0.1.0"}]
  > EOF
  $ mkpkg d 0.0.1 << EOF
  > depends: [ "a" {>= "0.0.1"} "b"]
  > EOF
  $ mkpkg e 0.0.1
  $ mkpkg e 0.1.0
  $ mkpkg e 0.1.1

  $ cat > dune-project << EOF
  > (lang dune 3.15)
  > (name abc)
  > (package
  >  (name abc)
  >  (depends
  >    a
  >    b
  >    c
  >    d
  >    (e (= 0.1.0))
  >    foobar))
  > EOF

  $ dune pkg lock
  File "default/.lock/_unknown_", line 1, characters 0-0:
  Error: Couldn't solve the package dependency formula.
  The following packages couldn't be found: foobar
  [1]
We only report about non-existent packages.
