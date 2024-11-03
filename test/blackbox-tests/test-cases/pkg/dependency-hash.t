Tests for the `dune describe pkg dependency-hash` command.

The case where there are no local packages:
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > EOF
  $ dune describe pkg dependency-hash
  Error: No non-local dependencies
  [1]

The case where there are local packages but no dependencies:
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package (name a))
  > (package (name b))
  > EOF
  $ dune describe pkg dependency-hash
  Error: No non-local dependencies
  [1]

The case where there are local packages with dependencies but no non-local
dependencies:
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package (name a))
  > (package
  >  (name b)
  >  (depends a))
  > EOF
  $ dune describe pkg dependency-hash
  Error: No non-local dependencies
  [1]

A single package with a single non-local dependency:
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name a)
  >  (depends
  >   foo))
  > EOF
  $ dune describe pkg dependency-hash | tee hash1.txt
  36e640fbcda71963e7e2f689f6c96c3e

Adding another dependency causes the hash to change:
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name a)
  >  (depends
  >   foo
  >   bar))
  > EOF
  $ dune describe pkg dependency-hash | tee hash2.txt
  b6404e14c268884f825aa1fb7d1b4ead
  $ diff hash1.txt hash2.txt
  1c1
  < 36e640fbcda71963e7e2f689f6c96c3e
  ---
  > b6404e14c268884f825aa1fb7d1b4ead
  [1]

Adding a new local package which depends on one of the existing dependencies
changes the hash:

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name a)
  >  (depends
  >   foo
  >   bar))
  > (package
  >  (name b)
  >  (depends
  >   foo))
  > EOF
  $ dune describe pkg dependency-hash | tee hash3.txt
  fa35416284004d71ff802a4c582f8797
  $ diff hash2.txt hash3.txt
  1c1
  < b6404e14c268884f825aa1fb7d1b4ead
  ---
  > fa35416284004d71ff802a4c582f8797
  [1]

Adding a constraint to one of the dependencies causes the hash to change:
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name a)
  >  (depends
  >   foo
  >   bar))
  > (package
  >  (name b)
  >  (depends
  >   (foo (and :with-test (> 0.1)))))
  > EOF
  $ dune describe pkg dependency-hash | tee hash4.txt
  fdf713b190b56d52d8cdbdd72a382654
  $ diff hash3.txt hash4.txt
  1c1
  < fa35416284004d71ff802a4c582f8797
  ---
  > fdf713b190b56d52d8cdbdd72a382654
  [1]

Adding another local package with the same dependency and constraint changes
the hash:

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name a)
  >  (depends
  >   foo
  >   bar))
  > (package
  >  (name b)
  >  (depends
  >   (foo (and :with-test (> 0.1)))))
  > (package
  >  (name c)
  >  (depends
  >   (foo (and :with-test (> 0.1)))))
  > EOF
  $ dune describe pkg dependency-hash | tee hash5.txt
  38fa247158bdf36939ec8b10b3205508
  $ diff hash4.txt hash5.txt
  1c1
  < fdf713b190b56d52d8cdbdd72a382654
  ---
  > 38fa247158bdf36939ec8b10b3205508
  [1]

Make sure that the hash changes when the formula changes from a conjunction to
a disjunction, thus changing the solution:

  $ cat > dune-project <<EOF
  > (lang dune 3.11)
  > EOF
  $ cat > local.opam <<EOF
  > opam-version: "2.0"
  > depends: [ "a" "b" ]
  > EOF
  $ dune describe pkg dependency-hash | tee hash-a-b.txt
  d18946fdd9833ae312d309f654f11c1b
  $ cat > local.opam <<EOF
  > opam-version: "2.0"
  > depends: [ "a" | "b" ]
  > EOF
  $ dune describe pkg dependency-hash | tee hash-a-or-b.txt
  5b2db8d296a969fbd80033f70919b2ec
  $ diff hash-a-and-b.txt hash-a-or-b.txt
  diff: hash-a-and-b.txt: No such file or directory
  [2]

The formula also changes if the dependencies being picked end up being the same
("a" and "b") but the formula changed by including another dependency:

  $ cat > local.opam <<EOF
  > opam-version: "2.0"
  > depends: [ "a" & ("b" | "c") ]
  > EOF
  $ dune describe pkg dependency-hash | tee hash-a-b-or-c.txt
  b704ac23e0d16a5e7a1b10aa6a8cbe0e
  $ diff hash-a-b.txt hash-a-b-or-c.txt
  1c1
  < d18946fdd9833ae312d309f654f11c1b
  ---
  > b704ac23e0d16a5e7a1b10aa6a8cbe0e
  [1]
