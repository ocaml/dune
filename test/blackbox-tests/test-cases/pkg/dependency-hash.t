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
  7ca1f99e145b303327501bfbbc37d9c3

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
  2a25c11343df3a8259d7c024aaa0ee61
  $ diff hash1.txt hash2.txt
  1c1
  < 7ca1f99e145b303327501bfbbc37d9c3
  ---
  > 2a25c11343df3a8259d7c024aaa0ee61
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
  578d4513e42a927739d440803aca51db
  $ diff hash2.txt hash3.txt
  1c1
  < 2a25c11343df3a8259d7c024aaa0ee61
  ---
  > 578d4513e42a927739d440803aca51db
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
  22079ffcd21b09d251335cf9af50952e
  $ diff hash3.txt hash4.txt
  1c1
  < 578d4513e42a927739d440803aca51db
  ---
  > 22079ffcd21b09d251335cf9af50952e
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
  bd8e515e2ebc57716e85d885fd92ac72
  $ diff hash4.txt hash5.txt
  1c1
  < 22079ffcd21b09d251335cf9af50952e
  ---
  > bd8e515e2ebc57716e85d885fd92ac72
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
  0957b29d20339bd1b51e20e42066782c
  $ cat > local.opam <<EOF
  > opam-version: "2.0"
  > depends: [ "a" | "b" ]
  > EOF
  $ dune describe pkg dependency-hash | tee hash-a-or-b.txt
  d46871f184041027247cf4495376acc8
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
  498c68b425dcbea875ff4248ec63c3a7
  $ diff hash-a-b.txt hash-a-b-or-c.txt
  1c1
  < 0957b29d20339bd1b51e20e42066782c
  ---
  > 498c68b425dcbea875ff4248ec63c3a7
  [1]
