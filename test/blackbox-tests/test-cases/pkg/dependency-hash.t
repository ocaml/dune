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
  65f81963821a7d85d097492eb291c056

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
  9ff493c5828eb20423a750fcce2a398a
  $ diff hash1.txt hash2.txt
  1c1
  < 65f81963821a7d85d097492eb291c056
  ---
  > 9ff493c5828eb20423a750fcce2a398a
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
  0fa245ec7efc807bb09d1e3025703006
  $ diff hash2.txt hash3.txt
  1c1
  < 9ff493c5828eb20423a750fcce2a398a
  ---
  > 0fa245ec7efc807bb09d1e3025703006
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
  cef4eb066b1ebb65cd9c9a2ac5a50434
  $ diff hash3.txt hash4.txt
  1c1
  < 0fa245ec7efc807bb09d1e3025703006
  ---
  > cef4eb066b1ebb65cd9c9a2ac5a50434
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
  221410e0ccae4048b10fae20508f4085
  $ diff hash4.txt hash5.txt
  1c1
  < cef4eb066b1ebb65cd9c9a2ac5a50434
  ---
  > 221410e0ccae4048b10fae20508f4085
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
  042ac3b5e7278b59ead45a429cb5f41f
  $ cat > local.opam <<EOF
  > opam-version: "2.0"
  > depends: [ "a" | "b" ]
  > EOF
  $ dune describe pkg dependency-hash | tee hash-a-or-b.txt
  f915a9b3c6c1aeb0fef5fd35e119221c
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
  36f1aeb9d1a55dee71685c4d783cbde9
  $ diff hash-a-b.txt hash-a-b-or-c.txt
  1c1
  < 042ac3b5e7278b59ead45a429cb5f41f
  ---
  > 36f1aeb9d1a55dee71685c4d783cbde9
  [1]
