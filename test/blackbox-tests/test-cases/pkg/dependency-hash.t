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
  342113efdcc34a7a10e7bd6f70e2463c

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
  832e7a002ca8215a6507963d27b06b01
  $ diff hash1.txt hash2.txt
  1c1
  < 342113efdcc34a7a10e7bd6f70e2463c
  ---
  > 832e7a002ca8215a6507963d27b06b01
  [1]

Adding a new local package which depends on one of the existing dependencies
doesn't change the hash:
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
  832e7a002ca8215a6507963d27b06b01
  $ diff hash2.txt hash3.txt

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
  33b01608bfca250def72900422dcea38
  $ diff hash3.txt hash4.txt
  1c1
  < 832e7a002ca8215a6507963d27b06b01
  ---
  > 33b01608bfca250def72900422dcea38
  [1]

Adding another local package with the same dependency and constraint doesn't
change the hash:
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
  33b01608bfca250def72900422dcea38
  $ diff hash4.txt hash5.txt
