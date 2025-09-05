Demonstrate the generation of the lock directory in the presence of "|"

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg a1 0.0.1 <<EOF
  > conflicts: [ "a2" ]
  > EOF
  $ mkpkg a2 0.0.1 <<EOF
  > conflicts: [ "a1" ]
  > EOF

  $ mkpkg b <<EOF
  > depends: [ "a1" | "a2" ]
  > EOF

  $ solve b
  Solution for dune.lock:
  - a1.0.0.1
  - b.0.0.1
Only a1 or a2 should appear but not both.

  $ cat ${default_lock_dir}/b.pkg
  (version 0.0.1)
  
  (depends a1)

Release a new version of the second package in the disjunction to
demonstrate that relative version numbers don't affect the
choice. This makes sense as a1 and a2 are two completely different
packages, so comparing their version numbers is meaningless.
  $ mkpkg a2 0.0.2 <<EOF
  > conflicts: [ "a1" ]
  > EOF

  $ solve b
  Solution for dune.lock:
  - a1.0.0.1
  - b.0.0.1


Release a new version of b specifying version numers of deps. Note
that only a2 exists with the specified version. If the solver chooses
the new version of b then it must also choose a2 rather than a1, and
this is probably what will happen as it will get us the latest version
of b and the latest version of a2 (but not the latest version of a1
which is completely omitted from the solution).
  $ mkpkg b 0.0.2 <<EOF
  > depends: [ "a1" {= "0.0.2" } | "a2" {= "0.0.2" } ]
  > EOF

  $ solve b
  Solution for dune.lock:
  - a2.0.0.2
  - b.0.0.2

Same solution if a1 only known version is excluded:

  $ mkpkg b 0.0.2 <<EOF
  > depends: [ "a1" {!= "0.0.1" } | "a2" {= "0.0.2" } ]
  > EOF

  $ solve b
  Solution for dune.lock:
  - a2.0.0.2
  - b.0.0.2

Update a2.0.0.2 marking it as avoid-version which should tell the
solver to try to find a solution which doesn't include it.

  $ mkpkg a2 0.0.2 <<EOF
  > conflicts: [ "a1" ]
  > flags: [avoid-version]
  > EOF

  $ solve b
  Solution for dune.lock:
  - a1.0.0.1
  - b.0.0.1

Update a1.0.0.1 to also be flagged as avoid-version:

  $ mkpkg a1 0.0.1 <<EOF
  > flags: [avoid-version]
  > EOF

  $ solve b
  Solution for dune.lock:
  - a2.0.0.1
  - b.0.0.1

Then there are no "good" solution remaining once a2.0.0.1 is also flagged
avoid-version:

  $ mkpkg a2 0.0.1 <<EOF
  > flags: [avoid-version]
  > EOF

  $ solve b
  Solution for dune.lock:
  - a2.0.0.2 (this version should be avoided)
  - b.0.0.2

In general, the solver should select the minimal number of avoid-version possible:

  $ mkpkg a 0.2 <<EOF
  > depends: [ "c" ]
  > EOF

  $ mkpkg a 0.1 <<EOF
  > depends: [ "d" ]
  > EOF

  $ mkpkg c 0.2 <<EOF
  > flags: [avoid-version]
  > depends: [ "d" ]
  > EOF

  $ mkpkg c 0.1 <<EOF
  > flags: [avoid-version]
  > depends: [ "e" ]
  > EOF

  $ mkpkg d 0.2 <<EOF
  > flags: [avoid-version]
  > depends: [ "e" ]
  > EOF

  $ mkpkg d 0.1 <<EOF
  > flags: [avoid-version]
  > depends: [ "f" ]
  > EOF

  $ mkpkg e 0.1 <<EOF
  > flags: [avoid-version]
  > depends: [ "f" ]
  > EOF

  $ mkpkg f 0.1 <<EOF
  > flags: [avoid-version]
  > EOF

In this example, the solver will first find a solution by DFS (always selecting
the largest possible version of each package), which will result in 4
avoid-version for packages c, d, e, f:

a.0.2 => c.0.2 => d.0.2 => e.0.1 => f.0.1

But after minimizing this upperbound, the solver will find that ignoring the
latest versions of packages a and d allow it to reduce the solution to 2
avoid-version:

  $ solve a
  Solution for dune.lock:
  - a.0.1
  - d.0.1 (this version should be avoided)
  - f.0.1 (this version should be avoided)
