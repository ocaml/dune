Demonstrate the generation of the lock directory in the presence of "|"

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg a1 <<EOF
  > conflicts: [ "a2" ]
  > EOF
  $ mkpkg a2 <<EOF
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

  $ cat dune.lock/b.pkg
  (version 0.0.1)
  
  (depends a1)
