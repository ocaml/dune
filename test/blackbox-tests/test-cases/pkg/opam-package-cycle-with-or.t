We test an repository with a package cycle that has an alternative solution which avoids a
cycle. We have the following packages:

>      a --> b --> c

Now c depends either on a or on a fourth package d. Therefore there is a valid solution
available that avoids a cycle.

  $ . ./helpers.sh
  $ mkrepo

  $ mkpkg a <<EOF
  > depends: [ "b" ]
  > EOF
  $ mkpkg b <<EOF
  > depends: [ "c" ]
  > EOF
  $ mkpkg c <<EOF
  > depends: [ "a" | "d" ]
  > EOF
  $ mkpkg d

Solver finds the invalid solution as it doesn't check cycles.

  $ solve c
  Solution for dune.lock:
  - a.0.0.1
  - b.0.0.1
  - c.0.0.1
