  $ . ../helpers.sh 

If both make and gmake are available, we should prefer make.

opam doesn't resolve make but rather runs make literally so it is up to whatever is in
PATH to resolve it. This does mean that make is preferred over gmake so we should
replicate that behaviour.

We create a dummy package that will output the value of the opam make variable:
  $ mkrepo
  > mkpkg testpkg << EOF
  > build: [ "echo" make ]
  > EOF
  > solve testpkg 2> /dev/null

We now create dummy versions of make and gmake.
  $ cat > make; cat > gmake
We add the current directory to PATH. Dune will expand %{make} and should prefer make over
gmake.
  $ PATH=.:$PATH
  > build_pkg testpkg
  $TESTCASE_ROOT/gmake

