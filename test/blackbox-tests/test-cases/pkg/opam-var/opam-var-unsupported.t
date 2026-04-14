  $ mkrepo
  > fail_solve() {
  >   mkpkg testpkg <<EOF
  > build: [ "echo" $1 ]
  > EOF
  >   solve_output=$(solve testpkg 2>&1)
  >   if [ $? -eq 0 ]; then
  >     echo "Expected solve to fail, but it succeeded" >&2
  >   else
  >     echo "$solve_output" >&2
  >   fi
  > }

opam variables that are explicitly unsupported in dune

These should all have nice error messages explaining that they are not supported.

# opam-version
  $ fail_solve opam-version
  File "$TESTCASE_ROOT/mock-opam-repository/packages/testpkg/testpkg.0.0.1/opam", line 1, characters 0-0:
  Error: Cannot translate opam variable "opam-version".
  Dune only supports a subset of opam variables when translating opam files.
# root
  $ fail_solve root 
  File "$TESTCASE_ROOT/mock-opam-repository/packages/testpkg/testpkg.0.0.1/opam", line 1, characters 0-0:
  Error: Cannot translate opam variable "root".
  Dune only supports a subset of opam variables when translating opam files.
# _:hash
  $ fail_solve _:hash
  File "$TESTCASE_ROOT/mock-opam-repository/packages/testpkg/testpkg.0.0.1/opam", line 1, characters 0-0:
  Error: Cannot translate opam variable "hash".
  Dune only supports a subset of opam variables when translating opam files.
# misc
  $ fail_solve misc
  File "$TESTCASE_ROOT/mock-opam-repository/packages/testpkg/testpkg.0.0.1/opam", line 1, characters 0-0:
  Error: Cannot translate opam variable "misc".
  Dune only supports a subset of opam variables when translating opam files.
# _:misc
  $ fail_solve _:misc
  File "$TESTCASE_ROOT/mock-opam-repository/packages/testpkg/testpkg.0.0.1/opam", line 1, characters 0-0:
  Error: Cannot translate opam variable "misc".
  Dune only supports a subset of opam variables when translating opam files.
# _:depends
  $ fail_solve _:depends
  File "$TESTCASE_ROOT/mock-opam-repository/packages/testpkg/testpkg.0.0.1/opam", line 1, characters 0-0:
  Error: Cannot translate opam variable "depends".
  Dune only supports a subset of opam variables when translating opam files.
# _:build
  $ fail_solve _:build
  File "$TESTCASE_ROOT/mock-opam-repository/packages/testpkg/testpkg.0.0.1/opam", line 1, characters 0-0:
  Error: Cannot translate opam variable "build".
  Dune only supports a subset of opam variables when translating opam files.
# _:opamfile
  $ fail_solve _:opamfile
  File "$TESTCASE_ROOT/mock-opam-repository/packages/testpkg/testpkg.0.0.1/opam", line 1, characters 0-0:
  Error: Cannot translate opam variable "opamfile".
  Dune only supports a subset of opam variables when translating opam files.
