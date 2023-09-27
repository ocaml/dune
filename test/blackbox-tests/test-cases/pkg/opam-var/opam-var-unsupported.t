  $ . ./opam-var-helpers.sh

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
  Error: Variable "opam-version" occuring in opam package "testpkg.0.0.1" is
  not supported.
# root
  $ fail_solve root 
  Error: Variable "root" occuring in opam package "testpkg.0.0.1" is not
  supported.
# _:hash
  $ fail_solve _:hash
  Error: Variable "hash" occuring in opam package "testpkg.0.0.1" is not
  supported.
# _:build-id
  $ fail_solve _:build-id
  Error: Variable "build-id" occuring in opam package "testpkg.0.0.1" is not
  supported.
# misc
  $ fail_solve misc
  Error: Variable "misc" occuring in opam package "testpkg.0.0.1" is not
  supported.
# _:misc
  $ fail_solve _:misc
  Error: Variable "misc" occuring in opam package "testpkg.0.0.1" is not
  supported.
