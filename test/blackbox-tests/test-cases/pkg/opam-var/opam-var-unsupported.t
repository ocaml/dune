  $ . ./opam-var-helpers.sh

  $ mkrepo
  > fail_solve() {
  >   mkpkg testpkg <<EOF
  > opam-version: "2.0"
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
  Expected solve to fail, but it succeeded
# root
  $ fail_solve root 
  Expected solve to fail, but it succeeded
# _:hash
  $ fail_solve _:hash
  Expected solve to fail, but it succeeded
# _:build-id
  $ fail_solve _:build-id
  Expected solve to fail, but it succeeded
# misc
  $ fail_solve misc
  Expected solve to fail, but it succeeded
# _:misc
  $ fail_solve _:misc
  Expected solve to fail, but it succeeded
