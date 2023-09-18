In this test we test the translation of a package with a build-env field into a dune lock
file.

  $ . ./helpers.sh
  $ mkrepo

Make a package with a build-env field
  $ mkpkg with-build-env <<EOF
  > opam-version: "2.0"
  > build-env: [ [ MY_ENV_VAR = "Hello from env var!" ] ]
  > build: ["printenv" "MY_ENV_VAR"]
  > install: ["printenv" "MY_ENV_VAR"]
  > EOF

  $ mkdir -p $mock_packages/with-build-env/with-build-env.0.0.1/

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends with-build-env))
  > EOF
  Solution for dune.lock:
  with-build-env.0.0.1
  
The lockfile should contain a setenv action.

  $ cat dune.lock/with-build-env.pkg 
  (version 0.0.1)
  
  (install
   (run printenv MY_ENV_VAR))
  
  (build
   (run printenv MY_ENV_VAR))

  $ mkdir source
  $ cat > source/foo.ml <<EOF
  > This is wrong
  > EOF

printenv should print the value given in the build-env field.

  $ build_pkg with-build-env 
  Command exited with code 1.
  -> required by _build/_private/default/.pkg/with-build-env/target
  [1]
