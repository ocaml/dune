In this test we test the translation of a package with a build-env field into a dune lock
file.

  $ . ./helpers.sh
  $ mkrepo

Make a package with a build-env field
  $ mkpkg with-build-env <<'EOF'
  > build-env: [ [ MY_ENV_VAR = "Hello from env var!" ] ]
  > build: ["sh" "-c" "echo $MY_ENV_VAR"]
  > install: ["sh" "-c" "echo $MY_ENV_VAR"]
  > EOF

  $ solve with-build-env
  Solution for dune.lock:
  - with-build-env.0.0.1
The lockfile should contain a setenv action.

  $ cat dune.lock/with-build-env.pkg 
  (version 0.0.1)
  
  (install
   (withenv
    ((= MY_ENV_VAR "Hello from env var!"))
    (run sh -c "echo $MY_ENV_VAR")))
  
  (build
   (withenv
    ((= MY_ENV_VAR "Hello from env var!"))
    (run sh -c "echo $MY_ENV_VAR")))

This should print the value given in the build-env field.

  $ MY_ENV_VAR="invisible" build_pkg with-build-env 
  Hello from env var!
  Hello from env var!
