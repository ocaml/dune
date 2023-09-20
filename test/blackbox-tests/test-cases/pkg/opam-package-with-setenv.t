Testing the translation of the setenv field of an opam file into the dune lock dir.

  $ . ./helpers.sh
  $ mkrepo

Make a package with a setenv 
  $ mkpkg with-setenv <<EOF
  > opam-version: "2.0"
  > setenv: [
  >  [EXPORTED_ENV_VAR = "Hello from the other package!"]
  > ]
  > EOF

Make another package that depends on that and outputs the exported env var
  $ mkpkg deps-on-with-setenv <<'EOF'
  > opam-version: "2.0"
  > depends: [ "with-setenv" ]
  > build: [ "sh" "-c" "echo $EXPORTED_ENV_VAR" ]
  > EOF

  $ mkdir -p $mock_packages/with-setenv/with-setenv.0.0.1
  $ mkdir -p $mock_packages/deps-on-with-setenv/deps-on-with-setenv.0.0.1

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends deps-on-with-setenv))
  > EOF
  Solution for dune.lock:
  deps-on-with-setenv.0.0.1
  with-setenv.0.0.1
  
The exported env from the first package should be in the lock dir.

  $ cat dune.lock/with-setenv.pkg
  (version 0.0.1)
  $ cat dune.lock/deps-on-with-setenv.pkg
  (version 0.0.1)
  
  (build
   (run sh -c "echo $EXPORTED_ENV_VAR"))
  
  (deps with-setenv)

When building the second package the exported env var from the first package should be
available.

  $ EXPORTED_ENV_VAR="I have not been exported yet." build_pkg deps-on-with-setenv 
  I have not been exported yet.
