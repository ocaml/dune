Testing the translation of the setenv field of an opam file into the dune lock dir.

  $ . ./helpers.sh
  $ mkrepo

Make a package with a setenv. We also test all the kinds of env updates here expcept for
=+= which isn't used at all in the wild. 
  $ mkpkg with-setenv <<EOF
  > opam-version: "2.0"
  > setenv: [
  >  [EXPORTED_ENV_VAR = "Hello from the other package!"]
  >  [prepend_with_trailing_sep := "Prepended with trailing sep"]
  >  [prepend_without_trailing_sep += "Prepended without trailing sep"]
  >  [append_without_leading_sep =+ "Appended without leading sep"]
  >  [append_with_leading_sep =: "Appended with leading sep"]
  > ]
  > EOF

Make another package that depends on that and outputs the exported env vars
  $ mkpkg deps-on-with-setenv <<'EOF'
  > opam-version: "2.0"
  > depends: [ "with-setenv" ]
  > build: [
  >  [ "sh" "-c" "echo $EXPORTED_ENV_VAR" ]
  >  [ "sh" "-c" "echo $prepend_without_trailing_sep" ]
  >  [ "sh" "-c" "echo $prepend_with_trailing_sep" ]
  >  [ "sh" "-c" "echo $append_without_leading_sep" ]
  >  [ "sh" "-c" "echo $append_with_leading_sep" ]
  > ]
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
  
  (exported_env
   (= EXPORTED_ENV_VAR "Hello from the other package!")
   (:= prepend_with_trailing_sep "Prepended with trailing sep")
   (+= prepend_without_trailing_sep "Prepended without trailing sep")
   (=+ append_without_leading_sep "Appended without leading sep")
   (=: append_with_leading_sep "Appended with leading sep"))
  $ cat dune.lock/deps-on-with-setenv.pkg
  (version 0.0.1)
  
  (build
   (progn
    (run sh -c "echo $EXPORTED_ENV_VAR")
    (run sh -c "echo $prepend_without_trailing_sep")
    (run sh -c "echo $prepend_with_trailing_sep")
    (run sh -c "echo $append_without_leading_sep")
    (run sh -c "echo $append_with_leading_sep")))
  
  (deps with-setenv)

When building the second package the exported env vars from the first package should be
available and all the env updates should be applied correctly.

  $ EXPORTED_ENV_VAR="I have not been exported yet." \
  > prepend_without_trailing_sep="foo:bar" \
  > prepend_with_trailing_sep="foo:bar" \
  > append_without_leading_sep="foo:bar" \
  > append_with_leading_sep="foo:bar" \
  > build_pkg deps-on-with-setenv 
  Hello from the other package!
  Prepended without trailing sep
  Prepended with trailing sep
  Appended without leading sep
  Appended with leading sep
