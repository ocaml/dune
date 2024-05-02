Testing the translation of the setenv field of an opam file into the dune lock dir.

  $ . ./helpers.sh
  $ mkrepo

Make a package with a setenv. We also test all the kinds of env updates here expcept for
=+= which isn't used at all in the wild. 
  $ mkpkg with-setenv <<EOF
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
  > depends: [ "with-setenv" ]
  > build: [
  >  [ "sh" "-c" "echo $EXPORTED_ENV_VAR" ]
  >  [ "sh" "-c" "echo $prepend_without_trailing_sep" ]
  >  [ "sh" "-c" "echo $prepend_with_trailing_sep" ]
  >  [ "sh" "-c" "echo $append_without_leading_sep" ]
  >  [ "sh" "-c" "echo $append_with_leading_sep" ]
  > ]
  > EOF
  > solve deps-on-with-setenv
  Solution for dune.lock:
  - deps-on-with-setenv.0.0.1
  - with-setenv.0.0.1
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
  
  (depends with-setenv)

When building the second package the exported env vars from the first package should be
available and all the env updates should be applied correctly.

The output of opam when building the equivalent package is:

Hello from the other package!
Prepended without trailing sep
Prepended with trailing sep:
Appended without leading sep
:Appended with leading sep

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

We now make a third package that updates the env in a similar way, in order to see the
difference between a propagated export_env versus the initial env.

  $ mkpkg with-setenv-2 <<EOF
  > depends: [ "with-setenv" ]
  > setenv: [
  >  [EXPORTED_ENV_VAR = "Hello from the second package!"]
  >  [prepend_with_trailing_sep := "Prepended 2nd time with sep"]
  >  [prepend_without_trailing_sep += "Prepended 2nd time without trailing sep"]
  >  [append_without_leading_sep =+ "Appended 2nd time without leading sep"]
  >  [append_with_leading_sep =: "Appended 2nd time with leading sep"]
  > ]
  > EOF
  > mkpkg deps-on-with-setenv-2 <<'EOF'
  > depends: [ "with-setenv-2" ]
  > build: [
  >  [ "sh" "-c" "echo $EXPORTED_ENV_VAR" ]
  >  [ "sh" "-c" "echo $prepend_without_trailing_sep" ]
  >  [ "sh" "-c" "echo $prepend_with_trailing_sep" ]
  >  [ "sh" "-c" "echo $append_without_leading_sep" ]
  >  [ "sh" "-c" "echo $append_with_leading_sep" ]
  > ]
  > EOF
  > solve deps-on-with-setenv-2
  Solution for dune.lock:
  - deps-on-with-setenv-2.0.0.1
  - with-setenv.0.0.1
  - with-setenv-2.0.0.1
We can now observe how the environment updates are applied a second time.

We currently have the following issues:
- The leading and trailing separators are missing.
- The initial environment is missing.
- The order of applying environment updates is different from opam's.

The output of opam when building the equivalent package is:

Hello from the other package!
Prepended without trailing sep:Prepended 2nd time without trailing sep
Prepended with trailing sep:Prepended 2nd time with sep:
Appended 2nd time without leading sep:Appended without leading sep
:Appended 2nd time with leading sep:Appended with leading sep

  $ EXPORTED_ENV_VAR="I have not been exported yet." \
  > prepend_without_trailing_sep="foo:bar" \
  > prepend_with_trailing_sep="foo:bar" \
  > append_without_leading_sep="foo:bar" \
  > append_with_leading_sep="foo:bar" \
  > build_pkg deps-on-with-setenv-2
  Hello from the second package!
  Prepended 2nd time without trailing sep:Prepended without trailing sep
  Prepended 2nd time with sep:Prepended with trailing sep
  Appended without leading sep:Appended 2nd time without leading sep
  Appended with leading sep:Appended 2nd time with leading sep


