We test how opam files with substs fields together with patches fields are translated into
the dune.lock file. Opam allows substitution to happen before the patches phase, so we
must do the same.
 
  $ . ./helpers.sh
  $ mkrepo

Make a package with a substs and patches field field 
  $ mkpkg with-substs-and-patches <<EOF
  > substs: ["foo.patch"]
  > patches: ["foo.patch"]
  > build: [ "sh" "-c" "[ -e foo.ml ] && cat foo.ml" ]
  > EOF

  $ opam_repo=$mock_packages/with-substs-and-patches/with-substs-and-patches.0.0.1

  $ solve with-substs-and-patches
  Solution for dune.lock:
  - with-substs-and-patches.0.0.1
  $ cat >>dune.lock/with-substs-and-patches.pkg <<EOF
  > (source (copy $PWD/source))
  > EOF

The lockfile should contain the substitute and patch actions.

  $ cat dune.lock/with-substs-and-patches.pkg 
  (version 0.0.1)
  
  (build
   (progn
    (substitute foo.patch.in foo.patch)
    (patch foo.patch)
    (run sh -c "[ -e foo.ml ] && cat foo.ml")))
  (source (copy $TESTCASE_ROOT/source))

  $ mkdir source

  $ cat > source/foo.patch.in <<EOF
  > diff --git a/foo.ml b/foo.ml
  > index b69a69a5a..ea988f6bd 100644
  > --- a/foo.ml
  > +++ b/foo.ml
  > @@ -1,1 +1,1 @@
  > -This is wrong
  > +This is right
  > EOF

  $ cat > source/foo.ml <<EOF
  > This is wrong
  > EOF

The file foo.ml should have been built:

  $ build_pkg with-substs-and-patches
  This is right
