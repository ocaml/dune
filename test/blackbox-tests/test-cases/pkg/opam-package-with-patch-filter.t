We test the translation of an opam package with a patches field with a filter into a dune
lock file.

  $ . ./helpers.sh
  $ mkrepo

Make a package with a patch behind a filter
  $ mkpkg with-patch-filter <<EOF
  > opam-version: "2.0"
  > patches: ["foo.patch" {with-test}]
  > build: ["cat" "foo.ml"]
  > EOF

  $ mkdir -p $mock_packages/with-patch-filter/with-patch-filter.0.0.1/files
  $ cat >$mock_packages/with-patch-filter/with-patch-filter.0.0.1/files/foo.patch <<EOF
  > diff --git a/foo.ml b/foo.ml
  > index b69a69a5a..ea988f6bd 100644
  > --- a/foo.ml
  > +++ b/foo.ml
  > @@ -1,2 +1,2 @@
  > -This is right; the patch should never be applied.
  > +This is wrong; this patch should have been filtered out.
  > EOF

  $ solve_project <<EOF
  > (lang dune 3.8)
  > (package
  >  (name x)
  >  (allow_empty)
  >  (depends with-patch-filter))
  > EOF
  Solution for dune.lock:
  with-patch-filter.0.0.1
  
  $ cat >>dune.lock/with-patch-filter.pkg <<EOF
  > (source (copy $PWD/source))
  > EOF

The lockfile should contain the patch action with the appropriate filter. 

  $ cat dune.lock/with-patch-filter.pkg 
  (version 0.0.1)
  
  (build
   (progn
    (patch foo.patch)
    (run cat foo.ml)))
  (source (copy $TESTCASE_ROOT/source))

  $ mkdir source
  $ cat > source/foo.ml <<EOF
  > This is right; the patch should never be applied.
  > EOF

  $ build_pkg with-patch-filter 
  This is wrong; this patch should have been filtered out.
