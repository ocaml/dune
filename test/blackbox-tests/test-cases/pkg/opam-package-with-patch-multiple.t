We test the conversion and build of the opam file patch field with multiple entries with
patch files that patch multiple files.

  $ . ./helpers.sh
  $ mkrepo

Make a package with two patches, one inside a directory. The first patch patches a single
file and the second patches two, one of the files is in a subdirectory.:w

  $ mkpkg with-patch <<EOF
  > patches: ["foo.patch" "dir/bar.patch"]
  > build: ["cat" "foo.ml" "bar.ml" "dir/baz.ml"]
  > EOF

  $ opam_dir=$mock_packages/with-patch/with-patch.0.0.1

  $ mkdir -p $opam_dir/files/dir

  $ cat >$opam_dir/files/foo.patch <<EOF
  > diff --git a/foo.ml b/foo.ml
  > index b69a69a5a..ea988f6bd 100644
  > --- a/foo.ml
  > +++ b/foo.ml
  > @@ -1,1 +1,1 @@
  > -This is wrong
  > +This is right
  > EOF

  $ cat >$opam_dir/files/dir/bar.patch <<EOF
  > diff --git a/bar.ml b/bar.ml
  > index b69a69a5a..ea988f6bd 100644
  > --- a/bar.ml
  > +++ b/bar.ml
  > @@ -1,1 +1,1 @@
  > -This is wrong
  > +This is right
  > 
  > diff --git a/dir/baz.ml b/dir/baz.ml
  > new file mode 100644
  > index b69a69a5a..ea988f6bd 100644
  > --- a/dir/baz.ml
  > +++ b/dir/baz.ml
  > @@ -1,1 +1,1 @@
  > -This is wrong
  > +This is right
  > EOF

  $ solve with-patch
  Solution for dune.lock:
  - with-patch.0.0.1
  $ cat >>dune.lock/with-patch.pkg <<EOF
  > (source (copy $PWD/source))
  > EOF

Checking that the patch files have been copied to the dune.lock dir

  $ [ -d dune.lock/with-patch.files ] && ls dune.lock/with-patch.files/foo.patch
  dune.lock/with-patch.files/foo.patch
  $ [ -d dune.lock/with-patch.files/dir ] && ls dune.lock/with-patch.files/dir/bar.patch
  dune.lock/with-patch.files/dir/bar.patch

The lockfile should contain the patch action. 
  $ cat dune.lock/with-patch.pkg 
  (version 0.0.1)
  
  (build
   (progn
    (patch foo.patch)
    (patch dir/bar.patch)
    (run cat foo.ml bar.ml dir/baz.ml)))
  (source (copy $TESTCASE_ROOT/source))

  $ mkdir -p source/dir
  $ cat > source/foo.ml <<EOF
  > This is wrong
  > EOF
  $ cat > source/bar.ml <<EOF
  > This is wrong
  > EOF
  $ cat > source/dir/baz.ml <<EOF
  > This is wrong
  > EOF

The build step of the opam file correctly cats the patched files.

  $ build_pkg with-patch 
  This is right
  This is right
  This is right
