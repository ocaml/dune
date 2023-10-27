We want to make sure our OPAM-repository in git support works well.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo 1.0 <<EOF
  > EOF
  $ cd mock-opam-repository
  $ git init --quiet
  $ git add -A
  $ git commit --quiet -m "foo 1.0"
  $ cd ..

We'll set up a project that uses (only this) this repository, so doesn't use
:standard:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (repository
  >  (name mock)
  >  (source "git+file://$PWD/mock-opam-repository"))
  > (context
  >  (default
  >   (name default)
  >   (repositories mock)))
  > EOF

We depend on the foo package

  $ cat > dune-project <<EOF
  > (lang dune 3.10)
  > 
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF

Locking should produce the newest package from the repo

  $ mkdir dune-cache
  $ XDG_CACHE_HOME=$PWD/dune-cache dune pkg lock
  Solution for dune.lock:
  - foo.1.0

Now let's assume a new version of foo is released.

  $ mkpkg foo 1.1 <<EOF
  > EOF
  $ cd mock-opam-repository
  $ git add -A
  $ git commit --quiet -m "foo 1.1 -> new version"
  $ cd ..

Locking should update the git repo in our cache folder and give us the newer
version in the lock file

  $ XDG_CACHE_HOME=$PWD/dune-cache dune pkg lock
  Solution for dune.lock:
  - foo.1.1

If the package selected has some additional files that are supposed to be
included for building, these should also be part of the lockfile.

So if we create an extra-file (in OPAM parlance) and attach it to foo.1.2 it
should also be included.

  $ FILES_NAME=hello.txt
  $ cat > $FILES_NAME <<EOF
  > Hello World
  > EOF
  $ FILES_CHECKSUM=e59ff97941044f85df5297e1c302d260
  $ mkpkg foo 1.2 <<EOF
  > EOF
  $ echo "extra-files: [\"$FILES_NAME\" \"md5=$FILES_CHECKSUM\"]" >> mock-opam-repository/packages/foo/foo.1.2/opam
  $ FILES_FOLDER=mock-opam-repository/packages/foo/foo.1.2/files/
  $ mkdir -p "$FILES_FOLDER"
  $ mv "$FILES_NAME" "$FILES_FOLDER/$FILES_NAME"
  $ cd mock-opam-repository
  $ git add -A
  $ git commit --quiet -m "foo 1.2 with files"
  $ cd ..

Locking should be successful and it should include the additional file

  $ XDG_CACHE_HOME=$PWD/dune-cache dune pkg lock
  Solution for dune.lock:
  - foo.1.2

  $ find dune.lock | sort
  dune.lock
  dune.lock/foo.files
  dune.lock/foo.files/hello.txt
  dune.lock/foo.pkg
  dune.lock/lock.dune

The extra-file should have the same content as the original file, we determine
that by hashing with the checksum that we expected in the OPAM file

  $ cmp -s dune.lock/foo.files/$FILES_NAME "$FILES_FOLDER/$FILES_NAME" && echo "The contents match"
  The contents match
