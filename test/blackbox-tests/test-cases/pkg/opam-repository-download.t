Helper shell function that generates an opam file for a package:

  $ . ./helpers.sh
  $ mkrepo

Make a mock repo tarball that will get used by dune to download the package

  $ mkpkg foo <<EOF
  > EOF
  $ mkpkg bar <<EOF
  > depends: [ "foo" ]
  > EOF
  $ cd mock-opam-repository
  $ git init --quiet
  $ git add -A
  $ git commit -m "Initial commit" --quiet
  $ REPO_HASH=$(git rev-parse HEAD)
  $ cd ..
  $ mkdir fake-xdg-cache

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (generate_opam_files true)
  > 
  > (package
  >   (name baz)
  >   (depends bar))
  > EOF

  $ XDG_CACHE_HOME=$(pwd)/fake-xdg-cache dune pkg lock --opam-repository-url=git+file://$(pwd)/mock-opam-repository
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1

Our custom cache folder should be populated with the unpacked tarball
containing the repository:

  $ find fake-xdg-cache | grep HEAD | sort
  fake-xdg-cache/dune/git-repo/FETCH_HEAD
  fake-xdg-cache/dune/git-repo/HEAD

Automatically download the repo and make sure lock.dune contains the repo hash
(exit code 0 from grep, we don't care about the output as it is not
reproducible)

  $ grep "git_hash $REPO_HASH" dune.lock/lock.dune > /dev/null

Now try it with an a path, given it is not a URL, it can't be reproduced on
other systems and thus shouldn't be included.

  $ rm -r dune.lock
  $ dune pkg lock --opam-repository-path=$(pwd)/mock-opam-repository
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1

  $ grep "git_hash $REPO_HASH" dune.lock/lock.dune > /dev/null || echo "not found"
  not found


The repository can also be injected via the dune-workspace file

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (repository
  >  (name foo)
  >  (source "git+file://$(pwd)/mock-opam-repository"))
  > (context
  >  (default
  >   (name default)
  >   (repositories foo)))
  > EOF
  $ mkdir dune-workspace-cache
  $ XDG_CACHE_HOME=$(pwd)/dune-workspace-cache dune pkg lock
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1

  $ grep "git_hash $REPO_HASH" dune.lock/lock.dune > /dev/null

A new package is released in the repo:

  $ mkpkg bar 1.0.0 <<EOF
  > depends: [ "foo" ]
  > EOF
  $ cd mock-opam-repository
  $ git add -A
  $ git commit -m "bar.0.1.0" --quiet
  $ cd ..

Since we have a working cached copy we get the old version of `bar` if we opt
out of the auto update.

To be safe it doesn't access the repo, we make sure to move the mock-repo away

  $ mv mock-opam-repository elsewhere

So now the test should work as it can't access the repo:

  $ rm -r dune.lock
  $ XDG_CACHE_HOME=$(pwd)/dune-workspace-cache dune pkg lock --skip-update
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1

But it will also get the new version of bar if we attempt to lock again (having
restored the repo to where it was before)

  $ mv elsewhere mock-opam-repository
  $ rm -r dune.lock
  $ XDG_CACHE_HOME=$(pwd)/dune-workspace-cache dune pkg lock
  Solution for dune.lock:
  - bar.1.0.0
  - foo.0.0.1
