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

  $ cat > dune-project <<EOF
  > (lang dune 3.10)
  > (generate_opam_files true)
  > 
  > (package
  >   (name baz)
  >   (depends bar))
  > EOF
  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "git+file://$(pwd)/mock-opam-repository"))
  > EOF

  $ dune pkg lock
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1

Our cache folder should be populated with a revision store:

  $ find $XDG_CACHE_HOME | grep HEAD | sort
  $TESTCASE_ROOT/.cache/dune/git-repo/FETCH_HEAD
  $TESTCASE_ROOT/.cache/dune/git-repo/HEAD

Make sure lock.dune contains the repo hash:

  $ grep "git_hash $REPO_HASH" dune.lock/lock.dune > /dev/null

Now try it with an a path. Given it is not a git URL, it can't be reproduced on
other systems and thus shouldn't be included.

  $ rm -r dune.lock
  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "file://$(pwd)/mock-opam-repository"))
  > EOF
  $ dune pkg lock
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1

  $ grep "git_hash $REPO_HASH" dune.lock/lock.dune > /dev/null || echo "not found"
  not found

We also test that it is possible to specify a specific commit when locking a
repo, in this case the one from an older revision. So we create a new package
in the repo and make sure it locks the older version.

  $ mkpkg foo 0.1.0 <<EOF
  > EOF
  $ cd mock-opam-repository
  $ git add -A
  $ git commit -m "new release of foo" --quiet
  $ NEW_REPO_HASH=$(git rev-parse HEAD)
  $ cd ..

  $ rm -r dune.lock
  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (repositories mock))
  > (repository
  >  (name mock)
  >  (source "git+file://$(pwd)/mock-opam-repository#${REPO_HASH}"))
  > EOF
  $ dune pkg lock
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.0.1
  $ grep "git_hash $REPO_HASH" dune.lock/lock.dune > /dev/null

If we specify no branch however, it should be using the latest commit in the
repository and thus the new foo package.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (repository
  >  (name foo)
  >  (source "git+file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories foo))
  > EOF
  $ dune pkg lock
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.1.0
  $ grep "git_hash $NEW_REPO_HASH" dune.lock/lock.dune > /dev/null

A new package is released in the repo:

  $ mkpkg bar 1.0.0 <<EOF
  > depends: [ "foo" ]
  > EOF
  $ cd mock-opam-repository
  $ git add -A
  $ git commit -m "bar.1.0.0" --quiet
  $ cd ..

Since we have a working cached copy we get the old version of `bar` if we opt
out of the auto update.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (repository
  >  (name mock)
  >  (source "git+file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock))
  > EOF

To be safe it doesn't access the repo, we make sure to move the mock-repo away

  $ mv mock-opam-repository elsewhere

So now the test should work as it can't access the repo:

  $ rm -r dune.lock
  $ dune pkg lock --skip-update
  Solution for dune.lock:
  - bar.0.0.1
  - foo.0.1.0

But it will also get the new version of bar if we attempt to lock again (having
restored the repo to where it was before)

  $ mv elsewhere mock-opam-repository
  $ rm -r dune.lock
  $ dune pkg lock
  Solution for dune.lock:
  - bar.1.0.0
  - foo.0.1.0

We also want to make sure that branches work, so we add `bar.2.0.0` as a new
package on a `bar-2` branch (and switch back to the default branch, to make
sure that the default branch differs from `bar-2`).

  $ mkpkg bar 2.0.0 <<EOF
  > depends: [ "foo" ]
  > EOF
  $ cd mock-opam-repository
  $ git switch --quiet -c bar-2
  $ git add -A
  $ git commit -m "bar.2.0.0" --quiet
  $ git switch --quiet -
  $ cd ..

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (repository
  >  (name mock)
  >  (source "git+file://$(pwd)/mock-opam-repository#bar-2"))
  > (lock_dir
  >  (repositories mock))
  > EOF

Locking that branch should work and pick `bar.2.0.0`:

  $ rm -r dune.lock
  $ dune pkg lock
  Solution for dune.lock:
  - bar.2.0.0
  - foo.0.1.0

We want to make sure tagging a specific tag works as well, so we tag the
current state as `1.0`, add a new package (that `1.0` does not include) on top
of the main branch.

  $ mkpkg bar 3.0.0 <<EOF
  > depends: [ "foo" ]
  > EOF
  $ cd mock-opam-repository
  $ git tag 1.0
  $ git add -A
  $ git commit -m "bar.3.0.0" --quiet
  $ cd ..

The repo should be using the `1.0` tag, as we don't want `bar.3.0.0`.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (repository
  >  (name mock)
  >  (source "git+file://$(pwd)/mock-opam-repository#1.0"))
  > (lock_dir
  >  (repositories mock))
  > EOF

So we should get `bar.1.0.0` when locking.

  $ rm -r dune.lock
  $ dune pkg lock
  Solution for dune.lock:
  - bar.1.0.0
  - foo.0.1.0

We want to make sure locking works even with submodules:

  $ rm -r mock-opam-repository
  $ mkrepo
  $ mkpkg foo <<EOF
  > EOF
  $ mkpkg bar <<EOF
  > depends: [ "foo" ]
  > EOF

We move `packages` into a different folder

  $ mkdir remote-submodule
  $ mv mock-opam-repository/packages/* remote-submodule
  $ rm -r mock-opam-repository/packages

And create it as a separate repo, this time with the packages in the root:

  $ cd remote-submodule
  $ git init --quiet
  $ git add -A
  $ git commit -m "Initial subrepo commit" --quiet
  $ cd ..

In our mock repository, we make sure to add the submodule as `packages`:

  $ cd mock-opam-repository
  $ git init --quiet
  $ GIT_ALLOW_PROTOCOL=file git submodule add ../remote-submodule packages
  Cloning into '$TESTCASE_ROOT/mock-opam-repository/packages'...
  done.
  $ git commit -m "Initial opam-repo commit" --quiet
  $ git submodule init
  $ cd ..

We'll use the mock repository as source:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (repository
  >  (name mock)
  >  (source "git+file://$(pwd)/mock-opam-repository"))
  > (lock_dir
  >  (repositories mock))
  > EOF

We should be able to successfully solve the project with `foo` and `bar`,
however due to some issues this currently fails:

  $ rm -r dune.lock
  $ dune pkg lock
  Error: Unable to solve dependencies for dune.lock:
  Can't find all required versions.
  Selected: baz.dev
  - bar -> (problem)
      No known implementations at all
  [1]
