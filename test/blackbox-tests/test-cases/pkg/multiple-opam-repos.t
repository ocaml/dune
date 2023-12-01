We want to test that support for multiple opam repositories works.

  $ . ./helpers.sh
  $ mkrepo
  $ mkpkg foo 1.0 <<EOF
  > EOF
  $ cd mock-opam-repository
  $ git init --quiet
  $ git add -A
  $ git commit --quiet -m "Initial commit"
  $ cd ..

We move this mock repo to a different place, so we have two mock repos:

  $ mv mock-opam-repository old-mock-opam-repository

Create a new mock repo, with a different foo package

  $ mkrepo
  $ mkpkg foo 2.0 <<EOF
  > EOF
  $ cd mock-opam-repository
  $ git init --quiet
  $ git add -A
  $ git commit --quiet -m "Initial commit"
  $ cd ..

We have to define both repositories in the workspace, but will only use `new`.

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (repositories new))
  > (repository
  >  (name new)
  >  (source "git+file://$(pwd)/mock-opam-repository"))
  > (repository
  >  (name old)
  >  (source "git+file://$(pwd)/old-mock-opam-repository"))
  > EOF

  $ cat > dune-project <<EOF
  > (lang dune 3.10)
  > 
  > (package
  >  (name bar)
  >  (depends foo))
  > EOF
  $ cat > dune <<EOF
  > EOF

Locking should produce the newest package from `new`

  $ mkdir dune-workspace-cache
  $ XDG_CACHE_HOME=$(pwd)/dune-workspace-cache dune pkg lock
  Solution for dune.lock:
  - foo.2.0

If we just use `old` we should get the older `foo` package in our lockfile
solution:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (repositories old))
  > (repository
  >  (name new)
  >  (source "git+file://$(pwd)/mock-opam-repository"))
  > (repository
  >  (name old)
  >  (source "git+file://$(pwd)/old-mock-opam-repository"))
  > (context
  >  (default
  >   (name default)))
  > EOF
 
  $ rm -r dune-workspace-cache && mkdir dune-workspace-cache
  $ XDG_CACHE_HOME=$(pwd)/dune-workspace-cache dune pkg lock
  Solution for dune.lock:
  - foo.1.0

If we specify both repositories to be used, we should still get the new foo
package:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (lock_dir
  >  (repositories old new))
  > (repository
  >  (name new)
  >  (source "git+file://$(pwd)/mock-opam-repository"))
  > (repository
  >  (name old)
  >  (source "git+file://$(pwd)/old-mock-opam-repository"))
  > (context
  >  (default
  >   (name default)))
  > EOF

  $ rm -r dune-workspace-cache && mkdir dune-workspace-cache
  $ XDG_CACHE_HOME=$(pwd)/dune-workspace-cache dune pkg lock
  Solution for dune.lock:
  - foo.2.0

If we use the ordered set language format and try to exclude `new` from the
set, we should get a solution that only has `old` and will thus include the
older version of foo:

  $ cat > dune-workspace <<EOF
  > (lang dune 3.10)
  > (repository
  >  (name new)
  >  (source "git+file://$(pwd)/mock-opam-repository"))
  > (repository
  >  (name old)
  >  (source "git+file://$(pwd)/old-mock-opam-repository"))
  > (lock_dir
  >  (repositories new old \ new))
  > (context
  >  (default
  >   (name default)))
  > EOF

  $ rm -r dune-workspace-cache && mkdir dune-workspace-cache
  $ XDG_CACHE_HOME=$(pwd)/dune-workspace-cache dune pkg lock
  Solution for dune.lock:
  - foo.1.0
