Show what Dune watches via inotify

  $ mkdir test
  $ cd test

  $ echo '(lang dune 3.0)' > dune-project
  $ mkdir src
  $ echo '(library (name foo))' > src/dune
  $ touch src/a.ml
  $ touch src/b.ml

  $ strace -o ../log -e inotify_add_watch dune build --passive-watch-mode &
  Success, waiting for filesystem changes...

  $ dune rpc build --wait .
  Success
  $ dune shutdown
  $ wait

The pattern below excludes absolute files as Dune currently watches
everything in the PATH, which is not very reproducible.

  $ sed -nE 's/inotify_add_watch\([0-9]*, "([^/].*)", .*\) (=.*)/watch \1 \2/p' ../log | sort -u
  watch . = 2
  watch _build/.sync = 1
  watch src = 3
