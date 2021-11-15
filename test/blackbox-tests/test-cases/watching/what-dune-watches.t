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

The below pattern excludes absolute files as Dune currently watches
everything in the PATH, which is not very reproducible.

  $ sed -nE 's/inotify_add_watch\([0-9]*, "([^/].*)", .*\) (=.*)/watch \1 \2/p' ../log
  watch _build/.sync = 1
  watch dune-workspace = -1 ENOENT (No such file or directory)
  watch . = 2
  watch dune-workspace = -1 ENOENT (No such file or directory)
  watch . = 2
  watch src = 3
  watch src/dune = 4
  watch src/a.ml = 15
  watch src/b.ml = 16
