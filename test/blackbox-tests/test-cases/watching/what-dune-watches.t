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

Since the pattern above is not reproducible, uncomment the next lines
to see what it is really doing
#  $ sed -nE 's/inotify_add_watch\([0-9]*, "(.*)", .*\) (=.*)/watch \1 \2/p' ../log | sort -u -k4n -k2 | sed "s,$OPAM_SWITCH_PREFIX,OPAM_SWITCH_PREFIX," | sed "s,$DUNE_SOURCEROOT,DUNE_SOURCEROOT,"
#  watch _build/.sync = 1
#  watch . = 2
#  watch src = 3
#  watch DUNE_SOURCEROOT/_build/default/test/blackbox-tests/test-cases/.bin = 4
#  watch DUNE_SOURCEROOT/_build/install/default/bin = 5
#  watch OPAM_SWITCH_PREFIX/bin = 6
#  watch OPAM_SWITCH_PREFIX/lib/ocaml = 7
