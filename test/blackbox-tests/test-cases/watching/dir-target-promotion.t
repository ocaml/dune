Test directory target promotion in file-watching mode.

  $ echo '(lang dune 3.0)' > dune-project
  $ mkdir test; cd test
  $ cat > dune-project <<EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF
  $ cat > dune <<EOF
  > (rule
  >  (mode promote)
  >  (deps src (sandbox always))
  >  (targets (dir d1))
  >  (action (system "mkdir -p d1/d2; cp src d1/a; cp src d1/b; cp src d1/d2/c")))
  > EOF

  $ echo -n "*" > src
  $ start_dune

  $ build d1
  Success
  $ cat d1/a d1/b d1/d2/c
  ***

Change [src] and rebuild.

  $ echo -n "+" > src
  $ build d1
  Success
  $ cat d1/a d1/b d1/d2/c
  +++

Remove a file and rebuild.

  $ rm d1/a
  $ build d1
  Success
  $ cat d1/a d1/b d1/d2/c
  +++

Remove a directory and rebuild.

  $ rm -rf d2
  $ build d1
  Success
  $ cat d1/a d1/b d1/d2/c
  +++

Modify a file and rebuild.

  $ echo -n "*" > d1/b
  $ cat d1/a d1/b d1/d2/c
  +*+
  $ build d1
  Success
  $ cat d1/a d1/b d1/d2/c
  +++

Replace a directory with a file and rebuild.

  $ rm -rf d1/d2
  $ touch d1/d2
  $ build d1
  Success
  $ cat d1/a d1/b d1/d2/c
  +++

Replace a file with a directory and rebuild.

  $ rm d1/b
  $ mkdir -p d1/b
  $ build d1
  Success
  $ cat d1/a d1/b d1/d2/c
  +++

Add some unexpected files and directories and check that Dune deletes them.

  $ mkdir -p d1/unexpected-dir-1
  $ mkdir -p d1/d2/unexpected-dir-2
  $ touch d1/unexpected-file-1
  $ touch d1/unexpected-dir-1/unexpected-file-2
  $ touch d1/d2/unexpected-file-3
  $ build d1
  Success
  $ cat d1/a d1/b d1/d2/c
  +++
  $ ls d1 | grep unexpected
  [1]
  $ ls d1/d2 | grep unexpected
  [1]

We're done.

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...

Now test file-system events generated during directory target promotion.

  $ rm -rf d1
  $ export DUNE_TRACE=cache
  $ start_dune
  $ build d1
  Success

  $ stop_dune > /dev/null

Show that Dune ignores the initial "dune-workspace" events (injected by Dune).

  $ dune trace cat | jq 'include "dune"; cacheEvent("dune-workspace")'
  {
    "cache_type": "dir_contents",
    "path": "dune-workspace",
    "result": "skipped"
  }
  {
    "cache_type": "file_digest",
    "path": "dune-workspace",
    "result": "skipped"
  }
  {
    "cache_type": "path_stat",
    "path": "dune-workspace",
    "result": "unchanged"
  }

Dune correctly notices that the contents of . changed because [d1] was created.

  $ dune trace cat | jq 'include "dune"; cacheEvent(".")'
  {
    "cache_type": "dir_contents",
    "path": ".",
    "result": "changed"
  }
  {
    "cache_type": "file_digest",
    "path": ".",
    "result": "skipped"
  }
  {
    "cache_type": "path_stat",
    "path": ".",
    "result": "unchanged"
  }


Here [path_stat] of [d1] changed, because it didn't exist before the build. Dune
later recomputed [path_stat] once again, when [d1/b] was modified, and the
result remained unchanged (because fields like [mtime] are ignored). The result
of [dir_contents] also remained unchanged because Dune fixed the listing of [d1]
by re-promoting the directory target.

  $ dune trace cat | jq 'include "dune"; cacheEvent("d1")'
  {
    "cache_type": "dir_contents",
    "path": "d1",
    "result": "unchanged"
  }
  {
    "cache_type": "file_digest",
    "path": "d1",
    "result": "skipped"
  }
  {
    "cache_type": "path_stat",
    "path": "d1",
    "result": "changed"
  }
  {
    "cache_type": "dir_contents",
    "path": "d1",
    "result": "unchanged"
  }
  {
    "cache_type": "file_digest",
    "path": "d1",
    "result": "skipped"
  }
  {
    "cache_type": "path_stat",
    "path": "d1",
    "result": "unchanged"
  }

Events below occurred because we replaced file [d1/b] with a directory. Dune
undid this change to bring the promoted directory target up to date, which
explains why [file_digest] remained unchanged.

  $ dune trace cat | jq 'include "dune"; cacheEvent("d1/b")'
  {
    "cache_type": "dir_contents",
    "path": "d1/b",
    "result": "skipped"
  }
  {
    "cache_type": "file_digest",
    "path": "d1/b",
    "result": "unchanged"
  }
  {
    "cache_type": "path_stat",
    "path": "d1/b",
    "result": "skipped"
  }
