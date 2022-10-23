Test directory target promotion in file-watching mode.

  $ . ./helpers.sh

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
  >  (action (bash "mkdir -p d1/d2; cp src d1/a; cp src d1/b; cp src d1/d2/c")))
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
  $ start_dune --debug-cache=fs
  $ build d1
  Success

  $ stop_dune > .#debug-output

Show that Dune ignores the initial "dune-workspace" events (injected by Dune).

  $ cat .#debug-output | grep dune-workspace
  Updating dir_contents cache for "dune-workspace": Skipped
  Updating file_digest cache for "dune-workspace": Skipped
  Updating path_stat cache for "dune-workspace": Updated { changed = false }

Dune correctly notices that the contents of . changed because [d1] was created.

  $ cat .#debug-output | grep '"."'
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating file_digest cache for ".": Skipped
  Updating path_stat cache for ".": Updated { changed = false }


Here [path_stat] of [d1] changed, because it didn't exist before the build. Dune
later recomputed [path_stat] once again, when [d1/b] was modified, and the
result remained unchanged (because fields like [mtime] are ignored). The result
of [dir_contents] also remained unchanged because Dune fixed the listing of [d1]
by re-promoting the directory target.

  $ cat .#debug-output | grep \"d1\"
  Updating dir_contents cache for "d1": Updated { changed = false }
  Updating file_digest cache for "d1": Skipped
  Updating path_stat cache for "d1": Updated { changed = true }
  Updating dir_contents cache for "d1": Updated { changed = false }
  Updating file_digest cache for "d1": Skipped
  Updating path_stat cache for "d1": Updated { changed = false }

Events below occurred because we replaced file [d1/b] with a directory. Dune
undid this change to bring the promoted directory target up to date, which
explains why [file_digest] remained unchanged.

  $ cat .#debug-output | grep d1/b
  Updating dir_contents cache for "d1/b": Skipped
  Updating file_digest cache for "d1/b": Updated { changed = false }
  Updating path_stat cache for "d1/b": Skipped
