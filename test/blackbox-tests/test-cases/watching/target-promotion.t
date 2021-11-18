Test target promotion in file-watching mode.

  $ . ./helpers.sh

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >   (mode promote)
  >   (deps original)
  >   (target promoted)
  >   (action (copy %{deps} %{target})))
  > (rule
  >   (deps promoted)
  >   (target result)
  >   (action (bash "cat promoted promoted > result")))
  > EOF
  $ echo hi > original

  $ start_dune

  $ build result
  Success
  $ cat promoted
  hi
  $ cat _build/default/result
  hi
  hi

Now change the [original] and rebuild.

  $ echo bye > original
  $ build result
  Success
  $ cat promoted
  bye
  $ cat _build/default/result
  bye
  bye

Now try deleting the promoted file.

  $ rm promoted
  $ build result
  Success
  $ cat promoted
  bye
  $ cat _build/default/result
  bye
  bye

Now try replacing its content.

  $ echo hi > promoted
  $ build result
  Success
  $ cat promoted
  bye
  $ cat _build/default/result
  bye
  bye

Now switch the mode to standard. Dune reports an error about multiple rules for
[_build/default/promoted], as expected (see the error at the end of the test).

  $ cat > dune <<EOF
  > (rule
  >   (mode standard)
  >   (deps original)
  >   (target promoted)
  >   (action (copy %{deps} %{target})))
  > (rule
  >   (deps promoted)
  >   (target result)
  >   (action (bash "cat promoted promoted > result")))
  > EOF

  $ build result
  Failure

We use the hint and it starts to work.

  $ rm -f promoted
  $ build result
  Success
  $ cat promoted
  cat: promoted: No such file or directory
  [1]
  $ cat _build/default/promoted
  bye
  $ cat _build/default/result
  bye
  bye

Now use [fallback] to override the rule that generates [promoted].

  $ cat > dune <<EOF
  > (rule
  >   (mode fallback)
  >   (deps original)
  >   (target promoted)
  >   (action (copy %{deps} %{target})))
  > (rule
  >   (deps promoted)
  >   (target result)
  >   (action (bash "cat promoted promoted > result")))
  > EOF

At first, we don't have the source, so the rule is used.

  $ build result
  Success
  $ cat promoted
  cat: promoted: No such file or directory
  [1]
  $ cat _build/default/promoted
  bye
  $ cat _build/default/result
  bye
  bye

Now we create the source file and it overrides the rule.

  $ echo hi > promoted
  $ build result
  Success
  $ cat promoted
  hi
  $ cat _build/default/promoted
  hi
  $ cat _build/default/result
  hi
  hi

We're done.

  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Error: Multiple rules generated for _build/default/promoted:
  - file present in source tree
  - dune:1
  Hint: rm -f promoted
  Had errors, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...

Now test file-system events generated during target promotion.

  $ cat > dune <<EOF
  > (rule
  >   (mode promote)
  >   (deps original)
  >   (target promoted)
  >   (action (copy %{deps} %{target})))
  > (rule
  >   (deps promoted)
  >   (target result)
  >   (action (bash "cat promoted promoted > result")))
  > EOF

  $ start_dune --debug-cache=fs
  $ build result
  Success

  $ stop_dune > debug-output

Show that Dune ignores the initial "dune-workspace" events (injected by Dune).

  $ cat debug-output | grep dune-workspace
  Updating dir_contents cache for "dune-workspace": Skipped
  Updating path_digest cache for "dune-workspace": Skipped
  Updating path_stat cache for "dune-workspace": Skipped
  Updating path_exists cache for "dune-workspace": Updated { changed = false }

Show that Dune ignores "promoted" events. Events for ".#promoted.dune-temp" are
filtered out by Dune's file watcher and don't show up here.

  $ cat debug-output | grep promoted
  Updating dir_contents cache for "promoted": Skipped
  Updating path_digest cache for "promoted": Updated { changed = false }
  Updating path_stat cache for "promoted": Skipped
  Updating path_exists cache for "promoted": Skipped

Show how Dune processes events for the . directory.

# CR-someday amokhov: We see two events where [path_stat] changed but nothing
# else did: specifically, the directory still exists and its content is unchanged.
# We believe the [path_stat]'s changes are due to the [mtimes] field, which can
# change, for example, because of creation of a temporary file. We should remove
# [mtimes] from [Fs_cache.Reduced_stats] to avoid such unnecessary retriggering.

  $ cat debug-output | grep '"."'
  Updating dir_contents cache for ".": Updated { changed = false }
  Updating path_digest cache for ".": Skipped
  Updating path_stat cache for ".": Updated { changed = true }
  Updating path_exists cache for ".": Skipped
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating path_digest cache for ".": Skipped
  Updating path_stat cache for ".": Updated { changed = true }
  Updating path_exists cache for ".": Skipped
