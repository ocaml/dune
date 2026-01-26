Test target promotion in file-watching mode.

  $ export DUNE_TRACE="cache"

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (mode promote)
  >  (deps original)
  >  (target promoted)
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (deps promoted)
  >  (target result)
  >  (action (system "cat promoted promoted > result")))
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
  >  (mode standard)
  >  (deps original)
  >  (target promoted)
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (deps promoted)
  >  (target result)
  >  (action (system "cat promoted promoted > result")))
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
  >  (mode fallback)
  >  (deps original)
  >  (target promoted)
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (deps promoted)
  >  (target result)
  >  (action (system "cat promoted promoted > result")))
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
  - dune:1
  - file present in source tree
  Hint: rm -f promoted
  Had 1 error, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...

Now test file-system events generated during target promotion.

  $ cat > dune <<EOF
  > (rule
  >  (mode promote)
  >  (deps original)
  >  (target promoted)
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (deps promoted)
  >  (target result)
  >  (action (system "cat promoted promoted > result")))
  > EOF

  $ cat promoted
  hi
  $ start_dune
  $ build result
  Success
  $ cat promoted
  bye

  $ stop_dune > /dev/null

Show that Dune ignores the initial "dune-workspace" events (injected by Dune).

  $ dune trace cat | jq 'include "dune"; fsUpdateWithPath("dune-workspace")'
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

Show that Dune ignores "promoted" events. Events for ".#promoted.dune-temp" are
filtered out by Dune's file watcher and don't show up here. The [path_digest]
event for [promoted] is more interesting: the file's content did change from "hi"
to "bye" but Dune subscribed to it *after* making the promotion, precisely to
avoid unnecessarily restarting after receiving the event that it caused itself.

  $ dune trace cat | jq 'include "dune"; fsUpdateWithPath("promoted")'
  {
    "cache_type": "dir_contents",
    "path": "promoted",
    "result": "skipped"
  }
  {
    "cache_type": "file_digest",
    "path": "promoted",
    "result": "unchanged"
  }
  {
    "cache_type": "path_stat",
    "path": "promoted",
    "result": "skipped"
  }

Show that Dune ignores events for the . directory: [dir_contents] didn't change
because [promoted] existed before running the build. Also, the subset of fields
of [path_stat] that matter to Dune didn't change either (the [mtime] field did
change but [fs_memo] does not provide a way to subscribe to it).

  $ dune trace cat | jq 'include "dune"; fsUpdateWithPath(".")'
  {
    "cache_type": "dir_contents",
    "path": ".",
    "result": "unchanged"
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
