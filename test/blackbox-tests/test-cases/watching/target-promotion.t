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
  $ cat _build/default/promoted
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

Promoting a source file must not suppress events that are needed to refresh a
source-copy target for the same source path.

  $ mkdir promote-and-source-copy
  $ cd promote-and-source-copy
  $ echo '(lang dune 3.23)' > dune-project
  $ mkdir sub
  $ printf old > data
  $ runs="$PWD/runs"
  $ cat > dune <<EOF
  > (rule
  >  (deps data)
  >  (target result)
  >  (action
  >   (bash "cat data > result; printf 'copy:%s\n' \"\$(cat data)\" >> $runs")))
  > (alias
  >  (name idle))
  > EOF
  $ cat > sub/dune <<EOF
  > (rule
  >  (mode (promote (into ..)))
  >  (target data)
  >  (action (bash "printf new > data; printf 'promote\n' >> $runs")))
  > EOF
  $ start_dune @idle
  $ build result
  Success
  $ cat _build/default/result
  old
  $ build sub/data
  Success
  $ with_timeout dune rpc flush-file-watcher --wait
  $ cat data
  new
  $ build result
  Success
  $ cat _build/default/result
  new
  $ cat runs
  copy:old
  copy:old
  promote
  copy:new
  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  $ cd ..

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
  $ cat _build/default/promoted
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
    "cache_type": "path_stat",
    "path": "dune-workspace",
    "result": "unchanged"
  }

Show that Dune ignores "promoted" events. Events for ".#promoted.dune-temp" are
filtered out by Dune's file watcher and don't show up here. The event for
[promoted] is more interesting: the file's content did change from "hi" to
"bye" but Dune subscribed to it *after* making the promotion, precisely to avoid
unnecessarily restarting after receiving the event that it caused itself.

  $ dune trace cat | jq 'include "dune"; fsUpdateWithPath("promoted")'

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
    "cache_type": "path_stat",
    "path": ".",
    "result": "unchanged"
  }

File-system events caused by promotion should not run the promotion rule twice.
Same-content events for promoted source files should not rerun dependent rules,
but real content changes should still be restored by promotion.

  $ mkdir promotion-same-content
  $ cd promotion-same-content
  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (mode promote)
  >  (deps original (sandbox none))
  >  (target promoted)
  >  (action
  >   (bash "cp original promoted; printf 'promote\n' >> ../runs")))
  > (rule
  >  (deps promoted (sandbox none))
  >  (target result)
  >  (action
  >   (bash "cat promoted > result; printf 'result\n' >> ../runs")))
  > (alias
  >  (name idle))
  > EOF
  $ echo one > original
  $ echo stale > promoted
  $ start_dune @idle
  $ build result
  Success
  $ cat _build/runs
  promote
  result
  $ touch promoted
  $ build result
  Success
  $ cat _build/runs
  promote
  result
  $ echo changed > promoted
  $ build result
  Success
  $ cat promoted
  one
  $ cat _build/runs
  promote
  result
  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...

Promotion up-to-date checks must include the executable bit, not just bytes.

  $ cd ..
  $ mkdir promotion-executable-bit
  $ cd promotion-executable-bit
  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >  (mode promote)
  >  (deps original (sandbox none))
  >  (target promoted)
  >  (action
  >   (bash "cp original promoted; chmod 655 promoted; printf 'promote\n' >> ../runs")))
  > (rule
  >  (deps promoted (sandbox none))
  >  (target result)
  >  (action
  >   (bash "dune_cmd stat permissions promoted > result; cat result >> ../runs")))
  > (alias
  >  (name idle))
  > EOF
  $ echo one > original
  $ echo one > promoted
  $ chmod 644 promoted
  $ start_dune @idle
  $ build result
  Success
  $ dune_cmd stat permissions promoted
  655
  $ cat _build/default/result
  455
  $ cat _build/runs
  promote
  455
  $ chmod 644 promoted
  $ touch promoted
  $ build result
  Success
  $ dune_cmd stat permissions promoted
  655
  $ cat _build/default/result
  455
  $ cat _build/runs
  promote
  455
  $ stop_dune
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
