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

Now replace the content and switch the mode to standard. Note that this case is
currently not handled correctly: instead of succeeding, we should report a rule
conflict, as we do in the batch build mode -- see [dep-on-promoted-target.t].

# CR-someday amokhov: Fix this test.

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
  $ echo hi > promoted

  $ build result
  Success
  $ cat promoted
  hi
  $ cat _build/default/promoted
  bye
  $ cat _build/default/result
  bye
  bye

We're done.

  $ stop_dune
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...
  waiting for inotify sync
  waited for inotify sync
  Success, waiting for filesystem changes...
