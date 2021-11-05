Modify an input file during the build so that Dune interrupts the build once.

  $ . ./helpers.sh

Bad rule! You are not supposed to modify the source tree. No ice-cream for you!

  $ echo '(lang dune 3.0)' > dune-project
  $ cat > dune <<EOF
  > (rule
  >   (deps (glob_files *.txt) (sandbox none))
  >   (target result)
  >   (action (bash "echo %{deps}; echo hi > result; echo oops > ../../new-source.txt")))
  > EOF

  $ touch old-source.txt
  $ start_dune --debug-cache=fs

This failure is not really a failure but is merely a re-triggering of the build
because a new source file has been created. This is confusing: we should really
send "Interrupted" or something like this via Dune RPC in this case.

# CR-someday amokhov: Change Dune RPC to send "Interrupted" instead of "Failure"
# when the current build is re-triggered.

  $ build result
  Failure

  $ cat new-source.txt
  oops

So, we try again and it works.

  $ build result
  Success

  $ cat _build/default/new-source.txt
  oops

Some notes on the debugging log below:

* The build is re-triggered because dir_contents for "." changed.

* The first time the rule is executed, it prints "old-source.txt", which is the
| only source file it observes.

* The second time it prints "new-source.txt old-source.txt" because Dune copied
| the new file to the build directory.

* The file watcher notices the new event for new-source.txt but it is ignored
| because the contents ("oops") is the same.

  $ stop_dune
  Updating dir_contents cache for "dune-workspace": Skipped
  Updating path_digest cache for "dune-workspace": Skipped
  Updating path_stat cache for "dune-workspace": Skipped
  Updating path_exists cache for "dune-workspace": Updated { changed = false }
  waiting for inotify sync
  waited for inotify sync
  Updating dir_contents cache for "new-source.txt": Skipped
  Updating path_digest cache for "new-source.txt": Skipped
  Updating path_stat cache for "new-source.txt": Skipped
  Updating path_exists cache for "new-source.txt": Skipped
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating path_digest cache for ".": Skipped
  Updating path_stat cache for ".": Updated { changed = true }
  Updating path_exists cache for ".": Skipped
  Updating dir_contents cache for "new-source.txt": Skipped
  Updating path_digest cache for "new-source.txt": Skipped
  Updating path_stat cache for "new-source.txt": Skipped
  Updating path_exists cache for "new-source.txt": Skipped
  old-source.txt
  waiting for inotify sync
  waited for inotify sync
  Updating dir_contents cache for "new-source.txt": Skipped
  Updating path_digest cache for "new-source.txt": Updated { changed = false }
  Updating path_stat cache for "new-source.txt": Skipped
  Updating path_exists cache for "new-source.txt": Skipped
  Updating dir_contents cache for "new-source.txt": Skipped
  Updating path_digest cache for "new-source.txt": Updated { changed = false }
  Updating path_stat cache for "new-source.txt": Skipped
  Updating path_exists cache for "new-source.txt": Skipped
  new-source.txt old-source.txt
  Success, waiting for filesystem changes...
