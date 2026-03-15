Tests for [Fs_memo] module.

  $ export DUNE_TRACE=cache

  $ test () {
  >   echo "------------------------------------------"
  >   before=$(cat _build/default/result 2>/dev/null)
  >   start_dune
  >   build . | grep -v Success
  >   between=$(cat _build/default/result)
  >   eval "$@"
  >   build . | grep -v Success
  >   stop_dune >> .#tmp
  >   after=$(cat _build/default/result)
  >   cat .#tmp
  >   echo "------------------------------------------"
  >   echo "result = '$before' -> '$between' -> '$after'"
  >   echo "------------------------------------------"
  >   dune trace cat | jq -c 'select(.name == "fs_update") | .args' | sort
  >   rm .#tmp
  > }

The action ignores the dependency [dep]. We use it to force rerunning the action
when necessary.

  $ echo "(lang dune 2.0)" > dune-project
  $ cat >dune <<EOF
  > (rule
  >  (alias default)
  >  (deps dep
  >   (glob_files file-?)
  >   (glob_files dir/file-?)
  >   (glob_files dir/subdir/file-?))
  >  (target result)
  >  (action (system "\| echo Executing rule...
  >                "\| echo %{deps}       |
  >                "\|   tr ' ' '\n'      |
  >                "\|   xargs -n 1       |
  >                "\|   grep -v dep      |
  >                "\|   xargs cat > result
  > )))
  > EOF

  $ echo -n 1 > file-1
  $ touch dep

Note that we receive two events for [file-2] because it's first created empty
and then the contents is written to it.

  $ test "echo -n 2 > file-2"
  ------------------------------------------
  Executing rule...
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '' -> '1' -> '12'
  ------------------------------------------
  {"cache_type":"dir_contents","path":".","result":"changed"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"dir_contents","path":"file-2","result":"skipped"}
  {"cache_type":"dir_contents","path":"file-2","result":"skipped"}
  {"cache_type":"file_digest","path":".","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"file-2","result":"skipped"}
  {"cache_type":"file_digest","path":"file-2","result":"skipped"}
  {"cache_type":"path_stat","path":".","result":"unchanged"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}
  {"cache_type":"path_stat","path":"file-2","result":"skipped"}
  {"cache_type":"path_stat","path":"file-2","result":"skipped"}

Verify that filesystem cache events are being traced:

  $ dune trace cat | jq -s '[ .[] | select(.cat == "cache" and .name == "fs_update") ] | length > 0'
  true

Note that Dune did not re-execute the rule because the set of files matching
the glob remains unchanged.

  $ test "mkdir dir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '12' -> '12' -> '12'
  ------------------------------------------
  {"cache_type":"dir_contents","path":".","result":"changed"}
  {"cache_type":"dir_contents","path":"dir","result":"skipped"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":".","result":"skipped"}
  {"cache_type":"file_digest","path":"dir","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":".","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

We create [dir/file-3] before running Dune, so we only observe a single
[file_digest] change event with the file watcher.

  $ echo -n '?' > dir/file-3
  $ test "echo -n 3 > dir/file-3"
  ------------------------------------------
  Executing rule...
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '12' -> '12?' -> '123'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dir/file-3","result":"skipped"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/file-3","result":"changed"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dir/file-3","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

Now, Dune similarly updates [file_digest] for [file-2].

  $ test "echo -n '*' > file-2"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '123' -> '123' -> '1*3'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"dir_contents","path":"file-2","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"file-2","result":"changed"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}
  {"cache_type":"path_stat","path":"file-2","result":"skipped"}

On deletion of a file, we receive events for the file and the parent directory.

  $ test "rm file-2"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '1*3' -> '1*3' -> '13'
  ------------------------------------------
  {"cache_type":"dir_contents","path":".","result":"changed"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"dir_contents","path":"file-2","result":"skipped"}
  {"cache_type":"file_digest","path":".","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"file-2","result":"changed"}
  {"cache_type":"path_stat","path":".","result":"unchanged"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}
  {"cache_type":"path_stat","path":"file-2","result":"skipped"}

Dune notices that [dir_contents] of both [dir] and [.] changed, and also that
[dir/file-3]'s digest changed (from a digest to the error about missing file).

  $ test "mv dir/file-3 ."
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------
  {"cache_type":"dir_contents","path":".","result":"changed"}
  {"cache_type":"dir_contents","path":"dir","result":"changed"}
  {"cache_type":"dir_contents","path":"dir/file-3","result":"skipped"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"dir_contents","path":"file-3","result":"skipped"}
  {"cache_type":"file_digest","path":".","result":"skipped"}
  {"cache_type":"file_digest","path":"dir","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/file-3","result":"changed"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"file-3","result":"skipped"}
  {"cache_type":"path_stat","path":".","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir/file-3","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}
  {"cache_type":"path_stat","path":"file-3","result":"skipped"}

  $ test "mkdir dir/subdir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dir","result":"changed"}
  {"cache_type":"dir_contents","path":"dir/subdir","result":"skipped"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dir","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/subdir","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dir","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir/subdir","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

Again, there are two events for [file-4]: for creation and modification.

  $ test "echo -n 4 > dir/subdir/file-4"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '134'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dir/subdir","result":"changed"}
  {"cache_type":"dir_contents","path":"dir/subdir/file-4","result":"skipped"}
  {"cache_type":"dir_contents","path":"dir/subdir/file-4","result":"skipped"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/subdir","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/subdir/file-4","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/subdir/file-4","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dir/subdir","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir/subdir/file-4","result":"skipped"}
  {"cache_type":"path_stat","path":"dir/subdir/file-4","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

Here we are getting duplicate events for directories [dir] and [dir/subdir]
because we watch each of them both directly and via their parents.

  $ test "mv dir/subdir ."
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '134' -> '134' -> '13'
  ------------------------------------------
  {"cache_type":"dir_contents","path":".","result":"changed"}
  {"cache_type":"dir_contents","path":"dir","result":"changed"}
  {"cache_type":"dir_contents","path":"dir","result":"unchanged"}
  {"cache_type":"dir_contents","path":"dir/subdir","result":"changed"}
  {"cache_type":"dir_contents","path":"dir/subdir","result":"unchanged"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"dir_contents","path":"subdir","result":"skipped"}
  {"cache_type":"file_digest","path":".","result":"skipped"}
  {"cache_type":"file_digest","path":"dir","result":"skipped"}
  {"cache_type":"file_digest","path":"dir","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/subdir","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/subdir","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"subdir","result":"skipped"}
  {"cache_type":"path_stat","path":".","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir/subdir","result":"changed"}
  {"cache_type":"path_stat","path":"dir/subdir","result":"unchanged"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}
  {"cache_type":"path_stat","path":"subdir","result":"skipped"}

This seems to be a bug: Dune doesn't notice that a directory's permission
changed and succeeds instead of failing.

  $ test "chmod -r subdir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

If we repeat the test, we finally see the failure.

# CR-someday amokhov: Fix this.

  $ test "echo How about now?"
  ------------------------------------------
  Failure
  How about now?
  Failure
  Error: inotify_add_watch(subdir): Permission denied
  Had 1 error, waiting for filesystem changes...
  Error: inotify_add_watch(subdir): Permission denied
  Had 1 error, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

Same problem in the other direction.

  $ test "chmod +r subdir"
  ------------------------------------------
  Failure
  Failure
  Error: inotify_add_watch(subdir): Permission denied
  Had 1 error, waiting for filesystem changes...
  Error: inotify_add_watch(subdir): Permission denied
  Had 1 error, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

  $ test "echo How about now?"
  ------------------------------------------
  How about now?
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

Same problem for files.

  $ test "chmod -r file-1"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

  $ test "chmod +r file-1"
  ------------------------------------------
  Failure
  Failure
  File "file-1", line 1, characters 0-0:
  Error: File unavailable: file-1
  open(file-1): Permission denied
  Had 1 error, waiting for filesystem changes...
  File "file-1", line 1, characters 0-0:
  Error: File unavailable: file-1
  open(file-1): Permission denied
  Had 1 error, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

Dune receives one event for [file-1] when moving it within the same directory,
and two events for [file-5]: for moving and for changing. There are two events
for [dir_contents] of [.] because moving a file is interpreted as deleting and
then creating a file.

  $ test "mv file-1 file-5; echo -n 5 > file-5"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '35'
  ------------------------------------------
  {"cache_type":"dir_contents","path":".","result":"changed"}
  {"cache_type":"dir_contents","path":".","result":"unchanged"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"dir_contents","path":"file-1","result":"skipped"}
  {"cache_type":"dir_contents","path":"file-5","result":"skipped"}
  {"cache_type":"dir_contents","path":"file-5","result":"skipped"}
  {"cache_type":"file_digest","path":".","result":"skipped"}
  {"cache_type":"file_digest","path":".","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"file-1","result":"changed"}
  {"cache_type":"file_digest","path":"file-5","result":"skipped"}
  {"cache_type":"file_digest","path":"file-5","result":"skipped"}
  {"cache_type":"path_stat","path":".","result":"unchanged"}
  {"cache_type":"path_stat","path":".","result":"unchanged"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}
  {"cache_type":"path_stat","path":"file-1","result":"skipped"}
  {"cache_type":"path_stat","path":"file-5","result":"skipped"}
  {"cache_type":"path_stat","path":"file-5","result":"skipped"}
