Tests for [Fs_memo] permission change handling.

Dune doesn't notice permission changes because chmod doesn't trigger inotify
events. This is a known bug.

# CR-someday amokhov: Fix this.

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
  $ echo -n 3 > file-3
  $ touch dep
  $ mkdir dir
  $ mkdir subdir

Dune doesn't notice that a directory's permission changed and succeeds instead
of failing.

  $ test "chmod -r subdir"
  ------------------------------------------
  Executing rule...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '' -> '13' -> '13'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

If we repeat the test, we finally see the failure.

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
