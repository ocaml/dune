Tests for [Fs_memo] permission change handling.

Dune doesn't notice permission changes because chmod doesn't trigger inotify
events. This is a known bug.

# CR-someday amokhov: Fix this.

  $ export DUNE_TRACE=cache
  $ setup_xdg_runtime_dir

  $ fs_memo_test () {
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
  >   dune trace cat | jq -c 'select(.name == "fs_update") | .args | select(.path != "dune-workspace")' | sort
  >   rm .#tmp
  > }

  $ make_fs_memo_project

  $ echo -n 1 > file-1
  $ echo -n 3 > file-3
  $ touch dep
  $ mkdir dir
  $ mkdir subdir

Dune doesn't notice that a directory's permission changed and succeeds instead
of failing.

  $ fs_memo_test "chmod -r subdir"
  ------------------------------------------
  Executing rule...
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '' -> '13' -> '13'
  ------------------------------------------

If we repeat the test, we finally see the failure.

  $ fs_memo_test "echo How about now?"
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

Same problem in the other direction.

  $ fs_memo_test "chmod +r subdir"
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

  $ fs_memo_test "echo How about now?"
  ------------------------------------------
  How about now?
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------

Same problem for files.

  $ fs_memo_test "chmod -r file-1"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------

  $ fs_memo_test "chmod +r file-1"
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
