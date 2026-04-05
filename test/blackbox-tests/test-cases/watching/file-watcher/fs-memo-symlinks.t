Tests for [Fs_memo] symlink handling.

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

  $ echo -n 3 > file-3
  $ echo -n 5 > file-5
  $ touch dep
  $ mkdir dir

Tests for watching symbolic links.

First, create a symbolic link. Dune correctly updates the [result].

  $ test "ln -s ../file-3 dir/file-6"
  ------------------------------------------
  Executing rule...
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '' -> '35' -> '353'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dir","result":"changed"}
  {"cache_type":"dir_contents","path":"dir/file-6","result":"skipped"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dir","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/file-6","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dir","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir/file-6","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

Now, delete the symbolic link. Dune receives the corresponding events and
reruns the affected action.

  $ test "rm dir/file-6"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '353' -> '353' -> '35'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dir","result":"changed"}
  {"cache_type":"dir_contents","path":"dir/file-6","result":"skipped"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dir","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/file-6","result":"changed"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dir","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir/file-6","result":"changed"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

Now test symbolic links to directories.

  $ mkdir another-dir
  $ rmdir dir
  $ ln -s another-dir dir

At first, things appear to be working well: we correctly discover [dir/file-7]
and re-execute the rule.

  $ test "echo -n 7 > another-dir/file-7"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '35' -> '35' -> '357'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dir","result":"changed"}
  {"cache_type":"dir_contents","path":"dir/file-7","result":"skipped"}
  {"cache_type":"dir_contents","path":"dir/file-7","result":"skipped"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dir","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/file-7","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/file-7","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dir","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir/file-7","result":"skipped"}
  {"cache_type":"path_stat","path":"dir/file-7","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

Deleting [dir] triggers a rebuild.

  $ test "rm dir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '357' -> '357' -> '35'
  ------------------------------------------
  {"cache_type":"dir_contents","path":".","result":"changed"}
  {"cache_type":"dir_contents","path":"dir","result":"changed"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":".","result":"skipped"}
  {"cache_type":"file_digest","path":"dir","result":"skipped"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":".","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir","result":"changed"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

Restoring the symlink is correctly noticed.

  $ test "ln -s another-dir dir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '35' -> '35' -> '357'
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

However, deleting [another-dir] isn't handled correctly.

# CR-someday amokhov: Fix this.

  $ test "rm another-dir/file-7; sleep 0.001; rmdir another-dir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '357' -> '357' -> '35'
  ------------------------------------------
  {"cache_type":"dir_contents","path":".","result":"changed"}
  {"cache_type":"dir_contents","path":"another-dir","result":"changed"}
  {"cache_type":"dir_contents","path":"dir","result":"changed"}
  {"cache_type":"dir_contents","path":"dir/file-7","result":"skipped"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":".","result":"skipped"}
  {"cache_type":"file_digest","path":"another-dir","result":"skipped"}
  {"cache_type":"file_digest","path":"dir","result":"skipped"}
  {"cache_type":"file_digest","path":"dir/file-7","result":"changed"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":".","result":"unchanged"}
  {"cache_type":"path_stat","path":"another-dir","result":"changed"}
  {"cache_type":"path_stat","path":"dir","result":"unchanged"}
  {"cache_type":"path_stat","path":"dir/file-7","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}

If we force a rebuild, Dune belatedly notices that [another-dir/file-7] is now
unreachable but doesn't complain about the symlink [dir] now being broken. We
should fix this too.

  $ test "echo force > dep"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '35' -> '35' -> '35'
  ------------------------------------------
  {"cache_type":"dir_contents","path":"dep","result":"skipped"}
  {"cache_type":"dir_contents","path":"dune-workspace","result":"skipped"}
  {"cache_type":"file_digest","path":"dep","result":"changed"}
  {"cache_type":"file_digest","path":"dune-workspace","result":"skipped"}
  {"cache_type":"path_stat","path":"dep","result":"skipped"}
  {"cache_type":"path_stat","path":"dune-workspace","result":"unchanged"}
