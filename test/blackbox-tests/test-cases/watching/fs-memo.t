Tests for [Fs_memo] module.

  $ . ./helpers.sh

  $ test () {
  >   start_dune --debug-cache=fs
  >   build . | grep -v Success
  >   eval "$@"
  >   build . | grep -v Success
  >   stop_dune | grep -v dune-workspace > .#tmp
  >   echo "------------------------------------------"
  >   cat .#tmp | grep -v Updating
  >   echo "------------------------------------------"
  >   cat .#tmp | grep Updating | sort
  >   rm .#tmp
  > }

  $ echo "(lang dune 2.0)" > dune-project
  $ cat >dune <<EOF
  > (rule
  >  (alias default)
  >  (deps (glob_files file-?) (glob_files dir/file-?) (glob_files dir/subdir/file-?))
  >  (action (bash "cat %{deps}")))
  > EOF

  $ echo -n 1 > file-1

Note that we receive duplicate events for [file-2] because we watch it both
directly and via its parent.

# CR-someday amokhov: Fix this.

  $ test "echo -n 2 > file-2"
  ------------------------------------------
  1
  Success, waiting for filesystem changes...
  12
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "file-2": Skipped
  Updating dir_contents cache for "file-2": Skipped
  Updating path_digest cache for ".": Skipped
  Updating path_digest cache for "file-2": Skipped
  Updating path_digest cache for "file-2": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "file-2": Skipped
  Updating path_stat cache for "file-2": Skipped

Note that Dune does not re-executed the rule because the set of files matching
the glob remains unchanged.

  $ test "mkdir dir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "dir": Skipped
  Updating path_digest cache for ".": Skipped
  Updating path_digest cache for "dir": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "dir": Skipped

Again, duplicate events for [dir/file-3].

  $ test "echo -n 3 > dir/file-3"
  ------------------------------------------
  Success, waiting for filesystem changes...
  123
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating dir_contents cache for "dir/file-3": Skipped
  Updating dir_contents cache for "dir/file-3": Skipped
  Updating path_digest cache for "dir": Skipped
  Updating path_digest cache for "dir/file-3": Skipped
  Updating path_digest cache for "dir/file-3": Skipped
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir/file-3": Skipped
  Updating path_stat cache for "dir/file-3": Skipped

Again, duplicate events for [file-2].

  $ test "echo -n '*' > file-2"
  ------------------------------------------
  Success, waiting for filesystem changes...
  1*3
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for "file-2": Skipped
  Updating dir_contents cache for "file-2": Skipped
  Updating path_digest cache for "file-2": Updated { changed = false }
  Updating path_digest cache for "file-2": Updated { changed = true }
  Updating path_stat cache for "file-2": Skipped
  Updating path_stat cache for "file-2": Skipped

On deletion of a file, we receive events for the file and the parent directory.

  $ test "rm file-2"
  ------------------------------------------
  Success, waiting for filesystem changes...
  13
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "file-2": Skipped
  Updating path_digest cache for ".": Skipped
  Updating path_digest cache for "file-2": Updated { changed = true }
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "file-2": Skipped

Here we receive duplicate events for [dir] because we watch it directly, plus
we also receive an event about a file inside, which we propagate to the parent.

# CR-someday amokhov: Fix this.

  $ test "mv dir/file-3 ."
  ------------------------------------------
  Success, waiting for filesystem changes...
  13
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "dir": Updated { changed = false }
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating dir_contents cache for "dir/file-3": Skipped
  Updating dir_contents cache for "dir/file-3": Skipped
  Updating dir_contents cache for "file-3": Skipped
  Updating path_digest cache for ".": Skipped
  Updating path_digest cache for "dir": Skipped
  Updating path_digest cache for "dir": Skipped
  Updating path_digest cache for "dir/file-3": Updated { changed = false }
  Updating path_digest cache for "dir/file-3": Updated { changed = true }
  Updating path_digest cache for "file-3": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir/file-3": Skipped
  Updating path_stat cache for "dir/file-3": Skipped
  Updating path_stat cache for "file-3": Skipped

  $ test "mkdir dir/subdir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating dir_contents cache for "dir/subdir": Skipped
  Updating path_digest cache for "dir": Skipped
  Updating path_digest cache for "dir/subdir": Skipped
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir/subdir": Skipped

Again, duplicate events for [file-4].

  $ test "echo -n 4 > dir/subdir/file-4"
  ------------------------------------------
  Success, waiting for filesystem changes...
  134
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for "dir/subdir": Updated { changed = true }
  Updating dir_contents cache for "dir/subdir/file-4": Skipped
  Updating dir_contents cache for "dir/subdir/file-4": Skipped
  Updating path_digest cache for "dir/subdir": Skipped
  Updating path_digest cache for "dir/subdir/file-4": Skipped
  Updating path_digest cache for "dir/subdir/file-4": Skipped
  Updating path_stat cache for "dir/subdir": Updated { changed = false }
  Updating path_stat cache for "dir/subdir/file-4": Skipped
  Updating path_stat cache for "dir/subdir/file-4": Skipped

Here we are getting duplicate events for directories [dir] and [dir/subdir].

  $ test "mv dir/subdir ."
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "dir": Updated { changed = false }
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating dir_contents cache for "dir/subdir": Updated { changed = false }
  Updating dir_contents cache for "dir/subdir": Updated { changed = true }
  Updating dir_contents cache for "subdir": Skipped
  Updating path_digest cache for ".": Skipped
  Updating path_digest cache for "dir": Skipped
  Updating path_digest cache for "dir": Skipped
  Updating path_digest cache for "dir/subdir": Skipped
  Updating path_digest cache for "dir/subdir": Skipped
  Updating path_digest cache for "subdir": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir/subdir": Updated { changed = false }
  Updating path_stat cache for "dir/subdir": Updated { changed = true }
  Updating path_stat cache for "subdir": Skipped


This seems to be a bug: Dune doesn't notice that a directory's permission
changed and succeeds instead of failing.

  $ test "chmod -r subdir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------

If we repeat the test, we finally see the failure.

# CR-someday amokhov: Fix this.

  $ test "echo How about now?"
  Failure
  How about now?
  Failure
  ------------------------------------------
  Error: inotify_add_watch: subdir: Permission denied
  Had errors, waiting for filesystem changes...
  Error: inotify_add_watch: subdir: Permission denied
  Had errors, waiting for filesystem changes...
  ------------------------------------------

Same problem in the other direction.

  $ test "chmod +r subdir"
  Failure
  Failure
  ------------------------------------------
  Error: inotify_add_watch: subdir: Permission denied
  Had errors, waiting for filesystem changes...
  Error: inotify_add_watch: subdir: Permission denied
  Had errors, waiting for filesystem changes...
  ------------------------------------------

  $ test "echo How about now?"
  How about now?
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------

Same problem for files.

  $ test "chmod -r file-1"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------

  $ test "chmod +r file-1"
  Failure
  Failure
  ------------------------------------------
  Error: inotify_add_watch: file-1: Permission denied
  -> required by _build/default/file-1
  -> required by alias default in dune:1
  Had errors, waiting for filesystem changes...
  Error: inotify_add_watch: file-1: Permission denied
  -> required by _build/default/file-1
  -> required by alias default in dune:1
  Had errors, waiting for filesystem changes...
  ------------------------------------------

We receive three events for [file-1] when moving it within the same directory.

  $ test "mv file-1 file-5; echo -n 5 > file-5"
  ------------------------------------------
  Success, waiting for filesystem changes...
  35
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = false }
  Updating dir_contents cache for ".": Updated { changed = false }
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "file-1": Skipped
  Updating dir_contents cache for "file-1": Skipped
  Updating dir_contents cache for "file-1": Skipped
  Updating dir_contents cache for "file-5": Skipped
  Updating dir_contents cache for "file-5": Skipped
  Updating path_digest cache for ".": Skipped
  Updating path_digest cache for ".": Skipped
  Updating path_digest cache for ".": Skipped
  Updating path_digest cache for "file-1": Updated { changed = false }
  Updating path_digest cache for "file-1": Updated { changed = false }
  Updating path_digest cache for "file-1": Updated { changed = true }
  Updating path_digest cache for "file-5": Skipped
  Updating path_digest cache for "file-5": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "file-1": Skipped
  Updating path_stat cache for "file-1": Skipped
  Updating path_stat cache for "file-1": Skipped
  Updating path_stat cache for "file-5": Skipped
  Updating path_stat cache for "file-5": Skipped
