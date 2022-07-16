Tests for [Fs_memo] module.

  $ . ./helpers.sh

  $ test () {
  >   echo "------------------------------------------"
  >   before=$(cat _build/default/result 2>/dev/null)
  >   start_dune --debug-cache=fs
  >   build . | grep -v Success
  >   between=$(cat _build/default/result)
  >   eval "$@"
  >   build . | grep -v Success
  >   stop_dune | grep -v dune-workspace >> .#tmp
  >   after=$(cat _build/default/result)
  >   cat .#tmp | grep -v Updating
  >   echo "------------------------------------------"
  >   echo "result = '$before' -> '$between' -> '$after'"
  >   echo "------------------------------------------"
  >   cat .#tmp | grep Updating | sort
  >   rm .#tmp
  > }

The action ignores the dependency [dep]. We use it to force rerunning the action
when necessary.

  $ echo "(lang dune 2.0)" > dune-project
  $ cat >dune <<EOF
  > (rule
  >  (alias default)
  >  (deps dep
  >        (glob_files file-?)
  >        (glob_files dir/file-?)
  >        (glob_files dir/subdir/file-?))
  >  (target result)
  >  (action (bash "\| echo Executing rule...
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
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "file-2": Skipped
  Updating dir_contents cache for "file-2": Skipped
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for "file-2": Skipped
  Updating file_digest cache for "file-2": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "file-2": Skipped
  Updating path_stat cache for "file-2": Skipped

Note that Dune did not re-execute the rule because the set of files matching
the glob remains unchanged.

  $ test "mkdir dir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '12' -> '12' -> '12'
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "dir": Skipped
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "dir": Skipped

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
  Updating dir_contents cache for "dir/file-3": Skipped
  Updating file_digest cache for "dir/file-3": Updated { changed = true }
  Updating path_stat cache for "dir/file-3": Skipped

Now, Dune similarly updates [file_digest] for [file-2].

  $ test "echo -n '*' > file-2"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '123' -> '123' -> '1*3'
  ------------------------------------------
  Updating dir_contents cache for "file-2": Skipped
  Updating file_digest cache for "file-2": Updated { changed = true }
  Updating path_stat cache for "file-2": Skipped

On deletion of a file, we receive events for the file and the parent directory.

  $ test "rm file-2"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '1*3' -> '1*3' -> '13'
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "file-2": Skipped
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for "file-2": Updated { changed = true }
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "file-2": Skipped

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
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating dir_contents cache for "dir/file-3": Skipped
  Updating dir_contents cache for "file-3": Skipped
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating file_digest cache for "dir/file-3": Updated { changed = true }
  Updating file_digest cache for "file-3": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir/file-3": Skipped
  Updating path_stat cache for "file-3": Skipped

  $ test "mkdir dir/subdir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating dir_contents cache for "dir/subdir": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating file_digest cache for "dir/subdir": Skipped
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir/subdir": Skipped

Again, there are two events for [file-4]: for creation and modification.

  $ test "echo -n 4 > dir/subdir/file-4"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '134'
  ------------------------------------------
  Updating dir_contents cache for "dir/subdir": Updated { changed = true }
  Updating dir_contents cache for "dir/subdir/file-4": Skipped
  Updating dir_contents cache for "dir/subdir/file-4": Skipped
  Updating file_digest cache for "dir/subdir": Skipped
  Updating file_digest cache for "dir/subdir/file-4": Skipped
  Updating file_digest cache for "dir/subdir/file-4": Skipped
  Updating path_stat cache for "dir/subdir": Updated { changed = false }
  Updating path_stat cache for "dir/subdir/file-4": Skipped
  Updating path_stat cache for "dir/subdir/file-4": Skipped

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
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "dir": Updated { changed = false }
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating dir_contents cache for "dir/subdir": Updated { changed = false }
  Updating dir_contents cache for "dir/subdir": Updated { changed = true }
  Updating dir_contents cache for "subdir": Skipped
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating file_digest cache for "dir/subdir": Skipped
  Updating file_digest cache for "dir/subdir": Skipped
  Updating file_digest cache for "subdir": Skipped
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
  result = '13' -> '13' -> '13'
  ------------------------------------------

If we repeat the test, we finally see the failure.

# CR-someday amokhov: Fix this.

  $ test "echo How about now?"
  ------------------------------------------
  Failure
  How about now?
  Failure
  Error: inotify_add_watch(subdir): Permission denied
  Had errors, waiting for filesystem changes...
  Error: inotify_add_watch(subdir): Permission denied
  Had errors, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------

Same problem in the other direction.

  $ test "chmod +r subdir"
  ------------------------------------------
  Failure
  Failure
  Error: inotify_add_watch(subdir): Permission denied
  Had errors, waiting for filesystem changes...
  Error: inotify_add_watch(subdir): Permission denied
  Had errors, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------

  $ test "echo How about now?"
  ------------------------------------------
  How about now?
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------

Same problem for files.

  $ test "chmod -r file-1"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------

  $ test "chmod +r file-1"
  ------------------------------------------
  Failure
  Failure
  Error: file-1: Permission denied
  -> required by _build/default/file-1
  -> required by _build/default/result
  -> required by alias default
  Had errors, waiting for filesystem changes...
  Error: file-1: Permission denied
  -> required by _build/default/file-1
  -> required by _build/default/result
  -> required by alias default
  Had errors, waiting for filesystem changes...
  ------------------------------------------
  result = '13' -> '13' -> '13'
  ------------------------------------------

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
  Updating dir_contents cache for ".": Updated { changed = false }
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "file-1": Skipped
  Updating dir_contents cache for "file-5": Skipped
  Updating dir_contents cache for "file-5": Skipped
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for "file-1": Updated { changed = true }
  Updating file_digest cache for "file-5": Skipped
  Updating file_digest cache for "file-5": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "file-1": Skipped
  Updating path_stat cache for "file-5": Skipped
  Updating path_stat cache for "file-5": Skipped

Tests for watching symbolic links.

First, create a symbolic link. Dune correctly updates the [result].

  $ test "ln -s ../file-3 dir/file-6"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '35' -> '35' -> '353'
  ------------------------------------------
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating dir_contents cache for "dir/file-6": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating file_digest cache for "dir/file-6": Skipped
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir/file-6": Skipped

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
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating dir_contents cache for "dir/file-6": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating file_digest cache for "dir/file-6": Updated { changed = true }
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir/file-6": Updated { changed = true }

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
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating dir_contents cache for "dir/file-7": Skipped
  Updating dir_contents cache for "dir/file-7": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating file_digest cache for "dir/file-7": Skipped
  Updating file_digest cache for "dir/file-7": Skipped
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir/file-7": Skipped
  Updating path_stat cache for "dir/file-7": Skipped

Deleting [dir] triggers a rebuild.

  $ test "rm dir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '357' -> '357' -> '35'
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "dir": Updated { changed = true }

Restoring the symlink is correctly noticed.

  $ test "ln -s another-dir dir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '35' -> '35' -> '357'
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "dir": Skipped
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "dir": Skipped

However, deleting [another-dir] isn't handled correctly.

# CR-someday amokhov: Fix this.

  $ test "rm another-dir/file-7; sleep 0.001; rmdir another-dir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '357' -> '357' -> '357'
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "another-dir": Updated { changed = true }
  Updating dir_contents cache for "another-dir": Updated { changed = true }
  Updating dir_contents cache for "another-dir/file-7": Skipped
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for "another-dir": Skipped
  Updating file_digest cache for "another-dir": Skipped
  Updating file_digest cache for "another-dir/file-7": Updated { changed = true }
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "another-dir": Updated { changed = false }
  Updating path_stat cache for "another-dir": Updated { changed = true }
  Updating path_stat cache for "another-dir/file-7": Skipped

If we force a rebuild, Dune belatedly notices that [another-dir/file-7] is now
unreachable but doesn't complain about the symlink [dir] now being broken. We
should fix this too.

  $ test "echo force > dep"
  ------------------------------------------
  Executing rule...
  Success, waiting for filesystem changes...
  Executing rule...
  Success, waiting for filesystem changes...
  ------------------------------------------
  result = '357' -> '35' -> '35'
  ------------------------------------------
  Updating dir_contents cache for "dep": Skipped
  Updating file_digest cache for "dep": Updated { changed = true }
  Updating path_stat cache for "dep": Skipped
