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

The action ignores the dependency [dep]. We use it to force rerunning the action
when necessary.

  $ echo "(lang dune 2.0)" > dune-project
  $ cat >dune <<EOF
  > (rule
  >  (alias default)
  >  (deps dep (glob_files file-?) (glob_files dir/file-?) (glob_files dir/subdir/file-?))
  >  (action (bash "echo %{deps} | xargs -d' ' -n 1 | grep -v dep | xargs cat")))
  > EOF

  $ echo -n 1 > file-1
  $ touch dep

Note that we receive two events for [file-2] because it's first created empty
and then the contents is written to it.

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
  12?
  Success, waiting for filesystem changes...
  123
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for "dir/file-3": Skipped
  Updating file_digest cache for "dir/file-3": Updated { changed = true }
  Updating path_stat cache for "dir/file-3": Skipped

Now, Dune similarly updates [file_digest] for [file-2].

  $ test "echo -n '*' > file-2"
  ------------------------------------------
  Success, waiting for filesystem changes...
  1*3
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for "file-2": Skipped
  Updating file_digest cache for "file-2": Updated { changed = true }
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
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for "file-2": Updated { changed = true }
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "file-2": Skipped

Dune notices that [dir_contents] of both [dir] and [.] changed, and also that
[dir/file-3]'s digest changed (from a digest to the error about missing file).

  $ test "mv dir/file-3 ."
  ------------------------------------------
  Success, waiting for filesystem changes...
  13
  Success, waiting for filesystem changes...
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
  134
  Success, waiting for filesystem changes...
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
  Success, waiting for filesystem changes...
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

If we repeat the test, we finally see the failure.

# CR-someday amokhov: Fix this.

  $ test "echo How about now?"
  Failure
  How about now?
  Failure
  ------------------------------------------
  Error: inotify_add_watch(subdir): Permission denied
  Had errors, waiting for filesystem changes...
  Error: inotify_add_watch(subdir): Permission denied
  Had errors, waiting for filesystem changes...
  ------------------------------------------

Same problem in the other direction.

  $ test "chmod +r subdir"
  Failure
  Failure
  ------------------------------------------
  Error: inotify_add_watch(subdir): Permission denied
  Had errors, waiting for filesystem changes...
  Error: inotify_add_watch(subdir): Permission denied
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
  Error: file-1: Permission denied
  -> required by _build/default/file-1
  -> required by alias default in dune:1
  Had errors, waiting for filesystem changes...
  Error: file-1: Permission denied
  -> required by _build/default/file-1
  -> required by alias default in dune:1
  Had errors, waiting for filesystem changes...
  ------------------------------------------

Dune receives one event for [file-1] when moving it within the same directory,
and two events for [file-5]: for moving and for changing. There are two events
for [dir_contents] of [.] because moving a file is interpreted as deleting and
then creating a file.

  $ test "mv file-1 file-5; echo -n 5 > file-5"
  ------------------------------------------
  Success, waiting for filesystem changes...
  35
  Success, waiting for filesystem changes...
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

Demonstrate that watching symbolic links doesn't currently work.

First, create a symbolic link.

  $ test "ln -s ../file-3 dir/file-6"
  ------------------------------------------
  Success, waiting for filesystem changes...
  353
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating dir_contents cache for "dir/file-6": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating file_digest cache for "dir/file-6": Skipped
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir/file-6": Skipped

Now, delete the symbolic link. Dune receives the corresponding events but
doesn't rerun the affected action.

# CR-someday amokhov: Fix this.

  $ test "rm dir/file-6"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating dir_contents cache for "dir/file-6": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating file_digest cache for "dir/file-6": Updated { changed = true }
  Updating path_stat cache for "dir": Updated { changed = false }
  Updating path_stat cache for "dir/file-6": Updated { changed = true }

If we force the action to be rerun by modifying [dep], it prints [35] as it
should have done in the previous run.

  $ test "echo force-1 > dep"
  ------------------------------------------
  Success, waiting for filesystem changes...
  35
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for "dep": Skipped
  Updating file_digest cache for "dep": Updated { changed = true }
  Updating path_stat cache for "dep": Skipped

Now demonstrate one more problem with symlinks.

  $ mkdir another-dir
  $ rmdir dir
  $ ln -s another-dir dir

At first, things appear to be working well: we correctly discover [dir/file-7]
and re-execute the rule.

  $ test "echo -n 7 > another-dir/file-7"
  ------------------------------------------
  Success, waiting for filesystem changes...
  357
  Success, waiting for filesystem changes...
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

However, deleting [dir] doesn't trigger rerunning the rule.

# CR-someday amokhov: Fix this.

  $ test "rm dir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "dir": Updated { changed = true }
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "dir": Updated { changed = true }

By changing [dep], we can force Dune to print the correct output.

  $ test "echo force-2 > dep"
  ------------------------------------------
  Success, waiting for filesystem changes...
  35
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for "dep": Skipped
  Updating file_digest cache for "dep": Updated { changed = true }
  Updating path_stat cache for "dep": Skipped

Restoring the symlink is correctly noticed.

  $ test "ln -s another-dir dir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  357
  Success, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for ".": Updated { changed = true }
  Updating dir_contents cache for "dir": Skipped
  Updating file_digest cache for ".": Skipped
  Updating file_digest cache for "dir": Skipped
  Updating path_stat cache for ".": Updated { changed = false }
  Updating path_stat cache for "dir": Skipped

However, deleting [another-dir] isn't handled correctly either.

# CR-someday amokhov: Fix this.

  $ test "rm another-dir/file-7; sleep 0.001; rmdir another-dir"
  ------------------------------------------
  Success, waiting for filesystem changes...
  Success, waiting for filesystem changes...
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

Here is what should have happened:

  $ test "echo force-3 > dep"
  Failure
  Failure
  ------------------------------------------
  File "dir", line 1, characters 0-0:
  Error: File unavailable: dir
  Broken symlink
  Had errors, waiting for filesystem changes...
  File "dir", line 1, characters 0-0:
  Error: File unavailable: dir
  Broken symlink
  Had errors, waiting for filesystem changes...
  ------------------------------------------
  Updating dir_contents cache for "dep": Skipped
  Updating file_digest cache for "dep": Updated { changed = true }
  Updating path_stat cache for "dep": Skipped
