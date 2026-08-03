Pinning a git+file:// URL with a branch tracks the locked branch commit.

  $ setup_source_locking_project

First lock main at an updated commit.

  $ write_dependency_version "git+file:// updated"
  $ commit_dependency "Update"
  $ write_pin_project "git+file://$PWD/_dependency"
  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// updated

Create a change in a branch, making sure the worktree is not left on the branch.

  $ git -C _dependency switch -c branch --quiet
  $ write_dependency_version "git+file:// branched"
  $ commit_dependency "Change in branch"
  $ git -C _dependency switch - --quiet

We shouldn't observe any changes:

  $ dune exec ./main.exe
  git+file:// updated

Even relocking should not cause changes:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// updated

If we set the location to the new branch specifically:

  $ write_pin_project "git+file://$PWD/_dependency#branch"

No change until we re-lock as we track the locked revision:

  $ dune exec ./main.exe
  git+file:// updated

But once we relock we should see changes:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// branched

If we update the branch:

  $ git -C _dependency switch branch --quiet
  $ write_dependency_version "git+file:// branched and updated"
  $ commit_dependency "Update in branch"
  $ git -C _dependency switch - --quiet

We shouldn't see a change:

  $ dune exec ./main.exe
  git+file:// branched

But the change should be visible once we relock:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// branched and updated
