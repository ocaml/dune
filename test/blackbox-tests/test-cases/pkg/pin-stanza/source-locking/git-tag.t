Pinning a git+file:// URL with a tag tracks the locked tag commit.

  $ setup_source_locking_project

First lock main at an updated commit.

  $ write_dependency_version "git+file:// before tag"
  $ commit_dependency "Update before tag"
  $ write_pin_project "git+file://$PWD/_dependency"
  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// before tag

Make sure this also works with tags. We update the repo to have a new change
that is tagged but not the HEAD commit:

  $ write_dependency_version "git+file:// main tagged v1"
  $ commit_dependency "Update on main"
  $ git -C _dependency tag v1
  $ write_dependency_version "git+file:// main dev"
  $ commit_dependency "Update"

We tell dune to lock the v1 tag:

  $ write_pin_project "git+file://$PWD/_dependency#v1"

We expect no update from before:

  $ dune exec ./main.exe
  git+file:// before tag

However if we lock we want to see the state of v1:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// main tagged v1

We have realized that v1 has some issue, so we want to re-tag it:

  $ write_dependency_version "git+file:// main retagged v1"
  $ commit_dependency "Update on main"
  $ git -C _dependency tag -d v1 > /dev/null
  $ git -C _dependency tag v1
  $ write_dependency_version "git+file:// main dev, again"
  $ commit_dependency "Update"

We expect no change, as we did not relock since:

  $ dune exec ./main.exe
  git+file:// main tagged v1

After relocking, we expect to see the revised v1 tag:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// main retagged v1
