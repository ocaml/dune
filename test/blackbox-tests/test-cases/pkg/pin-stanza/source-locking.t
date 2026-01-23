We test the behavior of pinning when locking, depending on the source that is being pinned.

  $ mkrepo
  $ add_mock_repo_if_needed

Let's set up a dependency with a value:

  $ mkdir _dependency
  $ cat > _dependency/dune-project <<EOF
  > (lang dune 3.21)
  > (package
  >   (name dependency))
  > EOF
  $ cat > _dependency/dependency.ml <<EOF
  > let version = "initial"
  > EOF
  $ cat > _dependency/dune <<EOF
  > (library
  >  (public_name dependency))
  > EOF

We also make sure that the dependency is a git repo

  $ git -C _dependency init --initial-branch=main --quiet
  $ git -C _dependency add -A
  $ git -C _dependency commit -m "Initial" --quiet

Let's set up an executable that uses the dependency.

  $ cat > main.ml <<EOF
  > print_endline Dependency.version
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name main)
  >  (libraries dependency))
  > EOF

Pin without a prefix:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (pin
  >  (url "$PWD/_dependency")
  >  (package (name dependency)))
  > (package
  >  (name main)
  >  (depends dependency))
  > EOF

This should work and display the initial value:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  initial

Updating the pinned dependency should update the output:

  $ cat > _dependency/dependency.ml <<EOF
  > let version = "updated"
  > EOF
  $ dune exec ./main.exe
  updated

Now pin using the file:// prefix

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (pin
  >  (url "file://$PWD/_dependency")
  >  (package (name dependency)))
  > (package
  >  (name main)
  >  (depends dependency))
  > EOF
  $ cat > _dependency/dependency.ml <<EOF
  > let version = "file:// initial"
  > EOF

We should be getting the value that the work dir currently has:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  file:// initial

When we update the work dir we should be getting the new content:

  $ cat > _dependency/dependency.ml <<EOF
  > let version = "file:// updated"
  > EOF
  $ dune exec ./main.exe
  file:// updated

Now we switch to a git repo:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (pin
  >  (url "git+file://$PWD/_dependency")
  >  (package (name dependency)))
  > (package
  >  (name main)
  >  (depends dependency))
  > EOF
  $ cat > _dependency/dependency.ml <<EOF
  > let version = "git+file:// initial"
  > EOF

We should be getting the latest committed version from the git repo:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  initial

If we update the dependency and commit

  $ cat > _dependency/dependency.ml <<EOF
  > let version = "git+file:// updated"
  > EOF
  $ git -C _dependency add -A
  $ git -C _dependency commit -m "Update" --quiet

We should still be getting the initial message since the lock dir has not been
updated:

  $ dune exec ./main.exe
  initial

When we re-lock, we should lock the new revision of the dependency and build
that:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// updated

Let's make sure locking works with branches by creating a change in a branch
(and make sure the worktree is not pointing at this branch):

  $ git -C _dependency switch -c branch --quiet
  $ cat > _dependency/dependency.ml <<EOF
  > let version = "git+file:// branched"
  > EOF
  $ git -C _dependency add -A
  $ git -C _dependency commit -m "Change in branch" --quiet
  $ git -C _dependency switch - --quiet

We shouldn't observe any changes:

  $ dune exec ./main.exe
  git+file:// updated

Even relocking should not cause changes:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// updated

If we set the location to the new branch specifically:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (pin
  >  (url "git+file://$PWD/_dependency#branch")
  >  (package (name dependency)))
  > (package
  >  (name main)
  >  (depends dependency))
  > EOF

No change until we re-lock as we track the locked revision:

  $ dune exec ./main.exe
  git+file:// updated

But once we relock we should see changes:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// branched

If we update the branch:

  $ git -C _dependency switch branch --quiet
  $ cat > _dependency/dependency.ml <<EOF
  > let version = "git+file:// branched and updated"
  > EOF
  $ git -C _dependency add -A
  $ git -C _dependency commit -m "Update in branch" --quiet
  $ git -C _dependency switch - --quiet

We shouldn't see a change:

  $ dune exec ./main.exe
  git+file:// branched

But the change should be visible once we relock:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// branched and updated

Make sure this also works with tags. We update the repo to have a new change
that is tagged but not the HEAD commit:

  $ cat > _dependency/dependency.ml <<EOF
  > let version = "git+file:// main tagged v1"
  > EOF
  $ git -C _dependency add -A
  $ git -C _dependency commit -m "Update on main" --quiet
  $ git -C _dependency tag v1
  $ cat > _dependency/dependency.ml <<EOF
  > let version = "git+file:// main dev"
  > EOF
  $ git -C _dependency add -A
  $ git -C _dependency commit -m "Update" --quiet

We tell dune to lock the v1 tag:

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > (pin
  >  (url "git+file://$PWD/_dependency#v1")
  >  (package (name dependency)))
  > (package
  >  (name main)
  >  (depends dependency))
  > EOF

We expect no update from before:

  $ dune exec ./main.exe
  git+file:// branched and updated

However if we lock we want to see the state of v1:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// main tagged v1

We have realized that v1 has some issue, so we want to re-tag it:

  $ cat > _dependency/dependency.ml <<EOF
  > let version = "git+file:// main retagged v1"
  > EOF
  $ git -C _dependency add -A
  $ git -C _dependency commit -m "Update on main" --quiet
  $ git -C _dependency tag -d v1 > /dev/null
  $ git -C _dependency tag v1
  $ cat > _dependency/dependency.ml <<EOF
  > let version = "git+file:// main dev, again"
  > EOF
  $ git -C _dependency add -A
  $ git -C _dependency commit -m "Update" --quiet

We expect no change, as we did not relock since:

  $ dune exec ./main.exe
  git+file:// main tagged v1

After relocking, we expect to see the revised v1 tag:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// main retagged v1
