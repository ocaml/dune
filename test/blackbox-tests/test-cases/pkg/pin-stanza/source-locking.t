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

  $ git -C _dependency init --quiet
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

We should be getting the 

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  file:// initial

When we update the dependency we should be getting the new content

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
  git+file:// updated

However at the moment immediately getting the HEAD revision of the git repo.

When we re-lock, we should lock the new revision of the dependency and build
that:

  $ dune pkg lock 2> /dev/null
  $ dune exec ./main.exe
  git+file:// updated
