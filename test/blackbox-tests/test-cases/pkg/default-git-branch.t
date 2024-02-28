Modify the default branch of a source repo.

Once the branch is modified, dune should rebuild the package.

This bug is reported in #10063

  $ . ./helpers.sh

  $ src="_git_source"

  $ mkdir $src && cd $src
  $ git init --quiet
  $ git checkout -b "branch1"
  Switched to a new branch 'branch1'
  $ echo "branch 1" > file
  $ git add -A
  $ git commit --quiet -m "branch 1"
  $ git checkout -b branch2
  Switched to a new branch 'branch2'
  $ echo "branch 2" > file
  $ git add -A
  $ git commit --quiet -m "branch 2"
  $ cd ..

  $ make_lockdir
  $ make_lockpkg foo <<EOF
  > (source (fetch (url "git+file://$PWD/$src")))
  > (version 0.0.1)
  > (build (run cat file))
  > EOF

Build the package

  $ build_pkg foo
  branch 2

Change the default branch

  $ git -C $src checkout branch1
  Switched to branch 'branch1'

And now rebuild

  $ build_pkg foo
  branch 1
