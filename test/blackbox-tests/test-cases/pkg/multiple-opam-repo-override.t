Multiple opam repositories that define the same package:

  $ . ./helpers.sh

  $ pkg="packages/foo"
  $ mkdir -p repo1/$pkg repo2/$pkg

  $ mkpkg() {
  > local p="$1/$pkg/foo.$2"
  > mkdir -p $p
  > cat >$p/opam <<EOF
  > opam-version: "2.0"
  > build: [ "echo" "$1" ]
  > EOF
  > }

  $ repos12=`cat <<EOF
  > (repository
  >  (name repo1)
  >  (source "file://$PWD/repo1"))
  > (repository
  >  (name repo2)
  >  (source "file://$PWD/repo2"))
  > EOF
  > `

  $ cat >dune-workspace <<EOF
  > (lang dune 3.11)
  > (lock_dir
  >  (repositories repo1 repo2))
  > $repos12
  > EOF

  $ make_project foo | cat >dune-project

  $ runtest () {
  > dune pkg lock
  > cat dune.lock/foo.pkg
  > }

Define 1.0.0 in repo1 and 2.0.0 in repo2 for the same package:

  $ mkpkg repo1 1.0.0
  $ runtest
  Solution for dune.lock:
  - foo.1.0.0
  (version 1.0.0)
  
  (build
   (run echo repo1))

  $ mkpkg repo2 2.0.0
  $ runtest
  Solution for dune.lock:
  - foo.2.0.0
  (version 2.0.0)
  
  (build
   (run echo repo2))


We define 2.0.0 in both repo1 and repo2, but repo1 is listed first, so it
should take priority

  $ mkpkg repo1 2.0.0
  $ runtest
  Solution for dune.lock:
  - foo.2.0.0
  (version 2.0.0)
  
  (build
   (run echo repo1))

Even though repo2 is of lesser priority, it has the best version so it should
be selected:

  $ mkpkg repo2 3.0.0
  $ runtest
  Solution for dune.lock:
  - foo.3.0.0
  (version 3.0.0)
  
  (build
   (run echo repo2))

Now we repeat the tests but with a git repo:

  $ mkdir -p git-repo
  $ mkpkg git-repo 3.0.0
  $ cd git-repo
  $ git init --quiet
  $ git add -A
  $ git commit --quiet -m "Initial commit"
  $ cd ..

  $ mkworkspace() {
  > cat >dune-workspace <<EOF
  > (lang dune 3.11)
  > (lock_dir
  >  (repositories $@))
  > $repos12
  > (repository
  >  (name git-repo)
  >  (source "git+file://$PWD/git-repo"))
  > EOF
  > }

  $ mkworkspace "repo1 repo2 git-repo"
  $ runtest
  Solution for dune.lock:
  - foo.3.0.0
  (version 3.0.0)
  
  (build
   (run echo repo2))

  $ mkworkspace "git-repo repo1 repo2"
  $ runtest
  Solution for dune.lock:
  - foo.3.0.0
  (version 3.0.0)
  
  (build
   (run echo git-repo))
