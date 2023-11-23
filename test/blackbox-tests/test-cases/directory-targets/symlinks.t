Create directory targets with symlinks in them

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (using directory-targets 0.1)
  > EOF

  $ runtest() {
  > cat >dune <<EOF
  > (rule
  >  (target (dir foo))
  >  (action (system "mkdir foo && cd foo && $1")))
  > EOF
  > dune build ./foo
  > dir=_build/default/foo
  > [ -d $dir ] && ls $dir
  > }

Symlink to a file

  $ runtest "touch foo && ln -s foo bar"
  bar
  foo

Symlink to a directory

  $ runtest "mkdir subdir && ln -s subdir2 subdir"
  subdir

Broken symlink

  $ runtest "ln -s doesnotexist bar"
  bar
