We shouldn't copy files that aren't sources

  $ . ./helpers.sh
  $ make_lockdir

  $ src=_foo
  $ mkdir $src
  $ cd $src

Should we include node_modules in this list?
  $ mkdir .git .hg _darcs _esy _opam _build node_modules

We include an empty file because of a bug that prevents us from copying empty directories
  $ find . -type d -exec touch {}/file \;
  $ touch .#foo
  $ cd ..

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build (system "find . | sort"))
  > (source (copy $PWD/$src))
  > EOF

  $ build_pkg foo
  .
  ./file
  ./node_modules
  ./node_modules/file
