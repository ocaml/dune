Demonstrate that we copy empty directories

  $ . ./helpers.sh
  $ make_lockdir

  $ src=_foo
  $ mkdir $src
  $ cd $src
  $ mkdir empty
  $ cd ..

  $ make_lockpkg foo <<EOF
  > (version 0.0.1)
  > (build (system "find . | sort"))
  > (source (copy $PWD/$src))
  > EOF

  $ build_pkg foo
  .
