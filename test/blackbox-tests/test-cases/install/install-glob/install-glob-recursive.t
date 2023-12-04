Test that `glob_files_rec` can recursively find files

  $ cat >dune-project <<EOF
  > (lang dune 3.5)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (install
  >  (files (glob_files_rec b/*.txt))
  >  (section share))
  > EOF

Make a file outside of the "b" directory. This file should be skipped.
  $ touch a.txt

Create a hierarchy of files and directories which should be included.
  $ mkdir b
  $ touch b/b.txt
  $ mkdir b/c
  $ touch b/c/c.txt

Make sure we don't crash on empty directories.
  $ mkdir b/c/empty-dir

Add some files which don't match the glob.
  $ touch b/b
  $ touch b/txt
  $ mkdir b/d
  $ touch b/d/d

  $ dune build
  File "dune", line 2, characters 8-32:
  2 |  (files (glob_files_rec b/*.txt))
              ^^^^^^^^^^^^^^^^^^^^^^^^
  Error: 'glob_files_rec' is only available since version 3.6 of the dune
  language. Please update your dune-project file to have (lang dune 3.6).
  [1]

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (package (name foo))
  > EOF

  $ dune build

  $ cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/foo/b/b.txt" {"b/b.txt"}
    "_build/install/default/share/foo/b/c/c.txt" {"b/c/c.txt"}
  ]

  $ find _build/install | sort
  _build/install
  _build/install/default
  _build/install/default/lib
  _build/install/default/lib/foo
  _build/install/default/lib/foo/META
  _build/install/default/lib/foo/dune-package
  _build/install/default/share
  _build/install/default/share/foo
  _build/install/default/share/foo/b
  _build/install/default/share/foo/b/b.txt
  _build/install/default/share/foo/b/c
  _build/install/default/share/foo/b/c/c.txt
