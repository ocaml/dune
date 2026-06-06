Simple example of using a glob to specify files to install

  $ make_dune_project_with_package 3.5 foo

  $ cat >dune <<EOF
  > (install
  >  (files (glob_files *.txt))
  >  (section share))
  > EOF

  $ touch a.txt b.txt c.txt

  $ dune build @install
  File "dune", line 2, characters 8-26:
  2 |  (files (glob_files *.txt))
              ^^^^^^^^^^^^^^^^^^
  Error: 'glob_files' is only available since version 3.6 of the dune language.
  Please update your dune-project file to have (lang dune 3.6).
  [1]

  $ make_dune_project_with_package 3.6 foo

  $ dune build @install

  $ cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/foo/a.txt"
    "_build/install/default/share/foo/b.txt"
    "_build/install/default/share/foo/c.txt"
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
  _build/install/default/share/foo/a.txt
  _build/install/default/share/foo/b.txt
  _build/install/default/share/foo/c.txt
