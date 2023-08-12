Testing the source_trees field which is used to entire source_trees

  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (allow_empty)
  >  (name mypkg))
  > EOF

  $ test() {
  > cat >dune <<EOF
  > (install
  >  (section doc)
  >  (source_trees $1))
  > EOF
  > dune build mypkg.install && cat _build/default/mypkg.install
  > }

Try to build a source directory that doesn't exist:

  $ test "mydocs"
  File "dune", line 3, characters 15-21:
  3 |  (source_trees mydocs))
                     ^^^^^^
  Error: This source directory does not exist
  [1]

Create the source directory and fill it up with some dummy stuff for the test:

  $ mkdir -p mydocs/foo
  $ touch mydocs/foo.md mydocs/baz.md mydocs/foo/bar.md

  $ test "mydocs"
  lib: [
    "_build/install/default/lib/mypkg/META"
    "_build/install/default/lib/mypkg/dune-package"
  ]
  doc: [
    "_build/install/default/doc/mypkg/mydocs/baz.md" {"mydocs/baz.md"}
    "_build/install/default/doc/mypkg/mydocs/foo.md" {"mydocs/foo.md"}
    "_build/install/default/doc/mypkg/mydocs/foo/bar.md" {"mydocs/foo/bar.md"}
  ]

  $ test "(mydocs as yourdocs)"
  lib: [
    "_build/install/default/lib/mypkg/META"
    "_build/install/default/lib/mypkg/dune-package"
  ]
  doc: [
    "_build/install/default/doc/mypkg/yourdocs/baz.md" {"yourdocs/baz.md"}
    "_build/install/default/doc/mypkg/yourdocs/foo.md" {"yourdocs/foo.md"}
    "_build/install/default/doc/mypkg/yourdocs/foo/bar.md" {"yourdocs/foo/bar.md"}
  ]

  $ test "(mydocs as your/docs)"
  lib: [
    "_build/install/default/lib/mypkg/META"
    "_build/install/default/lib/mypkg/dune-package"
  ]
  doc: [
    "_build/install/default/doc/mypkg/your/docs/baz.md" {"your/docs/baz.md"}
    "_build/install/default/doc/mypkg/your/docs/foo.md" {"your/docs/foo.md"}
    "_build/install/default/doc/mypkg/your/docs/foo/bar.md" {"your/docs/foo/bar.md"}
  ]

  $ test "(mydocs as ../)"
  lib: [
    "_build/install/default/lib/mypkg/META"
    "_build/install/default/lib/mypkg/dune-package"
  ]
  doc: [
    "_build/install/default/doc/baz.md" {"../baz.md"}
    "_build/install/default/doc/foo.md" {"../foo.md"}
    "_build/install/default/doc/foo/bar.md" {"../foo/bar.md"}
  ]
