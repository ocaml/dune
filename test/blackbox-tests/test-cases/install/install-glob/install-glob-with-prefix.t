Examples of the with-prefix feature

  $ touch a.txt b.txt c.txt d.md e.md

  $ cat >dune <<EOF
  > (install
  >  (files (glob_files (*.txt with_prefix bar)))
  >  (section share))
  > EOF

Test that the feature is unavailable before 3.11
  $ cat >dune-project <<EOF
  > (lang dune 3.10)
  > (package
  >  (name foo))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  File "dune", line 2, characters 20-43:
  2 |  (files (glob_files (*.txt with_prefix bar)))
                          ^^^^^^^^^^^^^^^^^^^^^^^
  Error: This syntax is only available since version 3.11 of the dune language.
  Please update your dune-project file to have (lang dune 3.11).
  [1]

Basic example of using this feature:
  $ cat >dune-project <<EOF
  > (lang dune 3.11)
  > (package
  >  (name foo))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/foo/bar/a.txt" {"bar/a.txt"}
    "_build/install/default/share/foo/bar/b.txt" {"bar/b.txt"}
    "_build/install/default/share/foo/bar/c.txt" {"bar/c.txt"}
  ]

Test with multiple with_prefix blocks in a single files entry:
  $ cat >dune <<EOF
  > (install
  >  (files
  >   (glob_files (*.txt with_prefix txt))
  >   (glob_files (*.md with_prefix md)))
  >  (section share))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/foo/md/d.md" {"md/d.md"}
    "_build/install/default/share/foo/md/e.md" {"md/e.md"}
    "_build/install/default/share/foo/txt/a.txt" {"txt/a.txt"}
    "_build/install/default/share/foo/txt/b.txt" {"txt/b.txt"}
    "_build/install/default/share/foo/txt/c.txt" {"txt/c.txt"}
  ]

Use "." as the prefix:
  $ cat >dune <<EOF
  > (install
  >  (files (glob_files (*.txt with_prefix .)))
  >  (section share))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/foo/a.txt" {"./a.txt"}
    "_build/install/default/share/foo/b.txt" {"./b.txt"}
    "_build/install/default/share/foo/c.txt" {"./c.txt"}
  ]

Use a pform in the prefix:
  $ printf baz > prefix
  $ cat >dune <<EOF
  > (install
  >  (files (glob_files (*.txt with_prefix %{read:prefix})))
  >  (section share))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/foo/baz/a.txt" {"baz/a.txt"}
    "_build/install/default/share/foo/baz/b.txt" {"baz/b.txt"}
    "_build/install/default/share/foo/baz/c.txt" {"baz/c.txt"}
  ]

Use a prefix with a recursive glob:
  $ mkdir -p a/b/c
  $ touch a/x.txt a/b/y.txt a/b/c/z.txt
  $ cat >dune <<EOF
  > (install
  >  (files (glob_files_rec (*.txt with_prefix qux)))
  >  (section share))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/foo/qux/a.txt" {"qux/a.txt"}
    "_build/install/default/share/foo/qux/a/b/c/z.txt" {"qux/a/b/c/z.txt"}
    "_build/install/default/share/foo/qux/a/b/y.txt" {"qux/a/b/y.txt"}
    "_build/install/default/share/foo/qux/a/x.txt" {"qux/a/x.txt"}
    "_build/install/default/share/foo/qux/b.txt" {"qux/b.txt"}
    "_build/install/default/share/foo/qux/c.txt" {"qux/c.txt"}
  ]

Demonstrating behaviour of `with_prefix` on globs with a prefix:
  $ mkdir -p path/to/files
  $ touch path/to/files/foo.txt path/to/files/bar.txt path/to/files/baz.txt
  $ cat >dune <<EOF
  > (install
  >  (files (glob_files (path/to/files/*.txt with_prefix some/new/path/)))
  >  (section share))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/foo/some/new/path/bar.txt" {"some/new/path/bar.txt"}
    "_build/install/default/share/foo/some/new/path/baz.txt" {"some/new/path/baz.txt"}
    "_build/install/default/share/foo/some/new/path/foo.txt" {"some/new/path/foo.txt"}
  ]

Replacing the prefix with the empty string works the same as with ".".
  $ cat >dune <<EOF
  > (install
  >  (files (glob_files_rec (*.txt with_prefix "")))
  >  (section share))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/foo/a.txt"
    "_build/install/default/share/foo/a/b/c/z.txt" {"a/b/c/z.txt"}
    "_build/install/default/share/foo/a/b/y.txt" {"a/b/y.txt"}
    "_build/install/default/share/foo/a/x.txt" {"a/x.txt"}
    "_build/install/default/share/foo/b.txt"
    "_build/install/default/share/foo/c.txt"
    "_build/install/default/share/foo/path/to/files/bar.txt" {"path/to/files/bar.txt"}
    "_build/install/default/share/foo/path/to/files/baz.txt" {"path/to/files/baz.txt"}
    "_build/install/default/share/foo/path/to/files/foo.txt" {"path/to/files/foo.txt"}
  ]

It's an error to use an absolute path as the prefix.
  $ cat >dune <<EOF
  > (install
  >  (files (glob_files_rec (*.txt with_prefix /some/absolute/path)))
  >  (section share))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  File "dune", line 2, characters 43-62:
  2 |  (files (glob_files_rec (*.txt with_prefix /some/absolute/path)))
                                                 ^^^^^^^^^^^^^^^^^^^
  Error: Absolute paths are not allowed in the install stanza.
  [1]

The root directory is treated like an absolute path (ie. it's an error).
  $ cat >dune <<EOF
  > (install
  >  (files (glob_files_rec (*.txt with_prefix /)))
  >  (section share))
  > EOF
  $ dune build foo.install && cat _build/default/foo.install
  File "dune", line 2, characters 43-44:
  2 |  (files (glob_files_rec (*.txt with_prefix /)))
                                                 ^
  Error: Absolute paths are not allowed in the install stanza.
  [1]
