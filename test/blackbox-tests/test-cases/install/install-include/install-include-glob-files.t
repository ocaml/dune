Test that `(include ...)` composes with `(glob_files ...)`

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (package (name foo))
  > EOF

  $ cat >dune <<EOF
  > (install
  >  (files
  >   (include foo.sexp))
  >  (section share))
  > EOF

  $ cat >foo.sexp <<EOF
  > ((glob_files dir1/*.txt)
  >  (glob_files_rec dir2/*.txt))
  > EOF

  $ mkdir -p dir1 dir2/foo/bar
  $ touch dir1/a.txt dir1/b.txt dir2/c.txt dir2/foo/d.txt dir2/foo/bar/e.txt

  $ dune build @install

  $ cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
  ]
  share: [
    "_build/install/default/share/foo/dir1/a.txt" {"dir1/a.txt"}
    "_build/install/default/share/foo/dir1/b.txt" {"dir1/b.txt"}
    "_build/install/default/share/foo/dir2/c.txt" {"dir2/c.txt"}
    "_build/install/default/share/foo/dir2/foo/bar/e.txt" {"dir2/foo/bar/e.txt"}
    "_build/install/default/share/foo/dir2/foo/d.txt" {"dir2/foo/d.txt"}
  ]
