Including a file in the install stanza which includes another file

  $ make_install_include_project
  $ install_file=_build/default/hello.install

  $ cat >foo.sexp <<EOF
  > ((include bar.sexp))
  > EOF

  $ cat >bar.sexp <<EOF
  > (a.txt)
  > EOF

  $ touch a.txt

  $ dune build "$install_file"

  $ cat "$install_file"
  lib: [
    "_build/install/default/lib/hello/META"
    "_build/install/default/lib/hello/dune-package"
  ]
  bin: [
    "_build/install/default/bin/hello"
  ]
  share: [
    "_build/install/default/share/hello/a.txt"
  ]
