In #5491 it is reported that Dune deletes .cmt files after successive builds. This test
reproduces the reported issue.

  $ cat > dune-project <<EOF
  > (lang dune 3.5)
  > EOF

  $ cat > dune <<EOF
  > (library
  >  (name ok))
  > EOF

  $ cat > ok.ml <<EOF
  > let ok _ = 1
  > EOF

  $ dune build ok.cmxa _build/default/
  $ find _build -name *.cm*
  _build/default/ok.cma
  _build/default/ok.cmxa
  _build/default/ok.cmxs
  _build/default/.ok.objs/byte/ok.cmt
  _build/default/.ok.objs/byte/ok.cmo
  _build/default/.ok.objs/byte/ok.cmi
  _build/default/.ok.objs/native/ok.cmx
  $ ls _build/default/.ok.objs/b/ok.cmt
  ls: cannot access '_build/default/.ok.objs/b/ok.cmt': No such file or directory
  [2]
  $ dune build ok.cmxa
  $ ls _build/default/.ok.objs/byte/ok.cmt
  _build/default/.ok.objs/byte/ok.cmt
