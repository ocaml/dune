  $ env OCAMLFIND_CONF=$PWD/etc/findlib.conf $JBUILDER build --root . -j1 -x foo file
      ocamldep bin/blah.depends.ocamldep-output
      ocamldep lib/p.depends.ocamldep-output
        ocamlc lib/p.{cmi,cmo,cmt}
        ocamlc bin/blah.{cmi,cmo,cmt}
      ocamlopt lib/p.{cmx,o}
      ocamlopt bin/blah.{cmx,o}
      ocamlopt lib/p.{a,cmxa}
      ocamlopt bin/blah.exe
          blah file [default.foo]
          blah file
  $ cat _build/default.foo/file
  42
