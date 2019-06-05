  $ dune build @install --display short
      ocamlopt mli-only/foobar.{a,cmxa}
      ocamlopt mli-only/foobar.cmxs
        ocamlc mli-only/foobar.cma
      ocamldep mli-only/.foobar.objs/foobar.mli.d
        ocamlc mli-only/.foobar.objs/byte/foobar.{cmi,cmti}
      ocamldep impl/.foobar_impl.objs/foobar.mli.d
        ocamlc impl/.foobar_impl.objs/byte/foobar.{cmi,cmti}
      ocamldep impl/.foobar_impl.objs/foobar.ml.d
      ocamlopt impl/.foobar_impl.objs/native/foobar.{cmx,o}
      ocamlopt impl/foobar_impl.{a,cmxa}
      ocamlopt impl/foobar_impl.cmxs
        ocamlc impl/.foobar_impl.objs/byte/foobar.{cmo,cmt}
        ocamlc impl/foobar_impl.cma
  $ dune runtest
          test alias test/runtest
  testing
  $ dune install --prefix ./installed
  Installing installed/lib/foobar/META
  Installing installed/lib/foobar/dune-package
  Installing installed/lib/foobar/foobar$ext_lib
  Installing installed/lib/foobar/foobar.cma
  Installing installed/lib/foobar/foobar.cmi
  Installing installed/lib/foobar/foobar.cmti
  Installing installed/lib/foobar/foobar.cmxa
  Installing installed/lib/foobar/foobar.cmxs
  Installing installed/lib/foobar/foobar.mli
  Installing installed/lib/foobar/impl/foobar.cmi
  Installing installed/lib/foobar/impl/foobar.cmt
  Installing installed/lib/foobar/impl/foobar.cmti
  Installing installed/lib/foobar/impl/foobar.cmx
  Installing installed/lib/foobar/impl/foobar.ml
  Installing installed/lib/foobar/impl/foobar.mli
  Installing installed/lib/foobar/impl/foobar_impl$ext_lib
  Installing installed/lib/foobar/impl/foobar_impl.cma
  Installing installed/lib/foobar/impl/foobar_impl.cmxa
  Installing installed/lib/foobar/impl/foobar_impl.cmxs
  Installing installed/lib/foobar/opam
