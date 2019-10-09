Reproduction of https://github.com/ocaml/dune/issues/1895

An implementation and a virtual library both define the same (non virtual
) module. However, the module is private in the public library hence it should
just quietly shadow the module in the virtual library.

  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ mkdir vlib impl
  $ touch impl/dom.ml vlib/dom.mli vlib/overlap.ml impl/overlap.ml
  $ cat > vlib/dune <<EOF
  > (library
  >  (name vlib1855)
  >  (virtual_modules dom))
  > EOF
  $ cat > impl/dune <<EOF
  > (library
  >  (name impl1855)
  >  (implements vlib1855)
  >  (private_modules overlap))
  > EOF

  $ dune build @all --display short
        ocamlc vlib/.vlib1855.objs/byte/vlib1855.{cmi,cmo,cmt}
      ocamlopt vlib/.vlib1855.objs/native/vlib1855.{cmx,o}
      ocamldep vlib/.vlib1855.objs/dom.mli.d
        ocamlc vlib/.vlib1855.objs/byte/vlib1855__Dom.{cmi,cmti}
      ocamldep vlib/.vlib1855.objs/overlap.ml.d
        ocamlc vlib/.vlib1855.objs/byte/vlib1855__Overlap.{cmi,cmo,cmt}
      ocamlopt vlib/.vlib1855.objs/native/vlib1855__Overlap.{cmx,o}
      ocamldep impl/.impl1855.objs/dom.ml.d
        ocamlc impl/.impl1855.objs/byte/vlib1855__impl1855__.{cmi,cmo,cmt}
        ocamlc impl/.impl1855.objs/byte/vlib1855__Dom.{cmo,cmt}
      ocamldep impl/.impl1855.objs/overlap.ml.d
        ocamlc impl/.impl1855.objs/byte/vlib1855__impl1855____Overlap.{cmi,cmo,cmt}
        ocamlc impl/impl1855.cma
      ocamlopt impl/.impl1855.objs/native/vlib1855__Dom.{cmx,o}
      ocamlopt impl/.impl1855.objs/native/vlib1855__impl1855__.{cmx,o}
      ocamlopt impl/.impl1855.objs/native/vlib1855__impl1855____Overlap.{cmx,o}
      ocamlopt impl/impl1855.{a,cmxa}
      ocamlopt impl/impl1855.cmxs
