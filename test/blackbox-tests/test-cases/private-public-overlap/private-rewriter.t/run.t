On the other hand, public libraries may have private preprocessors
  $ dune build --display short
        ocamlc .ppx_internal.objs/byte/ppx_internal.{cmi,cmo,cmt}
        ocamlc .ppx/be26d3600214af2fa78c2c9ef25e9069/dune__exe___ppx.{cmi,cmo}
      ocamlopt .ppx_internal.objs/native/ppx_internal.{cmx,o}
      ocamlopt .ppx/be26d3600214af2fa78c2c9ef25e9069/dune__exe___ppx.{cmx,o}
      ocamlopt ppx_internal.{a,cmxa}
      ocamlopt .ppx/be26d3600214af2fa78c2c9ef25e9069/ppx.exe
           ppx mylib.pp.ml
        ocamlc .mylib.objs/byte/mylib.{cmi,cmo,cmt}
      ocamlopt .mylib.objs/native/mylib.{cmx,o}
        ocamlc mylib.cma
      ocamlopt mylib.{a,cmxa}
      ocamlopt mylib.cmxs
