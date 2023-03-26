Test virtual lib in an exe / melange environment

The native build passes

  $ dune exec ./ml.exe --display=short
      ocamldep impl_ml/.impl_ml.objs/virt.impl.d
      ocamldep vlib/.vlib.objs/shared.impl.d
      ocamldep vlib/.vlib.objs/virt.intf.d
      ocamldep vlib/.vlib.objs/vlib_impl.impl.d
        ocamlc vlib/.vlib.objs/byte/virt.{cmi,cmti}
        ocamlc vlib/.vlib.objs/byte/vlib_impl.{cmi,cmo,cmt}
      ocamlopt vlib/.vlib.objs/native/vlib_impl.{cmx,o}
        ocamlc vlib/.vlib.objs/byte/shared.{cmi,cmo,cmt}
        ocamlc .ml.eobjs/byte/dune__exe__Ml.{cmi,cmti}
      ocamlopt vlib/.vlib.objs/native/shared.{cmx,o}
      ocamlopt impl_ml/.impl_ml.objs/native/virt.{cmx,o}
      ocamlopt .ml.eobjs/native/dune__exe__Ml.{cmx,o}
      ocamlopt impl_ml/impl_ml.{a,cmxa}
      ocamlopt ml.exe
  Hello from ml

Melange can't produce a `.cmj` solely from a virtual module `.cmi`, because it
needs to consult the `.cmj` files of dependencies to know where the require
call should be emitted

  $ dune build @mel --display=short 2>&1 | grep -v 'node_modules/melange'
      ocamldep impl_melange/.impl_melange.objs/virt.impl.d
          melc vlib/.vlib.objs/melange/virt.{cmi,cmti}
          melc vlib/.vlib.objs/melange/vlib_impl.{cmi,cmj,cmt} (exit 2)
  File "vlib/vlib_impl.ml", line 1:
  Error: Virt not found, it means either the module does not exist or it is a namespace

  $ output=_build/default/output/mel.js
  $ test -f "$output" && node "$output"
  [1]

