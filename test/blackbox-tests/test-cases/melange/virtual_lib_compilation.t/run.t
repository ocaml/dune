Test virtual lib in an exe / melange environment

  $ dune build @melange --display=short
      ocamldep impl_melange/.impl_melange.objs/virt.impl.d
      ocamldep vlib/.vlib.objs/shared.impl.d
      ocamldep vlib/.vlib.objs/virt.intf.d
      ocamldep vlib/.vlib.objs/vlib_impl.impl.d
          melc vlib/.vlib.objs/melange/virt.{cmi,cmti}
          melc vlib/.vlib.objs/melange/vlib_impl.{cmi,cmj,cmt}
          melc vlib/.vlib.objs/melange/shared.{cmi,cmj,cmt}
          melc output/vlib/vlib_impl.js
          melc impl_melange/.impl_melange.objs/melange/virt.{cmj,cmt}
          melc output/vlib/shared.js
          melc .output.mobjs/melange/melange__Mel.{cmi,cmj,cmt}
          melc output/impl_melange/virt.js
          melc output/mel.js
  $ output=_build/default/output/mel.js
  $ test -f "$output" && node "$output"
  melange

  $ dune exec ./ml.exe --display=short
      ocamldep impl_ml/.impl_ml.objs/virt.impl.d
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
  ml
  $ ls _build/default/vlib/.vlib.objs/byte
  shared.cmi
  shared.cmo
  shared.cmt
  virt.cmi
  virt.cmti
  vlib_impl.cmi
  vlib_impl.cmo
  vlib_impl.cmt
