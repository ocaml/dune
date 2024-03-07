Test virtual lib in an exe / melange environment

The native build passes

  $ dune exec ./ml.exe --display=short
      ocamldep vlib/.vlib.objs/shared.impl.d
      ocamldep vlib/.vlib.objs/virt.intf.d
      ocamldep vlib/.vlib.objs/vlib_impl.impl.d
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
  Hello from ml

Any module requiring a virtual module (including modules within the virtual
library itself) needs to consult the `.cmj` file for the concrete
implementation being seleced to know where to `import` from in the generated
JS. The following build works because Dune tracks concrete implementation
`.cmj` files as dependencies of the JS rules.

  $ dune build @mel --display=short 2>&1 | grep -v 'node_modules/melange'
          melc vlib/.vlib.objs/melange/virt.{cmi,cmti}
      ocamldep impl_melange/.impl_melange.objs/virt.impl.d
          melc vlib/.vlib.objs/melange/vlib_impl.{cmi,cmj,cmt}
          melc vlib/.vlib.objs/melange/shared.{cmi,cmj,cmt}
          melc impl_melange/.impl_melange.objs/melange/virt.{cmj,cmt}
          melc .output.mobjs/melange/melange__Mel.{cmi,cmj,cmt}
          melc output/impl_melange/virt.js
          melc output/vlib/shared.js
          melc output/vlib/vlib_impl.js
          melc output/mel.js

  $ output=_build/default/output/mel.js
  $ test -f "$output" && node "$output"
  Hello from melange

