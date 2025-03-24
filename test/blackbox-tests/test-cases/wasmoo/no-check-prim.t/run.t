Compilation using WasmOO

  $ dune build --display short bin/technologic.bc.wasm.js @install  2>&1 | \
  > sed s,^\ *$(ocamlc -config-var c_compiler),\ \ C_COMPILER,g
  wasm_of_ocaml bin/.technologic.eobjs/jsoo/technologic.bc.runtime.wasma
      ocamldep bin/.technologic.eobjs/dune__exe__Technologic.impl.d
      ocamldep lib/.x.objs/x.impl.d
        ocamlc lib/.x.objs/byte/x__.{cmi,cmo,cmt}
      ocamldep lib/.x.objs/x__Y.impl.d
      ocamldep bin/.technologic.eobjs/dune__exe__Z.impl.d
      ocamlopt lib/.x.objs/native/x__.{cmx,o}
        ocamlc lib/.x.objs/byte/x__Y.{cmi,cmo,cmt}
  wasm_of_ocaml .js/default/js_of_ocaml-compiler.runtime/jsoo_runtime.wasma
  wasm_of_ocaml .js/default/js_of_ocaml/js_of_ocaml.wasma
  wasm_of_ocaml .js/default/stdlib/std_exit.wasmo
  wasm_of_ocaml .js/default/stdlib/stdlib.wasma
        ocamlc bin/.technologic.eobjs/byte/dune__exe.{cmi,cmo,cmt}
      ocamldep bin/.technologic.eobjs/dune__exe__Technologic.intf.d
      ocamlopt lib/.x.objs/native/x__Y.{cmx,o}
        ocamlc lib/.x.objs/byte/x.{cmi,cmo,cmt}
  wasm_of_ocaml bin/.technologic.eobjs/jsoo/dune__exe.wasmo
      ocamlopt lib/.x.objs/native/x.{cmx,o}
        ocamlc bin/.technologic.eobjs/byte/dune__exe__Technologic.{cmi,cmti}
        ocamlc lib/x.cma
        ocamlc bin/.technologic.eobjs/byte/dune__exe__Z.{cmi,cmo,cmt}
      ocamlopt lib/x.{a,cmxa}
  wasm_of_ocaml lib/.x.objs/jsoo/default/x.wasma
        ocamlc bin/.technologic.eobjs/byte/dune__exe__Technologic.{cmo,cmt}
  wasm_of_ocaml bin/.technologic.eobjs/jsoo/dune__exe__Z.wasmo
      ocamlopt lib/x.cmxs
  wasm_of_ocaml bin/.technologic.eobjs/jsoo/dune__exe__Technologic.wasmo
  wasm_of_ocaml bin/technologic.bc.wasm.{js,assets}
  $ node ./_build/default/bin/technologic.bc.wasm.js
  buy it
  use it
  break it
  fix it
  $ dune build bin/technologic.bc.wasm.js @install --profile release
  $ node ./_build/default/bin/technologic.bc.wasm.js
  buy it
  use it
  break it
  fix it
  $ cat >dune-workspace <<EOF
  > (lang dune 3.17)
  > (context
  >   (default (disable_dynamically_linked_foreign_archives true)))
  > EOF
  $ dune build bin/technologic.bc.wasm.js @install --profile dev
  $ dune build bin/technologic.bc.wasm.js @install --profile release
