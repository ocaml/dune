Compilation using WasmOO

  $ dune build --display short bin/technologic.bc.wasm.js @install  2>&1 | \
  > sed s,^\ *$(ocamlc -config-var c_compiler),\ \ C_COMPILER,g
      ocamldep bin/.technologic.eobjs/dune__exe__Technologic.impl.d
      ocamldep lib/.x.objs/x.impl.d
        ocamlc lib/.x.objs/byte/x__.{cmi,cmo,cmt}
      ocamldep lib/.x.objs/x__Y.impl.d
  wasm_of_ocaml bin/.technologic.eobjs/jsoo/technologic.bc.runtime.wasma
      ocamldep bin/.technologic.eobjs/dune__exe__Z.impl.d
      ocamlopt lib/.x.objs/native/x__.{cmx,o}
        ocamlc lib/.x.objs/byte/x__Y.{cmi,cmo,cmt}
  wasm_of_ocaml .js/default/stdlib/stdlib.wasma
  wasm_of_ocaml .js/default/stdlib/std_exit.wasmo
        ocamlc bin/.technologic.eobjs/byte/dune__exe.{cmi,cmo,cmt}
      ocamldep bin/.technologic.eobjs/dune__exe__Technologic.intf.d
      ocamlopt lib/.x.objs/native/x__Y.{cmx,o}
        ocamlc lib/.x.objs/byte/x.{cmi,cmo,cmt}
  wasm_of_ocaml .js/default/js_of_ocaml-compiler.runtime/jsoo_runtime.wasma
      ocamlopt lib/.x.objs/native/x.{cmx,o}
        ocamlc bin/.technologic.eobjs/byte/dune__exe__Z.{cmi,cmo,cmt}
        ocamlc lib/x.cma
        ocamlc bin/.technologic.eobjs/byte/dune__exe__Technologic.{cmi,cmti}
  wasm_of_ocaml .js/default/js_of_ocaml/js_of_ocaml.wasma
      ocamlopt lib/x.{a,cmxa}
        ocamlc bin/.technologic.eobjs/byte/dune__exe__Technologic.{cmo,cmt}
  wasm_of_ocaml lib/.x.objs/jsoo/default/x.wasma
      ocamlopt lib/x.cmxs
  wasm_of_ocaml bin/.technologic.eobjs/jsoo/dune__exe.wasmo
  wasm_of_ocaml bin/.technologic.eobjs/jsoo/dune__exe__Z.wasmo
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
