  $ env ORIG_PATH="$PATH" PATH="$PWD/ocaml-bin:$PATH" dune build @all --display short
      ocamldep bin/.toto.eobjs/toto.ml.d
      ocamldep src/.foo.objs/foo.ml.d
        ocamlc build-info/.build_info.objs/byte/build_info.{cmi,cmo,cmt}
      ocamldep build-info/.build_info.objs/build_info_data.mli.d
      ocamldep bin-with-build-info/.print_version.eobjs/print_version.ml.d
        ocamlc bin/.toto.eobjs/byte/dune__exe__Toto.{cmi,cmo,cmt}
        ocamlc src/.foo.objs/byte/foo.{cmi,cmo,cmt}
        ocamlc build-info/build_info.cma
        ocamlc build-info/.build_info.objs/byte/build_info__Build_info_data.{cmi,cmti}
        ocamlc bin/toto.exe
        ocamlc bin/toto.bc
        ocamlc src/foo.cma
        ocamlc bin-with-build-info/.print_version.eobjs/byte/build_info__Build_info_data.{cmo,cmt}
        ocamlc bin-with-build-info/.print_version.eobjs/byte/dune__exe__Print_version.{cmi,cmo,cmt}
        ocamlc bin-with-build-info/print_version.bc
        ocamlc bin-with-build-info/print_version.exe

  $ _build/default/bin-with-build-info/print_version.exe
  <version missing>

Check that building a native only executable fails
  $ env ORIG_PATH="$PATH" PATH="$PWD/ocaml-bin:$PATH" dune build native-only/foo.exe
  Error: Don't know how to build native-only/foo.exe
  [1]
