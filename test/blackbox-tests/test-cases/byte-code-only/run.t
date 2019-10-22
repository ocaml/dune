  $ env ORIG_PATH="$PATH" PATH="$PWD/ocaml-bin:$PATH" dune build @all --display short
  File "bin-with-build-info/dune", line 2, characters 7-20:
  2 |  (name print_version)
             ^^^^^^^^^^^^^
  Error: No rule found for
  bin-with-build-info/.print_version.eobjs/native/build_info__Build_info_data$ext_obj
        ocamlc build-info/.build_info.objs/byte/build_info.{cmi,cmo,cmt}
        ocamlc build-info/build_info.cma
      ocamldep build-info/.build_info.objs/build_info_data.mli.d
        ocamlc build-info/.build_info.objs/byte/build_info__Build_info_data.{cmi,cmti}
        ocamlc bin-with-build-info/.print_version.eobjs/byte/build_info__Build_info_data.{cmo,cmt}
      ocamldep bin-with-build-info/.print_version.eobjs/print_version.ml.d
        ocamlc bin-with-build-info/.print_version.eobjs/byte/dune__exe__Print_version.{cmi,cmo,cmt}
      ocamldep bin/.toto.eobjs/toto.ml.d
        ocamlc bin/.toto.eobjs/byte/dune__exe__Toto.{cmi,cmo,cmt}
        ocamlc bin/toto.bc
      ocamldep src/.foo.objs/foo.ml.d
        ocamlc src/.foo.objs/byte/foo.{cmi,cmo,cmt}
        ocamlc src/foo.cma
        ocamlc bin/toto.exe
  [1]

Check that building a native only executable fails
  $ env ORIG_PATH="$PATH" PATH="$PWD/ocaml-bin:$PATH" dune build --display short native-only/foo.exe
  Error: Don't know how to build native-only/foo.exe
  [1]
