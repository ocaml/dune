  $ dune exec ./qnativerun/run.exe --display short
        ocamlc q/q_stub$ext_obj
      ocamldep q/.q.objs/q.mli.d
      ocamldep q/.q.objs/q.ml.d
      ocamldep qnativerun/.run.eobjs/run.ml.d
    ocamlmklib q/dllq_stubs$ext_dll,q/libq_stubs$ext_lib
        ocamlc q/.q.objs/byte/q.{cmi,cmti}
      ocamlopt q/.q.objs/native/q.{cmx,o}
      ocamlopt q/q.{a,cmxa}
        ocamlc qnativerun/.run.eobjs/byte/run.{cmi,cmo,cmt}
      ocamlopt qnativerun/.run.eobjs/native/run.{cmx,o}
      ocamlopt qnativerun/run.exe
  42
#  $ dune exec ./qbyterun/run.bc --display short
