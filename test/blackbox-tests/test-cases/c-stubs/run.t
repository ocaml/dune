  $ $JBUILDER exec -j1 ./qnativerun/run.exe --display short --root .
      ocamldep qnativerun/run.ml.d
        ocamlc q/q_stub.o
      ocamldep q/q.ml.d
      ocamldep q/q.mli.d
    ocamlmklib q/dllq_stubs.so,q/libq_stubs.a
        ocamlc q/.q.objs/q.{cmi,cmti}
        ocamlc qnativerun/run.{cmi,cmo,cmt}
      ocamlopt q/.q.objs/q.{cmx,o}
      ocamlopt qnativerun/run.{cmx,o}
      ocamlopt q/q.{a,cmxa}
      ocamlopt qnativerun/run.exe
  42
#  $ $JBUILDER exec -j1 ./qbyterun/run.bc --display short --root .
