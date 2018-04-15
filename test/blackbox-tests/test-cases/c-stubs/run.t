  $ jbuilder exec ./qnativerun/run.exe --display short
      ocamldep qnativerun/run.ml.d
        ocamlc q/q_stub.o
    ocamlmklib q/dllq_stubs.so,q/libq_stubs.a
      ocamldep q/q.ml.d
      ocamldep q/q.mli.d
        ocamlc q/.q.objs/q.{cmi,cmti}
      ocamlopt q/.q.objs/q.{cmx,o}
      ocamlopt q/q.{a,cmxa}
        ocamlc qnativerun/.run.eobjs/run.{cmi,cmo,cmt}
      ocamlopt qnativerun/.run.eobjs/run.{cmx,o}
      ocamlopt qnativerun/run.exe
  42
#  $ jbuilder exec ./qbyterun/run.bc --display short
