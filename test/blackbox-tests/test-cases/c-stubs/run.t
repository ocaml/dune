  $ dune exec ./qnativerun/run.exe --display short
        ocamlc q/q_stub.o
    ocamlmklib q/dllq_stubs.so,q/libq_stubs.a
      ocamldep q/.q.objs/q.mli.d
        ocamlc q/.q.objs/byte/q.{cmi,cmti}
      ocamldep q/.q.objs/q.ml.d
      ocamlopt q/.q.objs/native/q.{cmx,o}
      ocamlopt q/q.{a,cmxa}
      ocamldep qnativerun/.run.eobjs/run.ml.d
        ocamlc qnativerun/.run.eobjs/byte/run.{cmi,cmo,cmt}
      ocamlopt qnativerun/.run.eobjs/native/run.{cmx,o}
      ocamlopt qnativerun/run.exe
  42
#  $ dune exec ./qbyterun/run.bc --display short
