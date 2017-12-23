  $ $JBUILDER exec -j1 ./qnativerun/run.exe --root .
      ocamldep qnativerun/run.depends.ocamldep-output
        ocamlc q/q_stub.o
      ocamldep q/q.depends.ocamldep-output
      ocamldep q/q.dependsi.ocamldep-output
    ocamlmklib q/dllq_stubs.so,q/libq_stubs.a
        ocamlc q/q.{cmi,cmti}
        ocamlc qnativerun/run.{cmi,cmo,cmt}
      ocamlopt q/q.{cmx,o}
      ocamlopt qnativerun/run.{cmx,o}
      ocamlopt q/q.{a,cmxa}
      ocamlopt qnativerun/run.exe
  42
#  $ $JBUILDER exec -j1 ./qbyterun/run.bc --root .
