  $ $JBUILDER build -j1 --root . --display quiet
        ocamlc test.bc.o (exit 2)
  (cd _build/default && /home/jdimino/code/opam-root/4.05.0/bin/ocamlc.opt -w -40 -g -o test.bc.o -output-complete-obj .test.eobjs/test.cmo)
  File "_none_", line 1:
  Error: Cannot overwrite existing file /tmp/camlobj665e96.c
  [1]
  $ $JBUILDER build -j1 --root . --display quiet stub_native
  $ $JBUILDER build -j1 --root . --display quiet stub_native
  $ $JBUILDER build -j1 --root . --display quiet stub_byte
        ocamlc test.bc.o (exit 2)
  (cd _build/default && /home/jdimino/code/opam-root/4.05.0/bin/ocamlc.opt -w -40 -g -o test.bc.o -output-complete-obj .test.eobjs/test.cmo)
  File "_none_", line 1:
  Error: Cannot overwrite existing file /tmp/camlobj540fc0.c
  [1]
  $ $JBUILDER build -j1 --root . --display quiet stub_byte
        ocamlc test.bc.o (exit 2)
  (cd _build/default && /home/jdimino/code/opam-root/4.05.0/bin/ocamlc.opt -w -40 -g -o test.bc.o -output-complete-obj .test.eobjs/test.cmo)
  File "_none_", line 1:
  Error: Cannot overwrite existing file /tmp/camlobjf732ed.c
  [1]
