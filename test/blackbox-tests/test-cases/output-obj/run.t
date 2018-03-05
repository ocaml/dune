  $ $JBUILDER build -j1 --root . --display quiet
        ocamlc test.bc.o (exit 2)
  (cd _build/default && /home/jdimino/code/opam-root/4.05.0/bin/ocamlc.opt -w -40 -g -o test.bc.o -output-complete-obj .test.eobjs/test.cmo)
  [1mFile "[1m_none_", line 1[0m[0m:
  [1;31mError[0m: Cannot overwrite existing file /tmp/camlobjee8643.c
        ocamlc test.exe.o (exit 2)
  (cd _build/default && /home/jdimino/code/opam-root/4.05.0/bin/ocamlc.opt -w -40 -g -o test.exe.o -output-complete-obj .test.eobjs/test.cmo)
  [1mFile "[1m_none_", line 1[0m[0m:
  [1;31mError[0m: Cannot overwrite existing file /tmp/camlobj39c5f6.c
  [1]
  $ $JBUILDER build -j1 --root . --display quiet stub_native
        ocamlc test.exe.o (exit 2)
  (cd _build/default && /home/jdimino/code/opam-root/4.05.0/bin/ocamlc.opt -w -40 -g -o test.exe.o -output-complete-obj .test.eobjs/test.cmo)
  [1mFile "[1m_none_", line 1[0m[0m:
  [1;31mError[0m: Cannot overwrite existing file /tmp/camlobjb0b2af.c
  [1]
  $ $JBUILDER build -j1 --root . --display quiet stub_native
        ocamlc test.exe.o (exit 2)
  (cd _build/default && /home/jdimino/code/opam-root/4.05.0/bin/ocamlc.opt -w -40 -g -o test.exe.o -output-complete-obj .test.eobjs/test.cmo)
  [1mFile "[1m_none_", line 1[0m[0m:
  [1;31mError[0m: Cannot overwrite existing file /tmp/camlobjea0514.c
  [1]
  $ $JBUILDER build -j1 --root . --display quiet stub_byte
        ocamlc test.bc.o (exit 2)
  (cd _build/default && /home/jdimino/code/opam-root/4.05.0/bin/ocamlc.opt -w -40 -g -o test.bc.o -output-complete-obj .test.eobjs/test.cmo)
  [1mFile "[1m_none_", line 1[0m[0m:
  [1;31mError[0m: Cannot overwrite existing file /tmp/camlobj6c7651.c
  [1]
  $ $JBUILDER build -j1 --root . --display quiet stub_byte
        ocamlc test.bc.o (exit 2)
  (cd _build/default && /home/jdimino/code/opam-root/4.05.0/bin/ocamlc.opt -w -40 -g -o test.bc.o -output-complete-obj .test.eobjs/test.cmo)
  [1mFile "[1m_none_", line 1[0m[0m:
  [1;31mError[0m: Cannot overwrite existing file /tmp/camlobj798273.c
  [1]
