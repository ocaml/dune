  $ rm -f /tmp/camlobj*
  $ $JBUILDER build -j1 --root . --display short
      ocamldep test.ml.d
        ocamlc .test.eobjs/test.{cmi,cmo,cmt}
        ocamlc test.bc.o
           gcc stub_byte (exit 1)
  (cd _build/default && /home/jdimino/code/linuxbrew/bin/gcc -std=gnu99 -O2 -fno-strict-aliasing -fwrapv -fno-builtin-memcmp -fPIC -o stub_byte -I /home/jdimino/code/opam-root/4.06.0/lib/ocaml -I . test.bc.o -lm -ldl stub.c)
  test.bc.o: In function `caml_terminfo_setup':
  /usr/local/home/jdimino/opam-root/4.06.0/build/ocaml/byterun/terminfo.c:56: undefined reference to `tgetent'
  /usr/local/home/jdimino/opam-root/4.06.0/build/ocaml/byterun/terminfo.c:58: undefined reference to `tgetnum'
  /usr/local/home/jdimino/opam-root/4.06.0/build/ocaml/byterun/terminfo.c:59: undefined reference to `tgetstr'
  /usr/local/home/jdimino/opam-root/4.06.0/build/ocaml/byterun/terminfo.c:60: undefined reference to `tgetstr'
  /usr/local/home/jdimino/opam-root/4.06.0/build/ocaml/byterun/terminfo.c:61: undefined reference to `tgetstr'
  /usr/local/home/jdimino/opam-root/4.06.0/build/ocaml/byterun/terminfo.c:62: undefined reference to `tgetstr'
  /usr/local/home/jdimino/opam-root/4.06.0/build/ocaml/byterun/terminfo.c:64: undefined reference to `tgetstr'
  test.bc.o:/usr/local/home/jdimino/opam-root/4.06.0/build/ocaml/byterun/terminfo.c:65: more undefined references to `tgetstr' follow
  test.bc.o: In function `caml_terminfo_backup':
  /usr/local/home/jdimino/opam-root/4.06.0/build/ocaml/byterun/terminfo.c:88: undefined reference to `tputs'
  test.bc.o: In function `caml_terminfo_standout':
  /usr/local/home/jdimino/opam-root/4.06.0/build/ocaml/byterun/terminfo.c:95: undefined reference to `tputs'
  test.bc.o: In function `caml_terminfo_resume':
  /usr/local/home/jdimino/opam-root/4.06.0/build/ocaml/byterun/terminfo.c:104: undefined reference to `tputs'
  collect2: error: ld returned 1 exit status
      ocamlopt .test.eobjs/test.{cmx,o}
      ocamlopt test.exe.o
           gcc stub_native
  [1]

$ $JBUILDER build -j1 --root . --display quiet stub_native
$ $JBUILDER build -j1 --root . --display quiet stub_native
$ $JBUILDER build -j1 --root . --display quiet stub_byte
$ $JBUILDER build -j1 --root . --display quiet stub_byte
