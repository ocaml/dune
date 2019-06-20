  $ dune build 2>&1 | grep -v ocamlc
      ocamlopt lib/mylib.{a,cmxa} (exit 2)
  (cd _build/default && /Users/rgrinberg/.opam/4.07.1/bin/ocamlopt.opt -w @a-4-29-40-41-42-44-45-48-58-59-60-66-40 -strict-sequence -strict-formats -short-paths -keep-locs -g -a -o lib/mylib.cmxa lib/.mylib.objs/native/mylib__.cmx lib/.mylib.objs/native/mylib__Bar.cmx lib/.mylib.objs/native/mylib__Foo.cmx lib/.mylib.objs/native/mylib.cmx lib/.mylib.objs/native/bar.cmx lib/.mylib.objs/native/foo.cmx lib/.mylib.objs/native/intf_only.cmx lib/.mylib.objs/native/bar.cmx lib/.mylib.objs/native/foo.cmx lib/.mylib.objs/native/intf_only.cmx)
  File "_none_", line 1:
  Error: Files lib/.mylib.objs/native/bar.cmx
         and lib/.mylib.objs/native/bar.cmx both define a module named Bar
