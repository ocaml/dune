  $ (cd vendor; opam source ppxlib.0.29.1)
  Successfully extracted to $TESTCASE_ROOT/vendor/ppxlib.0.29.1
  $ dune build ./hello.exe
  File "vendor/ppxlib.0.29.1/traverse/dune", line 9, characters 7-22:
  9 |   (pps ppxlib_metaquot)))
             ^^^^^^^^^^^^^^^
  Error: Library "ppxlib_metaquot" not found.
  -> required by
     _build/default/vendor/ppxlib.0.29.1/traverse/ppxlib_traverse.pp.ml
  -> required by
     _build/default/vendor/ppxlib.0.29.1/traverse/.ppxlib_traverse.objs/byte/ppxlib_traverse.cmi
  -> required by _build/default/.hello.eobjs/byte/dune__exe__Hello.cmi
  -> required by _build/default/.hello.eobjs/native/dune__exe__Hello.cmx
  -> required by _build/default/hello.exe
  [1]
