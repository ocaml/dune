NB: warning 58 is caused by ocaml/ocamlfind#78.

  $ dune build
  File "_none_", line 1:
  Warning 58 [no-cmx-file]: no cmx file was found in path for module Findlib, and its interface was not compiled with -opaque
  File "_none_", line 1:
  Warning 58 [no-cmx-file]: no cmx file was found in path for module Findlib, and its interface was not compiled with -opaque
  File "_none_", line 1:
  Warning 58 [no-cmx-file]: no cmx file was found in path for module Findlib, and its interface was not compiled with -opaque
  File "_none_", line 1:
  Warning 58 [no-cmx-file]: no cmx file was found in path for module Fl_dynload, and its interface was not compiled with -opaque
  
  File "_none_", line 1:
  Warning 58 [no-cmx-file]: no cmx file was found in path for module Fl_package_base, and its interface was not compiled with -opaque
  File "_none_", line 1:
  Warning 58 [no-cmx-file]: no cmx file was found in path for module Findlib, and its interface was not compiled with -opaque
  
  File "_none_", line 1:
  Warning 58 [no-cmx-file]: no cmx file was found in path for module Fl_dynload, and its interface was not compiled with -opaque
  
  File "_none_", line 1:
  Warning 58 [no-cmx-file]: no cmx file was found in path for module Fl_package_base, and its interface was not compiled with -opaque
  File "_none_", line 1:
  Warning 58 [no-cmx-file]: no cmx file was found in path for module Fl_dynload, and its interface was not compiled with -opaque
  
  File "_none_", line 1:
  Warning 58 [no-cmx-file]: no cmx file was found in path for module Fl_package_base, and its interface was not compiled with -opaque

  $ dune exec mytool
  m: init

  $ dune exec mytool inexistent
  m: init
  The package "inexistent" can't be found.

  $ dune exec mytool a
  m: init
  a: init

  $ dune exec mytool_modes_byte a
  m: init
  a: init

  $ dune exec mytool mytool-plugin-b
  m: init
  a: init
  b: init
  b: registering
  b: called
  a: called

  $ dune exec mytool mytool-plugin-b a
  m: init
  a: init
  b: init
  b: registering
  b: called
  a: called

  $ dune exec mytool_with_a
  a: init
  m: init

  $ dune exec mytool_with_a mytool-plugin-b
  a: init
  m: init
  b: init
  b: registering
  b: called
  a: called

  $ dune exec mytool_with_a a mytool-plugin-b
  a: init
  m: init
  b: init
  b: registering
  b: called
  a: called

  $ dune exec mytool_auto
  m: init
  a: init
  b: init
  b: registering
  b: called
  a: called

  $ dune exec mytool c_thread
  m: init
  c_thread: registering

  $ cat _build/default/.main.eobjs/findlib_initl.ml-gen
  Findlib.record_package Findlib.Record_core "dynlink";;
  Findlib.record_package Findlib.Record_core "findlib";;
  Findlib.record_package Findlib.Record_core "findlib.dynload";;
  Findlib.record_package Findlib.Record_core "findlib.internal";;
  Findlib.record_package Findlib.Record_core "mytool";;
  Findlib.record_package Findlib.Record_core "threads";;
  Findlib.record_package Findlib.Record_core "unix";;
  let preds =
    [ "mt"
    ; "mt_posix"
    ; "ppx_driver"
    ]
  in
  let preds =
    (if Dynlink.is_native then "native" else "byte") :: preds
  in
  Findlib.record_package_predicates preds;;
