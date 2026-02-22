  $ dune init proj oxcaml-test .
  Success: initialized project component named oxcaml-test
  $ sed -i -e 's/depends ocaml/depends (ocaml-variants (= 5.2.0+ox))/' dune-project
  $ cat > dune-workspace <<EOF
  > (lang dune 3.21)
  > 
  > (pkg enabled)
  > 
  > (repository
  >   (name oxcaml)
  >   (url git+https://github.com/oxcaml/opam-repository))
  > 
  > (lock_dir (repositories overlay oxcaml upstream))
  $ dune build
  autoconf (GNU Autoconf) 2.72
  Copyright (C) 2023 Free Software Foundation, Inc.
  License GPLv3+/Autoconf: GNU GPL version 3 or later
  <https://gnu.org/licenses/gpl.html>, <https://gnu.org/licenses/exceptions.html>
  This is free software: you are free to change and redistribute it.
  There is NO WARRANTY, to the extent permitted by law.
  
  Written by David J. MacKenzie and Akim Demaille.
  /usr/bin/which
  Error:
  open(_build/.sandbox/872740fa0e754203dd645f09218586ef/_private/default/.pkg/ocaml-variants.5.2.0+ox-e8081e89b062d32c64c33444259d0956/source/ignore-opam.patch): No such file or directory
  -> required by
     _build/_private/default/.pkg/ocaml-variants.5.2.0+ox-e8081e89b062d32c64c33444259d0956/target/cookie
  -> required by loading the OCaml compiler for context "default"
  [1]
