  $ ocamlc_where="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/OCAMLC_WHERE=$ocamlc_where:$BUILD_PATH_PREFIX_MAP"
  $ ocamlfind_libs="$(ocamlfind printconf path | while read line; do printf lib=${line}:; done)"
  $ export BUILD_PATH_PREFIX_MAP="$ocamlfind_libs:$BUILD_PATH_PREFIX_MAP"
  $ melc_compiler="$(which melc)"
  $ export BUILD_PATH_PREFIX_MAP="/MELC_COMPILER=$melc_compiler:$BUILD_PATH_PREFIX_MAP"

CRAM sanitization
  $ dune build exe/.merlin-conf/exe-x --profile release
  $ dune ocaml merlin dump-config --format=json exe | jq -r '
  >   include "dune";
  >   .[]
  >   | select(.module_name == "X" and (.source_path | startswith("default/exe/")))
  >   | merlinJsonEntryWithConfigNames(["SUFFIX", "READER"])'
  X: _build/default/exe/x
  ["SUFFIX",".mlx .mlx"]
  X: _build/default/exe/x.mlx
  ["SUFFIX",".mlx .mlx"]
  ["READER",["mlx"]]
  X: _build/default/exe/x.mlx.mli
  ["SUFFIX",".mlx .mlx"]

CRAM sanitization
  $ dune build lib/.merlin-conf/lib-x --profile release
  $ dune ocaml merlin dump-config --format=json lib | jq -r '
  >   include "dune";
  >   .[]
  >   | select(.module_name == "X" and (.source_path | startswith("default/lib/")))
  >   | merlinJsonEntryWithConfigNames(["SUFFIX", "READER"])'
  X: _build/default/lib/x
  ["SUFFIX",".mlx .mlx"]
  ["READER",["mlx"]]
  X: _build/default/lib/x.mlx
  ["SUFFIX",".mlx .mlx"]
  ["READER",["mlx"]]

CRAM sanitization
  $ dune build melange/.merlin-conf/lib-x_mel --profile release
  $ dune ocaml merlin dump-config --format=json melange | jq -r '
  >   include "dune";
  >   .[]
  >   | select(.module_name == "X_mel" and (.source_path | startswith("default/melange/")))
  >   | merlinJsonEntryWithConfigNames(["SUFFIX", "READER"])'
  X_mel: _build/default/melange/x_mel
  ["SUFFIX",".mlx .mlx"]
  ["READER",["mlx"]]
  X_mel: _build/default/melange/x_mel.mlx
  ["SUFFIX",".mlx .mlx"]
  ["READER",["mlx"]]
