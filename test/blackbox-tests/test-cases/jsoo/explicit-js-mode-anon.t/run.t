Anonymous projects have explicit_js_mode enabled

  $ echo '(lang dune 2.8)' > dune-project
  $ dune build @all
  $ dune trace cat | jq -r 'include "dune";
  >   processes
  > | select(.args.prog | test("js_of_ocaml$"))
  > | .args | targets | .[] | sub("^_build/[^/]+/"; "")'
