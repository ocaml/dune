Anonymous projects have explicit_js_mode enabled

  $ echo '(lang dune 2.8)' > dune-project
  $ dune build --display short @all 2>&1 | grep -F .js
  [1]
