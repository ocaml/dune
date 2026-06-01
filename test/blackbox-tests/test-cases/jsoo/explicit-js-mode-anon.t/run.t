Anonymous projects have explicit_js_mode enabled

  $ echo '(lang dune 2.8)' > dune-project
  $ dune build

  $ dune trace cat | jq_dune -c '
  > targetsMatchingFilter(test("\\.js$"))
  > '
