Compiler packages use user-wide toolchain installation prefixes. Distinct
dependency closures must not install to the same destination. Compare the
actual installation arguments in the process trace.

  $ make_dune_project 3.26
  $ make_lockdir
  $ make_lockpkg bar <<EOF
  > (version 1)
  > EOF
  $ make_lockpkg foo <<EOF
  > (version 1)
  > (depends bar)
  > EOF
  $ make_lockpkg ocaml-base-compiler <<EOF
  > (version 1)
  > (depends foo)
  > (install (run mkdir -p %{bin}/marker))
  > EOF
  $ installation_path() {
  >   dune trace cat | jq -c '
  >     select(.cat == "process" and .name == "start")
  >     | .args.process_args[]? | select(endswith("/bin/marker"))
  >   '
  > }
  $ digest=$(dune pkg print-digest ocaml-base-compiler)
  $ build_pkg ocaml-base-compiler
  $ installation_path > first-path

Only the transitive dependency changes. Both installations run, but their
traced destinations currently collide despite the different package keys.

  $ make_lockpkg bar <<EOF
  > (version 2)
  > EOF
  $ test "$digest" != "$(dune pkg print-digest ocaml-base-compiler)"
  $ build_pkg ocaml-base-compiler
  $ installation_path | cat first-path - | jq -cs \
  >   '{installs: length, distinct_destinations: (unique | length)}'
  {"installs":2,"distinct_destinations":1}
