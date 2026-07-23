Melange rules should honor explicit sandboxing

  $ make_melange_sandbox_project

The test suite sets `DUNE_SANDBOX`, so clear it and use the command-line
option explicitly.

  $ unset DUNE_SANDBOX
  $ rm -rf _build
  $ dune build @mel --sandbox=symlink --display quiet --trace-file trace.csexp

  $ cat > melc_dirs.jq <<EOF
  > select(
  >   .cat == "process"
  >   and .name == "start"
  >   and (.args.prog | test("/melc$"))
  >   and (.args.process_args[0]? != "--where")
  > )
  > | .args.dir
  > EOF

  $ dune trace cat --trace-file trace.csexp \
  > | jq -r -f melc_dirs.jq \
  > | sort -u \
  > | head -n 1 \
  > | dune_cmd subst '_build/\.sandbox/[0-9a-f]+' '_build/.sandbox/$SANDBOX'
  _build/.sandbox/$SANDBOX/default
