Test that compile_commands.json matches actual C compiler invocations from trace.

  $ make_dune_project 3.23

Create a library with a C stub:

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (foreign_stubs (language c) (names stub)))
  > EOF

  $ cat >stub.c <<EOF
  > #include <caml/mlvalues.h>
  > value foo_stub(value unit) { return Val_int(42); }
  > EOF

  $ cat >foo.ml <<EOF
  > external stub : unit -> int = "foo_stub"
  > EOF

Build with trace to capture actual C compiler invocation:

  $ DUNE_TRACE=process dune build

Extract the C compiler invocation from trace (filter for .c files):

  $ dune trace cat \
  > | jq -c 'select(.cat == "process") | select(.args.process_args // [] | any(endswith(".c"))) | .args.process_args' \
  > | head -1 > trace_args.json

Verify we captured a trace entry:

  $ test -s trace_args.json

Build compile_commands.json:

  $ dune build compile_commands.json

Extract from compile_commands.json (first entry):

  $ jq '.[0].arguments[1:]' compile_commands.json > cc_args.json

Verify we got args:

  $ test -s cc_args.json

Compare the arguments (they should match):

  $ diff <(jq -S '.' trace_args.json) <(jq -S '.' cc_args.json)
