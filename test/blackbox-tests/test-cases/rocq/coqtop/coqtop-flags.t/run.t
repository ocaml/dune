Testing that the correct flags are being passed to dune rocq top

The flags passed to coqc:
  $ dune build --trace-file trace.json
  $ jq '.[] | select(.cat == "process" and (.args.process_args.[0] == "compile")) | .args.process_args | .[] | sub(".*/coq/theories"; "coq/theories")' trace.json
  "compile"
  "-w"
  "-notation-overridden"
  "-w"
  "-deprecated-native-compiler-option"
  "-w"
  "-native-compiler-disabled"
  "-native-compiler"
  "ondemand"
  "-boot"
  "-R"
  "coq/theories"
  "Corelib"
  "-R"
  "."
  "minimal"
  "Test.v"

The flags passed to coqtop:
  $ dune rocq top --toplevel=echo Test.v | ../../scrub_coq_args.sh
  -topfile $TESTCASE_ROOT/_build/default/Test.v
  -w -notation-overridden
  -w -deprecated-native-compiler-option
  -w -native-compiler-disabled
  -native-compiler ondemand
  -boot
  -R coq/theories Corelib
  -R $TESTCASE_ROOT/_build/default minimal
