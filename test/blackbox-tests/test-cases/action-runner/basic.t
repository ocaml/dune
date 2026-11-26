`--action-runner` does not invalidate stale outputs when it is toggled.

  $ make_dune_project 3.23
  $ # Needed when upgrading this test to Dune language 3.25:
  $ # export DUNE_CONFIG__LANDLOCK=disabled
  $ export TEST_DIR=$PWD
  $ export DUNE_TRACE=action,process
  $ echo one > input
  $ cat > dune <<'EOF'
  > (rule
  >  (target probe)
  >  (deps input)
  >  (action
  >   (bash
  >    "count=0; if [ -e \"$TEST_DIR/counter\" ]; then count=$(cat \"$TEST_DIR/counter\"); fi; count=$((count + 1)); echo $count > \"$TEST_DIR/counter\"; echo ran-$count > %{target}")))
  > EOF

  $ dune build probe
  $ cat counter
  1

  $ dune build --action-runner probe
  $ cat counter
  1
  $ dune trace cat | jq -s 'include "dune"; runnerRequestEvents'
  []

  $ echo two > input
  $ dune build --action-runner probe
  $ cat counter
  2
  $ dune trace cat | jq -s 'include "dune"; runnerRequestEvents'
  [
    {
      "cat": "action",
      "name": "runner-request-sent",
      "args": {
        "name": "action-runner"
      }
    }
  ]

  $ dune build probe
  $ cat counter
  2
  $ dune trace cat | jq -s 'include "dune"; runnerRequestEvents'
  []

The worker can also be started when dune itself was invoked via a relative path
that contains a directory component.

  $ ln -s "$(command -v dune)" ./dune.exe
  $ echo three > input
  $ ./dune.exe build --action-runner probe
  $ cat counter
  3
  $ dune trace cat | jq -s 'include "dune"; runnerRequestEvents'
  [
    {
      "cat": "action",
      "name": "runner-request-sent",
      "args": {
        "name": "action-runner"
      }
    }
  ]

The worker uses the same build directory as the parent process.

  $ rm -rf _build _custom
  $ cat > dune <<'EOF'
  > (rule
  >  (target custom)
  >  (action
  >   (with-stdout-to %{target}
  >    (bash "printf custom"))))
  > EOF
  $ dune build --build-dir _custom --action-runner custom
  $ cat _custom/default/custom
  custom
  $ test ! -e _build/default/custom

Generated ocamldep processes only use the runner when they invoke a preprocessor.

  $ rm -rf _build _custom
  $ rm -f counter dune dune.exe input plain.ml probe
  $ cat > dune <<'EOF'
  > (library
  >  (name plain)
  >  (modules plain plain_dep))
  > EOF
  $ echo 'let x = 1' > plain_dep.ml
  $ echo 'let _ = Plain_dep.x' > plain.ml

  $ dune build --action-runner plain.cma
  $ dune trace cat | jq -s 'include "dune"; ocamldepActionRunnerStartCount'
  0
  $ dune trace cat | jq -s 'include "dune"; runnerRequestEvents'
  []

  $ rm -rf _build
  $ rm -f plain.ml plain_dep.ml
  $ cat > dune <<'EOF'
  > (library
  >  (name ppx_noop)
  >  (modules ppx_noop)
  >  (kind ppx_rewriter)
  >  (ppx.driver (main Ppx_noop.main)))
  > 
  > (library
  >  (name staged)
  >  (modules staged staged_dep)
  >  (preprocess (staged_pps ppx_noop)))
  > EOF
  $ cat > ppx_noop.ml <<'EOF'
  > let main () =
  >   if Array.length Sys.argv >= 3 then (
  >     let input_file = Sys.argv.(Array.length Sys.argv - 2) in
  >     let output_file = Sys.argv.(Array.length Sys.argv - 1) in
  >     let ic = open_in_bin input_file in
  >     let contents = really_input_string ic (in_channel_length ic) in
  >     close_in ic;
  >     let oc = open_out_bin output_file in
  >     output_string oc contents;
  >     close_out oc)
  >   else
  >     exit 2
  > EOF
  $ echo 'let x = 1' > staged_dep.ml
  $ echo 'let _ = Staged_dep.x' > staged.ml

  $ dune build --action-runner staged.cma
  $ dune trace cat | jq -s 'include "dune"; ocamldepPreprocessorUsesActionRunner("action-runner")'
  true

Both commands in a sequence should write to the same redirected stdout,
regardless of whether the commands run in the action runner. This also works
when built-ins and spawned commands share stdout. Two `dd` commands sharing
stdin should each read the next byte, writing to separate stdout and stderr.

  $ printf 'ab' > bytes
  $ printf 'third\n' > third
  $ cat > dune <<'EOF'
  > (rule
  >  (with-stdout-to local.out
  >   (progn (run echo first) (run echo second))))
  > (rule
  >  (with-stdout-to runner.out
  >   (progn (run echo first) (run echo second))))
  > (rule
  >  (deps third)
  >  (action
  >   (with-stdout-to mixed.out
  >    (progn (echo "first\n") (run echo second) (cat third) (run echo fourth)))))
  > (rule
  >  (targets bytes.out bytes.err)
  >  (deps bytes)
  >  (action
  >   (with-stdin-from bytes
  >    (with-stderr-to bytes.err
  >     (with-stdout-to bytes.out
  >      (progn
  >       (bash "dd bs=1 count=1 2>/dev/null")
  >       (bash "dd bs=1 count=1 >&2 2>/dev/null")))))))
  > EOF
  $ dune build local.out
  $ cat _build/default/local.out
  first
  second

Build the rules together so their requests can overlap. The trace should show
that all six spawned commands run in the worker.

CR: The worker opens each redirection again instead of sharing the original
file. This loses earlier output and resets the input offset.

  $ dune build -j 3 --action-runner runner.out mixed.out bytes.out
  $ cat _build/default/runner.out
  second
  $ cat _build/default/mixed.out
  fourth
  $ printf 'stdout=%s\nstderr=%s\n' "$(cat _build/default/bytes.out)" "$(cat _build/default/bytes.err)"
  stdout=
  stderr=a
  $ dune trace cat | jq -s '
  >   include "dune";
  >   actionRunnerTraceEvents("action-runner")
  >   | map(select(.cat == "process" and .name == "start")) | length'
  6
