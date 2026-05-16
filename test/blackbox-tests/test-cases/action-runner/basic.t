`--action-runner` does not invalidate stale outputs when it is toggled.

  $ make_dune_project 3.23
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
  $ dune trace cat | jq -s 'include "dune"; runnerRequestSummary'
  {
    "request_sent": false,
    "exec_start": 0,
    "names": []
  }

  $ echo two > input
  $ dune build --action-runner probe
  $ cat counter
  2
  $ dune trace cat | jq -s 'include "dune"; runnerRequestSummary'
  {
    "request_sent": true,
    "exec_start": 1,
    "names": [
      "action-runner"
    ]
  }

  $ dune build probe
  $ cat counter
  2
  $ dune trace cat | jq -s 'include "dune"; runnerRequestSummary'
  {
    "request_sent": false,
    "exec_start": 0,
    "names": []
  }

The worker can also be started when dune itself was invoked via a relative path
that contains a directory component.

  $ ln -s "$(command -v dune)" ./dune.exe
  $ echo three > input
  $ ./dune.exe build --action-runner probe
  $ cat counter
  3
  $ dune trace cat | jq -s 'include "dune"; runnerRequestSummary'
  {
    "request_sent": true,
    "exec_start": 1,
    "names": [
      "action-runner"
    ]
  }

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
  $ dune trace cat | jq -s '[ .[] | select(.cat == "process" and .name == "start" and (.args.prog | contains("ocamldep")) and .args.action_runner?) ] | length'
  0
  $ dune trace cat | jq -s 'include "dune"; runnerRequestSummary | {request_sent}'
  {
    "request_sent": false
  }

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
  $ dune trace cat | jq -s '[ .[] | select(.cat == "process" and .name == "start" and (.args.prog | contains("ocamldep")) and ((.args.process_args | index("-pp")) or (.args.process_args | index("-ppx"))) and .args.action_runner == "action-runner") ] | length > 0'
  true
