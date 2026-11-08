Action niceness is a relative adjustment, and must not change Dune itself.

  $ cat >dune-project <<'EOF'
  > (lang dune 3.0)
  > EOF
  $ cat >dune <<'EOF'
  > (rule
  >  (target observed)
  >  (action
  >   (with-stdout-to %{target}
  >    (bash "nice; awk '{ print $19 }' /proc/$PPID/stat"))))
  > (rule
  >  (target no-query)
  >  (action (with-stdout-to %{target} (run true))))
  > EOF

Account for any niceness inherited by the test runner.

  $ initial=$(nice)
  $ check_nice() {
  >   inherited=$1
  >   adjustment=$2
  >   build_dir=$3
  >   shift 3
  >   "$@" dune build --build-dir "$build_dir" observed
  >   { read -r actual; read -r parent; } < "$build_dir/default/observed"
  >   expected_parent=$((initial + inherited))
  >   if [ "$expected_parent" -gt 19 ]; then expected_parent=19; fi
  >   expected=$((expected_parent + adjustment))
  >   if [ "$expected" -gt 19 ]; then expected=19; fi
  >   if [ "$actual" != "$expected" ] ||
  >      [ "$parent" != "$expected_parent" ]; then
  >     echo "expected action=$expected parent=$expected_parent"
  >     echo "observed action=$actual parent=$parent"
  >     return 1
  >   fi
  >   echo "action +$adjustment, inherited +$inherited: ok"
  > }

By default no niceness adjustment is made.

  $ check_nice 0 0 _build-default env -u DUNE_CONFIG__ACTIONS_NICE
  action +0, inherited +0: ok

An explicit adjustment affects the action but leaves Dune unchanged.

  $ check_nice 0 5 _build-five env DUNE_CONFIG__ACTIONS_NICE=5
  action +5, inherited +0: ok
  $ check_nice 0 10 _build-ten env DUNE_CONFIG__ACTIONS_NICE=10
  action +10, inherited +0: ok
  $ check_nice 0 0 _build-zero env DUNE_CONFIG__ACTIONS_NICE=0
  action +0, inherited +0: ok

An explicit "none" leaves niceness untouched without priority system calls.

  $ check_nice 0 0 _build-none env DUNE_CONFIG__ACTIONS_NICE=none
  action +0, inherited +0: ok
  $ DUNE_CONFIG__ACTIONS_NICE=none strace -f \
  >   -e trace=getpriority,setpriority -o none.trace \
  >   dune build --build-dir _build-syscalls-none no-query
  $ ! grep -E 'getpriority|setpriority' none.trace

An explicit zero is a configured adjustment, not "none".

  $ DUNE_CONFIG__ACTIONS_NICE=0 strace -f \
  >   -e trace=getpriority,setpriority -o zero.trace \
  >   dune build --build-dir _build-syscalls-zero no-query
  $ grep -q 'setpriority(' zero.trace

Add to inherited niceness, rather than setting an absolute priority.

  $ check_nice 7 5 _build-inherited env DUNE_CONFIG__ACTIONS_NICE=5 nice -n 7
  action +5, inherited +7: ok

Saturate at 19, including when Dune already runs at 19.

  $ check_nice 7 19 _build-saturated env DUNE_CONFIG__ACTIONS_NICE=19 nice -n 7
  action +19, inherited +7: ok
  $ check_nice 19 5 _build-maximum env DUNE_CONFIG__ACTIONS_NICE=5 nice -n 19
  action +5, inherited +19: ok

Compiler configuration queries are internal jobs, not actions.

  $ mkdir tools
  $ export REAL_OCAMLC=$(command -v ocamlc)
  $ export CONFIG_NICE_LOG=$PWD/compiler-nice
  $ cat >tools/ocamlc <<'EOF'
  > #!/bin/sh
  > nice > "$CONFIG_NICE_LOG"
  > exec "$REAL_OCAMLC" "$@"
  > EOF
  $ chmod +x tools/ocamlc
  $ ln -s ocamlc tools/ocamlc.opt
  $ check_nice 0 5 _build-internal env DUNE_CONFIG__ACTIONS_NICE=5 \
  >   PATH="$PWD/tools:$PATH"
  action +5, inherited +0: ok
  $ read -r compiler_nice < compiler-nice
  $ test "$compiler_nice" = "$initial"

Reject invalid adjustments.

  $ DUNE_CONFIG__ACTIONS_NICE=-1 dune build observed
  Error: Invalid value for "DUNE_CONFIG__ACTIONS_NICE"
  expected "none" or an integer between 0 and 19
  [1]
  $ DUNE_CONFIG__ACTIONS_NICE=20 dune build observed
  Error: Invalid value for "DUNE_CONFIG__ACTIONS_NICE"
  expected "none" or an integer between 0 and 19
  [1]
  $ DUNE_CONFIG__ACTIONS_NICE=invalid dune build observed
  Error: Invalid value for "DUNE_CONFIG__ACTIONS_NICE"
  expected "none" or an integer between 0 and 19
  [1]
