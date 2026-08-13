dune trace cat --follow

  $ make_dune_project 3.21

  $ tmpdir="$(mktemp -d)"
  $ fifo="$tmpdir/fifo"
  $ trace_file="$tmpdir/trace.csexp"
  $ mkfifo $fifo

  $ cat >dune <<EOF
  > (rule
  >  (target x)
  >  (action (bash "read line < $fifo; touch x")))
  > EOF

  $ checkStart() {
  >   dune trace cat --trace-file "$trace_file" \
  >     | jq -e 'select(.name == "init" and .cat == "config")'
  > } 1> /dev/null 2>&1

The poll must not find an ancestor build's trace before this build starts:

  $ ! checkStart

  $ dune build ./x --trace-file "$trace_file" &

  $ while ! checkStart; do sleep 0.1; done

  $ ( dune trace cat --follow --trace-file "$trace_file" \
  > | jq 'select(.cat == "config" and (.name == "init" or .name == "exit")) | .name' ) &
  "init"
  "exit"

  $ echo resume > $fifo

  $ wait
