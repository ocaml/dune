dune trace cat --follow

  $ cat >dune-project <<EOF
  > (lang dune 3.21)
  > EOF

  $ fifo="$(mktemp -d)/fifo"
  $ mkfifo $fifo

  $ cat >dune <<EOF
  > (rule
  >  (target x)
  >  (action (bash "read line < $fifo; touch x")))
  > EOF

  $ checkStart() {
  >   dune trace cat \
  >     | jq 'select(.name == "init" and .cat == "config")' \
  >     | head -n 1 \
  >     || true
  > } 1> /dev/null 2>&1

  $ dune build ./x &

  $ while ! checkStart; do sleep 0.1; done

  $ ( dune trace cat --follow \
  > | jq 'select(.cat == "config" and (.name == "init" or .name == "exit")) | .name' ) &
  "init"
  "exit"

  $ echo resume > $fifo

  $ wait
