Test that dune trace cat --follow doesn't stop on exit events from nested dune
runs (which have a digest hash), only on the main dune exit event (which has
no digest hash).

  $ make_dune_project 3.22

  $ fifo="$(mktemp -d)/fifo"
  $ mkfifo $fifo

Set up a nested dune project:

  $ mkdir inner
  $ cat >inner/dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (deps (source_tree inner))
  >  (target x)
  >  (action
  >   (progn
  >    (chdir inner (run dune build))
  >    (bash "read line < $fifo; touch x"))))
  > EOF

  $ checkStart() {
  >   dune trace cat \
  >     | jq 'select(.name == "init" and .cat == "config")' \
  >     | head -n 1 \
  >     || true
  > } 1> /dev/null 2>&1

  $ dune build ./x &

  $ while ! checkStart; do sleep 0.1; done

Follow mode should see both init/exit pairs. Inner dune has a digest hash in its
events, outer dune does not:

  $ ( dune trace cat --follow \
  > | jq 'select(.cat == "config" and (.name == "init" or .name == "exit"))
  >       | { name, has_digest_hash: (if .args.digest then (.args.digest | type == "string") else false end) }' ) &
  {
    "name": "init",
    "has_digest_hash": false
  }
  {
    "name": "init",
    "has_digest_hash": true
  }
  {
    "name": "exit",
    "has_digest_hash": true
  }
  {
    "name": "exit",
    "has_digest_hash": false
  }

  $ echo resume > $fifo

  $ wait
