  $ cat > dune-project <<EOF
  > (lang dune 2.0)
  > EOF

  $ cat > dune << EOF
  > (rule (target a) (deps) (action (bash "echo a > a")))
  > EOF

  $ dune build a

Delete last 10 chars of the .db file to corrupt it

  $ truncate -s -10 _build/.db

Dune log the corrupted file and recover

  $ dune build a
  $ grep "truncated object" _build/log
  # Failed to load corrupted file _build/.db: input_value: truncated object
