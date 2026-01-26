Test that dune produces a clear error when a rule creates special files
in its output.

FIFO:

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using directory-targets 0.1)
  > EOF

  $ cat > dune <<EOF
  > (rule
  >   (targets (dir output))
  >   (action
  >    (progn
  >     (run mkdir -p output)
  >     (run mkfifo output/myfifo))))
  > EOF

  $ dune build output
  File "dune", lines 1-6, characters 0-110:
  1 | (rule
  2 |   (targets (dir output))
  3 |   (action
  4 |    (progn
  5 |     (run mkdir -p output)
  6 |     (run mkfifo output/myfifo))))
  Error: Rule produced file "output/myfifo" with unrecognised kind "S_FIFO"
  [1]

  $ rm -rf _build

Socket:

  $ cat > dune <<EOF
  > (rule
  >   (targets (dir output))
  >   (action
  >    (progn
  >     (run mkdir -p output)
  >     (run dune_cmd mksocket output/mysocket))))
  > EOF

  $ dune build output
  File "dune", lines 1-6, characters 0-123:
  1 | (rule
  2 |   (targets (dir output))
  3 |   (action
  4 |    (progn
  5 |     (run mkdir -p output)
  6 |     (run dune_cmd mksocket output/mysocket))))
  Error: Rule produced file "output/mysocket" with unrecognised kind "S_SOCK"
  [1]
