Here we demonstrate the --stop-on-first-error behaviour.

  $ dune build
  File "dune", lines 1-4, characters 0-50:
  1 | (rule
  2 |  (target foo)
  3 |  (action
  4 |   (system "exit 1")))
  Command exited with code 1.
  File "dune", lines 6-9, characters 0-50:
  6 | (rule
  7 |  (target bar)
  8 |  (action
  9 |   (system "exit 1")))
  Command exited with code 1.
  [1]

  $ dune build --stop-on-first-error
  File "dune", lines 6-9, characters 0-50:
  6 | (rule
  7 |  (target bar)
  8 |  (action
  9 |   (system "exit 1")))
  Command exited with code 1.
  [1]
