When compiling vendored code, all alerts should be disabled

  $ cat > dune << EOF
  > (rule
  >  (alias no-alerts-please)
  >  (deps vendored/lib.cmxa)
  >  (action (echo "There should be no OCaml alerts!")))
  > EOF
  $ dune build @no-alerts-please
  File "vendored/lib.ml", line 1, characters 8-24:
  1 | let x = Trigger_alerts.x
              ^^^^^^^^^^^^^^^^
  Alert alertx: Lib__.Trigger_alerts.x
  You should not use x
  File "vendored/lib.ml", line 3, characters 8-24:
  3 | let y = Trigger_alerts.y
              ^^^^^^^^^^^^^^^^
  Alert alerty: Lib__.Trigger_alerts.y
  You should not use y
  File "vendored/lib.ml", line 1, characters 8-24:
  1 | let x = Trigger_alerts.x
              ^^^^^^^^^^^^^^^^
  Alert alertx: Lib__.Trigger_alerts.x
  You should not use x
  File "vendored/lib.ml", line 3, characters 8-24:
  3 | let y = Trigger_alerts.y
              ^^^^^^^^^^^^^^^^
  Alert alerty: Lib__.Trigger_alerts.y
  You should not use y
  There should be no OCaml alerts!
  $ echo '(vendored_dirs vendored)' >> dune
  $ dune clean
  $ dune build @no-alerts-please
  There should be no OCaml alerts!
