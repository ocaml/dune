  $ cat > dune-project << EOF
  > (lang dune 2.0)
  > (using action-plugin 0.1)
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name exit)
  >  (modules exit))
  > (rule (with-stdout-to exit.ml (echo "let () = exit (int_of_string Sys.argv.(1))")))
  > EOF

  $ cat >> dune <<EOF
  > (rule
  >  (alias a)
  >  (action (with-accepted-exit-codes 
  >           1
  >           (with-stdout-to out.txt
  >            (run ./exit.exe 1)))))
  > EOF

  $ dune build --display=short --root . @a
  File "dune", lines 9-10, characters 10-64:
   9 |           (with-stdout-to out.txt
  10 |            (run ./exit.exe 1)))))
  Error: nesting modifiers under 'with-accepted-exit-codes' is only available
  since version 2.2 of the dune language. Please update your dune-project file
  to have (lang dune 2.2).
  [1]
