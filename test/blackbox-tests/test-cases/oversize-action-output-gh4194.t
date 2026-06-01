Truncates oversized action output instead of crashing.

  $ make_dune_project 3.0

  $ cat > dune << EOF
  > (rule
  >  (alias runtest)
  >  (action
  >   (run ocaml %{dep:gen.ml})))
  > EOF

  $ cat > gen.ml << 'EOF'
  > let () =
  >   let s = String.make 0x10_00_00 '\n' in
  >   for _ = 1 to 16 do
  >     print_string s
  >   done
  > EOF

  $ dune runtest 2>&1 | grep -v '^$' | head -n 5 | grep -v 'fn ='
  ...TRUNCATED BY DUNE...
