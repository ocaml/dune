  $ cat > dune-project << EOF
  > (lang dune 3.0)
  > EOF

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

  $ dune runtest 2>&1 | head -n 5 |grep -v 'fn ='
  Internal error, please report upstream including the contents of _build/log.
  Description:
    ("read_file: file is larger than Sys.max_string_length",
  Raised at Stdune__Code_error.raise in file
