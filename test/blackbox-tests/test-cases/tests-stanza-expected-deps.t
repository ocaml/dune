For a test based on an .expected file, the deps field is ignored.
This is visible when trying to build the `@all` alias. See #5950.

  $ cat > dune-project << EOF
  > (lang dune 3.3)
  > EOF

  $ cat > dune << EOF
  > (test
  >  (name t)
  >  (deps data.txt))
  > EOF

  $ cat > t.ml << EOF
  > let () =
  >   In_channel.with_open_bin "data.txt"
  >     (fun ic -> print_string (In_channel.input_all ic))
  > EOF

  $ cat > data.txt << EOF
  > data
  > EOF

  $ cp data.txt t.expected

  $ dune build t.output
