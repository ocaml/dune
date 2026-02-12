Demonstrate that test stanza is now sandbox

  $ make_dune_project 3.21

  $ mkdir dir
  $ cat > dir/dune <<EOF
  > (test (name foo))
  > EOF

  $ cat >dir/foo.ml <<EOF
  > let () =
  >   Sys.readdir "."
  >   |> Array.to_list
  >   |> List.filter (fun x -> not (String.starts_with ~prefix:"." x))
  >   |> List.sort String.compare
  >   |> List.iter print_endline
  > EOF

  $ dune runtest dir/foo
  foo.exe
  foo.ml
  foo.mli

  $ make_dune_project 3.22
  $ dune runtest dir/foo
  foo.exe
