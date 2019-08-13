open Dune_action

(* Should we detect that some file appears in both dependencies and targets? *)

let race file1 =
  let read = read_file ~path:file1
  and write = write_file ~path:file1 ~data:"foobar" in
  both read write |> map ~f:(fun (data, ()) -> write_file ~path:file1 ~data)

let () = run @@ race "foo"
