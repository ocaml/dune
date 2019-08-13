open Dune_action

let contains equal list elem = List.find list (equal elem) |> Option.is_some

(* More subtle race. Should we allow target that is inside a directory that is
   a dependency?

   What about symlinks? *)

let race () =
  let directory = read_directory ~path:"foo"
  and write = write_file ~path:"foo/bar" ~data:"foobar" in
  both read write
  |> map ~f:(fun (directory, ()) ->
         if contains String.equal directory "foo" then
           print_endline "YES"
         else
           print_endline "NO")

let () = run race
