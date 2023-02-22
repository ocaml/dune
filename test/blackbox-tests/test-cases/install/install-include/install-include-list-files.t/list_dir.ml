(* Prints a sexp listing the contents of a given directory in the form:

  ((dir/foo as dir/foo)
   (dir/bar as dir/bar)
   (dir/baz as dir/baz)
   ...)

  where foo, bar, baz, ... are files in the given directory. *)

let list_dir dir =
  Sys.readdir dir
  |> Array.to_list
  |> List.map (fun f -> String.concat "/" [dir; f])

let () =
  list_dir (Sys.argv.(1))
  |> List.map (fun f -> Printf.sprintf "(%s as %s)" f f )
  |> String.concat " "
  |> Printf.printf "(%s)"
