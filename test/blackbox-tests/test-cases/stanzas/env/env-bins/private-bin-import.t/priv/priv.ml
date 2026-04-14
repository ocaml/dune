let count_segments s ~segment =
  s
  |> String.split_on_char '/'
  |> List.filter (fun part -> String.equal part segment)
  |> List.length

let is_in_inner_build_dir s =
  count_segments ~segment:"_build" s = 2

let () =
  let name = Filename.basename Sys.argv.(0) in
  Printf.printf "Executing priv as %s\n" name;
  let path =
    match Sys.getenv "PATH" with
    | exception Not_found -> "<empty>"
    | s ->
      s
      |> String.split_on_char ':'
      |> List.filter is_in_inner_build_dir
      |> String.concat "\n\t"
  in
  Printf.printf "PATH:\n\t%s\n" path
