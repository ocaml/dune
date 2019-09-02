open Stdune
open Dune_action

let action =
  write_file ~path:(Path.of_string "../bar") ~data:"Hello from bar!"
  |> map ~f:(fun r -> r |> Result.to_option |> Option.value_exn)

let () = run action
