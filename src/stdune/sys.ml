include Stdlib.Sys

let linux =
  match Io.String_path.read_file "/proc/sys/kernel/ostype" |> String.trim with
  | "Linux" -> true
  | _ -> false
  | exception _ -> false
