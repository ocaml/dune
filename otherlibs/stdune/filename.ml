include Stdlib.Filename

let split_extension fn =
  let ext = extension fn in
  (String.sub fn ~pos:0 ~len:(String.length fn - String.length ext), ext)

let split_extension_after_dot fn =
  match extension fn with
  | "" -> (fn, "")
  | s -> String.split_n fn (String.length fn - String.length s + 1)

type program_name_kind =
  | In_path
  | Relative_to_current_dir
  | Absolute

let analyze_program_name fn =
  if not (is_relative fn) then
    Absolute
  else if String.contains fn '/' || (Stdlib.Sys.win32 && String.contains fn '\\')
  then
    Relative_to_current_dir
  else
    In_path
