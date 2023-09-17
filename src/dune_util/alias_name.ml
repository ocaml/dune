open Stdune
include String

(* DUNE3 once we get rid the "loose" validation, implement this module using
   [Stringlike] *)
let of_string_opt_loose s = Option.some_if (Filename.basename s = s) s

let of_string_opt = function
  (* The [""] case is caught by of_string_opt_loose. But there's no harm in
     being more explicit about it *)
  | "" | "." | "/" | ".." -> None
  | s -> of_string_opt_loose s
;;

let of_string s =
  match of_string_opt s with
  | Some s -> s
  | None -> Code_error.raise "invalid alias name" [ "s", Dyn.string s ]
;;

let to_string s = s
let to_dyn = String.to_dyn

let parse_local_path (loc, p) =
  match Path.Local.parent p with
  | Some dir -> dir, Path.Local.basename p
  | None ->
    User_error.raise
      ~loc
      [ Pp.textf "Invalid alias path: %S" (Path.Local.to_string_maybe_quoted p) ]
;;
