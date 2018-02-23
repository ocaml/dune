open Import

module Entry = struct
  type t =
    | Path of Path.t
    | Alias of Path.t
    | Library of Path.t * string
    | Preprocess of string list
    | Loc of Loc.t

  let jbuild_file_in ~dir = Path (Utils.jbuild_file_in ~dir)

  let to_string = function
    | Path p -> Utils.describe_target p
    | Alias p -> "alias " ^ Utils.describe_target p
    | Library (path, lib_name) ->
      sprintf "library %S in %s" lib_name (Path.to_string_maybe_quoted path)
    | Preprocess l ->
      Sexp.to_string (List [Sexp.unsafe_atom_of_string "pps";
                            Sexp.To_sexp.(list string) l])
    | Loc loc ->
      Loc.to_file_colon_line loc

  let pp ppf x =
    Format.pp_print_string ppf (to_string x)
end

module Entries = struct
  type t = Entry.t list

  let pp ppf t =
    Format.fprintf ppf "@[<v>%a@]"
      (Format.pp_print_list
         (fun ppf x ->
            Format.fprintf ppf "-> required by %a" Entry.pp x))
      t
end

exception E of exn * Entry.t list

let prepend_exn exn entry =
  match exn with
  | E (exn, entries) -> E (exn, entry :: entries)
  | exn -> E (exn, [entry])

let reraise exn entry = reraise (prepend_exn exn entry)

let unwrap_exn = function
  | E (exn, entries) -> (exn, Some entries)
  | exn -> (exn, None)

let () =
  Printexc.register_printer (function
    | E (exn, _) -> Some (Printexc.to_string exn)
    | _ -> None)
