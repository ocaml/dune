open! Stdune

module Entry = struct
  type t =
    | Path of Path.t
    | Alias of Path.t
    | Library of Path.t * Lib_name.t
    | Executables of (Loc.t * string) list
    | Preprocess of Lib_name.t list
    | Loc of Loc.t

  let pp = function
    | Path p -> Pp.text (Dpath.describe_path p)
    | Alias p -> Pp.textf "alias %s" (Dpath.describe_path p)
    | Library (path, lib_name) ->
      Pp.textf "library %S in %s"
        (Lib_name.to_string lib_name)
        (Path.to_string_maybe_quoted path)
    | Executables [(loc, name)] ->
      Pp.textf "executable %s in %s"
        name
        (Loc.to_file_colon_line loc)
    | Executables names ->
      let (loc, _) = List.hd names in
      Pp.textf "executables %s in %s"
        (String.enumerate_and (List.map ~f:snd names))
        (Loc.to_file_colon_line loc)
    | Preprocess l ->
      Pp.textf "%s"
        (Dyn.to_string
           (List [ String "pps"
                 ; Dyn.Encoder.(list Lib_name.to_dyn) l ]))
    | Loc loc ->
      Pp.text (Loc.to_file_colon_line loc)
end

module Entries = struct
  type t = Entry.t list

  let pp t =
    Pp.vbox (Pp.concat ~sep:Pp.cut (List.map t ~f:(fun x ->
      Pp.box ~indent:3
        (Pp.seq (Pp.verbatim "-> ")
           (Pp.seq (Pp.text "required by ") (Entry.pp x))))))
end

exception E of exn * Entry.t list

let prepend_exn exn entry =
  match exn with
  | E (exn, entries) -> E (exn, entry :: entries)
  | exn -> E (exn, [entry])

let reraise exn entry =
  Exn_with_backtrace.map_and_reraise exn ~f:(fun exn -> prepend_exn exn entry)

let unwrap_exn = function
  | E (exn, entries) -> (exn, Some entries)
  | exn -> (exn, None)

let map ~f = function
  | E (exn, entries) -> begin
      match f exn with
      | E (exn, entries') -> E (exn, entries' @ entries)
      | exn -> E (exn, entries)
    end
  | exn -> f exn

let () =
  Printexc.register_printer (function
    | E (exn, _) -> Some (Printexc.to_string exn)
    | _ -> None)
