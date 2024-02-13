open Stdune
open Dune_config
module Stringlike = Dune_util.Stringlike
module Syntax = Dune_sexp.Syntax

module Name = struct
  include String

  include Stringlike.Make (struct
      type t = string

      let module_ = "Warning.Name"
      let description = "Warning name"
      let description_of_valid_string = None
      let hint_valid = None
      let of_string_opt x = if Dune_sexp.Atom.is_valid x then Some x else None
      let to_string x = x
    end)
end

type t =
  { name : Name.t
  ; since : Syntax.Version.t (* The version where this warning was introduced. *)
  ; default : Syntax.Version.t -> Config.Toggle.t
  (* Decide the version where this warning should be enabled. This is
     needed because some warnings were introduced before this module
     existed *)
  }

let name t = Name.to_string t.name
let frozen = ref false
let all : (Name.t, t) Table.t = Table.create (module Name) 12

let make ~default ~name ~since =
  if !frozen
  then
    Code_error.raise
      "Warning.create may not be called after warning settings have been parsed. \
       Warnings should be created at the top level (not inside function definitions) in \
       the file src/dune_rules/warnings.ml"
      [ "name", Name.to_dyn name ];
  let name = Name.of_string name in
  let t = { name; since; default } in
  Table.add_exn all name t;
  t
;;

module Settings = struct
  type nonrec t = (t * Config.Toggle.t) list

  let empty = []
  let to_dyn = Dyn.opaque

  let decode : t Dune_sexp.Decoder.t =
    let all_warnings =
      lazy
        (frozen := true;
         Table.values all)
    in
    let open Dune_sexp.Decoder in
    let warning w =
      Syntax.since Stanza.syntax w.since
      >>> string
      |> map_validate ~f:(fun s ->
        match Config.Toggle.of_string s with
        | Ok s -> Ok (w, s)
        | Error s -> Error (User_message.make [ Pp.text s ]))
    in
    let* () = return () in
    Lazy.force all_warnings
    |> List.map ~f:(fun w -> Name.to_string w.name, warning w)
    |> leftover_fields_as_sums
    |> fields
  ;;

  let active t warn version =
    match
      List.find_map t ~f:(fun (warn', value) ->
        Option.some_if (Name.equal warn.name warn'.name) value)
    with
    | Some w -> w
    | None -> warn.default version
  ;;
end
