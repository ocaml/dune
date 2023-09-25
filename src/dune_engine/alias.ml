open Import

module Name = struct
  include Dune_util.Alias_name

  let encode s = Dune_sexp.Encoder.string (to_string s)

  let of_string s =
    match of_string_opt s with
    | Some s -> s
    | None -> Code_error.raise "invalid alias name" [ "s", Dyn.string s ]
  ;;

  let default = of_string "default"

  let parse_local_path (loc, p) =
    match Path.Local.parent p with
    | Some dir ->
      ( dir
      , (* TODO one day we should validate the name properly here *)
        Path.Local.basename p |> of_string_opt_loose |> Option.value_exn )
    | None ->
      User_error.raise
        ~loc
        [ Pp.textf "Invalid alias path: %S" (Path.Local.to_string_maybe_quoted p) ]
  ;;
end

module T : sig
  type t = private
    { dir : Path.Build.t
    ; name : Name.t
    }

  val make : Name.t -> dir:Path.Build.t -> t
  val of_user_written_path : loc:Loc.t -> Path.t -> t
end = struct
  type t =
    { dir : Path.Build.t
    ; name : Name.t
    }

  let make name ~dir = { dir; name }

  let of_user_written_path ~loc path =
    match Path.as_in_build_dir path with
    | Some path ->
      let name =
        Path.Build.basename path |> Name.of_string_opt_loose |> Option.value_exn
      in
      { dir = Path.Build.parent_exn path; name }
    | None ->
      User_error.raise
        ~loc
        [ Pp.text "Invalid alias!"
        ; Pp.textf
            "Tried to reference path outside build dir: %S"
            (Path.to_string_maybe_quoted path)
        ]
  ;;
end

include T

let compare { dir; name } t =
  let open Ordering.O in
  let= () = Name.compare name t.name in
  Path.Build.compare dir t.dir
;;

let equal x y = compare x y = Eq
let hash { dir; name } = Tuple.T2.hash Path.Build.hash Name.hash (dir, name)

let to_dyn { dir; name } =
  let open Dyn in
  Record [ "dir", Path.Build.to_dyn dir; "name", Name.to_dyn name ]
;;

let name t = t.name
let dir t = t.dir
let fully_qualified_name t = Path.Build.relative t.dir (Name.to_string t.name)

(* This mutable table is safe: it's modified only at the top level. *)
let standard_aliases = Table.create (module Name) 7
let is_standard name = Table.mem standard_aliases name

let make_standard name =
  Table.add_exn standard_aliases name ();
  make name
;;

let register_as_standard name =
  let (_ : (unit, _) result) = Table.add standard_aliases name () in
  ()
;;

let default = make_standard Name.default

let encode { dir; name } =
  let open Dune_sexp.Encoder in
  record [ "dir", Dpath.encode (Path.build dir); "name", Name.encode name ]
;;

let get_ctx (path : Path.Build.t) =
  match Path.Build.extract_first_component path with
  | None -> None
  | Some (name, sub) ->
    (match Context_name.of_string_opt name with
     | None -> None
     | Some ctx -> Some (ctx, Path.Source.of_local sub))
;;

let describe ?(loc = Loc.none) alias =
  let open Pp.O in
  let pp =
    match get_ctx alias.dir with
    | None ->
      Pp.textf "invalid-alias %s" (Path.Build.to_string (fully_qualified_name alias))
    | Some (ctx, dir_in_context) ->
      let pp =
        Pp.textf "alias "
        ++ Pp.verbatim
             (Path.Source.to_string_maybe_quoted
                (Path.Source.relative dir_in_context (Name.to_string alias.name)))
      in
      if Context_name.is_default ctx
      then pp
      else pp ++ Pp.textf " (context %s)" (Context_name.to_string ctx)
  in
  if Loc.is_none loc then pp else pp ++ Pp.textf " in %s" (Loc.to_file_colon_line loc)
;;
