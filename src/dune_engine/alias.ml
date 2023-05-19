open Import

module Name : sig
  type t

  include Dune_sexp.Conv.S with type t := t

  val equal : t -> t -> bool

  val hash : t -> int

  val compare : t -> t -> Ordering.t

  val of_string : string -> t

  val parse_string_exn : Loc.t * string -> t

  val to_string : t -> string

  val to_dyn : t -> Dyn.t

  val default : t

  val runtest : t

  val install : t

  val fmt : t

  val all : t

  val parse_local_path : Loc.t * Path.Local.t -> Path.Local.t * t

  include Comparable_intf.S with type key := t
end = struct
  include String

  (* DUNE3 once we get rid the "loose" validation, implement this module using
     [Stringlike] *)
  let of_string_opt_loose s = Option.some_if (Filename.basename s = s) s

  let of_string_opt = function
    (* The [""] case is caught by of_string_opt_loose. But there's no harm in
       being more explicit about it *)
    | "" | "." | "/" | ".." -> None
    | s -> of_string_opt_loose s

  let invalid_alias = Pp.textf "%S is not a valid alias name"

  let parse_string_exn ~syntax (loc, s) =
    let of_string_opt =
      if syntax >= (2, 0) then of_string_opt else of_string_opt_loose
    in
    match of_string_opt s with
    | None -> User_error.raise ~loc [ invalid_alias s ]
    | Some s -> s

  let encode = Dune_sexp.Encoder.string

  let decode =
    let open Dune_sexp.Decoder in
    let* syntax = Dune_sexp.Syntax.get_exn Dune_lang.Stanza.syntax in
    plain_string (fun ~loc s -> parse_string_exn ~syntax (loc, s))

  let parse_string_exn =
    parse_string_exn ~syntax:Dune_lang.Stanza.latest_version

  let of_string s =
    match of_string_opt s with
    | Some s -> s
    | None -> Code_error.raise "invalid alias name" [ ("s", Dyn.string s) ]

  let to_string s = s

  let default = "default"

  let runtest = "runtest"

  let install = "install"

  let fmt = "fmt"

  let all = "all"

  let to_dyn = String.to_dyn

  let parse_local_path (loc, p) =
    match Path.Local.parent p with
    | Some dir -> (dir, Path.Local.basename p)
    | None ->
      User_error.raise ~loc
        [ Pp.textf "Invalid alias path: %S"
            (Path.Local.to_string_maybe_quoted p)
        ]
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
      let name = Name.of_string (Path.Build.basename path) in
      { dir = Path.Build.parent_exn path; name }
    | None ->
      User_error.raise ~loc
        [ Pp.text "Invalid alias!"
        ; Pp.textf "Tried to reference path outside build dir: %S"
            (Path.to_string_maybe_quoted path)
        ]
end

include T

let compare { dir; name } t =
  let open Ordering.O in
  let= () = Name.compare name t.name in
  Path.Build.compare dir t.dir

let equal x y = compare x y = Eq

let hash { dir; name } = Tuple.T2.hash Path.Build.hash Name.hash (dir, name)

let to_dyn { dir; name } =
  let open Dyn in
  Record [ ("dir", Path.Build.to_dyn dir); ("name", Name.to_dyn name) ]

let name t = t.name

let dir t = t.dir

let fully_qualified_name t = Path.Build.relative t.dir (Name.to_string t.name)

(* This mutable table is safe: it's modified only at the top level. *)
let standard_aliases = Table.create (module Name) 7

let is_standard name = Table.mem standard_aliases name

let make_standard name =
  Table.add_exn standard_aliases name ();
  make name

let register_as_standard name =
  let (_ : (unit, _) result) = Table.add standard_aliases name () in
  ()

let default = make_standard Name.default

let runtest = make_standard Name.runtest

let install = make_standard Name.install

let doc = make_standard (Name.of_string "doc")

let private_doc = make_standard (Name.of_string "doc-private")

let lint = make_standard (Name.of_string "lint")

let all = make_standard (Name.of_string "all")

let check = make_standard (Name.of_string "check")

let fmt = make_standard Name.fmt

let encode { dir; name } =
  let open Dune_sexp.Encoder in
  record [ ("dir", Dpath.encode (Path.build dir)); ("name", Name.encode name) ]

let get_ctx (path : Path.Build.t) =
  match Path.Build.extract_first_component path with
  | None -> None
  | Some (name, sub) -> (
    match Context_name.of_string_opt name with
    | None -> None
    | Some ctx -> Some (ctx, Path.Source.of_local sub))

let describe ?(loc = Loc.none) alias =
  let open Pp.O in
  let pp =
    match get_ctx alias.dir with
    | None ->
      Pp.textf "invalid-alias %s"
        (Path.Build.to_string (fully_qualified_name alias))
    | Some (ctx, dir_in_context) ->
      let pp =
        Pp.textf "alias "
        ++ Pp.verbatim
             (Path.Source.to_string_maybe_quoted
                (Path.Source.relative dir_in_context
                   (Name.to_string alias.name)))
      in
      if Context_name.is_default ctx then pp
      else pp ++ Pp.textf " (context %s)" (Context_name.to_string ctx)
  in
  if Loc.is_none loc then pp
  else pp ++ Pp.textf " in %s" (Loc.to_file_colon_line loc)
