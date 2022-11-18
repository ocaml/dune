open Stdune
include Cmdliner.Arg
module Stanza = Dune_lang.Stanza
module Package = Dune_engine.Package
module String_with_vars = Dune_lang.String_with_vars
module Pform = Dune_lang.Pform
module Dep_conf = Dune_rules.Dep_conf
module Context_name = Dune_engine.Context_name

let package_name = conv Package.Name.conv

module Path = struct
  module External = struct
    type t = string

    let path p = Path.External.of_filename_relative_to_initial_cwd p

    let arg s = s

    let conv = conv ((fun p -> Ok p), Format.pp_print_string)
  end

  type t = string

  let path p = Path.of_filename_relative_to_initial_cwd p

  let arg s = s

  let conv = conv ((fun p -> Ok p), Format.pp_print_string)
end

let path = Path.conv

let external_path = Path.External.conv

let profile = conv Dune_rules.Profile.conv

module Dep = struct
  module Dep_conf = Dep_conf

  type t = Dep_conf.t

  let file s = Dep_conf.File (String_with_vars.make_text Loc.none s)

  let make_alias_sw ~dir s =
    let path =
      Dune_engine.Alias.Name.to_string s
      |> Stdune.Path.Local.relative dir
      |> Stdune.Path.Local.to_string
    in
    String_with_vars.make_text Loc.none path

  let alias ~dir s = Dep_conf.Alias (make_alias_sw ~dir s)

  let alias_rec ~dir s = Dep_conf.Alias_rec (make_alias_sw ~dir s)

  let parse_alias s =
    if not (String.is_prefix s ~prefix:"@") then None
    else
      let pos, recursive =
        if String.length s >= 2 && s.[1] = '@' then (2, false) else (1, true)
      in
      let s = String.drop s pos in
      Some (s, recursive)

  let parse_alias_dep s =
    let open Option.O in
    let+ s, recursive = parse_alias s in
    let sw = String_with_vars.make_text Loc.none s in
    if recursive then Dep_conf.Alias_rec sw else Alias sw

  let dep_parser =
    Dune_lang.Syntax.set Stanza.syntax (Active Stanza.latest_version)
      (String_with_vars.set_decoding_env
         (Pform.Env.initial Stanza.latest_version)
         Dep_conf.decode)

  let parser s =
    match parse_alias_dep s with
    | Some dep -> Ok dep
    | None -> (
      match
        Dune_lang.Decoder.parse dep_parser Univ_map.empty
          (Dune_lang.Parser.parse_string ~fname:"command line"
             ~mode:Dune_lang.Parser.Mode.Single s)
      with
      | x -> Ok x
      | exception User_error.E msg -> Error (User_message.to_string msg))

  let string_of_alias ~recursive sv =
    let prefix = if recursive then "@" else "@@" in
    String_with_vars.text_only sv |> Option.map ~f:(fun s -> prefix ^ s)

  let printer ppf t =
    let s =
      match t with
      | Dep_conf.Alias sv -> string_of_alias ~recursive:false sv
      | Alias_rec sv -> string_of_alias ~recursive:true sv
      | File sv -> Some (Dune_lang.to_string (String_with_vars.encode sv))
      | _ -> None
    in
    let s =
      match s with
      | Some s -> s
      | None -> Dune_lang.to_string (Dep_conf.encode t)
    in
    Format.pp_print_string ppf s

  let conv = conv' (parser, printer)

  let to_string_maybe_quoted t =
    String.maybe_quoted (Format.asprintf "%a" printer t)
end

let dep = Dep.conv

let bytes =
  let decode repr =
    let ast =
      Dune_lang.Parser.parse_string ~fname:"command line"
        ~mode:Dune_lang.Parser.Mode.Single repr
    in
    match
      Dune_lang.Decoder.parse Dune_lang.Decoder.bytes_unit Univ_map.empty ast
    with
    | x -> Result.Ok x
    | exception User_error.E msg ->
      Result.Error (`Msg (User_message.to_string msg))
  in
  let pp_print_int64 state i =
    Format.pp_print_string state (Int64.to_string i)
  in
  conv (decode, pp_print_int64)

let graph_format : Dune_graph.Graph.File_format.t conv =
  conv Dune_graph.Graph.File_format.conv

let context_name : Context_name.t conv = conv Context_name.conv

let lib_name = conv Dune_rules.Lib_name.conv

let version = pair ~sep:'.' int int
