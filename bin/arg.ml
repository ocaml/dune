open Stdune
open Dune
include Cmdliner.Arg

let package_name =
  conv ((fun p -> Ok (Package.Name.of_string p)), Package.Name.pp)

module Path = struct
  type t = string

  let path p = Path.of_filename_relative_to_initial_cwd p

  let arg s = s

  let conv = conv ((fun p -> Ok p), Format.pp_print_string)
end

let path = Path.conv

let profile =
  conv
    ( (fun p -> Ok (Profile.of_string p))
    , fun fmt t -> Format.pp_print_string fmt (Profile.to_string t) )

module Dep = struct
  module Dep_conf = Dune_file.Dep_conf

  type t = Dep_conf.t

  let file s = Dep_conf.File (String_with_vars.make_text Loc.none s)

  let alias s = Dep_conf.Alias (String_with_vars.make_text Loc.none s)

  let alias_rec s = Dep_conf.Alias_rec (String_with_vars.make_text Loc.none s)

  let parse_alias s =
    if not (String.is_prefix s ~prefix:"@") then
      None
    else
      let pos, recursive =
        if String.length s >= 2 && s.[1] = '@' then
          (2, false)
        else
          (1, true)
      in
      let s = String.drop s pos in
      Some (recursive, s)

  let dep_parser =
    Syntax.set Stanza.syntax Stanza.latest_version Dep_conf.decode

  let parser s =
    match parse_alias s with
    | Some (true, s) ->
      `Ok (alias_rec s)
    | Some (false, s) ->
      `Ok (alias s)
    | None -> (
      match
        Dune_lang.Decoder.parse dep_parser Univ_map.empty
          (Dune_lang.parse_string ~fname:"command line"
             ~mode:Dune_lang.Parser.Mode.Single s)
      with
      | x ->
        `Ok x
      | exception User_error.E msg ->
        `Error (User_message.to_string msg) )

  let string_of_alias ~recursive sv =
    let prefix =
      if recursive then
        "@"
      else
        "@@"
    in
    String_with_vars.text_only sv |> Option.map ~f:(fun s -> prefix ^ s)

  let printer ppf t =
    let s =
      match t with
      | Dep_conf.Alias sv ->
        string_of_alias ~recursive:false sv
      | Alias_rec sv ->
        string_of_alias ~recursive:true sv
      | File sv ->
        Some (Dune_lang.to_string (String_with_vars.encode sv))
      | _ ->
        None
    in
    let s =
      match s with
      | Some s ->
        s
      | None ->
        Dune_lang.to_string (Dep_conf.encode t)
    in
    Format.pp_print_string ppf s

  let conv = (parser, printer)

  let to_string_maybe_quoted t =
    String.maybe_quoted (Format.asprintf "%a" printer t)
end

let dep = Dep.conv
