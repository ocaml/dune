open! Stdune
open Import
open Fiber.O

let is_a_source_file path =
  ( match Path.extension path with
  | ".flv"
  | ".gif"
  | ".ico"
  | ".jpeg"
  | ".jpg"
  | ".mov"
  | ".mp3"
  | ".mp4"
  | ".otf"
  | ".pdf"
  | ".png"
  | ".ttf"
  | ".woff" ->
      false
  | _ ->
      true )
  && Path.is_file path

let make_watermark_map ~name ~version ~commit =
  let opam_file = Opam_file.load (Path.in_source (name ^ ".opam")) in
  let version_num =
    Option.value ~default:version (String.drop_prefix version ~prefix:"v")
  in
  let opam_var name sep =
    match Opam_file.get_field opam_file name with
    | None ->
        Error (sprintf "variable %S not found in opam file" name)
    | Some value -> (
        let err () =
          Error (sprintf "invalid value for variable %S in opam file" name)
        in
        match value with
        | String (_, s) ->
            Ok s
        | List (_, l) -> (
          match
            List.fold_left l ~init:(Ok []) ~f:(fun acc v ->
                match acc with
                | Error _ ->
                    acc
                | Ok l -> (
                  match v with
                  | OpamParserTypes.String (_, s) ->
                      Ok (s :: l)
                  | _ ->
                      err () ))
          with
          | Error _ as e ->
              e
          | Ok l ->
              Ok (String.concat ~sep (List.rev l)) )
        | _ ->
            err () )
  in
  String.Map.of_list_exn
    [ ("NAME", Ok name)
    ; ("VERSION", Ok version)
    ; ("VERSION_NUM", Ok version_num)
    ; ("VCS_COMMIT_ID", Ok commit)
    ; ("PKG_MAINTAINER", opam_var "maintainer" ", ")
    ; ("PKG_AUTHORS", opam_var "authors" ", ")
    ; ("PKG_HOMEPAGE", opam_var "homepage" " ")
    ; ("PKG_ISSUES", opam_var "bug-reports" " ")
    ; ("PKG_DOC", opam_var "doc" " ")
    ; ("PKG_LICENSE", opam_var "license" ", ")
    ; ("PKG_REPO", opam_var "dev-repo" " ")
    ]

let subst_string s path ~map =
  let len = String.length s in
  let longest_var = String.longest (String.Map.keys map) in
  let double_percent_len = String.length "%%" in
  let loc_of_offset ~ofs ~len =
    let rec loop lnum bol i =
      if i = ofs then
        let pos =
          { Lexing.pos_fname = Path.to_string path
          ; pos_cnum = i
          ; pos_lnum = lnum
          ; pos_bol = bol
          }
        in
        { Loc.start = pos; stop = { pos with pos_cnum = pos.pos_cnum + len } }
      else
        match s.[i] with
        | '\n' ->
            loop (lnum + 1) (i + 1) (i + 1)
        | _ ->
            loop lnum bol (i + 1)
    in
    loop 1 0 0
  in
  let rec loop i acc =
    if i = len then
      acc
    else
      match s.[i] with
      | '%' ->
          after_percent (i + 1) acc
      | _ ->
          loop (i + 1) acc
  and after_percent i acc =
    if i = len then
      acc
    else
      match s.[i] with
      | '%' ->
          after_double_percent ~start:(i - 1) (i + 1) acc
      | _ ->
          loop (i + 1) acc
  and after_double_percent ~start i acc =
    if i = len then
      acc
    else
      match s.[i] with
      | '%' ->
          after_double_percent ~start:(i - 1) (i + 1) acc
      | 'A' .. 'Z' | '_' ->
          in_var ~start (i + 1) acc
      | _ ->
          loop (i + 1) acc
  and in_var ~start i acc =
    if i - start > longest_var + double_percent_len then
      loop i acc
    else if i = len then
      acc
    else
      match s.[i] with
      | '%' ->
          end_of_var ~start (i + 1) acc
      | 'A' .. 'Z' | '_' ->
          in_var ~start (i + 1) acc
      | _ ->
          loop (i + 1) acc
  and end_of_var ~start i acc =
    if i = len then
      acc
    else
      match s.[i] with
      | '%' -> (
          let var = String.sub s ~pos:(start + 2) ~len:(i - start - 3) in
          match String.Map.find map var with
          | None ->
              in_var ~start:(i - 1) (i + 1) acc
          | Some (Ok repl) ->
              let acc = (start, i + 1, repl) :: acc in
              loop (i + 1) acc
          | Some (Error msg) ->
              let loc = loc_of_offset ~ofs:start ~len:(i + 1 - start) in
              User_error.raise ~loc [ Pp.text msg ] )
      | _ ->
          loop (i + 1) acc
  in
  match List.rev (loop 0 []) with
  | [] ->
      None
  | repls ->
      let result_len =
        List.fold_left repls ~init:(String.length s)
          ~f:(fun acc (a, b, repl) -> acc - (b - a) + String.length repl)
      in
      let buf = Buffer.create result_len in
      let pos =
        List.fold_left repls ~init:0 ~f:(fun pos (a, b, repl) ->
            Buffer.add_substring buf s pos (a - pos);
            Buffer.add_string buf repl;
            b)
      in
      Buffer.add_substring buf s pos (len - pos);
      Some (Buffer.contents buf)

let subst_file path ~map =
  let s = Io.read_file path in
  let s =
    if
      Path.is_root (Path.parent_exn path)
      && String.is_suffix (Path.to_string path) ~suffix:".opam"
    then
      "version: \"%%" ^ "VERSION_NUM" ^ "%%\"\n" ^ s
    else
      s
  in
  match subst_string s ~map path with
  | None ->
      ()
  | Some s ->
      Io.write_file path s

(* Minimal API for dune-project files that makes as little assumption about the
   contents as possible and keeps enough info for editing the file. *)
module Dune_project = struct
  type 'a simple_field =
    { loc : Loc.t
    ; loc_of_arg : Loc.t
    ; arg : 'a
    }

  type t =
    { contents : string
    ; name : string simple_field option
    ; version : string simple_field option
    }

  let file = Path.in_source Dune_project.filename

  let load file =
    let s = Io.read_file file in
    let lb = Lexbuf.from_string s ~fname:(Path.to_string file) in
    let sexp = Dune_lang.Parser.parse lb ~mode:Many_as_one in
    let parser =
      let open Dune_lang.Decoder in
      let simple_field name arg =
        let+ loc, x = located (field_o name (located arg)) in
        match x with
        | Some (loc_of_arg, arg) ->
            Some { loc; loc_of_arg; arg }
        | None ->
            None
      in
      enter
        (fields
           (let+ name = simple_field "name" string
            and+ version = simple_field "version" string
            and+ () = junk_everything in
            { contents = s; name; version }))
    in
    Dune_lang.Decoder.parse parser Univ_map.empty sexp

  let subst t ~map ~version =
    let s =
      let replace_text start_ofs stop_ofs repl =
        sprintf "%s%s%s"
          (String.sub t.contents ~pos:0 ~len:start_ofs)
          repl
          (String.sub t.contents ~pos:stop_ofs
             ~len:(String.length t.contents - stop_ofs))
      in
      match t.version with
      | Some v ->
          (* There is a [version] field, overwrite its argument *)
          replace_text v.loc_of_arg.start.pos_cnum v.loc_of_arg.stop.pos_cnum
            (Dune_lang.to_string (Dune_lang.atom_or_quoted_string version))
      | None ->
          let version_field =
            Dune_lang.to_string
              (List
                 [ Dune_lang.atom "version"
                 ; Dune_lang.atom_or_quoted_string version
                 ])
            ^ "\n"
          in
          let ofs =
            ref
              ( match t.name with
              | Some { loc; _ } ->
                  (* There is no [version] field but there is a [name] one, add
                     the version after it *)
                  loc.stop.pos_cnum
              | None ->
                  (* If all else fails, add the [version] field after the first
                     line of the file *)
                  0 )
          in
          let len = String.length t.contents in
          while !ofs < len && t.contents.[!ofs] <> '\n' do
            incr ofs
          done;
          if !ofs < len && t.contents.[!ofs] = '\n' then (
            incr ofs;
            replace_text !ofs !ofs version_field
          ) else
            replace_text !ofs !ofs ("\n" ^ version_field)
    in
    let s = Option.value (subst_string s ~map file) ~default:s in
    if s <> t.contents then Io.write_file file s
end

let get_name ~files ~(dune_project : Dune_project.t option) () =
  let package_names =
    List.filter_map files ~f:(fun fn ->
        match Path.parent fn with
        | Some p when Path.is_root p -> (
            let fn = Path.basename fn in
            match Filename.split_extension fn with
            | s, ".opam" ->
                Some s
            | _ ->
                None )
        | _ ->
            None)
  in
  if package_names = [] then
    User_error.raise [ Pp.textf "No <package>.opam files found." ];
  let loc, name =
    match dune_project with
    | None ->
        User_error.raise
          [ Pp.text
              "There is no dune-project file in the current directory, please \
               add one with a (name <name>) field in it."
          ]
          ~hints:
            [ Pp.text
                "dune subst must be executed from the root of the project."
            ]
    | Some { name = None; _ } ->
        User_error.raise
          [ Pp.textf
              "The project name is not defined, please add a (name <name>) \
               field to your dune-project file."
          ]
    | Some { name = Some n; _ } ->
        (n.loc_of_arg, n.arg)
  in
  if not (List.mem name ~set:package_names) then
    if Loc.is_none loc then
      User_error.raise [ Pp.textf "File %s.opam doesn't exist." name ]
    else
      User_error.raise ~loc
        [ Pp.textf
            "File %s.opam doesn't exist. It is inferred from the name in the \
             dune-project file"
            name
        ];
  name

let subst vcs =
  let+ (version, commit), files =
    Fiber.fork_and_join
      (fun () ->
        Fiber.fork_and_join
          (fun () -> Vcs.describe vcs)
          (fun () -> Vcs.commit_id vcs))
      (fun () -> Vcs.files vcs)
  in
  let dune_project =
    if List.exists files ~f:(Path.equal Dune_project.file) then
      Some (Dune_project.load Dune_project.file)
    else
      None
  in
  let name = get_name ~files ~dune_project () in
  let watermarks = make_watermark_map ~name ~version ~commit in
  Option.iter dune_project ~f:(Dune_project.subst ~map:watermarks ~version);
  List.iter files ~f:(fun path ->
      if is_a_source_file path && not (Path.equal path Dune_project.file) then
        subst_file path ~map:watermarks)

let subst () =
  match
    Sys.readdir "." |> Array.to_list |> String.Set.of_list
    |> Vcs.Kind.of_dir_contents
  with
  | None ->
      Fiber.return ()
  | Some kind ->
      subst { kind; root = Path.root }
