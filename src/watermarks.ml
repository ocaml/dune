open Import
open Jbuilder_opam_file_format

let ( >>= ) = Future.( >>= )

let is_a_source_file fn =
  match Filename.extension fn with
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
  | ".woff" -> false
  | _ -> true

let make_watermark_map ~package ~opam_file ~git_describe ~git_commit =
  let version_num =
    if String.is_prefix git_describe ~prefix:"v" then
      String.sub git_describe ~pos:1 ~len:(String.length git_describe - 1)
    else
      git_describe
  in
  let opam_var name sep =
    match
      List.find_map opam_file.OpamParserTypes.file_contents
        ~f:(function
          | Variable (_, var, value) when name = var ->
            Some value
          | _ -> None)
    with
    | None -> sprintf "<variable %S not found in opam file>" name
    | Some value ->
      let err = sprintf "<invalid value for variable %S in opam file>" name in
      match value with
      | String (_, s) -> s
      | List (_, l) ->
        List.map l ~f:(function
          | OpamParserTypes.String (_, s) -> s
          | _ -> err)
        |> String.concat ~sep
      | _ -> err
  in
  String_map.of_alist_exn
    [ "NAME"           , package
    ; "VERSION"        , git_describe
    ; "VERSION_NUM"    , version_num
    ; "VCS_COMMIT_ID"  , git_commit
    ; "PKG_MAINTAINER" , opam_var "maintainer"  ", "
    ; "PKG_AUTHORS"    , opam_var "authors"     ", "
    ; "PKG_HOMEPAGE"   , opam_var "homepage"    " "
    ; "PKG_ISSUES"     , opam_var "bug-reports" " "
    ; "PKG_DOC"        , opam_var "doc"         " "
    ; "PKG_LICENSE"    , opam_var "license"     ", "
    ; "PKG_REPO"       , opam_var "dev-repo"    " "
    ]

let subst_string s ~map =
  let len = String.length s in
  let longest_var = List.longest (String_map.keys map) in
  let rec loop i acc =
    if i = len then
      acc
    else
      match s.[i] with
      | '%' -> after_percent (i + 1) acc
      | _ -> loop (i + 1) acc
  and after_percent i acc =
    if i = len then
      acc
    else
      match s.[i] with
      | '%' -> after_double_percent ~start:(i - 1) (i + 1) acc
      | _ -> loop (i + 1) acc
  and after_double_percent ~start i acc =
    if i = len then
      acc
    else
      match s.[i] with
      | '%' -> after_double_percent ~start:(i - 1) (i + 1) acc
      | 'A'..'Z' | '_' -> in_var ~start (i + 1) acc
      | _ -> loop (i + 1) acc
  and in_var ~start i acc =
    if i - start > longest_var then
      loop i acc
    else if i = len then
      acc
    else
      match s.[i] with
      | '%' -> end_of_var ~start (i + 1) acc
      | 'A'..'Z' | '_' -> in_var ~start (i + 1) acc
      | _ -> loop (i + 1) acc
  and end_of_var ~start i acc =
    if i = len then
      acc
    else
      match s.[i] with
      | '%' -> begin
          let var = String.sub s ~pos:(start + 2) ~len:(i - start - 3) in
          match String_map.find var map with
          | None -> in_var ~start:(i - 1) (i + 1) acc
          | Some repl ->
            let acc = (start, i + 1, repl) :: acc in
            loop (i + 1) acc
        end
      | _ -> loop (i + 1) acc
  in
  match List.rev (loop 0 []) with
  | [] -> None
  | repls ->
    let result_len =
      List.fold_left repls ~init:(String.length s) ~f:(fun acc (a, b, repl) ->
        acc - (b - a) + String.length repl)
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

let subst_file fn ~map =
  match read_file fn |> subst_string ~map with
  | None -> ()
  | Some s -> write_file fn s

let subst_git ~package =
  let opam_file = OpamParser.file (package ^ ".opam") in
  let rev = "HEAD" in
  let git =
    match Bin.which "git" with
    | Some x -> Path.to_string x
    | None -> Utils.program_not_found "git"
  in
  Future.both
    (Future.both
       (Future.run_capture Strict git ["describe"; "--always"; "--dirty"])
       (Future.run_capture Strict git ["rev-parse"; rev]))
    (Future.run_capture_lines Strict git ["ls-tree"; "-r"; "--name-only"; rev])
  >>= fun ((git_describe, git_commit), files) ->
  let git_describe = String.trim git_describe in
  let git_commit = String.trim git_commit in
  let watermarks = make_watermark_map ~package ~opam_file ~git_describe ~git_commit in
  List.iter files ~f:(fun fn ->
    if is_a_source_file fn then
      subst_file fn ~map:watermarks);
  Future.return ()

let subst ~package =
  if Sys.file_exists ".git" then
    subst_git ~package
  else
    Future.return ()
