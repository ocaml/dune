open Import
open Jbuilder_opam_file_format

open Fiber.O

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

let make_watermark_map ~name ~version ~commit =
  let opam_file = Opam_file.load (name ^ ".opam") in
  let version_num =
    if String.is_prefix version ~prefix:"v" then
      String.sub version ~pos:1 ~len:(String.length version - 1)
    else
      version
  in
  let opam_var name sep =
    match Opam_file.get_field opam_file name with
    | None -> Error (sprintf "variable %S not found in opam file" name)
    | Some value ->
      let err = Error (sprintf "invalid value for variable %S in opam file" name) in
      match value with
      | String (_, s) -> Ok s
      | List (_, l) -> begin
          match
            List.fold_left l ~init:(Ok []) ~f:(fun acc v ->
              match acc with
              | Error _ -> acc
              | Ok l ->
                match v with
                | OpamParserTypes.String (_, s) -> Ok (s :: l)
                | _ -> err)
          with
          | Error _ as e -> e
          | Ok l -> Ok (String.concat ~sep (List.rev l))
        end
      | _ -> err
  in
  String_map.of_alist_exn
    [ "NAME"           , Ok name
    ; "VERSION"        , Ok version
    ; "VERSION_NUM"    , Ok version_num
    ; "VCS_COMMIT_ID"  , Ok commit
    ; "PKG_MAINTAINER" , opam_var "maintainer"  ", "
    ; "PKG_AUTHORS"    , opam_var "authors"     ", "
    ; "PKG_HOMEPAGE"   , opam_var "homepage"    " "
    ; "PKG_ISSUES"     , opam_var "bug-reports" " "
    ; "PKG_DOC"        , opam_var "doc"         " "
    ; "PKG_LICENSE"    , opam_var "license"     ", "
    ; "PKG_REPO"       , opam_var "dev-repo"    " "
    ]

let subst_string s ~fname ~map =
  let len = String.length s in
  let longest_var = List.longest (String_map.keys map) in
  let loc_of_offset ~ofs ~len =
    let rec loop lnum bol i =
      if i = ofs then
        let pos =
          { Lexing.
            pos_fname = fname
          ; pos_cnum  = i
          ; pos_lnum  = lnum
          ; pos_bol   = bol
          }
        in
        { Loc.start = pos; stop  = { pos with pos_cnum = pos.pos_cnum + len } }
      else
        match s.[i] with
        | '\n' -> loop (lnum + 1) (i + 1) (i + 1)
        | _    -> loop lnum bol (i + 1)
    in
    loop 1 0 0
  in
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
          | Some (Ok repl) ->
            let acc = (start, i + 1, repl) :: acc in
            loop (i + 1) acc
          | Some (Error msg) ->
            let loc = loc_of_offset ~ofs:start ~len:(i + 1 - start) in
            Loc.fail loc "%s" msg
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
  let s = Io.read_file fn in
  let s =
    if Filename.dirname fn = "." && String.is_suffix fn ~suffix:".opam" then
      "version: \"%%" ^ "VERSION_NUM" ^ "%%\"\n" ^ s
    else
      s
  in
  match subst_string s ~map ~fname:fn with
  | None -> ()
  | Some s -> Io.write_file fn s

let get_name ~files ?name () =
  let package_names =
    List.filter_map files ~f:(fun fn ->
      if Filename.dirname fn = "." then
        match Filename.split_extension fn with
        | s, ".opam" -> Some s
        | _ -> None
      else
        None)
  in
  if package_names = [] then die "@{<error>Error@}: no <package>.opam files found.";
  match name with
  | Some name ->
    if not (List.mem name ~set:package_names) then
      die "@{<error>Error@}: file %s.opam doesn't exist." name;
    name
  | None ->
    let shortest =
      match package_names with
      | [] -> assert false
      | first :: rest ->
        List.fold_left rest ~init:first ~f:(fun acc s ->
          if String.length s < String.length acc then
            s
          else
            acc)
    in
    if List.for_all package_names ~f:(String.is_prefix ~prefix:shortest) then
      shortest
    else
      die "@{<error>Error@}: cannot determine name automatically.\n\
           You must pass a [--name] command line argument."

let subst_git ?name () =
  let rev = "HEAD" in
  let git =
    match Bin.which "git" with
    | Some x -> Path.to_string x
    | None -> Utils.program_not_found "git"
  in
  Fiber.fork_and_join
    (fun () ->
       Fiber.fork_and_join
         (fun () ->
            Process.run_capture Strict git ["describe"; "--always"; "--dirty"])
         (fun () ->
            Process.run_capture Strict git ["rev-parse"; rev]))
    (fun () ->
       Process.run_capture_lines Strict git ["ls-tree"; "-r"; "--name-only"; rev])
  >>= fun ((version, commit), files) ->
  let version = String.trim version in
  let commit  = String.trim commit  in
  let name = get_name ~files ?name () in
  let watermarks = make_watermark_map ~name ~version ~commit in
  List.iter files ~f:(fun fn ->
    if is_a_source_file fn then
      subst_file fn ~map:watermarks);
  Fiber.return ()

let subst ?name () =
  if Sys.file_exists ".git" then
    subst_git ?name ()
  else
    Fiber.return ()
