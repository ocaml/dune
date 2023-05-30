(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) Inria 2019-2023                         *)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)

open Import

type t =
  { name : Coq_lib_name.t
  ; path : Path.t
  ; vo : Path.t list
  ; cmxs : Path.t list
  ; stdlib : bool
  }

let name t = t.name

let path t = t.path

let vo t = t.vo

let cmxs t = t.cmxs

let stdlib t = t.stdlib

let config_path_exn coq_config key =
  Coq_config.by_name coq_config key |> function
  | Some path -> (
    path |> function
    | Coq_config.Value.Path p -> p (* We have found a path for key *)
    | path ->
      (* This should never happen *)
      Code_error.raise "key is not a path"
        [ (key, Coq_config.Value.to_dyn path) ])
  | None ->
    (* This happens if the output of coqc --config doesn't include the key *)
    User_error.raise [ Pp.text "key not found from coqc --config"; Pp.text key ]

let config_path ~default coq_config key =
  Option.value ~default:(Coq_config.Value.Path default)
    (Coq_config.by_name coq_config key)
  |> function
  | Coq_config.Value.Path p -> p (* We have found a path for key *)
  | path ->
    (* This should never happen *)
    Code_error.raise "key is not a path" [ (key, Coq_config.Value.to_dyn path) ]

let stdlib_plugins_dir path =
  let open Memo.O in
  let path = Path.relative path "plugins" in
  let* dir_contents =
    Fs_memo.dir_contents (Path.as_outside_build_dir_exn path)
  in
  match dir_contents with
  | Error _ -> Memo.return []
  | Ok dir_contents ->
    let f (d, kind) =
      match kind with
      | File_kind.S_DIR | S_LNK -> Some (Path.relative path d)
      | _ -> None
    in
    Memo.return
      (List.filter_map ~f (Fs_cache.Dir_contents.to_list dir_contents))

let build_user_contrib ~cmxs ~vo ~subpath ~name =
  let path = subpath in
  { name; path; cmxs; vo; stdlib = false }

(* Scanning todos: blacklist? *)
let scan_vo_cmxs ~path dir_contents =
  let f (d, kind) =
    match kind with
    (* Skip some directories as Coq does, for now '-' and '.' *)
    | _ when String.contains d '-' -> List.Skip
    | _ when String.contains d '.' -> Skip
    | (File_kind.S_REG | S_LNK) when Filename.check_suffix d ".cmxs" ->
      Left (Path.relative path d)
    | (File_kind.S_REG | S_LNK) when Filename.check_suffix d ".vo" ->
      Right (Path.relative path d)
    | _ -> Skip
  in
  List.filter_partition_map ~f dir_contents

(* Note this will only work for absolute paths *)
let retrieve_vo_cmxs cps =
  (List.concat_map ~f:cmxs cps, List.concat_map ~f:vo cps)

(** [scan_user_path ~prefix path] Note that we already have very similar
    functionality in [Dir_status] *)
let rec scan_path ~f ~acc ~prefix path : 'a list Memo.t =
  let open Memo.O in
  let* dir_contents =
    Fs_memo.dir_contents (Path.as_outside_build_dir_exn path)
  in
  match dir_contents with
  | Error _ -> Memo.return []
  | Ok dir_contents ->
    let dir_contents = Fs_cache.Dir_contents.to_list dir_contents in
    let f (d, kind) =
      match kind with
      | File_kind.S_DIR | S_LNK ->
        let subpath = Path.relative path d in
        let prefix = acc prefix d in
        let* subpaths = scan_path ~f ~acc ~prefix subpath in
        f ~path ~prefix ~subpath ~subpaths dir_contents
      | _ -> Memo.return []
    in
    Memo.List.concat_map ~f dir_contents

let scan_user_path path =
  let f ~path ~prefix ~subpath ~subpaths dir_contents =
    let cmxs, vo = scan_vo_cmxs ~path dir_contents in
    let cmxs_r, vo_r = retrieve_vo_cmxs subpaths in
    let cmxs, vo = (cmxs @ cmxs_r, vo @ vo_r) in
    Memo.return (build_user_contrib ~cmxs ~vo ~subpath ~name:prefix :: subpaths)
  in
  scan_path path ~f ~acc:Coq_lib_name.append ~prefix:Coq_lib_name.empty

let scan_vo path =
  let f ~path ~prefix:_ ~subpath:_ ~subpaths dir_contents =
    let _, vo = scan_vo_cmxs ~path dir_contents in
    Memo.return (vo @ subpaths)
  in
  let acc _ _ = () in
  scan_path path ~f ~acc ~prefix:()

let of_coq_install coqc =
  let open Memo.O in
  let* coq_config = Coq_config.make ~coqc:(Ok coqc) in
  (* Now we query for coqlib *)
  let coqlib_path = config_path_exn coq_config "coqlib" in
  let coqcorelib = config_path coq_config "coqcorelib" ~default:coqlib_path in
  let* cmxs = stdlib_plugins_dir coqcorelib in
  let* vo = scan_vo coqlib_path in
  let stdlib =
    { name = Coq_lib_name.stdlib
    ; path = Path.relative coqlib_path "theories"
    ; vo
    ; cmxs
    ; stdlib = true
    }
  in
  let* user_contrib =
    let contrib_path = Path.relative coqlib_path "user-contrib" in
    scan_user_path contrib_path
  in
  Memo.return (stdlib :: user_contrib)

let of_coq_install coqc =
  (* If coqc was found in the _build directory then we must be composing
     with Coq and therefore cannot have any installed libs *)
  if Path.is_in_build_dir coqc then Memo.return [] else of_coq_install coqc

let of_coq_install context =
  let open Memo.O in
  let* coqc = Context.which context "coqc" in
  match coqc with
  | None -> Memo.return []
  | Some coqc -> of_coq_install coqc

let of_env env =
  let coqpath =
    (* windows uses ';' *)
    let coqpath_sep = if Sys.cygwin then ';' else Bin.path_sep in
    Env.get env "COQPATH" |> function
    | None -> []
    | Some coqpath -> Bin.parse_path ~sep:coqpath_sep coqpath
  in
  Memo.List.concat_map coqpath ~f:scan_user_path
