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
  ; cmxs_directories : Path.t list
  ; stdlib : bool
  }

let name t = t.name
let path t = t.path
let vo t = t.vo
let cmxs t = t.cmxs
let cmxs_directories t = t.cmxs_directories
let stdlib t = t.stdlib

let config_path_exn coq_config key =
  Coq_config.by_name coq_config key
  |> function
  | Some path ->
    path
    |> (function
     | Coq_config.Value.Path p -> p (* We have found a path for key *)
     | path ->
       (* This should never happen *)
       Code_error.raise "key is not a path" [ key, Coq_config.Value.to_dyn path ])
  | None ->
    (* This happens if the output of coqc --config doesn't include the key *)
    User_error.raise
      [ Pp.concat
          ~sep:Pp.space
          [ Pp.text "key not found from"; User_message.command "coqc --config" ]
        |> Pp.hovbox
      ; Pp.text key
      ]
;;

let config_path ~default coq_config key =
  Option.value
    ~default:(Coq_config.Value.path default)
    (Coq_config.by_name coq_config key)
  |> function
  | Coq_config.Value.Path p -> p (* We have found a path for key *)
  | path ->
    (* This should never happen *)
    Code_error.raise "key is not a path" [ key, Coq_config.Value.to_dyn path ]
;;

let build_user_contrib ~cmxs ~cmxs_directories ~vo ~path ~name =
  { name; path; cmxs; cmxs_directories; vo; stdlib = false }
;;

(* Scanning todos: blacklist? *)
let scan_vo_cmxs ~dir dir_contents =
  let f (d, kind) =
    match kind with
    (* Skip some files as Coq does, for now files with '-' *)
    | _ when String.contains d '-' -> List.Skip
    | (File_kind.S_REG | S_LNK) when Filename.check_suffix d ".cmxs" ->
      Left (Path.relative dir d)
    | (File_kind.S_REG | S_LNK) when Filename.check_suffix d ".vo" ->
      Right (Path.relative dir d)
    | _ -> Skip
  in
  List.filter_partition_map ~f dir_contents
;;

(* Note this will only work for absolute paths *)
let retrieve_vo_cmxs cps =
  ( List.concat_map ~f:cmxs cps
  , List.concat_map ~f:cmxs_directories cps
  , List.concat_map ~f:vo cps )
;;

module Scan_action = struct
  type ('prefix, 'res) t =
    dir:Path.t
    -> prefix:'prefix
    -> subresults:'res list
    -> (Filename.t * File_kind.t) list
    -> 'res list Memo.t
end

(** [scan_path ~f ~acc ~prefix ~dir dir_contents] Given
    [f ~dir
    ~prefix ~subresults dir_contents], [scan_path] will call [f]
    forall the subdirs of [dir] with [dir] set to the subpath, [prefix] set to
    [acc prefix d] for each subdirectory [d] and [subresults] the results of the
    scanning of children directories *)
let rec scan_path ~(f : ('prefix, 'res) Scan_action.t) ~acc ~prefix ~dir dir_contents
  : 'a list Memo.t
  =
  let open Memo.O in
  let f (d, kind) =
    match kind with
    (* We skip directories starting by . , this is mainly to avoid
       .coq-native *)
    | (File_kind.S_DIR | S_LNK) when d.[0] = '.' -> Memo.return []
    (* Need to check the link resolves to a directory! *)
    | File_kind.S_DIR | S_LNK ->
      let dir = Path.relative dir d in
      let* dir_contents = Fs_memo.dir_contents (Path.as_outside_build_dir_exn dir) in
      (match dir_contents with
       | Error _ -> Memo.return []
       | Ok dir_contents ->
         let dir_contents = Fs_cache.Dir_contents.to_list dir_contents in
         let prefix = acc prefix d in
         let* subresults = scan_path ~f ~acc ~prefix ~dir dir_contents in
         f ~dir ~prefix ~subresults dir_contents)
    | _ -> Memo.return []
  in
  Memo.List.concat_map ~f dir_contents
;;

let scan_path ~f ~acc ~prefix dir =
  let open Memo.O in
  let* dir_contents = Fs_memo.dir_contents (Path.as_outside_build_dir_exn dir) in
  match dir_contents with
  | Error _ -> Memo.return []
  | Ok dir_contents ->
    let dir_contents = Fs_cache.Dir_contents.to_list dir_contents in
    scan_path ~f ~acc ~prefix ~dir dir_contents
;;

(** Scan the plugins in stdlib, returns list of cmxs + list of directories with
    cmxs *)
let scan_stdlib_plugins coqcorelib : (Path.t list * Path.t) list Memo.t =
  let f ~dir ~prefix:() ~subresults dir_contents =
    let cmxs, _ = scan_vo_cmxs ~dir dir_contents in
    let res =
      match cmxs with
      | [] -> subresults
      | _ :: _ -> (cmxs, dir) :: subresults
    in
    Memo.return res
  in
  let pluginsdir = Path.relative coqcorelib "plugins" in
  let acc _ _ = () in
  scan_path ~f ~acc ~prefix:() pluginsdir
;;

(** [scan_user_path path] Note that we already have very similar functionality
    in [Dir_status] *)
let scan_user_path root_path =
  let f ~dir ~prefix ~subresults dir_contents =
    let cmxs, vo = scan_vo_cmxs ~dir dir_contents in
    let cmxs_directories = if not (List.is_empty cmxs) then [ dir ] else [] in
    let cmxs_r, cdir_r, vo_r = retrieve_vo_cmxs subresults in
    let cmxs, cmxs_directories, vo =
      cmxs @ cmxs_r, cmxs_directories @ cdir_r, vo @ vo_r
    in
    Memo.return
      (build_user_contrib ~cmxs ~cmxs_directories ~vo ~path:dir ~name:prefix :: subresults)
  in
  scan_path ~f ~acc:Coq_lib_name.append ~prefix:Coq_lib_name.empty root_path
;;

let scan_vo root_path =
  let f ~dir ~prefix:() ~subresults dir_contents =
    let _, vo = scan_vo_cmxs ~dir dir_contents in
    Memo.return (vo @ subresults)
  in
  let acc _ _ = () in
  scan_path ~f ~acc ~prefix:() root_path
;;

let of_coq_install coqc =
  let open Memo.O in
  let* coq_config = Coq_config.make ~coqc:(Ok coqc) in
  match coq_config with
  | Error msg ->
    User_warning.emit
      [ Pp.concat
          ~sep:Pp.space
          [ Pp.text "Skipping installed theories due to"
          ; User_message.command "coqc --config"
          ; Pp.text "failure:"
          ]
        |> Pp.hovbox
      ; Pp.enumerate ~f:Fun.id [ msg ]
      ]
      ~hints:
        [ Pp.concat
            ~sep:Pp.space
            [ Pp.text "Try running"
            ; User_message.command "coqc --config"
            ; Pp.text "manually to see the error."
            ]
          |> Pp.hovbox
        ];
    Memo.return []
  | Ok coq_config ->
    (* Now we query for coqlib *)
    let coqlib_path = config_path_exn coq_config "coqlib" in
    let coqcorelib = config_path coq_config "coqcorelib" ~default:coqlib_path in
    let* stdlib_plugs = scan_stdlib_plugins coqcorelib in
    let* vo = scan_vo coqlib_path in
    let cmxs, cmxs_directories = List.split stdlib_plugs in
    let cmxs = List.concat cmxs in
    let stdlib =
      { name = Coq_lib_name.stdlib
      ; path = Path.relative coqlib_path "theories"
      ; vo
      ; cmxs
      ; cmxs_directories
      ; stdlib = true
      }
    in
    let* user_contrib =
      let contrib_path = Path.relative coqlib_path "user-contrib" in
      scan_user_path contrib_path
    in
    Memo.return (stdlib :: user_contrib)
;;

let of_coq_install coqc =
  (* If coqc was found in the _build directory then we must be composing
     with Coq and therefore cannot have any installed libs *)
  if Path.is_in_build_dir coqc then Memo.return [] else of_coq_install coqc
;;

let of_coq_install context =
  let open Memo.O in
  let* coqc = Context.which context "coqc" in
  match coqc with
  | None -> Memo.return []
  | Some coqc -> of_coq_install coqc
;;

let of_env env =
  let coqpath =
    (* windows uses ';' *)
    let coqpath_sep = if Sys.cygwin then ';' else Bin.path_sep in
    Env.get env "COQPATH"
    |> function
    | None -> []
    | Some coqpath -> Bin.parse_path ~sep:coqpath_sep coqpath
  in
  Memo.List.concat_map coqpath ~f:scan_user_path
;;
