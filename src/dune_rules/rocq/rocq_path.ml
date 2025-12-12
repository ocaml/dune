(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open Import

type legacy =
  { name : Rocq_lib_name.t
  ; path : Path.t
  ; vo : Path.t list
  ; corelib : bool
  }

type t =
  | Rocq_package of Rocq_package.t
  | Legacy of legacy

let name = function
  | Legacy t -> t.name
  | Rocq_package t -> Rocq_package.name t
;;

let path = function
  | Legacy t -> t.path
  | Rocq_package t -> Rocq_package.path t
;;

let vo = function
  | Legacy t -> t.vo
  | Rocq_package t -> Rocq_package.vo t
;;

let corelib = function
  | Legacy t -> t.corelib
  | Rocq_package _ -> false
;;

let config_path_exn rocq_config key =
  Rocq_config.by_name rocq_config key
  |> function
  | Some path ->
    path
    |> (function
     | Rocq_config.Value.Path p -> p (* We have found a path for key *)
     | path ->
       (* This should never happen *)
       Code_error.raise "key is not a path" [ key, Rocq_config.Value.to_dyn path ])
  | None ->
    (* This happens if the output of rocq --config doesn't include the key *)
    User_error.raise
      [ Pp.concat
          ~sep:Pp.space
          [ Pp.text "key not found from"; User_message.command "rocq --config" ]
        |> Pp.hovbox
      ; Pp.text key
      ]
;;

let build_user_contrib_legacy ~vo ~path ~name = Legacy { name; path; vo; corelib = false }
let build_user_contrib ~vo ~path meta = Rocq_package (Rocq_package.make ~vo ~path meta)

(* Scanning todos: blacklist? *)
let scan_vo_and_pkg ~dir dir_contents =
  let f (d, kind) =
    match kind with
    | (File_kind.S_REG | S_LNK) when String.equal d Rocq_package.rocq_package_file ->
      List.Right (Path.relative dir d)
    (* Skip some files as Coq does, for now files with '-' *)
    | _ when String.contains d '-' -> Skip
    | (File_kind.S_REG | S_LNK) when Filename.check_suffix d ".vo" ->
      Left (Path.relative dir d)
    | _ -> Skip
  in
  List.filter_partition_map ~f dir_contents
;;

(* Note this will only work for absolute paths *)
let retrieve_vo_and_pkgs cps = List.concat_map ~f:vo cps, ()

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

(** [scan_user_path path] Note that we already have very similar functionality
    in [Dir_status] *)
let scan_user_path root_path =
  let f ~dir ~prefix ~subresults dir_contents =
    let vo, pkg_files = scan_vo_and_pkg ~dir dir_contents in
    match pkg_files with
    | [] ->
      let vo_r, _pkgs_r = retrieve_vo_and_pkgs subresults in
      let vo = vo @ vo_r in
      Memo.return (build_user_contrib_legacy ~vo ~path:dir ~name:prefix :: subresults)
    | pkg_file :: [] ->
      let meta = Rocq_package.of_file ~name:prefix pkg_file in
      Format.eprintf "TEST: %s\n%!" (Path.to_string dir);
      Memo.return (build_user_contrib ~vo ~path:dir meta :: subresults)
    | _ -> assert false
  in
  scan_path ~f ~acc:Rocq_lib_name.append ~prefix:Rocq_lib_name.empty root_path
;;

let scan_vo root_path =
  let f ~dir ~prefix:() ~subresults dir_contents =
    let vo, _pkgs = scan_vo_and_pkg ~dir dir_contents in
    Memo.return (vo @ subresults)
  in
  let acc _ _ = () in
  scan_path ~f ~acc ~prefix:() root_path
;;

let of_rocq_install rocq =
  let open Memo.O in
  let* rocq_config = Rocq_config.make ~rocq:(Ok rocq) in
  match rocq_config with
  | Error msg ->
    User_warning.emit
      [ Pp.concat
          ~sep:Pp.space
          [ Pp.text "Skipping installed theories due to"
          ; User_message.command "rocq --config"
          ; Pp.text "failure:"
          ]
        |> Pp.hovbox
      ; Pp.enumerate ~f:Fun.id [ msg ]
      ]
      ~hints:
        [ Pp.concat
            ~sep:Pp.space
            [ Pp.text "Try running"
            ; User_message.command "rocq --config"
            ; Pp.text "manually to see the error."
            ]
          |> Pp.hovbox
        ];
    Memo.return []
  | Ok rocq_config ->
    (* Now we query for rocqlib *)
    let rocqlib_path = config_path_exn rocq_config "rocqlib" in
    let* vo = scan_vo rocqlib_path in
    let corelib =
      Legacy
        { name = Rocq_lib_name.corelib
        ; path = Path.relative rocqlib_path "theories"
        ; vo
        ; corelib = true
        }
    in
    let* user_contrib =
      let contrib_path = Path.relative rocqlib_path "user-contrib" in
      scan_user_path contrib_path
    in
    Memo.return (corelib :: user_contrib)
;;

let of_rocq_install rocq =
  (* If rocq was found in the _build directory then we must be composing
     with Rocq and therefore cannot have any installed libs *)
  if Path.is_in_build_dir rocq then Memo.return [] else of_rocq_install rocq
;;

let of_rocq_install context =
  let open Memo.O in
  let* rocq = Context.which context "rocq" in
  match rocq with
  | None -> Memo.return []
  | Some rocq -> of_rocq_install rocq
;;

let of_env env =
  let rocqpath =
    (* windows uses ';' *)
    let rocqpath_sep = if Sys.cygwin then ';' else Bin.path_sep in
    Env.get env "ROCQPATH"
    |> function
    | None -> []
    | Some rocqpath -> Bin.parse_path ~sep:rocqpath_sep rocqpath
  in
  Memo.List.concat_map rocqpath ~f:scan_user_path
;;
