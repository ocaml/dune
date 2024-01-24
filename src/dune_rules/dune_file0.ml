open Import
open Memo.O

let fname = "dune"
let alternative_fname = "dune-file"

type kind =
  | Plain
  | Ocaml_script

let dyn_of_kind = function
  | Plain -> Dyn.variant "Plain" []
  | Ocaml_script -> Dyn.variant "Ocaml_script" []
;;

let equal_kind x y =
  match x, y with
  | Plain, Plain | Ocaml_script, Ocaml_script -> true
  | _, _ -> false
;;

type t =
  { path : Path.Source.t option
  ; kind : kind
  ; (* for [kind = Ocaml_script], this is the part inserted with subdir *)
    plain : Sub_dirs.Dir_map.t
  }

let plain t = t.plain

let to_dyn { path; kind; plain } =
  let open Dyn in
  record
    [ "path", option Path.Source.to_dyn path
    ; "kind", dyn_of_kind kind
    ; "plain", Sub_dirs.Dir_map.to_dyn plain
    ]
;;

let equal { path; kind; plain } t =
  Option.equal Path.Source.equal path t.path
  && equal_kind kind t.kind
  && Sub_dirs.Dir_map.equal plain t.plain
;;

let get_static_sexp t = (Sub_dirs.Dir_map.root t.plain).sexps
let kind t = t.kind
let path t = t.path

let sub_dirs (t : t option) =
  match t with
  | None -> Sub_dirs.default
  | Some t -> Sub_dirs.or_default (Sub_dirs.Dir_map.root t.plain).subdir_status
;;

let load_plain sexps ~file ~from_parent ~project =
  let+ parsed =
    match file with
    | None -> Memo.return Sub_dirs.Dir_map.empty
    | Some file -> Sub_dirs.decode ~file project sexps
  in
  match from_parent with
  | None -> parsed
  | Some from_parent -> Sub_dirs.Dir_map.merge parsed from_parent
;;

let sub_dirnames t = Sub_dirs.Dir_map.sub_dirs t.plain

let load file ~from_parent ~project =
  let+ kind, plain =
    let load_plain = load_plain ~file ~from_parent ~project in
    match file with
    | None ->
      let+ plain = load_plain [] in
      Plain, plain
    | Some file ->
      let* kind, ast =
        Fs_memo.with_lexbuf_from_file (In_source_dir file) ~f:(fun lb ->
          let kind, ast =
            if Dune_lang.Dune_file_script.is_script lb
            then Ocaml_script, []
            else Plain, Dune_lang.Parser.parse lb ~mode:Many
          in
          kind, ast)
      in
      let+ ast = load_plain ast in
      kind, ast
  in
  { path = file; kind; plain }
;;

let ensure_dune_project_file_exists =
  let impl ~is_error project =
    let project_file = Dune_project.file project in
    let+ exists =
      Path.Outside_build_dir.In_source_dir project_file |> Fs_memo.file_exists
    in
    if not exists
    then (
      let dir = Path.Source.parent_exn project_file in
      User_warning.emit
        ~is_error
        ~hints:[ Pp.text "generate the project file with: $ dune init project <name>" ]
        [ Pp.textf
            "No dune-project file has been found in directory %S. A default one is \
             assumed but the project might break when dune is upgraded. Please create a \
             dune-project file."
            (Path.Source.to_string dir)
        ])
  in
  let memo =
    (* memoization is here just to make sure we don't warn more than once per
       project. the computation itself is cheap *)
    Memo.create
      "ensure-dune-project-file-exists"
      ~input:(module Dune_project)
      (impl ~is_error:false)
  in
  fun project ->
    match !Clflags.on_missing_dune_project_file with
    | Ignore -> Memo.return ()
    | Warn -> Memo.exec memo project
    | Error -> impl ~is_error:true project
;;

let load ~dir (status : Source_dir_status.t) project ~files ~parent =
  let file =
    if status = Data_only
    then None
    else if Dune_project.accept_alternative_dune_file_name project
            && Filename.Set.mem files alternative_fname
    then Some alternative_fname
    else if Filename.Set.mem files fname
    then Some fname
    else None
  in
  let parent =
    Option.bind parent ~f:(fun parent ->
      Sub_dirs.Dir_map.descend parent.plain (Path.Source.basename dir))
  in
  match parent, file with
  | None, None -> Memo.return None
  | _, _ ->
    let* () = ensure_dune_project_file_exists project in
    let file = Option.map file ~f:(Path.Source.relative dir) in
    load file ~from_parent:parent ~project >>| Option.some
;;
