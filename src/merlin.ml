open Import
open Build.O
open! No_io

module SC = Super_context

type t =
  { requires   : (unit, Lib.t list) Build.t
  ; flags      : (unit, string list) Build.t
  ; preprocess : Jbuild.Preprocess.t
  ; libname    : string option
  ; source_dirs: Path.Set.t
  ; objs_dirs  : Path.Set.t
  }

let ppx_flags sctx ~dir:_ ~scope ~src_dir:_ { preprocess; libname; _ } =
  match preprocess with
  | Pps { pps; flags } ->
    let exe = SC.PP.get_ppx_driver sctx ~scope pps in
    let command =
      List.map (Path.to_absolute_filename exe
                :: "--as-ppx"
                :: SC.PP.cookie_library_name libname
                @ flags)
        ~f:quote_for_shell
      |> String.concat ~sep:" "
    in
    [sprintf "FLG -ppx %s" (Filename.quote command)]
  | _ -> []

let dot_merlin sctx ~dir ~scope ({ requires; flags; _ } as t) =
  match Path.drop_build_context dir with
  | Some remaindir ->
    let merlin_file = Path.relative dir ".merlin" in
    (* We make the compilation of .ml/.mli files depend on the
       existence of .merlin so that they are always generated, however
       the command themselves don't read the merlin file, so we don't
       want to declare a dependency on the contents of the .merlin
       file.

       Currently jbuilder doesn't support declaring a dependency only
       on the existence of a file, so we have to use this trick. *)
    SC.add_rule sctx
      (Build.path merlin_file
       >>>
       Build.create_file (Path.relative dir ".merlin-exists"));
    SC.add_rule sctx ~mode:Promote_but_delete_on_clean (
      requires &&& flags
      >>^ (fun (libs, flags) ->
        let ppx_flags = ppx_flags sctx ~dir ~scope ~src_dir:remaindir t in
        let libs =
          List.fold_left ~f:(fun acc (lib : Lib.t) ->
            let serialize_path = Path.reach ~from:remaindir in
            let bpath = serialize_path (Lib.obj_dir lib) in
            let spath =
              Lib.src_dir lib
              |> Path.drop_optional_build_context
              |> serialize_path
            in
            ("B " ^ bpath) :: ("S " ^ spath) :: acc
          ) libs ~init:[]
        in
        let source_dirs =
          Path.Set.fold t.source_dirs ~init:[] ~f:(fun path acc ->
            let path = Path.reach path ~from:remaindir in
            ("S " ^ path)::acc
          )
        in
        let objs_dirs =
          Path.Set.fold t.objs_dirs ~init:[] ~f:(fun path acc ->
            let path = Path.reach path ~from:remaindir in
            ("B " ^ path)::acc
          )
        in
        let flags =
          match flags with
          | [] -> []
          | _  ->
            let escaped_flags = List.map ~f:quote_for_shell flags in
            ["FLG " ^ String.concat escaped_flags ~sep:" "]
        in
        let dot_merlin =
          List.concat
            [ source_dirs
            ; objs_dirs
            ; libs
            ; flags
            ; ppx_flags
            ]
        in
        dot_merlin
        |> String_set.of_list
        |> String_set.to_list
        |> List.map ~f:(Printf.sprintf "%s\n")
        |> String.concat ~sep:"")
      >>>
      Build.write_file_dyn merlin_file
    )
  | _ ->
    ()

let merge_two a b =
  { requires =
      (Build.fanout a.requires b.requires
       >>^ fun (x, y) ->
       Lib.L.remove_dups (x @ y))
  ; flags = a.flags &&& b.flags >>^ (fun (a, b) -> a @ b)
  ; preprocess =
      if a.preprocess = b.preprocess then
        a.preprocess
      else
        No_preprocessing
  ; libname =
      (match a.libname with
       | Some _ as x -> x
       | None -> b.libname)
  ; source_dirs = Path.Set.union a.source_dirs b.source_dirs
  ; objs_dirs = Path.Set.union a.objs_dirs b.objs_dirs
  }

let merge_all = function
  | [] -> None
  | init::ts -> Some (List.fold_left ~init ~f:merge_two ts)

let add_rules sctx ~dir ~scope merlin =
  if (SC.context sctx).merlin then
    dot_merlin sctx ~dir ~scope merlin
