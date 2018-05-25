open Import
open Build.O
open! No_io

module SC = Super_context

module Preprocess = struct
  type t =
    | Pps of Jbuild.Preprocess.pps
    | Other

  let make : Jbuild.Preprocess.t -> t = function
    | Pps pps -> Pps pps
    | _       -> Other

  let merge a b =
    match a, b with
    | Other, Other -> Other
    | Pps _, Other -> a
    | Other, Pps _ -> b
    | Pps { pps = pps1; flags = flags1 },
      Pps { pps = pps2; flags = flags2 } ->
      match
        match List.compare flags1 flags2 ~compare:String.compare with
        | Eq ->
          List.compare pps1 pps2 ~compare:(fun (_, a) (_, b) ->
            Jbuild.Pp.compare a b)
        | ne -> ne
      with
      | Eq -> a
      | _  -> Other
end

module Dot_file = struct
  type t =
    { build_dirs: Path.Set.t
    ; src_dirs: Path.Set.t
    ; flags: string list
    ; ppx: string list
    ; remaindir: Path.t
    }

  let create ~flags ~ppx ~remaindir =
    { remaindir
    ; build_dirs = Path.Set.empty
    ; src_dirs = Path.Set.empty
    ; ppx
    ; flags
    }

  let serialize_path t p = Path.reach p ~from:t.remaindir

  let b = Buffer.create 256

  let printf = Printf.bprintf b
  let print = Buffer.add_string b

  let to_string t =
    Buffer.clear b;
    Path.Set.iter t.build_dirs ~f:(fun p ->
      printf "B %s\n" (serialize_path t p)
    );
    Path.Set.iter t.src_dirs ~f:(fun p ->
      printf "S %s\n" (serialize_path t p)
    );
    begin match t.ppx with
    | [] -> ()
    | ppx ->
      printf "FLG -ppx %s\n" (Filename.quote (String.concat ~sep:" " ppx));
    end;
    begin match t.flags with
    | [] -> ()
    | flags ->
      print "FLG";
      List.iter flags ~f:(fun f ->
        printf " %s" (quote_for_shell f)
      );
      print "\n"
    end;
    Buffer.contents b
end

type t =
  { requires   : Lib.Set.t
  ; flags      : (unit, string list) Build.t
  ; preprocess : Preprocess.t
  ; libname    : string option
  ; source_dirs: Path.Set.t
  ; objs_dirs  : Path.Set.t
  }

let make
      ?(requires=Ok [])
      ?(flags=Build.return [])
      ?(preprocess=Jbuild.Preprocess.No_preprocessing)
      ?libname
      ?(source_dirs=Path.Set.empty)
      ?(objs_dirs=Path.Set.empty)
      () =
  (* Merlin shouldn't cause the build to fail, so we just ignore errors *)
  let requires =
    match requires with
    | Ok    l -> Lib.Set.of_list l
    | Error _ -> Lib.Set.empty
  in
  { requires
  ; flags      = Build.catch flags    ~on_error:(fun _ -> [])
  ; preprocess = Preprocess.make preprocess
  ; libname
  ; source_dirs
  ; objs_dirs
  }

let add_source_dir t dir =
  { t with source_dirs = Path.Set.add t.source_dirs dir }

let ppx_flags sctx ~dir:_ ~scope ~src_dir:_ { preprocess; libname; _ } =
  match preprocess with
  | Pps { pps; flags } ->
    let exe = Preprocessing.get_ppx_driver sctx ~scope pps in
    (Path.to_absolute_filename exe ~root:!Clflags.workspace_root
     :: "--as-ppx"
     :: Preprocessing.cookie_library_name libname
     @ flags)
  | Other -> []

let dot_merlin sctx ~dir ~scope ({ requires; flags; _ } as t) =
  match Path.drop_build_context dir with
  | None -> ()
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
      flags
      >>^ (fun flags ->
        let dot_file =
          Dot_file.create ~flags
            ~ppx:(ppx_flags sctx ~dir ~scope ~src_dir:remaindir t)
            ~remaindir in
        let dot_file =
          Lib.Set.fold requires ~init:dot_file ~f:(fun (lib : Lib.t) acc ->
            { acc with
              Dot_file.
              src_dirs = Path.Set.add acc.src_dirs (Lib.src_dir lib)
            ; build_dirs = Path.Set.add acc.build_dirs (
                Lib.obj_dir lib
                |> Path.drop_optional_build_context
              )
            })
        in
        let dot_file =
          { dot_file with
            Dot_file.
            src_dirs = Path.Set.union t.source_dirs dot_file.src_dirs
          ; build_dirs = Path.Set.union t.objs_dirs dot_file.build_dirs
          }
        in
        Dot_file.to_string dot_file)
      >>>
      Build.write_file_dyn merlin_file
    )

let merge_two a b =
  { requires = Lib.Set.union a.requires b.requires
  ; flags = a.flags &&& b.flags >>^ (fun (a, b) -> a @ b)
  ; preprocess = Preprocess.merge a.preprocess b.preprocess
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
