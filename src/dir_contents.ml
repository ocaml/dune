open! Stdune
open Import
module Menhir_rules = Menhir
open Dune_file
open! No_io

module Executables_modules = struct
  type t = Module.Name_map.t
end

module Modules = struct
  type t =
    { libraries : Lib_modules.t Lib_name.Map.t
    ; executables : Executables_modules.t String.Map.t
    ; (* Map from modules to the buildable they are part of *)
      rev_map : Buildable.t Module.Name.Map.t
    }

  let empty =
    { libraries = Lib_name.Map.empty
    ; executables = String.Map.empty
    ; rev_map = Module.Name.Map.empty
    }
end

type t =
  { kind : kind
  ; dir : Path.Build.t
  ; text_files : String.Set.t
  ; modules : Modules.t Memo.Lazy.t
  ; c_sources : C_sources.t Memo.Lazy.t
  ; mlds : (Dune_file.Documentation.t * Path.Build.t list) list Memo.Lazy.t
  ; coq_modules : Coq_module.t list Lib_name.Map.t Memo.Lazy.t
  }

and kind =
  | Standalone
  | Group_root of t list
  | Group_part

type gen_rules_result =
  | Standalone_or_root of t * t list
  | Group_part of Path.Build.t

let dir t = t.dir

let dirs t =
  match t.kind with
  | Standalone -> [t]
  | Group_root subs -> t :: subs
  | Group_part ->
    Code_error.raise "Dir_contents.dirs called on a group part"
      [ "dir", Path.Build.to_dyn t.dir ]

let text_files t = t.text_files

let modules_of_library t ~name =
  let map = (Memo.Lazy.force t.modules).libraries in
  match Lib_name.Map.find map name with
  | Some m -> m
  | None ->
    Errors.code_error "Dir_contents.modules_of_library"
      [ "name", Lib_name.to_sexp name
      ; "available", Sexp.Encoder.(list Lib_name.to_sexp) (Lib_name.Map.keys map)
      ]

let modules_of_executables t ~first_exe =
  let map = (Memo.Lazy.force t.modules).executables in
  match String.Map.find map first_exe with
  | Some m -> m
  | None ->
    Errors.code_error "Dir_contents.modules_of_executables"
      [ "first_exe", Sexp.Encoder.string first_exe
      ; "available", Sexp.Encoder.(list string) (String.Map.keys map)
      ]

let c_sources_of_library t ~name =
  C_sources.for_lib (Memo.Lazy.force t.c_sources)
    ~dir:(Path.build t.dir) ~name

let lookup_module t name =
  Module.Name.Map.find (Memo.Lazy.force t.modules).rev_map name

let mlds t (doc : Documentation.t) =
  let map = Memo.Lazy.force t.mlds in
  match
    List.find_map map ~f:(fun (doc', x) ->
      Option.some_if (Loc.equal doc.loc doc'.loc) x)
  with
  | Some x -> x
  | None ->
    Errors.code_error "Dir_contents.mlds"
      [ "doc", Loc.to_sexp doc.loc
      ; "available", Sexp.Encoder.(list Loc.to_sexp)
                       (List.map map ~f:(fun (d, _) -> d.Documentation.loc))
      ]

let coq_modules_of_library t ~name =
  let map = Memo.Lazy.force t.coq_modules in
  match Lib_name.Map.find map name with
  | Some x -> x
  | None ->
    Errors.code_error "Dir_contents.coq_modules_of_library"
      [ "name", Lib_name.to_sexp name
      ; "available", Sexp.Encoder.(list Lib_name.to_sexp) (Lib_name.Map.keys map)
      ]

let modules_of_files ~dir ~files =
  let dir = Path.build dir in
  let make_module syntax base fn =
    (Module.Name.of_string base,
     Module.File.make syntax (Path.relative dir fn))
  in
  let impl_files, intf_files =
    String.Set.to_list files
    |> List.filter_partition_map ~f:(fun fn ->
      (* we aren't using Filename.extension because we want to handle
         filenames such as foo.cppo.ml *)
      match String.lsplit2 fn ~on:'.' with
      | Some (s, "ml" ) -> Left  (make_module OCaml  s fn)
      | Some (s, "re" ) -> Left  (make_module Reason s fn)
      | Some (s, "mli") -> Right (make_module OCaml  s fn)
      | Some (s, "rei") -> Right (make_module Reason s fn)
      | _ -> Skip)
  in
  let parse_one_set (files : (Module.Name.t * Module.File.t) list)  =
    match Module.Name.Map.of_list files with
    | Ok x -> x
    | Error (name, f1, f2) ->
      let src_dir = Path.drop_build_context_exn dir in
      die "Too many files for module %a in %a:\
           \n- %a\
           \n- %a"
        Module.Name.pp name
        Path.Source.pp src_dir
        Path.pp f1.path
        Path.pp f2.path
  in
  let impls = parse_one_set impl_files in
  let intfs = parse_one_set intf_files in
  Module.Name.Map.merge impls intfs ~f:(fun name impl intf ->
    Some (Module.Source.make name ?impl ?intf))

let build_mlds_map (d : _ Dir_with_dune.t) ~files =
  let dir = d.ctx_dir in
  let mlds = Memo.lazy_ (fun () -> (
      String.Set.fold files ~init:String.Map.empty ~f:(fun fn acc ->
        match String.lsplit2 fn ~on:'.' with
        | Some (s, "mld") -> String.Map.add acc s fn
        | _ -> acc)))
  in
  List.filter_map d.data ~f:(function
    | Documentation doc ->
      let mlds =
        let mlds = Memo.Lazy.force mlds in
        Ordered_set_lang.String.eval_unordered doc.mld_files
          ~parse:(fun ~loc s ->
            match String.Map.find mlds s with
            | Some s ->
              s
            | None ->
              Errors.fail loc "%s.mld doesn't exist in %s" s
                (Path.to_string_maybe_quoted
                   (Path.drop_optional_build_context (Path.build dir)))
          )
          ~standard:mlds
      in
      Some (doc, List.map (String.Map.values mlds) ~f:(Path.Build.relative dir))
    | _ -> None)

let coq_modules_of_files ~subdirs =
  let filter_v_files (dir, local, files) =
    (dir, local, String.Set.filter files ~f:(fun f -> Filename.check_suffix f ".v")) in
  let subdirs = List.map subdirs ~f:filter_v_files in
  let build_mod_dir (dir, prefix, files) =
    String.Set.to_list files |> List.map ~f:(fun file ->
      let name, _ = Filename.split_extension file in
      let name = Coq_module.Name.make name in
      Coq_module.make
        ~source:(Path.Build.relative dir file)
        ~prefix ~name) in
  let modules = List.concat_map ~f:build_mod_dir subdirs in
  modules

(* TODO: Build reverse map and check duplicates, however, are duplicates harmful?
 * In Coq all libs are "wrapped" so including a module twice is not so bad.
 *)
let build_coq_modules_map (d : _ Dir_with_dune.t) ~dir ~modules =
  List.fold_left d.data ~init:Lib_name.Map.empty ~f:(fun map -> function
    | Coq.T coq ->
      let modules = Coq_module.Eval.eval coq.modules
        ~parse:(Coq_module.parse ~dir) ~standard:modules in
      Lib_name.Map.add map (Dune_file.Coq.best_name coq) modules
    | _ -> map)

module rec Load : sig
  val get : Super_context.t -> dir:Path.Build.t -> t
  val gen_rules : Super_context.t -> dir:Path.Build.t -> gen_rules_result
end = struct
  let virtual_modules sctx vlib =
    let info = Lib.info vlib in
    let lib_modules =
      match Option.value_exn (Lib_info.virtual_ info) with
      | External lib_modules -> lib_modules
      | Local ->
        let src_dir =
          Lib_info.src_dir info
          |> Path.as_in_build_dir_exn
        in
        let t = Load.get sctx ~dir:src_dir in
        modules_of_library t ~name:(Lib.name vlib)
    in
    let existing_virtual_modules =
      Lib_modules.virtual_modules lib_modules
      |> Module.Name.Map.keys
      |> Module.Name.Set.of_list
    in
    let allow_new_public_modules =
      Lib_modules.wrapped lib_modules
      |> Wrapped.to_bool
      |> not
    in
    { Modules_field_evaluator.Implementation.
      existing_virtual_modules
    ; allow_new_public_modules
    }

  let make_modules sctx (d : _ Dir_with_dune.t) ~modules =
    let scope = d.scope in
    let libs, exes =
      List.filter_partition_map d.data ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Library lib ->
          let src_dir = d.ctx_dir in
          let resolved = lazy (
            let name = Library.best_name lib in
            Lib.DB.find_even_when_hidden (Scope.libs scope) name
            (* can't happen because this library is defined using the current
               stanza *)
            |> Option.value_exn
          ) in
          let kind : Modules_field_evaluator.kind =
            match lib.implements, lib.virtual_modules with
            | Some _, None ->
              (* diml: this [Result.ok_exn] means that if the user
                 writes an invalid [implements] field, we will get an
                 error immediately even if the library is not
                 built. We should change this to carry the [Or_exn.t]
                 a bit longer. *)
              let vlib = Result.ok_exn (
                let lib = Lazy.force resolved in
                (* This [Option.value_exn] is correct because the
                   above [lib.implements] is [Some _] and this [lib]
                   variable correspond to the same library. *)
                Option.value_exn (Lib.implements lib))
              in
              Implementation (virtual_modules sctx vlib)
            | None, Some virtual_modules ->
              Virtual { Modules_field_evaluator.Virtual.
                        virtual_modules
                      }
            | None, None -> Exe_or_normal_lib
            | Some _, Some _ -> assert false
          in
          let modules =
            Modules_field_evaluator.eval ~modules
              ~buildable:lib.buildable
              ~kind
              ~private_modules:(
                Option.value ~default:Ordered_set_lang.standard
                  lib.private_modules)
          in
          let (main_module_name, wrapped) =
            (* the common case are normal libs where this is all specified so we
               special case it so that we don't have to resolve anything the db *)
            match Library.main_module_name lib, lib.wrapped with
            (* these values are always either inherited together or specified *)
            | This _, From _
            | From _, This _ -> assert false
            | This mmn, This wrapped -> mmn, wrapped
            | From _, From _ ->
              Result.ok_exn (
                let lib = Lazy.force resolved in
                let open Result.O in
                let* main_module_name = Lib.main_module_name lib in
                let+ wrapped = Lib.wrapped lib in
                (main_module_name, Option.value_exn wrapped)
              )
          in
          Left ( lib
               , let src_dir = Path.build src_dir in
                 Lib_modules.make lib ~src_dir modules ~main_module_name
                   ~wrapped
               )
        | Executables exes
        | Tests { exes; _} ->
          let modules =
            Modules_field_evaluator.eval ~modules
              ~buildable:exes.buildable
              ~kind:Modules_field_evaluator.Exe_or_normal_lib
              ~private_modules:Ordered_set_lang.standard
          in
          Right (exes, modules)
        | _ -> Skip)
    in
    let libraries =
      match
        Lib_name.Map.of_list_map libs ~f:(fun (lib, m) -> Library.best_name lib, m)
      with
      | Ok x -> x
      | Error (name, _, (lib2, _)) ->
        Errors.fail lib2.buildable.loc
          "Library %a appears for the second time \
           in this directory"
          Lib_name.pp_quoted name
    in
    let executables =
      match
        String.Map.of_list_map exes
          ~f:(fun (exes, m) -> snd (List.hd exes.names), m)
      with
      | Ok x -> x
      | Error (name, _, (exes2, _)) ->
        Errors.fail exes2.buildable.loc
          "Executable %S appears for the second time \
           in this directory"
          name
    in
    let rev_map =
      let rev_modules =
        List.rev_append
          (List.concat_map libs ~f:(fun (l, m) ->
             let modules = Lib_modules.modules m in
             List.map (Module.Name.Map.values modules) ~f:(fun m ->
               (Module.name m, l.buildable))))
          (List.concat_map exes ~f:(fun (e, m) ->
             List.map (Module.Name.Map.values m) ~f:(fun m ->
               (Module.name m, e.buildable))))
      in
      match d.kind with
      | Dune -> begin
          match Module.Name.Map.of_list rev_modules with
          | Ok x -> x
          | Error (name, _, _) ->
            let open Module.Name.Infix in
            let locs =
              List.filter_map rev_modules ~f:(fun (n, b) ->
                Option.some_if (n = name) b.loc)
              |> List.sort ~compare
            in
            Errors.fail (Loc.drop_position (List.hd locs))
              "Module %a is used in several stanzas:@\n\
               @[<v>%a@]@\n\
               @[%a@]"
              Module.Name.pp_quote name
              (Fmt.list (Fmt.prefix (Fmt.string "- ") Loc.pp_file_colon_line))
              locs
              Format.pp_print_text
              "To fix this error, you must specify an explicit \"modules\" \
               field in every library, executable, and executables stanzas in \
               this dune file. Note that each module cannot appear in more \
               than one \"modules\" field - it must belong to a single library \
               or executable."
        end
      | Jbuild ->
        Module.Name.Map.of_list_multi rev_modules
        |> Module.Name.Map.mapi ~f:(fun name buildables ->
          match buildables with
          | [] -> assert false
          | [b] -> b
          | b :: rest ->
            let locs =
              List.sort ~compare
                (b.Buildable.loc :: List.map rest ~f:(fun b -> b.Buildable.loc))
            in
            (* DUNE2: make this an error *)
            Errors.warn (Loc.drop_position b.loc)
              "Module %a is used in several stanzas:@\n\
               @[<v>%a@]@\n\
               @[%a@]@\n\
               This warning will become an error in the future."
              Module.Name.pp_quote name
              (Fmt.list (Fmt.prefix (Fmt.string "- ") Loc.pp_file_colon_line))
              locs
              Format.pp_print_text
              "To remove this warning, you must specify an explicit \"modules\" \
               field in every library, executable, and executables stanzas in \
               this jbuild file. Note that each module cannot appear in more \
               than one \"modules\" field - it must belong to a single library \
               or executable.";
            b)
    in
    { Modules. libraries; executables; rev_map }

  (* As a side-effect, setup user rules and copy_files rules. *)
  let load_text_files sctx ft_dir
        { Dir_with_dune.
          ctx_dir = dir
        ; src_dir
        ; scope = _
        ; data = stanzas
        ; kind = _
        ; dune_version = _
        } =
    (* Interpret a few stanzas in order to determine the list of
       files generated by the user. *)
    let expander = Super_context.expander sctx ~dir in
    let generated_files =
      List.concat_map stanzas ~f:(fun stanza ->
        match (stanza : Stanza.t) with
        | Coqpp.T { modules; _ } ->
          List.map modules ~f:(fun m -> m ^ ".ml")
        | Menhir.T menhir ->
          Menhir_rules.targets menhir
        | Rule rule ->
          Simple_rules.user_rule sctx rule ~dir ~expander
          |> Path.Build.Set.to_list
          |> List.map ~f:Path.Build.basename
        | Copy_files def ->
          Simple_rules.copy_files sctx def ~src_dir ~dir ~expander
          |> Path.Set.to_list
          |> List.map ~f:Path.basename
        | Library { buildable; _ } | Executables { buildable; _ } ->
          (* Manually add files generated by the (select ...)
             dependencies *)
          List.filter_map buildable.libraries ~f:(fun dep ->
            match (dep : Dune_file.Lib_dep.t) with
            | Direct _ -> None
            | Select s -> Some s.result_fn)
        | _ -> [])
      |> String.Set.of_list
    in
    String.Set.union generated_files (File_tree.Dir.files ft_dir)


  type result0_here = {
    t : t;
    (* [rules] includes rules for subdirectories too *)
    rules : Rules.t option;
    subdirs : t Path.Build.Map.t;
  }

  type result0 =
    | See_above of Path.Build.t
    | Here of result0_here

  module Key = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Path.Build.t

    let to_dyn (sctx, path) =
      Dyn.Tuple [Super_context.to_dyn sctx; Path.Build.to_dyn path;]

    let equal = Tuple.T2.equal Super_context.equal Path.Build.equal
    let hash = Tuple.T2.hash Super_context.hash Path.Build.hash
  end

  let check_no_qualified loc qualif_mode =
    if qualif_mode = Include_subdirs.Qualified then
      Errors.fail loc "(include_subdirs qualified) is not supported yet"

  let check_no_unqualified loc qualif_mode =
    if qualif_mode = Include_subdirs.Unqualified then
      Errors.fail loc "(include_subdirs qualified) is not supported yet"

  let get0_impl (sctx, dir) : result0 =
    let dir_status_db = Super_context.dir_status_db sctx in
    match Dir_status.DB.get dir_status_db ~dir with
    | Standalone x ->
      (match x with
       | Some (ft_dir, Some d) ->
         let files, rules =
           Rules.collect_opt (fun () -> load_text_files sctx ft_dir d)
         in
         Here {
           t = { kind = Standalone
               ; dir
               ; text_files = files
               ; modules = Memo.lazy_ (fun () ->
                   make_modules sctx d
                     ~modules:(modules_of_files ~dir:d.ctx_dir ~files))
               ; mlds = Memo.lazy_ (fun () -> build_mlds_map d ~files)
               ; c_sources = Memo.lazy_ (fun () ->
                   let dune_version = d.dune_version in
                   C_sources.make d
                     ~c_sources:(
                       C_sources.load_sources ~dune_version ~dir:d.ctx_dir ~files))
               ; coq_modules = Memo.lazy_ (fun () ->
                   build_coq_modules_map d ~dir:d.ctx_dir
                     ~modules:(
                       coq_modules_of_files ~subdirs:[dir,[],files]))
               };
           rules;
           subdirs = Path.Build.Map.empty;
         }
       | Some (_, None)
       | None ->
         Here {
           t = { kind = Standalone
               ; dir
               ; text_files = String.Set.empty
               ; modules = Memo.Lazy.of_val Modules.empty
               ; mlds = Memo.Lazy.of_val []
               ; c_sources = Memo.Lazy.of_val C_sources.empty
               ; coq_modules = Memo.Lazy.of_val Lib_name.Map.empty
               };
           rules = None;
           subdirs = Path.Build.Map.empty;
         })
    | Is_component_of_a_group_but_not_the_root { group_root; _ } ->
      See_above group_root
    | Group_root (ft_dir, qualif_mode, d) ->
      let rec walk ft_dir ~dir ~local acc =
        match
          Dir_status.DB.get dir_status_db ~dir
        with
        | Is_component_of_a_group_but_not_the_root { stanzas = d; group_root = _ } ->
          let files =
            match d with
            | None -> File_tree.Dir.files ft_dir
            | Some d -> load_text_files sctx ft_dir d
          in
          walk_children ft_dir ~dir ~local ((dir, List.rev local, files) :: acc)
        | _ -> acc
      and walk_children ft_dir ~dir ~local acc =
        String.Map.foldi (File_tree.Dir.sub_dirs ft_dir) ~init:acc
          ~f:(fun name ft_dir acc ->
            let dir = Path.Build.relative dir name in
            let local = if qualif_mode = Qualified then name :: local else local in
            walk ft_dir ~dir ~local acc)
      in
      let (files, (subdirs : (Path.Build.t * _ * _) list)), rules =
        Rules.collect_opt (fun () ->
          let files = load_text_files sctx ft_dir d in
          let subdirs = walk_children ft_dir ~dir ~local:[] [] in
          files, subdirs)
      in
      let modules = Memo.lazy_ (fun () ->
        check_no_qualified Loc.none qualif_mode;
        let modules =
          List.fold_left ((dir, [], files) :: subdirs) ~init:Module.Name.Map.empty
            ~f:(fun acc ((dir : Path.Build.t), _local, files) ->
              let modules = modules_of_files ~dir ~files in
              Module.Name.Map.union acc modules ~f:(fun name x y ->
                Errors.fail (Loc.in_file
                               (Path.source (match File_tree.Dir.dune_file ft_dir with
                                  | None ->
                                    Path.Source.relative (File_tree.Dir.path ft_dir)
                                      "_unknown_"
                                  | Some d -> File_tree.Dune_file.path d)))
                  "Module %a appears in several directories:\
                   @\n- %a\
                   @\n- %a"
                  Module.Name.pp_quote name
                  Path.pp (Module.Source.src_dir x)
                  Path.pp (Module.Source.src_dir y)))
        in
        make_modules sctx d ~modules)
      in
      let c_sources = Memo.lazy_ (fun () ->
        check_no_qualified Loc.none qualif_mode;
        let dune_version = d.dune_version in
        let init = C.Kind.Dict.make_both String.Map.empty in
        let c_sources =
          List.fold_left ((dir, [], files) :: subdirs) ~init
            ~f:(fun acc (dir, _local, files) ->
              let sources = C_sources.load_sources ~dir ~dune_version ~files in
              let f acc sources =
                String.Map.union acc sources ~f:(fun name x y ->
                  Errors.fail (Loc.in_file
                                 (Path.source (match File_tree.Dir.dune_file ft_dir with
                                    | None ->
                                      Path.Source.relative (File_tree.Dir.path ft_dir)
                                        "_unknown_"
                                    | Some d -> File_tree.Dune_file.path d)))
                    "%a file %s appears in several directories:\
                     @\n- %a\
                     @\n- %a\
                     @\nThis is not allowed, please rename one of them."
                    (C.Kind.pp) (C.Source.kind x)
                    name
                    Path.pp_in_source (Path.build (C.Source.src_dir x))
                    Path.pp_in_source (Path.build (C.Source.src_dir y)))
              in
              C.Kind.Dict.merge acc sources ~f)
        in
        C_sources.make d ~c_sources
      ) in
      let coq_modules = Memo.lazy_ (fun () ->
        check_no_unqualified Loc.none qualif_mode;
        build_coq_modules_map d ~dir:d.ctx_dir
          ~modules:(coq_modules_of_files ~subdirs:((dir,[],files)::subdirs))) in
      let subdirs =
        List.map subdirs ~f:(fun (dir, _local, files) ->
          { kind = Group_part
          ; dir
          ; text_files = files
          ; modules
          ; c_sources
          ; mlds = Memo.lazy_ (fun () -> (build_mlds_map d ~files))
          ; coq_modules
          })
      in
      let t =
        { kind = Group_root subdirs
        ; dir
        ; text_files = files
        ; modules
        ; c_sources
        ; mlds = Memo.lazy_ (fun () -> build_mlds_map d ~files)
        ; coq_modules
        }
      in
      Here {
        t;
        rules;
        subdirs = Path.Build.Map.of_list_map_exn subdirs ~f:(fun x -> x.dir, x)
      }

  let memo0 =
    let module Output = struct
      type t = result0
      let to_dyn _ = Dyn.Opaque
    end
    in
    Memo.create
      "dir-contents-get0"
      ~input:(module Key)
      ~output:(Simple (module Output))
      ~doc:"dir contents"
      ~visibility:Hidden
      Sync
      get0_impl

  let get sctx ~dir =
    match Memo.exec memo0 (sctx, dir) with
    | Here { t; rules = _; subdirs = _ } -> t
    | See_above group_root ->
      match Memo.exec memo0 (sctx, group_root) with
      | See_above _ -> assert false
      | Here { t; rules = _; subdirs = _ } -> t

  let gen_rules sctx ~dir =
    match Memo.exec memo0 (sctx, dir) with
    | See_above group_root ->
      Group_part group_root
    | Here { t; rules; subdirs } ->
      Rules.produce_opt rules;
      Standalone_or_root (t, Path.Build.Map.values subdirs)
end

include Load
