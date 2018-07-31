open Import
module Menhir_rules = Menhir
open Jbuild
open! No_io

module Modules_field_evaluator : sig
  val eval
    :  modules:Module.t Module.Name.Map.t
    -> buildable:Buildable.t
    -> Module.t Module.Name.Map.t
end = struct

  let eval =
    let module Value = struct
      type t = (Module.t, Module.Name.t) result

      type key = Module.Name.t

      let key = function
        | Error s -> s
        | Ok m -> Module.name m
    end in
    let module Eval = Ordered_set_lang.Make_loc(Module.Name)(Value) in
    let parse ~all_modules ~fake_modules ~loc s =
      let name = Module.Name.of_string s in
      match Module.Name.Map.find all_modules name with
      | Some m -> Ok m
      | None ->
        fake_modules := Module.Name.Map.add !fake_modules name loc;
        Error name
    in
    fun ~all_modules ~standard osl ->
      let fake_modules = ref Module.Name.Map.empty in
      let parse = parse ~fake_modules ~all_modules in
      let standard = Module.Name.Map.map standard ~f:(fun m -> Ok m) in
      let modules = Eval.eval_unordered ~parse ~standard osl in
      ( !fake_modules
      , Module.Name.Map.filter_map modules ~f:(fun (loc, m) ->
          match m with
          | Ok m -> Some m
          | Error s ->
            Loc.fail loc "Module %a doesn't exist." Module.Name.pp s)
      , modules
      )

  let eval ~modules:(all_modules : Module.t Module.Name.Map.t)
        ~buildable:(conf : Buildable.t) =
    let (fake_modules, modules, _) =
      eval ~standard:all_modules ~all_modules conf.modules in
    let (fake_modules, intf_only, modules_without_implementation_locs) =
      let (fake_modules', intf_only, locs) =
        eval ~standard:Module.Name.Map.empty ~all_modules
          conf.modules_without_implementation in
      ( Module.Name.Map.superpose fake_modules' fake_modules
      , intf_only
      , locs
      )
    in
    Module.Name.Map.iteri fake_modules ~f:(fun m loc ->
      Loc.warn loc "Module %a is excluded but it doesn't exist."
        Module.Name.pp m
    );
    let real_intf_only =
      Module.Name.Map.filter modules
        ~f:(fun (m : Module.t) -> Option.is_none m.impl)
    in
    if Module.Name.Map.equal intf_only real_intf_only
         ~equal:(fun a b -> Module.name a = Module.name b) then
      modules
    else begin
      let should_be_listed, shouldn't_be_listed =
        Module.Name.Map.merge intf_only real_intf_only ~f:(fun name x y ->
          match x, y with
          | Some _, Some _ -> None
          | None  , Some _ -> Some (Left name)
          | Some _, None   -> Some (Right name)
          | None  , None   -> assert false)
        |> Module.Name.Map.values
        |> List.partition_map ~f:(fun x -> x)
      in
      let uncapitalized = List.map ~f:Module.Name.uncapitalize in
      if should_be_listed <> [] then begin
        match Ordered_set_lang.loc conf.modules_without_implementation with
        | None ->
          Loc.warn conf.loc
            "Some modules don't have an implementation.\
             \nYou need to add the following field to this stanza:\
             \n\
             \n  %s\
             \n\
             \nThis will become an error in the future."
            (let tag = Sexp.unsafe_atom_of_string
                         "modules_without_implementation" in
             let modules =
               should_be_listed
               |> uncapitalized
               |> List.map ~f:Sexp.To_sexp.string
             in
             Sexp.to_string ~syntax:Dune (List (tag :: modules)))
        | Some loc ->
          let list_modules l =
            uncapitalized l
            |> List.map ~f:(sprintf "- %s")
            |> String.concat ~sep:"\n"
          in
          Loc.warn loc
            "The following modules must be listed here as they don't \
             have an implementation:\n\
             %s\n\
             This will become an error in the future."
            (list_modules should_be_listed)
      end;
      if shouldn't_be_listed <> [] then begin
        let module_name = List.hd shouldn't_be_listed in
        let (loc, _) =
          Module.Name.Map.find modules_without_implementation_locs module_name
          |> Option.value_exn
        in
        (* CR-soon jdimino for jdimino: report all errors *)
        Loc.fail loc
          "Module %a has an implementation, it cannot be listed here"
          Module.Name.pp module_name
      end;
      modules
    end
end

module Library_modules = struct
  type t =
    { modules          : Module.t Module.Name.Map.t
    ; alias_module     : Module.t option
    ; main_module_name : Module.Name.t
    }

  let make (lib : Library.t) ~dir (modules : Module.t Module.Name.Map.t) =
    let main_module_name = Module.Name.of_string lib.name in
    let modules =
      if not lib.wrapped then
        modules
      else
        Module.Name.Map.map modules ~f:(fun m ->
          if m.name = main_module_name then
            m
          else
            Module.with_wrapper m ~libname:lib.name)
    in
    let alias_module =
      if not lib.wrapped ||
         (Module.Name.Map.cardinal modules = 1 &&
          Module.Name.Map.mem modules main_module_name) then
        None
      else if Module.Name.Map.mem modules main_module_name then
        (* This module needs an implementation for non-jbuilder
           users of the library:

           https://github.com/ocaml/dune/issues/567 *)
        Some
          (Module.make (Module.Name.add_suffix main_module_name "__")
             ~impl:(Module.File.make OCaml
                      (Path.relative dir (sprintf "%s__.ml-gen" lib.name)))
             ~obj_name:(lib.name ^ "__"))
      else
        Some
          (Module.make main_module_name
             ~impl:(Module.File.make OCaml
                      (Path.relative dir (lib.name ^ ".ml-gen")))
             ~obj_name:lib.name)
    in
    { modules; alias_module; main_module_name }
end

module Executables_modules = struct
  type t = Module.t Module.Name.Map.t
end

type modules =
  { libraries : Library_modules.t String.Map.t
  ; executables : Executables_modules.t String.Map.t
  ; (* Map from modules to the buildable they are part of *)
    rev_map : Buildable.t Module.Name.Map.t
  }

let empty_modules =
  { libraries = String.Map.empty
  ; executables = String.Map.empty
  ; rev_map = Module.Name.Map.empty
  }
type t =
  { kind : kind
  ; dir : Path.t
  ; text_files : String.Set.t
  ; modules : modules Lazy.t
  ; mlds : (Jbuild.Documentation.t * Path.t list) list Lazy.t
  }

and kind =
  | Standalone
  | Group_root of t list Lazy.t
  | Group_part of t

let kind t = t.kind
let dir t = t.dir

let text_files t = t.text_files

let modules_of_library t ~name =
  let map = (Lazy.force t.modules).libraries in
  match String.Map.find map name with
  | Some m -> m
  | None ->
    Exn.code_error "Dir_contents.modules_of_library"
      [ "name", Sexp.To_sexp.string name
      ; "available", Sexp.To_sexp.(list string) (String.Map.keys map)
      ]

let modules_of_executables t ~first_exe =
  let map = (Lazy.force t.modules).executables in
  match String.Map.find map first_exe with
  | Some m -> m
  | None ->
    Exn.code_error "Dir_contents.modules_of_executables"
      [ "first_exe", Sexp.To_sexp.string first_exe
      ; "available", Sexp.To_sexp.(list string) (String.Map.keys map)
      ]

let lookup_module t name =
  Module.Name.Map.find (Lazy.force t.modules).rev_map name

let mlds t (doc : Documentation.t) =
  let map = Lazy.force t.mlds in
  match
    List.find_map map ~f:(fun (doc', x) ->
      Option.some_if (doc.loc = doc'.loc) x)
  with
  | Some x -> x
  | None ->
    Exn.code_error "Dir_contents.mlds"
      [ "doc", Loc.sexp_of_t doc.loc
      ; "available", Sexp.To_sexp.(list Loc.sexp_of_t)
                       (List.map map ~f:(fun (d, _) -> d.Documentation.loc))
      ]

(* As a side-effect, setup user rules and copy_files rules. *)
let load_text_files sctx ft_dir d =
  let { Super_context.Dir_with_jbuild.
        ctx_dir = dir
      ; src_dir
      ; scope
      ; stanzas
      ; _
      } = d
  in
  (* Interpret a few stanzas in order to determine the list of
     files generated by the user. *)
  let generated_files =
    List.concat_map stanzas ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Menhir.T menhir ->
        Menhir_rules.targets menhir
      | Rule rule ->
        List.map (Simple_rules.user_rule sctx rule ~dir ~scope)
          ~f:Path.basename
      | Copy_files def ->
        List.map (Simple_rules.copy_files sctx def ~src_dir ~dir ~scope)
          ~f:Path.basename
      | Library { buildable; _ } | Executables { buildable; _ } ->
        (* Manually add files generated by the (select ...)
           dependencies *)
        List.filter_map buildable.libraries ~f:(fun dep ->
          match (dep : Jbuild.Lib_dep.t) with
          | Direct _ -> None
          | Select s -> Some s.result_fn)
      | _ -> [])
    |> String.Set.of_list
  in
  String.Set.union generated_files (File_tree.Dir.files ft_dir)

let modules_of_files ~dir ~files =
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
        Path.pp src_dir
        Path.pp f1.path
        Path.pp f2.path
  in
  let impls = parse_one_set impl_files in
  let intfs = parse_one_set intf_files in
  Module.Name.Map.merge impls intfs ~f:(fun name impl intf ->
    Some (Module.make name ?impl ?intf))

let build_modules_map (d : Super_context.Dir_with_jbuild.t) ~modules =
  let libs, exes =
    List.filter_partition_map d.stanzas ~f:(fun stanza ->
      match (stanza : Stanza.t) with
      | Library lib->
        let modules =
          Modules_field_evaluator.eval ~modules ~buildable:lib.buildable
        in
        Left (lib, Library_modules.make lib ~dir:d.ctx_dir modules)
      | Executables exes
      | Tests { exes; _} ->
        let modules =
          Modules_field_evaluator.eval ~modules ~buildable:exes.buildable
        in
        Right (exes, modules)
      | _ -> Skip)
  in
  let libraries =
    match
      String.Map.of_list_map libs ~f:(fun (lib, m) -> Library.best_name lib, m)
    with
    | Ok x -> x
    | Error (name, _, (lib2, _)) ->
      Loc.fail lib2.buildable.loc
        "Library %S appears for the second time \
         in this directory"
        name
  in
  let executables =
    match
      String.Map.of_list_map exes
        ~f:(fun (exes, m) -> snd (List.hd exes.names), m)
    with
    | Ok x -> x
    | Error (name, _, (exes2, _)) ->
      Loc.fail exes2.buildable.loc
        "Executable %S appears for the second time \
         in this directory"
        name
  in
  let rev_map =
    let rev_modules =
      List.rev_append
        (List.concat_map libs ~f:(fun (l, m) ->
           List.map (Module.Name.Map.values m.modules) ~f:(fun m ->
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
          let locs =
            List.filter_map rev_modules ~f:(fun (n, b) ->
              Option.some_if (n = name) b.loc)
            |> List.sort ~compare
          in
          Loc.fail (Loc.in_file (List.hd locs).start.pos_fname)
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
          Loc.warn (Loc.in_file b.loc.start.pos_fname)
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
  { libraries; executables; rev_map }

let build_mlds_map (d : Super_context.Dir_with_jbuild.t) ~files =
  let dir = d.ctx_dir in
  let mlds = lazy (
    String.Set.fold files ~init:String.Map.empty ~f:(fun fn acc ->
      match String.lsplit2 fn ~on:'.' with
      | Some (s, "mld") -> String.Map.add acc s fn
      | _ -> acc))
  in
  List.filter_map d.stanzas ~f:(function
    | Documentation doc ->
      let mlds =
        let mlds = Lazy.force mlds in
        Ordered_set_lang.String.eval_unordered doc.mld_files
          ~parse:(fun ~loc s ->
            match String.Map.find mlds s with
            | Some s ->
              s
            | None ->
              Loc.fail loc "%s.mld doesn't exist in %s" s
                (Path.to_string_maybe_quoted
                   (Path.drop_optional_build_context dir))
          )
          ~standard:mlds
      in
      Some (doc, List.map (String.Map.values mlds) ~f:(Path.relative dir))
    | _ -> None)

module Dir_status = struct
  type t =
    | Empty_standalone of File_tree.Dir.t option
    (* Directory with no libraries or executables that is not part of
       a multi-directory group *)

    | Is_component_of_a_group_but_not_the_root of
        Super_context.Dir_with_jbuild.t option
    (* Sub-directory of a directory with [(include_subdirs true)] *)

    | Standalone of File_tree.Dir.t
                    * Super_context.Dir_with_jbuild.t
    (* Directory with at least one library or executable *)

    | Group_root of File_tree.Dir.t
                    * Super_context.Dir_with_jbuild.t
    (* Directory with [(include_subdirs true)] *)

  let is_standalone = function
    | Standalone _ | Empty_standalone _ -> true
    | _ -> false

  let cache = Hashtbl.create 32

  let analyze_stanzas stanzas =
    List.fold_left stanzas ~init:(None, false) ~f:(fun acc stanza ->
      let is_group_root, has_modules_consumers = acc in
      match stanza with
      | Include_subdirs (loc, x) ->
        if Option.is_some is_group_root then
          Loc.fail loc "The 'include_subdirs' stanza cannot appear \
                        more than once";
        (Some x, has_modules_consumers)
      | Library _ | Executables _ | Tests _ ->
        (is_group_root, true)
      | _ -> acc)

  let rec get sctx ~dir =
    match Hashtbl.find cache dir with
    | Some t -> t
    | None ->
      let t =
        match
          Option.bind (Path.drop_build_context dir)
            ~f:(File_tree.find_dir (Super_context.file_tree sctx))
        with
        | None -> Empty_standalone None
        | Some ft_dir ->
          let project_root = Path.of_local (File_tree.Dir.project ft_dir).root in
          match Super_context.stanzas_in sctx ~dir with
          | None ->
            if dir = project_root ||
               is_standalone (get sctx ~dir:(Path.parent_exn dir)) then
              Empty_standalone (Some ft_dir)
            else
              Is_component_of_a_group_but_not_the_root None
          | Some d ->
            let is_group_root, has_modules_consumers =
              analyze_stanzas d.stanzas
            in
            if Option.is_some is_group_root then
              Group_root (ft_dir, d)
            else if not has_modules_consumers &&
                    dir <> project_root &&
                    not (is_standalone (get sctx ~dir:(Path.parent_exn dir)))
            then
              Is_component_of_a_group_but_not_the_root (Some d)
            else
              Standalone (ft_dir, d)
      in
      Hashtbl.add cache dir t;
      t

  let get_assuming_parent_is_part_of_group sctx ~dir ft_dir =
    match Hashtbl.find cache (File_tree.Dir.path ft_dir) with
    | Some t -> t
    | None ->
      let t =
        match Super_context.stanzas_in sctx ~dir with
        | None -> Is_component_of_a_group_but_not_the_root None
        | Some d ->
          let is_group_root, has_modules_consumers =
            analyze_stanzas d.stanzas
          in
          if Option.is_some is_group_root then
            Group_root (ft_dir, d)
          else if has_modules_consumers then
            Standalone (ft_dir, d)
          else
            Is_component_of_a_group_but_not_the_root (Some d)
      in
      Hashtbl.add cache dir t;
      t
end

let cache = Hashtbl.create 32

let rec get sctx ~dir =
  match Hashtbl.find cache dir with
  | Some t -> t
  | None ->
    match Dir_status.get sctx ~dir with
    | Empty_standalone ft_dir ->
      let t =
        { kind = Standalone
        ; dir
        ; text_files =
            (match ft_dir with
             | None -> String.Set.empty
             | Some x -> File_tree.Dir.files x)
        ; modules = lazy empty_modules
        ; mlds = lazy []
        }
      in
      Hashtbl.add cache dir t;
      t
    | Is_component_of_a_group_but_not_the_root _ ->
      (* Filled while scanning the group root *)
      Option.value_exn (Hashtbl.find cache dir)
    | Standalone (ft_dir, d) ->
      let files = load_text_files sctx ft_dir d in
      let t =
        { kind = Standalone
        ; dir
        ; text_files = files
        ; modules = lazy (build_modules_map d
                            ~modules:(modules_of_files ~dir:d.ctx_dir ~files))
        ; mlds = lazy (build_mlds_map d ~files)
        }
      in
      Hashtbl.add cache dir t;
      t
    | Group_root (ft_dir, d) ->
      let rec walk ft_dir ~dir acc =
        match
          Dir_status.get_assuming_parent_is_part_of_group sctx ft_dir ~dir
        with
        | Is_component_of_a_group_but_not_the_root d ->
          let files =
            match d with
            | None -> File_tree.Dir.files ft_dir
            | Some d -> load_text_files sctx ft_dir d
          in
          walk_children ft_dir ~dir ((dir, files) :: acc)
        | _ -> acc
      and walk_children ft_dir ~dir acc =
        String.Map.foldi (File_tree.Dir.sub_dirs ft_dir) ~init:acc
          ~f:(fun name ft_dir acc ->
            let dir = Path.relative dir name in
            walk ft_dir ~dir acc)
      in
      let files = load_text_files sctx ft_dir d in
      let subdirs = walk_children ft_dir ~dir [] in
      let modules = lazy (
        let modules =
          List.fold_left ((dir, files) :: subdirs) ~init:Module.Name.Map.empty
            ~f:(fun acc (dir, files) ->
              let modules = modules_of_files ~dir ~files in
              Module.Name.Map.union acc modules ~f:(fun name x y ->
                Loc.fail (Loc.in_file
                            (Path.to_string
                               (match File_tree.Dir.dune_file ft_dir with
                                | None ->
                                  Path.relative (File_tree.Dir.path ft_dir)
                                    "_unknown_"
                                | Some d -> File_tree.Dune_file.path d)))
                  "Module %a appears in several directories:\
                   @\n- %a\
                   @\n- %a"
                  Module.Name.pp_quote name
                  Path.pp (Module.dir x)
                  Path.pp (Module.dir y)))
        in
        build_modules_map d ~modules)
      in
      let t =
        { kind = Group_root
                   (lazy (List.map subdirs ~f:(fun (dir, _) -> get sctx ~dir)))
        ; dir
        ; text_files = files
        ; modules
        ; mlds = lazy (build_mlds_map d ~files)
        }
      in
      Hashtbl.add cache dir t;
      List.iter subdirs ~f:(fun (dir, files) ->
        Hashtbl.add cache dir
          { kind = Group_part t
          ; dir
          ; text_files = files
          ; modules
          ; mlds = lazy (build_mlds_map d ~files)
          });
      t
