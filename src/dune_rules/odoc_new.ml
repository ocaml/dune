open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let ( ++ ) = Path.Build.relative

let stdlib_lib ctx =
  let* public_libs = Scope.DB.public_libs ctx in
  Lib.DB.find public_libs (Lib_name.of_string "stdlib")
;;

let lib_equal l1 l2 = Lib.compare l1 l2 |> Ordering.is_eq

let is_public lib =
  match Lib.Local.to_lib lib |> Lib.info |> Lib_info.status with
  | Installed_private -> false
  | Installed -> true
  | Public _ -> true
  | Private (_project, _) -> false
;;

module Paths = struct
  let odoc_support_dirname = "docs/odoc.support"

  let root (context : Context.t) ~all:_ =
    let sub = "_doc_new" in
    Path.Build.relative (Context.build_dir context) sub
  ;;

  (* This is used to turn a fully-qualified library path
     into the bit below the findlib path. For example, it will
     convert /home/user/.opam/5.0.0/lib/foo/bar into foo/bar *)
  let local_path_of_findlib_path findlib_paths obj_dir =
    List.find_map findlib_paths ~f:(fun p ->
      if Path.is_descendant ~of_:p obj_dir
      then Some (Path.reach obj_dir ~from:p)
      else None)
    |> Option.value_exn
  ;;

  let html_root ctx ~all = root ctx ~all ++ "html"
  let odoc_support ctx ~all = html_root ctx ~all ++ odoc_support_dirname
end

module Index = struct
  (* The index represents the position in the output HTML where the
     an artifact will be found. *)

  (* A particular package might be a 'dune-with-modules' type package,
     where for every library within the package we know it was built
     with dune and we know all the modules that make it up. Or it might
     be a local one (which is, of course, built with dune and we know
     all of its modules!) or it might be a fallback type where at least
     one of its libraries doesn't satisfy the condition, or it might be
     unknown (used only when computing the alias that can be built to
     create all of the odoc files for a library within the package) *)
  type pkg_ty =
    | Unknown
    | Dune_with_modules
    | Fallback
    | Local

  (* The directories immediately below 'docs' represent packages or
     private libraries. Within the packages there are subdirectories
     for sublibraries. We also slightly abuse this for replicating
     the directory structure within opam for non-dune packages. For
     example, there will almost always be a

     [Package (Fallback, "ocaml")]

     index which will contain the standard library, unix an so on,
     even though there is no package 'ocaml' *)
  type ty =
    | Private_lib of string
    | Package of pkg_ty * string
    | Subdir of string

  type t = ty list

  let ty_name = function
    | Private_lib lnu -> lnu
    | Package (_, s) -> s
    | Subdir str -> str
  ;;

  let name : t -> string = function
    | [] -> "toplevel"
    | xs -> String.concat ~sep:"." (List.rev_map xs ~f:ty_name)
  ;;

  let rec might_be_fallback = function
    | [] -> false
    | Private_lib _ :: _ -> false
    | Package (Fallback, _) :: _ -> true
    | Package (Unknown, _) :: _ -> true
    | Package (Dune_with_modules, _) :: _ -> false
    | Package (Local, _) :: _ -> false
    | Subdir _ :: xs -> might_be_fallback xs
  ;;

  (* Used to suppress warnings on packages from the switch *)
  let rec is_external = function
    | [] -> false
    | Private_lib _ :: _ -> false
    | Package (Local, _) :: _ -> false
    | Package (_, _) :: _ -> true
    | Subdir _ :: xs -> is_external xs
  ;;

  let external_ty_to_dyn x =
    let open Dyn in
    match x with
    | Unknown -> variant "Unknown" []
    | Dune_with_modules -> variant "Dune_with_modules" []
    | Fallback -> variant "Fallback" []
    | Local -> variant "Local" []
  ;;

  let ty_to_dyn x =
    let open Dyn in
    match x with
    | Private_lib lnu -> variant "Private_lib" [ String lnu ]
    | Package (e, str) -> variant "Package" [ external_ty_to_dyn e; String str ]
    | Subdir str -> variant "LocalSubLib" [ String str ]
  ;;

  let to_dyn x = Dyn.list ty_to_dyn x
  let compare_ty x y = Dyn.compare (ty_to_dyn x) (ty_to_dyn y)

  let subdir = function
    | Package (_, str) -> str
    | Private_lib lnu -> lnu
    | Subdir str -> str
  ;;

  let local_dir : t -> string = fun v -> List.map ~f:subdir v |> String.concat ~sep:"/"

  (* Where we find the odoc files for the indexes *)
  let obj_dir ctx ~all : t -> Path.Build.t =
    let root = Paths.root ctx ~all ++ "index" in
    List.fold_right ~f:(fun x acc -> acc ++ subdir x) ~init:root
  ;;

  (* Where we find the output HTML files for artifacts that are children of
     this index *)
  let html_dir ctx ~all (m : t) =
    let init = Paths.html_root ctx ~all ++ "docs" in
    List.fold_right ~f:(fun x acc -> acc ++ subdir x) ~init m
  ;;

  (* Where we find odoc files for artifacts that are children of this index. *)
  let odoc_dir ctx ~all (m : t) =
    let init = Paths.root ctx ~all ++ "odoc" in
    List.fold_right ~f:(fun x acc -> acc ++ subdir x) ~init m
  ;;

  let mld_name_ty : ty -> string = subdir

  let mld_name : t -> string = function
    | [] -> "docs"
    | x :: _ -> mld_name_ty x
  ;;

  let mld_filename index = mld_name index ^ ".mld"
  let mld_path ctx ~all index = obj_dir ctx ~all index ++ mld_filename index

  (* Here we have the three constructors of the indexes. The first
     takes an external lib found in a findlib directory: *)
  let of_external_lib findlib_paths ty lib =
    let local =
      let local_full =
        Lib.info lib
        |> Lib_info.obj_dir
        |> Obj_dir.dir
        |> Paths.local_path_of_findlib_path findlib_paths
      in
      String.split ~on:'/' local_full
    in
    let init = [ Package (ty, List.hd local) ] in
    List.fold_left ~init ~f:(fun acc s -> Subdir s :: acc) (List.tl local)
  ;;

  (* This one is used for fallback findlib directories *)
  let of_dir dir =
    match String.split ~on:'/' dir with
    | [] -> assert false
    | x :: xs ->
      List.fold_left ~f:(fun acc s -> Subdir s :: acc) ~init:[ Package (Fallback, x) ] xs
  ;;

  (* And finally for a local library, it's either associated with a package
     or it's a private library. *)
  let of_local_lib (lib : Lib.t) =
    match
      let info = Lib.info lib in
      Lib_info.package info
    with
    | None -> [ Private_lib (Odoc.lib_unique_name (lib :> Lib.t)) ]
    | Some _pkg ->
      (match Lib_name.analyze (Lib.name (lib :> Lib.t)) with
       | Private (_, _) -> [ Private_lib (Odoc.lib_unique_name (lib :> Lib.t)) ]
       | Public (pkg, rest) ->
         List.fold_left
           ~f:(fun acc s -> Subdir s :: acc)
           rest
           ~init:[ Package (Local, Package.Name.to_string pkg) ])
  ;;
end

let add_rule sctx =
  let dir = Context.build_dir (Super_context.context sctx) in
  Super_context.add_rule sctx ~dir
;;

(* Returns a map from a 'local dir' - defined as a directory in
   your switch's lib dir, e.g. 'ocaml' or 'dune' - to a map from
   subdir to a map from library name to Dune_package.Lib.t. For
   example, on 4.14.1, looking up "ocaml" in the map gives a map
   containing the following:

   [ ocaml ->
     [ bigarray -> <lib>, bytes -> <lib>, dynlink -> <lib> ... ]
   [ ocaml/compiler-libs ->
     [ compiler-libs -> <lib>, compiler-libs.bytecomp -> <lib> ... ]
*)
let libs_of_local_dir_def =
  let f (ctx : Context.t) =
    let* findlib_paths = Context.findlib_paths ctx
    and* db = Scope.DB.public_libs ctx
    and* all_packages =
      let* findlib = Findlib.create (Context.name ctx) in
      Findlib.all_packages findlib
    in
    Memo.List.fold_left all_packages ~init:String.Map.empty ~f:(fun map entry ->
      match entry with
      | Dune_package.Entry.Library l ->
        let local =
          let obj_dir = Dune_package.Lib.info l |> Lib_info.obj_dir |> Obj_dir.dir in
          Paths.local_path_of_findlib_path findlib_paths obj_dir
        in
        let name = Dune_package.Lib.info l |> Lib_info.name in
        Lib.DB.find db name
        >>| (function
        | None -> map
        | Some lib ->
          let update_fn = function
            | None ->
              Some (String.Map.singleton local (Lib_name.Map.singleton name (l, lib)))
            | Some libs ->
              Some
                (String.Map.update libs local ~f:(function
                  | Some libs -> Some (Lib_name.Map.add_exn libs name (l, lib))
                  | None -> Some (Lib_name.Map.singleton name (l, lib))))
          in
          let toplocal = String.split local ~on:'/' |> List.hd in
          String.Map.update map toplocal ~f:update_fn)
      | _ -> Memo.return map)
  in
  let module Input = Context in
  Memo.create "libs_of_local_dir" ~input:(module Input) f
;;

let libs_of_local_dir ctx = Memo.exec libs_of_local_dir_def ctx

module Classify = struct
  (* Here we classify top-level dirs in the opam switch. They are either
     packages built by dune for which we have all the info we need -
     a [Modules.t] value - or there exists one or more libraries within
     the directory that don't satisfy this, and these are labelled
     fallback directories. *)
  exception Fallback

  (* You can get the Modules.t value out of the lib, but that yields an
     option, and since we've classified it as one where the modules are
     definitely available, we can avoid a `Option.value_exn` call later
     by just keeping it available. *)
  type dune_with_modules =
    { lib : Lib.t
    ; modules : Modules.t
    }

  let dwm_to_dyn x =
    let open Dyn in
    record [ "lib", Lib.to_dyn x.lib; "modules", Modules.to_dyn x.modules ]
  ;;

  let dwm_equal d1 d2 = Lib.equal d1.lib d2.lib && Modules.equal d1.modules d2.modules
  let dwm_hash d = Lib.hash d.lib

  (* The fallback is simply a map of subdirectory to list of libraries
     found in that subdir. This is the value returned by [libs_of_local_dir] *)
  type fallback = { libs : Lib.t list String.Map.t }

  let fallback_to_dyn x =
    let open Dyn in
    record [ "libs", String.Map.to_dyn (list Lib.to_dyn) x.libs ]
  ;;

  let fallback_equal f1 f2 =
    String.Map.equal ~equal:(List.equal Lib.equal) f1.libs f2.libs
  ;;

  let fallback_hash f =
    String.Map.fold f.libs ~init:0 ~f:(fun libs acc ->
      List.sort libs ~compare:Lib.compare
      |> List.fold_left ~init:acc ~f:(fun acc lib -> Poly.hash (acc, Lib.hash lib)))
  ;;

  type local_dir_type =
    | Nothing
    | Dune_with_modules of (Package.Name.t * dune_with_modules list)
    | Fallback of fallback

  (* We need to know if every single library within a opam package dir is
     confined to its own subdirectory (ie, no two libraries are found in the
     same dir), and that we have information about the modules of every
     library within the tree. If this is not the case, we'll fall back to a
     less specific mode that simply documents the modules found within each
     dir without assigning them a library. *)
  let classify_local_dir_memo =
    let run (ctx, local_dir) =
      (match String.index_opt local_dir '/' with
       | Some _ -> assert false
       | None -> ());
      let* map = libs_of_local_dir ctx in
      match String.Map.find map local_dir with
      | None ->
        Log.info [ Pp.textf "classify_local_dir: No lib at this path: %s" local_dir ];
        Memo.return Nothing
      | Some libs ->
        (try
           let f libs acc =
             match Lib_name.Map.values libs with
             | [ (lib, _) ] ->
               (match
                  let info = Dune_package.Lib.info lib in
                  let mods_opt = Lib_info.modules info in
                  mods_opt, Lib_info.entry_modules info
                with
                | External (Some modules), External (Ok _) -> (lib, modules) :: acc
                | _ -> raise Fallback)
             | _ -> raise Fallback
           in
           let* ms =
             String.Map.fold libs ~f ~init:[]
             |> Memo.parallel_map ~f:(fun (dune_package_lib, modules) ->
               let info = Dune_package.Lib.info dune_package_lib in
               let* resolved_lib =
                 let* public_libs = Lib.DB.installed ctx in
                 Lib.DB.resolve public_libs (Loc.none, Lib_info.name info)
               in
               let+ lib = Resolve.read_memo resolved_lib in
               let package = Lib_info.package info |> Option.value_exn in
               package, { lib; modules })
           in
           let pkg =
             List.fold_left ~init:None ms ~f:(fun acc m ->
               match acc with
               | None -> Some (fst m)
               | Some p -> if p <> fst m then raise Fallback else acc)
             |> Option.value_exn
           in
           Memo.return (Dune_with_modules (pkg, List.map ~f:snd ms))
         with
         | Fallback ->
           let libs =
             String.Map.map libs ~f:(Lib_name.Map.to_list_map ~f:(fun _name (_, y) -> y))
           in
           Memo.return (Fallback { libs }))
    in
    let module Input = struct
      type t = Context.t * string

      let equal (c1, s1) (c2, s2) = Context.equal c1 c2 && String.equal s1 s2
      let hash (c, s) = Poly.hash (Context.hash c, String.hash s)
      let to_dyn = Dyn.pair Context.to_dyn Dyn.string
    end
    in
    Memo.create "libs_and_packages" ~input:(module Input) run
  ;;

  let classify_local_dir ctx dir = Memo.exec classify_local_dir_memo (ctx, dir)
end

module Valid = struct
  (* These functions return a allowlist of libraries and packages that
     should be documented. There is one single function that performs this
     task because there needs to be an exact correspondence at various points
     in the process - e.g. the indexes need to know exactly which libraries will
     be documented and where. *)
  let valid_libs_and_packages =
    let run (ctx, all, projects) =
      let* libs_and_pkgs =
        let* mask =
          let+ mask = Only_packages.get_mask () in
          Option.map ~f:Package.Name.Map.keys mask
        in
        Scope.DB.with_all ctx ~f:(fun find ->
          Memo.List.fold_left projects ~init:([], []) ~f:(fun (libs_acc, pkg_acc) proj ->
            let* vendored = Source_tree.is_vendored (Dune_project.root proj) in
            if vendored
            then Memo.return (libs_acc, pkg_acc)
            else (
              let lib_db =
                let scope = find proj in
                Scope.libs scope
              in
              let+ libs_acc =
                let+ libs = Lib.DB.all lib_db in
                let libs =
                  match mask with
                  | None -> libs
                  | Some mask ->
                    Lib.Set.filter libs ~f:(fun lib ->
                      let info = Lib.info lib in
                      match Lib_info.package info with
                      | Some p -> List.mem ~equal:Package.Name.equal mask p
                      | None -> false)
                in
                (proj, lib_db, libs) :: libs_acc
              in
              let pkg_acc =
                let pkgs =
                  let proj_pkgs = Dune_project.packages proj |> Package.Name.Map.keys in
                  match mask with
                  | None -> proj_pkgs
                  | Some m ->
                    List.filter ~f:(List.mem ~equal:Package.Name.equal m) proj_pkgs
                in
                pkgs @ pkg_acc
              in
              libs_acc, pkg_acc)))
      in
      let* libs, packages = libs_and_pkgs in
      let+ libs_list =
        let+ libs_list =
          let+ libs_list =
            let* stdlib = stdlib_lib ctx in
            Memo.parallel_map libs ~f:(fun (_, _lib_db, libs) ->
              Lib.Set.fold ~init:(Memo.return []) libs ~f:(fun lib acc ->
                let* acc = acc in
                let+ libs =
                  let* libs = Lib.closure (lib :: Option.to_list stdlib) ~linking:false in
                  Resolve.read_memo libs
                in
                libs :: acc))
          in
          List.concat libs_list
          |> List.concat
          |> Lib.Set.of_list
          |> Lib.Set.to_list
          |> List.filter ~f:(fun lib ->
            let is_impl = Lib.info lib |> Lib_info.implements |> Option.is_some in
            not is_impl)
        in
        if all
        then libs_list
        else
          List.filter libs_list ~f:(fun lib ->
            match Lib.Local.of_lib lib with
            | None -> false
            | Some l -> is_public l)
      in
      libs_list, packages
    in
    let module Input = struct
      type t = Context.t * bool * Dune_project.t list

      let equal (c1, b1, ps1) (c2, b2, ps2) =
        Context.equal c1 c2 && List.equal Dune_project.equal ps1 ps2 && b1 = b2
      ;;

      let hash (c, b, ps) = Poly.hash (Context.hash c, b, List.hash Dune_project.hash ps)
      let to_dyn _ = Dyn.Opaque
    end
    in
    Memo.create "libs_and_packages" ~input:(module Input) run
  ;;

  let get ctx ~all =
    let* { projects; _ } = Dune_load.load () in
    Memo.exec valid_libs_and_packages (ctx, all, projects)
  ;;

  let is_local_package ctx ~all pkg =
    let+ _, packages = get ctx ~all in
    List.mem ~equal:Package.Name.equal packages pkg
  ;;

  (* Some functions to filter various values containing libraries
     against the allowlist *)

  let filter_libs ctx ~all libs =
    let+ valid_libs, _ = get ctx ~all in
    List.filter libs ~f:(fun l -> List.mem valid_libs l ~equal:lib_equal)
  ;;

  let filter_dune_with_modules ctx ~all dwms =
    let+ valid_libs, _ = get ctx ~all in
    List.filter dwms ~f:(fun (dwm : Classify.dune_with_modules) ->
      List.mem valid_libs dwm.lib ~equal:lib_equal)
  ;;

  let filter_fallback_libs ctx ~all libs =
    let+ valid_libs, _ = get ctx ~all in
    String.Map.filter_map libs ~f:(fun libs ->
      let filtered =
        List.filter libs ~f:(fun l -> List.mem valid_libs l ~equal:lib_equal)
      in
      if List.length filtered > 0 then Some filtered else None)
  ;;

  (* It's handy for the toplevel index generation to be able to construct
     a categorized list of all the packages, the libraries and everything
     else that will end up being documented. *)
  type categorized =
    { packages : Package.Name.Set.t
    ; local : Lib.Local.t Lib_name.Map.t
    ; localprivate : Lib.Local.t String.Map.t
    ; external_dirs : Classify.local_dir_type String.Map.t
    }

  let empty_categorized =
    { packages = Package.Name.Set.empty
    ; local = Lib_name.Map.empty
    ; localprivate = String.Map.empty
    ; external_dirs = String.Map.empty
    }
  ;;

  let get_categorized_memo =
    let run (ctx, all) =
      let* libs, packages = get ctx ~all in
      let* findlib_paths = Context.findlib_paths ctx in
      let init =
        Memo.return
          { empty_categorized with packages = Package.Name.Set.of_list packages }
      in
      List.fold_left libs ~init ~f:(fun cats lib ->
        let* cats = cats in
        match Lib.Local.of_lib lib with
        | Some llib ->
          (match Lib_info.package (Lib.Local.info llib) with
           | Some _pkg ->
             let local =
               match Lib_name.Map.add cats.local (Lib.name (llib :> Lib.t)) llib with
               | Ok l -> l
               | Error _ ->
                 Log.info
                   [ Pp.textf
                       "Error adding local library %s to categorized map"
                       (Lib.name (llib :> Lib.t) |> Lib_name.to_string)
                   ];
                 cats.local
             in
             Memo.return { cats with local }
           | None ->
             let localprivate =
               let lnu = Odoc.lib_unique_name (llib :> Lib.t) in
               match String.Map.add cats.localprivate lnu llib with
               | Ok l -> l
               | Error _ ->
                 Log.info
                   [ Pp.textf
                       "Error adding local private library %s to categorized map"
                       (Lib.name (llib :> Lib.t) |> Lib_name.to_string)
                   ];
                 cats.localprivate
             in
             Memo.return { cats with localprivate })
        | None ->
          let top_dir =
            let local =
              let obj_dir = Lib.info lib |> Lib_info.obj_dir |> Obj_dir.dir in
              Paths.local_path_of_findlib_path findlib_paths obj_dir
            in
            local |> String.split ~on:'/' |> List.hd
          in
          if String.Map.mem cats.external_dirs top_dir
          then Memo.return cats
          else
            let* c = Classify.classify_local_dir ctx top_dir in
            let external_dirs =
              match String.Map.add cats.external_dirs top_dir c with
              | Ok l -> l
              | Error _ ->
                Log.info
                  [ Pp.textf "Error adding external dir %s to categorized map" top_dir ];
                cats.external_dirs
            in
            Memo.return { cats with external_dirs })
    in
    let module Input = struct
      type t = Context.t * bool

      let equal (c1, b1) (c2, b2) = Context.equal c1 c2 && b1 = b2
      let hash (c, b) = Poly.hash (Context.hash c, b)
      let to_dyn _ = Dyn.Opaque
    end
    in
    Memo.create "categorized" ~input:(module Input) run
  ;;

  let get_categorized ctx all = Memo.exec get_categorized_memo (ctx, all)
end

module Dep : sig
  (** [html_alias ctx target] returns the alias that depends on all html targets
      produced by odoc for [target] *)
  val html_alias : Path.Build.t -> Alias.t

  (** [deps ctx all findlib_paths valid_libs pkg requires] returns all odoc
      dependencies of [requires]. If a package [pkg] is also specified, then
      the odoc dependencies of the package are returned - these are the odoc
      files representing mld files in the package. [findlib_paths] is the
      result of [Context.findlib_paths] and [valid_libs] comes from the
      [Valid] module above. *)
  val deps
    :  Context.t
    -> all:bool
    -> Path.t list
    -> Lib.t list
    -> string option
    -> Lib.t list Resolve.t
    -> unit Action_builder.t

  (*** [setup_deps ctx all index odocs] Adds [odocs] as dependencies for [index].
    These dependencies may be used using the [deps] function *)
  val setup_deps : Context.t -> all:bool -> Index.t -> Path.Set.t -> unit Memo.t
end = struct
  let html_alias dir = Alias.make Alias0.doc_new ~dir
  let alias = Alias.make (Alias.Name.of_string ".odoc-all")

  let deps ctx ~all findlib_paths valid_libs pkg requires =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
    Action_builder.deps
      (let init =
         match pkg with
         | None -> Dep.Set.empty
         | Some p ->
           let index = [ Index.Package (Unknown, p) ] in
           Dep.Set.singleton (Dep.alias (alias ~dir:(Index.odoc_dir ctx ~all index)))
       in
       List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
         match List.mem ~equal:lib_equal valid_libs lib with
         | false -> acc
         | true ->
           let index =
             match Lib.Local.of_lib lib with
             | None -> Index.of_external_lib findlib_paths Unknown lib
             | Some _ -> Index.of_local_lib lib
           in
           let dir = Index.odoc_dir ctx ~all index in
           let alias = alias ~dir in
           Dep.Set.add acc (Dep.alias alias)))
  ;;

  let alias ctx ~all index = alias ~dir:(Index.odoc_dir ctx ~all index)

  let setup_deps ctx ~all m files =
    Rules.Produce.Alias.add_deps (alias ctx ~all m) (Action_builder.path_set files)
  ;;
end

(* An artifact is a single compilation unit (module) or mld file, and contains
   all info to find the html output, the odoc file, the source, whether it's
   hidden or not, to construct a reference to it, to find the index under
   which it's found, and so on. *)
module Artifact : sig
  type artifact_ty =
    | Module of bool
    | Mld

  type t

  val odoc_file : t -> Path.Build.t
  val odocl_file : t -> Path.Build.t
  val html_file : t -> Path.Build.t
  val html_dir : t -> Path.Build.t
  val source_file : t -> Path.t
  val is_visible : t -> bool
  val is_module : t -> bool
  val artifact_ty : t -> artifact_ty
  val reference : t -> string
  val module_name : t -> Module_name.t option
  val name : t -> string
  val make_module : Context.t -> all:bool -> Index.t -> Path.t -> visible:bool -> t
  val external_mld : Context.t -> Index.t -> Path.t -> t
  val index : Context.t -> all:bool -> Index.t -> t
end = struct
  type artifact_ty =
    | Module of bool
    | Mld

  type t =
    { source : Path.t
    ; odoc : Path.Build.t
    ; html_dir : Path.Build.t
    ; html_file : Path.Build.t
    ; ty : artifact_ty
    }

  let odoc_file v = v.odoc
  let odocl_file v = Path.Build.set_extension v.odoc ~ext:".odocl"
  let source_file v = v.source
  let html_file v = v.html_file
  let html_dir v = v.html_dir

  let is_visible v =
    match v.ty with
    | Module x -> x
    | Mld -> true
  ;;

  let is_module v =
    match v.ty with
    | Module _ -> true
    | Mld -> false
  ;;

  let artifact_ty v = v.ty

  let reference v =
    match v.ty with
    | Mld ->
      let basename = Path.basename v.source |> Filename.chop_extension in
      sprintf "page-\"%s\"" basename
    | Module _ ->
      let basename =
        Path.basename v.source |> Filename.chop_extension |> Stdune.String.capitalize
      in
      sprintf "module-%s" basename
  ;;

  let module_name v =
    match v.ty with
    | Module _ ->
      let basename =
        Path.basename v.source |> Filename.chop_extension |> Stdune.String.capitalize
      in
      Some (Module_name.of_string_allow_invalid (Loc.none, basename))
    | _ -> None
  ;;

  let name v =
    let name = Path.basename v.source |> Filename.chop_extension in
    match v.ty with
    | Module _ -> name
    | Mld -> Filename.chop_extension name
  ;;

  let v ~source ~odoc ~html_dir ~html_file ~ty = { source; odoc; html_dir; html_file; ty }

  let make_module ctx ~all index source ~visible =
    let basename =
      Path.basename source |> Filename.chop_extension |> Stdune.String.uncapitalize
    in
    let odoc = Index.odoc_dir ctx ~all index ++ (basename ^ ".odoc") in
    let html_dir = Index.html_dir ctx ~all index ++ Stdune.String.capitalize basename in
    let html = html_dir ++ "index.html" in
    (* Note: odoc will not create any output for modules that it believes are
       hidden - which entirely depends upon whether there is a double underscore
       in the name. So we declare anything with a double underscore as hidden
       in addition to anything that dune believes should not be an entry module. *)
    let visible = visible && not (String.contains_double_underscore basename) in
    v ~source ~odoc ~html_dir ~html_file:html ~ty:(Module visible)
  ;;

  let int_make_mld ctx ~all index source ~is_index =
    let basename = Path.basename source |> Filename.chop_extension in
    let odoc =
      (if is_index then Index.obj_dir ctx ~all index else Index.odoc_dir ctx ~all index)
      ++ ("page-" ^ basename ^ ".odoc")
    in
    let html_dir = Index.html_dir ctx ~all index in
    let html =
      html_dir ++ if is_index then "index.html" else sprintf "%s.html" basename
    in
    v ~source ~odoc ~html_dir ~html_file:html ~ty:Mld
  ;;

  let external_mld ctx index source =
    int_make_mld ctx ~all:true index source ~is_index:false
  ;;

  let index ctx ~all index =
    let source =
      let filename = Index.mld_filename index in
      let dir = Index.obj_dir ctx ~all index in
      Path.build (dir ++ filename)
    in
    int_make_mld ctx ~all index source ~is_index:true
  ;;
end

(* A parent is always an index. Here we operate on the parent as an artifact
   to find the arguments to odoc. *)
let parent_args parent_opt =
  match parent_opt with
  | None -> []
  | Some mld ->
    let dir = Artifact.odoc_file mld |> Path.Build.parent_exn in
    let reference = Artifact.reference mld in
    let odoc_file =
      Artifact.odoc_file mld
      |> Path.build
      |> Dune_engine.Dep.file
      |> Dune_engine.Dep.Set.singleton
    in
    [ Command.Args.A "-I"
    ; Path (Path.build dir)
    ; A "--parent"
    ; A reference
    ; Hidden_deps odoc_file
    ]
;;

(* Given a list of dependency libraries, construct the command line options
   to odoc to use them. *)
let odoc_include_flags ctx all findlib_paths pkg requires indices =
  Resolve.args
  @@
  let open Resolve.O in
  let+ paths =
    let+ paths =
      let+ paths =
        requires
        >>| List.fold_left ~init:Path.Set.empty ~f:(fun paths lib ->
          let index =
            match Lib.Local.of_lib lib with
            | None -> Index.of_external_lib findlib_paths Unknown lib
            | Some lib -> Index.of_local_lib (lib :> Lib.t)
          in
          Index.odoc_dir ctx ~all index |> Path.build |> Path.Set.add paths)
      in
      match pkg with
      | None -> paths
      | Some p ->
        Index.odoc_dir ctx ~all [ Package (Unknown, p) ]
        |> Path.build
        |> Path.Set.add paths
    in
    List.fold_left indices ~init:paths ~f:(fun p index ->
      let odoc_dir = Artifact.odoc_file index |> Path.Build.parent_exn in
      Path.Set.add p (Path.build odoc_dir))
  in
  Command.Args.S
    (Path.Set.to_list paths
     |> List.concat_map ~f:(fun dir -> [ Command.Args.A "-I"; Path dir ]))
;;

(* Create a dependency on the odoc file of an index *)
let index_dep index =
  Artifact.odoc_file index
  |> Path.build
  |> Dune_engine.Dep.file
  |> Dune_engine.Dep.Set.singleton
;;

(* Compile a single module. *)
let compile_module
  sctx
  all
  ~artifact:a
  ~quiet
  ~requires
  ~package
  ~module_deps
  ~parent_opt
  ~indices
  =
  let odoc_file = Artifact.odoc_file a in
  let open Memo.O in
  let ctx = Super_context.context sctx in
  let* findlib_paths = Context.findlib_paths ctx in
  let* valid_libs, _ = Valid.get ctx ~all in
  let+ () =
    let* action_with_targets =
      let doc_dir = Path.parent_exn (Path.build (Artifact.odoc_file a)) in
      let+ run_odoc =
        let cmti = Artifact.source_file a in
        let iflags =
          Command.Args.memo
            (odoc_include_flags ctx all findlib_paths package requires indices)
        in
        let parent_args = parent_args parent_opt in
        let quiet_arg =
          if quiet then Command.Args.A "--print-warnings=false" else Command.Args.empty
        in
        Odoc.run_odoc
          sctx
          ~dir:doc_dir
          "compile"
          ~flags_for:(Some odoc_file)
          ~quiet
          ([ Command.Args.A "-I"
           ; Path doc_dir
           ; iflags
           ; A "-o"
           ; Target odoc_file
           ; Dep cmti
           ]
           @ parent_args
           @ [ quiet_arg ])
      in
      let file_deps = Dep.deps ctx ~all findlib_paths valid_libs package requires in
      let open Action_builder.With_targets.O in
      Action_builder.with_no_targets file_deps
      >>> Action_builder.with_no_targets module_deps
      >>> run_odoc
    in
    add_rule sctx action_with_targets
  in
  odoc_file
;;

(* Calculate the dependency libraries for the compilation step. We
   require all of the odoc files for all dependency libraries to be
   created rather than doing any fine-grained dependency management. *)
let compile_requires libs =
  let+ requires = Memo.List.map ~f:Lib.requires libs in
  let requires = Resolve.all requires |> Resolve.map ~f:List.flatten in
  Resolve.map
    requires
    ~f:(List.filter ~f:(fun l -> not (List.mem libs l ~equal:lib_equal)))
;;

let link_requires libs = Lib.closure libs ~linking:false

(* Compile an mld file *)
let compile_mld sctx a ~parent_opt ~quiet ~is_index ~children =
  assert (Artifact.artifact_ty a = Artifact.Mld);
  let odoc_file = Artifact.odoc_file a in
  let* run_odoc =
    let quiet_arg =
      if quiet then Command.Args.A "--print-warnings=false" else Command.Args.empty
    in
    let doc_dir = Path.Build.parent_exn (Artifact.odoc_file a) in
    let odoc_input = Artifact.source_file a in
    let parent_args =
      match parent_opt with
      | None -> []
      | _ -> parent_args parent_opt
    in
    let child_args =
      let child_args =
        List.fold_left children ~init:[] ~f:(fun args child ->
          match Artifact.artifact_ty child with
          | Module true | Mld -> "--child" :: Artifact.reference child :: args
          | Module false -> args)
      in
      if is_index && List.is_empty child_args then [ "--child"; "dummy" ] else child_args
    in
    Odoc.run_odoc
      sctx
      ~dir:(Path.build doc_dir)
      "compile"
      ~flags_for:(Some odoc_file)
      ~quiet
      (A "-o"
       :: Target odoc_file
       :: Dep odoc_input
       :: As child_args
       :: quiet_arg
       :: parent_args)
  in
  let+ () = add_rule sctx run_odoc in
  odoc_file
;;

(* Link a _set_ of odoc files into odocl files. *)
let link_odoc_rules sctx ~all (artifacts : Artifact.t list) ~quiet ~package ~libs ~indices
  =
  let ctx = Super_context.context sctx in
  let* findlib_paths = Context.findlib_paths ctx in
  let* requires = link_requires libs in
  let* deps =
    let+ valid_libs, _ = Valid.get ctx ~all in
    Dep.deps ctx ~all findlib_paths valid_libs package requires
  in
  let index_deps =
    List.map indices ~f:(fun x -> Command.Args.Hidden_deps (index_dep x))
  in
  let quiet_arg =
    if quiet then Command.Args.A "--print-warnings=false" else Command.Args.empty
  in
  let open Memo.O in
  Memo.parallel_iter artifacts ~f:(fun a ->
    let* run_odoc =
      Odoc.run_odoc
        sctx
        ~dir:(Path.parent_exn (Path.build (Artifact.odocl_file a)))
        "link"
        ~quiet
        ~flags_for:(Some (Artifact.odoc_file a))
        (index_deps
         @ [ odoc_include_flags ctx all findlib_paths package requires indices
           ; A "-o"
           ; Target (Artifact.odocl_file a)
           ; Dep (Path.build (Artifact.odoc_file a))
           ]
         @ [ quiet_arg ])
    in
    add_rule
      sctx
      (let open Action_builder.With_targets.O in
       Action_builder.with_no_targets deps >>> run_odoc))
;;

(* Output the actual html *)
let html_generate sctx all (a : Artifact.t) =
  let ctx = Super_context.context sctx in
  let open Memo.O in
  let html_output = Paths.html_root ctx ~all in
  let support_relative =
    let odoc_support_path = Paths.odoc_support ctx ~all in
    Path.reach (Path.build odoc_support_path) ~from:(Path.build html_output)
  in
  let* run_odoc =
    Odoc.run_odoc
      sctx
      ~quiet:false
      ~dir:(Path.build html_output)
      "html-generate"
      ~flags_for:None
      [ A "-o"
      ; Path (Path.build html_output)
      ; A "--support-uri"
      ; A support_relative
      ; A "--theme-uri"
      ; A support_relative
      ; Dep (Path.build (Artifact.odocl_file a))
      ]
  in
  let rule, result =
    match Artifact.artifact_ty a with
    | Mld ->
      ( Action_builder.With_targets.add ~file_targets:[ Artifact.html_file a ] run_odoc
      , None )
    | Module _ ->
      let dir = Artifact.html_dir a in
      ( Action_builder.With_targets.add_directories ~directory_targets:[ dir ] run_odoc
      , Some dir )
  in
  let+ () = add_rule sctx rule in
  result
;;

(* Intra-library module dependencies have to be found out for
   external libraries, but dune already knows these for internal libraries.
   For consistency however, we use the same method for both - we ask odoc. *)
let external_module_deps_rule sctx ~all a =
  match Artifact.artifact_ty a with
  | Module _ ->
    let ctx = Super_context.context sctx in
    let deps_file = Path.Build.set_extension (Artifact.odoc_file a) ~ext:".deps" in
    let+ () =
      let* odoc = Odoc.odoc_program sctx (Paths.root ctx ~all) in
      Super_context.add_rule
        sctx
        ~dir:(Paths.root ctx ~all)
        (Command.run
           odoc
           ~dir:(Path.parent_exn (Path.build deps_file))
           ~stdout_to:deps_file
           [ A "compile-deps"; Dep (Artifact.source_file a) ])
    in
    Some deps_file
  | _ -> Memo.return None
;;

(* We run [odoc compile-deps] on the cmti files to find out the dependencies.
   This function parses the output. *)
let parse_odoc_deps lines =
  let rec getdeps cur = function
    | x :: rest ->
      (match String.split ~on:' ' x with
       | [ m; hash ] -> getdeps ((Module_name.of_string m, hash) :: cur) rest
       | _ -> getdeps cur rest)
    | [] -> cur
  in
  getdeps [] lines
;;

(* Here we compile all artifacts - modules and mlds. *)
let compile_odocs sctx ~all ~quiet artifacts parent libs =
  let* requires =
    let ctx = Super_context.context sctx in
    let requires = compile_requires libs in
    Resolve.Memo.bind requires ~f:(fun libs ->
      let+ libs = Valid.filter_libs ctx ~all libs in
      Resolve.return libs)
  in
  Memo.parallel_iter artifacts ~f:(fun a ->
    let* deps_file = external_module_deps_rule sctx ~all a in
    match deps_file with
    | None ->
      (* mld file *)
      let+ (_ : Path.Build.t) =
        compile_mld
          sctx
          a
          ~quiet:false
          ~parent_opt:(Some parent)
          ~is_index:false
          ~children:[]
      in
      ()
    | Some deps_file ->
      let module_deps =
        let open Action_builder.O in
        let* l = Action_builder.lines_of (Path.build deps_file) in
        let deps' =
          parse_odoc_deps l
          |> List.filter_map ~f:(fun (m', _) ->
            if Artifact.module_name a = Some m'
            then None
            else
              List.find artifacts ~f:(fun a -> Artifact.module_name a = Some m')
              |> Option.map ~f:(fun a' -> Artifact.odoc_file a' |> Path.build))
        in
        Dune_engine.Dep.Set.of_files deps' |> Action_builder.deps
      in
      let+ (_odoc_file : Path.Build.t) =
        let parent_opt =
          match Artifact.artifact_ty a with
          | Module true -> Some parent
          | _ -> None
        in
        compile_module
          sctx
          all
          ~artifact:a
          ~requires
          ~module_deps
          ~quiet
          ~parent_opt
          ~package:None
          ~indices:[]
      in
      ())
;;

(* Read an _external_ directory and find all the 'odoc-interesting' files
   inside. This is used in the fallback case where we don't know what modules
   there are in a particular switch directory. *)
let modules_of_dir d : (Module_name.t * (Path.t * [ `Cmti | `Cmt | `Cmi ])) list Memo.t =
  let extensions = [ ".cmti", `Cmti; ".cmt", `Cmt; ".cmi", `Cmi ] in
  Fs_memo.dir_contents (Path.as_outside_build_dir_exn d)
  >>| function
  | Error _ -> []
  | Ok dc ->
    let list = Fs_cache.Dir_contents.to_list dc in
    List.filter_map list ~f:(fun (x, ty) ->
      match ty, List.assoc extensions (Filename.extension x) with
      | Unix.S_REG, Some _ -> Some (Filename.chop_extension x)
      | _, _ -> None)
    |> List.sort_uniq ~compare:String.compare
    |> List.map ~f:(fun m ->
      let ext, ty =
        List.find_exn extensions ~f:(fun (ext, _ty) ->
          List.exists list ~f:(fun (n, _) -> n = m ^ ext))
      in
      Module_name.of_string m, (Path.relative d (m ^ ext), ty))
;;

(* Here we are constructing the list of artifacts for various types of things
   to be documented - packages, fallback dirs, libraries (both private and those
   in packages) *)
let fallback_artifacts sctx (libs : Lib.t list String.Map.t) =
  let ctx = Super_context.context sctx in
  let* findlib_paths = Context.findlib_paths ctx in
  String.Map.foldi libs ~init:[] ~f:(fun local_dir libs acc ->
    let result =
      let+ mods =
        let cmti_paths =
          List.map findlib_paths ~f:(fun path -> Path.relative path local_dir)
        in
        Memo.parallel_map ~f:modules_of_dir cmti_paths
        >>| List.concat
        >>| List.fold_left ~init:[] ~f:(fun acc (mod_name, (cmti_file, _)) ->
          let artifact =
            let visible =
              Module_name.to_string mod_name |> String.contains_double_underscore |> not
            in
            let index = Index.of_dir local_dir in
            Artifact.make_module ctx ~all:true index cmti_file ~visible
          in
          artifact :: acc)
      in
      local_dir, mods, libs
    in
    result :: acc)
  |> Memo.all_concurrently
;;

let lib_artifacts ctx all index lib modules =
  let info = Lib.info lib in
  let cm_kind : Lib_mode.Cm_kind.t =
    match
      let modes = Lib_info.modes info in
      Lib_mode.Map.Set.for_merlin modes
    with
    | Ocaml _ -> Ocaml Cmi
    | Melange -> Melange Cmi
  in
  let obj_dir = Lib_info.obj_dir info in
  let entry_modules = Modules.entry_modules modules in
  Modules.fold_no_vlib modules ~init:[] ~f:(fun m acc ->
    let visible =
      let visible =
        List.mem entry_modules m ~equal:(fun m1 m2 ->
          Module_name.equal (Module.name m1) (Module.name m2))
      in
      visible
      && Module.name m
         |> Module_name.to_string
         |> String.contains_double_underscore
         |> not
    in
    let cmti_file = Obj_dir.Module.cmti_file obj_dir ~cm_kind m in
    Artifact.make_module ctx ~all index cmti_file ~visible :: acc)
;;

let ext_package_mlds (ctx : Context.t) (pkg : Package.Name.t) =
  let* findlib = Findlib.create (Context.name ctx) in
  Findlib.find_root_package findlib pkg
  >>| function
  | Error _ -> []
  | Ok dpkg ->
    let installed = dpkg.files in
    List.filter_map installed ~f:(function
      | Dune_section.Doc, fs ->
        let doc_path = Section.Map.find_exn dpkg.sections Doc in
        Some
          (List.filter_map fs ~f:(fun dst ->
             let str = Install.Entry.Dst.to_string dst in
             if Filename.check_suffix str ".mld"
             then Some (Path.relative doc_path str)
             else None))
      | _ -> None)
    |> List.concat
;;

let pkg_mlds sctx pkg =
  let* pkgs = Only_packages.get () in
  if Package.Name.Map.mem pkgs pkg
  then Packages.mlds sctx pkg >>| List.map ~f:Path.build
  else (
    let ctx = Super_context.context sctx in
    ext_package_mlds ctx pkg)
;;

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.rev_map mlds ~f:(fun mld -> Filename.chop_extension (Path.basename mld), mld)
    |> Filename.Map.of_list
  with
  | Ok m -> m
  | Error (_, p1, p2) ->
    User_error.raise
      [ Pp.textf
          "Package %s has two mld's with the same basename %s, %s"
          (Package.Name.to_string pkg)
          (Path.to_string_maybe_quoted p1)
          (Path.to_string_maybe_quoted p2)
      ]
;;

let pkg_artifacts sctx index pkg =
  let ctx = Super_context.context sctx in
  let+ mlds_map =
    let+ mlds = pkg_mlds sctx pkg in
    check_mlds_no_dupes ~pkg ~mlds
  in
  let artifacts =
    let mlds_noindex = String.Map.filteri ~f:(fun i _ -> i <> "index") mlds_map in
    String.Map.values mlds_noindex
    |> List.map ~f:(fun mld -> Artifact.external_mld ctx index mld)
  in
  let index_file = String.Map.find mlds_map "index" in
  index_file, artifacts
;;

module Index_tree = struct
  (* Here we are building up the trees that will become the documentation
     tree. At each node in the tree we have an index, which might contain
     artifacts itself, or it might just contain subtrees. The index itself
     might have been handwritten (ie., and index.mld file in a package), in
     which case we fill in [predefined] with the path to the mld. [libs]
     might contain some sublibraries with their artifacts - these might or
     might not be attached to this node of the tree. This is for the
     package index, where we can link to all the artifacts in that package
     even if they're in subdirectories. There's often a library that shares
     the same name as the package, and these are the artifacts that belong
     directly to this tree node.

     The trees are constructed by first constructing an index_info value for
     each library, package and private library, then combine them together
     to create the tree structure. The index itself is a list of index
     components, and when we construct the tree the key is a component of
     the index.

     For example, we might have a package with two sublibraries whose
     indexes and infos are:

     [Package (Dune_with_modules, "foo")], <foo package info>
     [Package (Dune_with_modules, "foo"); SubDir "bar"], <foo.baz info>
     [Package (Dune_with_modules, "foo"); SubDir "baz"], <foo.baz info>

     This will be represented by the tree:

     Br (empty, [ (Package (Dune_with_modules, "foo"), Br (<foo package info>,
      [ (Subdir "bar", Br (<foo.baz info>, []))
      ; (Subdir "baz", Br (<foo.baz info>, []))
      ]))])
  *)
  type info =
    { artifacts : Artifact.t list
    ; predefined_index : Path.t option
    ; libs : Artifact.t list Lib.Map.t
    ; package : string option
    }

  type 'a t = Br of 'a * (Index.ty * 'a t) list

  let of_index_list ~empty ~combine indexes =
    let cmp x (y, _) = Index.compare_ty x y = Eq in
    let add_one (cur : 'a t) (index, v) =
      let list = List.rev index in
      let rec inner (tree : 'a t) index_list =
        match tree, index_list with
        | Br (w, children), [] -> Br (combine w v, children)
        | Br (w, children), path :: paths ->
          let subtree, other_children =
            match List.partition children ~f:(cmp path) with
            | [ (_, tree) ], others -> tree, others
            | [], others -> Br (empty, []), others
            | _ -> assert false
          in
          Br (w, (path, inner subtree paths) :: other_children)
      in
      inner cur list
    in
    List.fold_left indexes ~init:(Br (empty, [])) ~f:add_one
  ;;

  let of_index_info =
    let empty =
      { artifacts = []; predefined_index = None; libs = Lib.Map.empty; package = None }
    in
    let combine x y =
      let predefined_index = Option.first_some x.predefined_index y.predefined_index in
      let libs = Lib.Map.union x.libs y.libs ~f:(fun _lib x _y -> Some x) in
      let package = Option.first_some x.package y.package in
      { artifacts = x.artifacts @ y.artifacts; predefined_index; libs; package }
    in
    fun x -> of_index_list ~empty ~combine x
  ;;

  let fold t ~init ~f =
    let rec inner index acc (Br (x, children)) =
      let acc = f index x acc in
      List.fold_left ~init:acc children ~f:(fun acc (i, x) ->
        let index = i :: index in
        inner index acc x)
    in
    inner [] init t
  ;;

  let iter_memo t ~f =
    let* _ =
      fold t ~init:[] ~f:(fun index x acc -> f index x :: acc) |> Memo.all_concurrently
    in
    Memo.return ()
  ;;
end

(* Here we construct the index info for each package, private library
   and external fallback *)
let index_info_of_pkg_def =
  let f (sctx, all, pkg_name, dwms) =
    let ctx = Super_context.context sctx in
    let* is_local = Valid.is_local_package ctx ~all pkg_name in
    let pkg_ty = if is_local then Index.Local else Dune_with_modules in
    let* main_index_path, main_artifacts =
      let index = [ Index.Package (pkg_ty, Package.Name.to_string pkg_name) ] in
      pkg_artifacts sctx index pkg_name
    in
    let+ index_infos =
      let+ findlib_paths = Context.findlib_paths ctx
      and+ dwms = Valid.filter_dune_with_modules ctx ~all dwms in
      List.fold_left ~init:[] dwms ~f:(fun acc (dwm : Classify.dune_with_modules) ->
        assert (Lib.is_local dwm.lib = is_local);
        let index =
          if is_local
          then Index.of_local_lib dwm.lib
          else Index.of_external_lib findlib_paths Dune_with_modules dwm.lib
        in
        let artifacts = lib_artifacts ctx all index dwm.lib dwm.modules in
        let entry_modules =
          artifacts
          |> List.filter ~f:Artifact.is_module
          |> List.filter ~f:Artifact.is_visible
        in
        let libs = Lib.Map.singleton dwm.lib entry_modules in
        ( index
        , { Index_tree.libs
          ; artifacts
          ; predefined_index = None
          ; package = Some (Package.Name.to_string pkg_name)
          } )
        :: acc)
    in
    (* The main package mlds can link to any library in the package *)
    let libs =
      List.fold_left index_infos ~init:Lib.Map.empty ~f:(fun acc (_, x) ->
        Lib.Map.union x.Index_tree.libs acc ~f:(fun _lib x _y -> Some x))
    in
    let pkg_index_info =
      ( [ Index.Package (pkg_ty, Package.Name.to_string pkg_name) ]
      , { Index_tree.libs
        ; artifacts = main_artifacts
        ; predefined_index = main_index_path
        ; package = Some (Package.Name.to_string pkg_name)
        } )
    in
    pkg_index_info :: index_infos
  in
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * bool * Package.Name.t * Classify.dune_with_modules list

    let equal (c1, b1, n1, dwms1) (c2, b2, n2, dwms2) =
      Super_context.equal c1 c2
      && b1 = b2
      && Package.Name.equal n1 n2
      && List.equal Classify.dwm_equal dwms1 dwms2
    ;;

    let hash (c, b, n, dwms) =
      Poly.hash
        (Super_context.hash c, b, Package.Name.hash n, List.hash Classify.dwm_hash dwms)
    ;;

    let to_dyn (c, b, n, dwms) =
      Dyn.Tuple
        [ Super_context.to_dyn c
        ; Dyn.Bool b
        ; Package.Name.to_dyn n
        ; Dyn.list Classify.dwm_to_dyn dwms
        ]
    ;;
  end
  in
  Memo.create "index_info_of_local_pkg" ~input:(module Input) f
;;

let index_info_of_pkg sctx all pkg_name dwms =
  Memo.exec index_info_of_pkg_def (sctx, all, pkg_name, dwms)
;;

let index_info_of_private_lib_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * bool * string * Lib.Local.t

    let equal (c1, b1, s1, l1) (c2, b2, s2, l2) =
      Super_context.equal c1 c2 && b1 = b2 && String.equal s1 s2 && Lib.Local.equal l1 l2
    ;;

    let hash (c, b, s, l) = Poly.hash (Super_context.hash c, b, s, Lib.Local.hash l)

    let to_dyn (c, b, s, l) =
      Dyn.Tuple [ Super_context.to_dyn c; Dyn.Bool b; Dyn.String s; Lib.Local.to_dyn l ]
    ;;
  end
  in
  let f : Input.t -> _ =
    fun (sctx, all, lnu, lib) ->
    let ctx = Super_context.context sctx in
    let index = [ Index.Private_lib lnu ] in
    let+ artifacts =
      let+ modules =
        Dir_contents.modules_of_lib sctx (lib :> Lib.t) >>| Option.value_exn
      in
      lib_artifacts ctx all index (lib :> Lib.t) modules
    in
    let libs =
      let entry_modules =
        artifacts
        |> List.filter ~f:Artifact.is_module
        |> List.filter ~f:Artifact.is_visible
      in
      Lib.Map.singleton (lib :> Lib.t) entry_modules
    in
    [ index, { Index_tree.libs; artifacts; predefined_index = None; package = None } ]
  in
  Memo.create "index_info_of_private_lib" ~input:(module Input) f
;;

let index_info_of_private_lib sctx all lnu lib =
  Memo.exec index_info_of_private_lib_def (sctx, all, lnu, lib)
;;

let index_info_of_external_fallback_def =
  let f (sctx, (fallback : Classify.fallback)) =
    let ctx = Super_context.context sctx in
    let* libs = Valid.filter_fallback_libs ctx ~all:true fallback.libs in
    let* artifacts = fallback_artifacts sctx libs in
    List.fold_left artifacts ~init:(Memo.return []) ~f:(fun acc (dir, artifacts, libs) ->
      let+ acc = acc in
      (* Urgh grotty hack time. The [ocaml] dir contains the [threads] library, which
         actually has no implementation and merely depends upon either [threads.vm]
         or [threads.posix], both of which are found as subdirectories. These libraries
         are in separate directories and, in turn, depend upon [unix], which is found in
         the [ocaml] directory. Since we only do fine-grained dependencies _between_
         directories this means that the [ocaml] dir depends on the [threads] dir, and
         the [threads] dir depends on the [ocaml] dir. We remove this dependency cycle
         by pretending the [threads] library is not present in the [ocaml] dir. This
         works because there actually aren't any modules in the [threads] library itself,
         they're all in the subdirs. *)
      let libs =
        if dir = "ocaml"
        then
          List.filter ~f:(fun lib -> Lib.name lib |> Lib_name.to_string <> "threads") libs
        else libs
      in
      let index = Index.of_dir dir in
      let libs =
        match List.map libs ~f:(fun lib -> lib, []) |> Lib.Map.of_list with
        | Ok m -> m
        | Error _ ->
          Log.info [ Pp.textf "Error combining libs for fallback dir" ];
          Lib.Map.empty
      in
      (index, { Index_tree.artifacts; libs; predefined_index = None; package = None })
      :: acc)
  in
  let module Local = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Classify.fallback

    let equal (sctx1, f1) (sctx2, f2) =
      Super_context.equal sctx1 sctx2 && Classify.fallback_equal f1 f2
    ;;

    let hash = Tuple.T2.hash Super_context.hash Classify.fallback_hash

    let to_dyn (sctx, f) =
      Dyn.Tuple [ Super_context.to_dyn sctx; Classify.fallback_to_dyn f ]
    ;;
  end
  in
  Memo.create "index_info_of_external_fallback" ~input:(module Local) f
;;

let index_info_of_external_fallback sctx fallback =
  Memo.exec index_info_of_external_fallback_def (sctx, fallback)
;;

(* Index generation

   The following are the functions for generating the text of the
   indexes for the three different types of level-1 indexes and the
   toplevel index. There are:

   0. The toplevel index
   1. Dune packages for which we have module info (both local and
   installed)
   2. External opam-lib directories
   3. Private libraries

   These should all be disjoint as local packages override external
   packages, local private libs have unique names generated for them
   and 'other' opam lib directories share the same namespace in the
   opam dir as external packages (as dune enforces that the package
   name is equal to the dir name in the opam lib directory).

   We actually use the same function for private libaries as for
   dune packages.
*)

let default_index ~main_name ~pkg_opt ~subindexes entry_modules =
  let b = Buffer.create 512 in
  Printf.bprintf
    b
    "{0 %s %s}\n"
    main_name
    (match pkg_opt with
     | None -> "index"
     | Some pkg ->
       (match pkg.Package.synopsis with
        | None -> ""
        | Some s -> " : " ^ s));
  (match pkg_opt with
   | None -> ()
   | Some pkg ->
     (match pkg.Package.description with
      | None -> ()
      | Some s -> Printf.bprintf b "%s\n" s));
  if List.length subindexes > 0
  then (
    Printf.bprintf b "{1 Sub-packages}\n%!";
    subindexes
    |> List.sort ~compare:(fun x y -> Dyn.compare (Index.to_dyn x) (Index.to_dyn y))
    |> List.iter ~f:(fun i ->
      Printf.bprintf b "- {{!page-\"%s\"}%s}\n" (Index.mld_name i) (Index.name i)));
  entry_modules
  |> List.sort ~compare:(fun (x, _) (y, _) -> Lib_name.compare x y)
  |> List.iter ~f:(fun (lib, modules) ->
    Printf.bprintf b "{1 Library %s}\n" (Lib_name.to_string lib);
    Buffer.add_string
      b
      (match modules with
       | [ x ] ->
         sprintf
           "The entry point of this library is the module:\n{!%s}.\n"
           (Artifact.reference x)
       | _ ->
         sprintf
           "This library exposes the following toplevel modules:\n{!modules:%s}\n"
           (modules
            |> List.map ~f:Artifact.name
            |> List.sort ~compare:String.compare
            |> String.concat ~sep:" ")));
  Buffer.contents b
;;

let default_fallback_index local_path ~subindexes artifacts =
  let b = Buffer.create 512 in
  Printf.bprintf b "{0 Index for filesystem path %s}\n" local_path;
  if List.length subindexes > 0
  then (
    Printf.bprintf b "{1 Sub-directories}\n%!";
    subindexes
    |> List.sort ~compare:(fun x y -> Dyn.compare (Index.to_dyn x) (Index.to_dyn y))
    |> List.iter ~f:(fun i ->
      Printf.bprintf b "- {{!page-\"%s\"}%s}\n" (Index.mld_name i) (Index.local_dir i)));
  Printf.bprintf
    b
    "{1 Modules}\n\
     The following is a list of all of the modules found at this filesystem path.\n\
     {!modules:%s}\n"
    (artifacts
     |> List.filter ~f:Artifact.is_visible
     |> List.filter_map ~f:Artifact.module_name
     |> List.sort ~compare:(fun x y -> Module_name.compare x y)
     |> List.map ~f:Module_name.to_string
     |> String.concat ~sep:" ");
  Buffer.contents b
;;

let toplevel_index_contents (Index_tree.Br (_, children)) =
  let b = Buffer.create 1024 in
  Printf.bprintf b "{0 Docs}\n\n";
  let output_indices label = function
    | [] -> ()
    | indices ->
      Printf.bprintf b "{1 %s}\n" label;
      List.iter indices ~f:(fun i ->
        Printf.bprintf b "- {!page-\"%s\"}\n" (Index.mld_name_ty i))
  in
  let top = List.map ~f:fst children in
  output_indices
    "Local Packages"
    (List.filter top ~f:(function
      | Index.Package (ty, _) when ty = Local -> true
      | _ -> false));
  output_indices
    "Switch-installed packages"
    (List.filter top ~f:(function
      | Index.Package (ty, _) when ty = Dune_with_modules -> true
      | _ -> false));
  output_indices
    "Other switch library directories"
    (List.filter top ~f:(function
      | Index.Package (ty, _) when ty = Unknown || ty = Fallback -> true
      | _ -> false));
  output_indices
    "Private libraries"
    (List.filter top ~f:(function
      | Index.Private_lib _ -> true
      | _ -> false));
  Buffer.contents b
;;

(* Here we construct the tree of all items that will end up being documented. *)
let full_tree sctx ~all =
  let ctx = Super_context.context sctx in
  let* categorized = Valid.get_categorized ctx all in
  let+ indexes =
    let indexes =
      let indexes =
        Package.Name.Set.fold
          categorized.packages
          ~init:(Memo.return [])
          ~f:(fun pkg acc ->
            let* acc = acc in
            let* libs_and_modules =
              let* libs = Odoc.libs_of_pkg ctx ~pkg in
              Memo.parallel_map libs ~f:(fun lib ->
                let+ modules =
                  Dir_contents.modules_of_lib sctx (lib :> Lib.t) >>| Option.value_exn
                in
                { Classify.lib :> Lib.t; modules })
            in
            let+ ii = index_info_of_pkg sctx all pkg libs_and_modules in
            List.rev_append ii acc)
      in
      String.Map.foldi ~init:indexes categorized.localprivate ~f:(fun name lib acc ->
        let* acc = acc in
        let+ ii = index_info_of_private_lib sctx all name lib in
        List.rev_append ii acc)
    in
    String.Map.fold ~init:indexes categorized.external_dirs ~f:(fun ty acc ->
      let* acc = acc in
      match ty with
      | Dune_with_modules (pkg_name, dwms) ->
        let+ ii = index_info_of_pkg sctx all pkg_name dwms in
        List.rev_append ii acc
      | Fallback fallback ->
        let+ ii = index_info_of_external_fallback sctx fallback in
        List.rev_append ii acc
      | Nothing -> Memo.return acc)
  in
  Index_tree.of_index_info indexes
;;

(* Here are the rules that operate on the Index_tree, for compiling and linking
   indexes, compiling and linking the artifacts of a package/library/fallback dir,
   and for generating the resulting HTML. *)

let hierarchical_index_rules sctx ~all (tree : Index_tree.info Index_tree.t) =
  let ctx = Super_context.context sctx in
  let main_name = "index" in
  let rec inner index (Index_tree.Br ((ii : Index_tree.info), children)) =
    let mld = Artifact.index ctx ~all index in
    let subindexes = List.map children ~f:(function x, _ -> x :: index) in
    let extra_children = List.map ~f:(Artifact.index ctx ~all:true) subindexes in
    let quiet = Index.is_external index in
    let* () =
      let index_path = Index.mld_path ctx ~all index in
      match ii.predefined_index with
      | Some p -> add_rule sctx (Action_builder.symlink ~src:p ~dst:index_path)
      | None ->
        let content =
          match index with
          | [] -> toplevel_index_contents tree
          | index when Index.might_be_fallback index ->
            default_fallback_index (Index.local_dir index) ~subindexes ii.artifacts
          | _ ->
            Lib.Map.foldi ~init:[] ~f:(fun l x acc -> (Lib.name l, x) :: acc) ii.libs
            |> default_index ~main_name ~pkg_opt:None ~subindexes
        in
        add_rule sctx (Action_builder.write_file index_path content)
    and* (_ : Path.Build.t) =
      let parent_opt =
        match index with
        | [] -> None
        | _ :: idx -> Some (Artifact.index ctx ~all idx)
      in
      compile_mld
        sctx
        mld
        ~quiet
        ~parent_opt
        ~is_index:true
        ~children:(extra_children @ ii.artifacts)
    and* () =
      let libs = Lib.Map.keys ii.libs in
      let package =
        match index with
        | [ Index.Package (_, pkg) ] -> Some pkg
        | _ -> None
      in
      link_odoc_rules
        sctx
        ~all
        [ mld ]
        ~package
        ~libs
        ~indices:(extra_children @ ii.artifacts)
        ~quiet
    in
    Memo.parallel_iter ~f:(fun (x, tree) -> inner (x :: index) tree) children
  in
  inner [] tree
;;

let hierarchical_html_rules sctx all tree =
  let ctx = Super_context.context sctx in
  Index_tree.fold ~init:(Memo.return []) tree ~f:(fun index (ii : Index_tree.info) dirs ->
    let* dirs = dirs in
    let artifacts =
      let index_artifact = Artifact.index ctx ~all:true index in
      List.filter ~f:Artifact.is_visible (index_artifact :: ii.artifacts)
    in
    let* new_dirs =
      Memo.List.filter_map artifacts ~f:(fun a -> html_generate sctx true a)
    in
    let+ () =
      let html_files =
        List.filter artifacts ~f:Artifact.is_visible
        |> List.map ~f:(fun a -> Path.build (Artifact.html_file a))
      in
      let html_alias =
        let html_dir = Index.html_dir ctx ~all index in
        Dep.html_alias html_dir
      in
      Rules.Produce.Alias.add_deps html_alias (Action_builder.paths html_files)
    in
    new_dirs @ dirs)
;;

let hierarchical_odoc_rules sctx ~all tree =
  let ctx = Super_context.context sctx in
  let* stdlib = stdlib_lib ctx in
  Index_tree.iter_memo tree ~f:(fun index (ii : Index_tree.info) ->
    let artifacts = ii.artifacts in
    let quiet = Index.is_external index in
    let libs =
      let libs = Lib.Map.keys ii.libs in
      match index, stdlib with
      | [ Index.Package (_, "ocaml") ], _ -> libs
      | _, Some lib -> lib :: libs
      | _, None -> libs
    in
    let* () =
      let parent = Artifact.index ctx ~all index in
      compile_odocs sctx ~all ~quiet artifacts parent libs
    and* () =
      link_odoc_rules sctx ~all artifacts ~package:ii.package ~libs ~indices:[] ~quiet
    in
    let all_deps =
      Path.Set.of_list_map artifacts ~f:(fun a -> Artifact.odoc_file a |> Path.build)
    in
    Dep.setup_deps ctx ~all index all_deps)
;;

(* And finally the rules to drive all of the above. *)
let setup_all_index_rules sctx ~all =
  let+ () = full_tree sctx ~all >>= hierarchical_index_rules sctx ~all in
  []
;;

let setup_odoc_rules sctx ~all =
  let+ () = full_tree sctx ~all >>= hierarchical_odoc_rules sctx ~all in
  []
;;

let setup_css_rule sctx ~all =
  let open Memo.O in
  let* run_odoc =
    let ctx = Super_context.context sctx in
    let dir = Paths.odoc_support ctx ~all in
    let+ cmd =
      Odoc.run_odoc
        sctx
        ~quiet:false
        ~dir:(Path.build (Context.build_dir ctx))
        "support-files"
        ~flags_for:None
        [ A "-o"; Path (Path.build dir) ]
    in
    Action_builder.With_targets.add_directories cmd ~directory_targets:[ dir ]
  in
  add_rule sctx run_odoc
;;

let static_html_rule ctx ~all =
  let open Paths in
  [ odoc_support ctx ~all ]
;;

let setup_all_html_rules sctx ~all =
  let ctx = Super_context.context sctx in
  let* tree = full_tree sctx ~all in
  let+ () =
    let html =
      let artifact = Artifact.index ctx ~all [] in
      Artifact.html_file artifact :: static_html_rule ctx ~all
      |> List.map ~f:(fun b -> Path.build b)
    in
    Rules.Produce.Alias.add_deps
      (Dep.html_alias (Index.html_dir ctx ~all []))
      (Action_builder.paths html)
  and+ () =
    let deps =
      Index_tree.fold ~init:[] tree ~f:(fun index _ acc ->
        if index = [] then acc else index :: acc)
      |> Dune_engine.Dep.Set.of_list_map ~f:(fun x ->
        Index.html_dir ctx ~all x |> Dep.html_alias |> Dune_engine.Dep.alias)
    in
    Rules.Produce.Alias.add_deps
      (Dep.html_alias (Index.html_dir ctx ~all []))
      (Action_builder.deps deps)
  and+ dirs = hierarchical_html_rules sctx all tree
  and+ () = setup_css_rule sctx ~all in
  Paths.odoc_support ctx ~all :: dirs
;;

(* End of external rules *)

let gen_project_rules sctx project =
  let ctx = Super_context.context sctx in
  Only_packages.packages_of_project project
  >>= Package.Name.Map_traversals.parallel_iter ~f:(fun _ (pkg : Package.t) ->
    let dir =
      let pkg_dir = Package.dir pkg in
      Path.Build.append_source (Context.build_dir ctx) pkg_dir
    in
    let register =
      let alias = Alias.make Alias0.doc_new ~dir in
      fun ~all ->
        let top_alias = Dep.html_alias (Index.html_dir ctx ~all []) in
        Dune_engine.Dep.alias top_alias
        |> Dune_engine.Dep.Set.singleton
        |> Action_builder.deps
        |> Rules.Produce.Alias.add_deps alias
    in
    let+ () = register ~all:false
    and+ () = register ~all:true in
    ())
;;

let has_rules m =
  let* dirs, rules = Rules.collect (fun () -> m) in
  let directory_targets =
    Path.Build.Map.of_list_map_exn dirs ~f:(fun dir -> dir, Loc.none)
  in
  Memo.return (Gen_rules.make ~directory_targets (Memo.return rules))
;;

let gen_rules sctx ~dir rest =
  let all = true in
  match rest with
  | [] ->
    Memo.return
      (Build_config.Gen_rules.make
         ~build_dir_only_sub_dirs:
           (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.all)
         (Memo.return Rules.empty))
  | [ "odoc" ] -> has_rules (setup_odoc_rules sctx ~all)
  | [ "index" ] -> has_rules (setup_all_index_rules sctx ~all)
  | [ "html"; "docs" ] -> has_rules (setup_all_html_rules sctx ~all)
  | _ -> Memo.return (Gen_rules.redirect_to_parent Gen_rules.Rules.empty)
;;
