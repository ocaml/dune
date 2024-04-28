open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules

let ( ++ ) = Path.Build.relative

module Ext_loc_map = Map.Make (Dune_package.External_location)

type ext_loc_maps =
  { findlib_paths : int Path.Map.t
  ; loc_of_pkg : Dune_package.External_location.t Package.Name.Map.t
  ; loc_of_lib : Dune_package.External_location.t Lib_name.Map.t
  ; libs_of_loc : (Dune_package.Lib.t * Lib.t) Lib_name.Map.t Ext_loc_map.t
  }

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

  let html_root ctx ~all = root ctx ~all ++ "html"
  let odoc_support ctx ~all = html_root ctx ~all ++ odoc_support_dirname
end

module Index = struct
  (* The index represents the position in the output HTML where the
     an artifact will be found. *)

  (* Packages are found in various places on the system*)
  type ext_ty =
    | Relative_to_stdlib
    | Relative_to_findlib of (int * Path.t)
    | Local_packages
    | Other (* plan b *)

  (* The directories immediately below 'docs' represent the various
     locations of packages, and also private libraries. Underneath
     the package locations are directories that map one-to-one with
     those in the filesystem, and within these there are subdirectories
     for sublibraries. For packages installed with dune the directories
     will have the same name as the package, but for packages installed
     with other build/packaging systems we fall back to simply replicating
     the filesystem structure, which in many cases is the package name,
     but not necessarily. *)

  type ty =
    | Private_lib of string
    | Top_dir of ext_ty
    | Sub_dir of string

  type t = ty list

  (* Used to suppress warnings on packages from the switch *)
  let rec is_external = function
    | [] -> false
    | Private_lib _ :: _ -> false
    | Top_dir (Relative_to_findlib _) :: _ -> true
    | Top_dir Relative_to_stdlib :: _ -> true
    | Top_dir Local_packages :: _ -> false
    | Top_dir Other :: _ -> true
    | Sub_dir _ :: xs -> is_external xs
  ;;

  let external_ty_to_dyn x =
    let open Dyn in
    match x with
    | Local_packages -> variant "Local_packages" []
    | Relative_to_stdlib -> variant "Relative_to_stdlib" []
    | Relative_to_findlib (x, _) -> variant "Relative_to_findlib" [ Int x ]
    | Other -> variant "Other" []
  ;;

  let ty_to_dyn x =
    let open Dyn in
    match x with
    | Private_lib lnu -> variant "Private_lib" [ String lnu ]
    | Top_dir e -> variant "Package" [ external_ty_to_dyn e ]
    | Sub_dir str -> variant "LocalSubLib" [ String str ]
  ;;

  let to_dyn x = Dyn.list ty_to_dyn x
  let compare_ty x y = Dyn.compare (ty_to_dyn x) (ty_to_dyn y)

  let subdir = function
    | Private_lib s -> s
    | Top_dir Local_packages -> "local"
    | Top_dir Relative_to_stdlib -> "stdlib"
    | Top_dir (Relative_to_findlib (n, _)) -> "findlib-" ^ string_of_int n
    | Top_dir Other -> "other"
    | Sub_dir str -> str
  ;;

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

  let of_local_lib lib =
    match
      let info = Lib.Local.info lib in
      Lib_info.package info
    with
    | None -> [ Private_lib (Odoc.lib_unique_name (lib :> Lib.t)) ]
    | Some _pkg ->
      (match Lib_name.analyze (Lib.name (lib :> Lib.t)) with
       | Private (_, _) -> [ Private_lib (Odoc.lib_unique_name (lib :> Lib.t)) ]
       | Public (pkg, rest) ->
         List.fold_left
           ~f:(fun acc s -> Sub_dir s :: acc)
           rest
           ~init:[ Sub_dir (Package.Name.to_string pkg); Top_dir Local_packages ])
  ;;

  let of_local_package pkg =
    [ Sub_dir (Package.Name.to_string pkg); Top_dir Local_packages ]
  ;;

  let of_external_loc maps (loc : Dune_package.External_location.t) : t option =
    let open Option.O in
    let* top, local =
      match loc with
      | Relative_to_stdlib local_path -> Some (Relative_to_stdlib, local_path)
      | Relative_to_findlib (findlib_path, local_path) ->
        let+ n = Path.Map.find maps.findlib_paths findlib_path in
        Relative_to_findlib (n, findlib_path), local_path
      | Absolute _ -> None
    in
    let s = Path.Local.explode local in
    let index =
      List.fold_left s ~f:(fun acc s -> Sub_dir s :: acc) ~init:[ Top_dir top ]
    in
    Some index
  ;;

  let of_external_lib maps lib =
    let name = Lib.name lib in
    match
      let open Option.O in
      let* loc = Lib_name.Map.find maps.loc_of_lib name in
      of_external_loc maps loc
    with
    | Some loc -> loc
    | None ->
      Log.info [ Pp.textf "Argh lib ex loc: %s" (Lib_name.to_string name) ];
      [ Sub_dir (Lib_name.to_string (Lib.name lib)); Top_dir Other ]
  ;;

  let of_pkg maps pkg =
    match
      let open Option.O in
      let* loc = Package.Name.Map.find maps.loc_of_pkg pkg in
      of_external_loc maps loc
    with
    | Some loc -> loc
    | None -> of_local_package pkg
  ;;
end

let add_rule sctx =
  let dir = Context.build_dir (Super_context.context sctx) in
  Super_context.add_rule sctx ~dir
;;

(* Returns a [ext_loc_maps] value that contains all the information
   needed to find the location of a library or package stored externally
   to the current workspace.

   Note that this list may contain entries for libraries that are also
   in the dune workspace, if they happen to be installed elsewhere. These
   are filtered out in the `Valid` module below, and this general function
   should not be used.

   Note: There are two reasons why we have a fallback mechanism for
   non-dune installed packages - first that there is more than one library
   in a particular directory, and the second is that we don't have a
   [Modules.t] value for the library. They essentially boil down to the
   problem that it's very hard to know which modules correspond to which
   library given a simple include path. Introspecting the cmas isn't
   sufficient because of the existence of cmi-only modules. A potential
   improvement is to handle the case of only one library per directory,
   though this is likely only of limited benefit. If we do this, we would
   need to modify this function to work on _all_ packages in the findlib
   directory so we can correctly identify those directories containing
   multiple libs.
*)
let libs_maps_def =
  let f (ctx, libs) =
    let* db = Scope.DB.public_libs (Context.name ctx)
    and* all_packages_entries =
      let* findlib = Findlib.create (Context.name ctx) in
      Memo.parallel_map ~f:(Findlib.find findlib) libs
      >>| List.filter_map ~f:Result.to_option
    in
    let* findlib_paths =
      let+ findlib_paths_list = Context.findlib_paths ctx in
      List.fold_left findlib_paths_list ~init:(0, Path.Map.empty) ~f:(fun (i, acc) path ->
        match Path.Map.add acc path i with
        | Ok acc -> i + 1, acc
        | Error _ ->
          Log.info [ Pp.textf "Error adding findlib path to map" ];
          i + 1, acc)
      |> snd
    in
    let init =
      { findlib_paths
      ; loc_of_pkg = Package.Name.Map.empty
      ; loc_of_lib = Lib_name.Map.empty
      ; libs_of_loc = Ext_loc_map.empty
      }
    in
    Memo.List.fold_left all_packages_entries ~init ~f:(fun maps entry ->
      match (entry : Dune_package.Entry.t) with
      | Deprecated_library_name _ | Hidden_library _ -> Memo.return maps
      | Dune_package.Entry.Library l ->
        (match Dune_package.Lib.external_location l with
         | None ->
           Log.info
             [ Pp.textf
                 "No location for lib %s"
                 (Dune_package.Lib.info l |> Lib_info.name |> Lib_name.to_string)
             ];
           Memo.return maps
         | Some location ->
           let info = Dune_package.Lib.info l in
           let name = Lib_info.name info in
           let pkg = Lib_info.package info in
           Lib.DB.find_lib_id db (Lib_info.lib_id info)
           >>| (function
            | None -> maps
            | Some lib ->
              let loc_of_lib =
                match Lib_name.Map.add maps.loc_of_lib name location with
                | Ok l -> l
                | Error _ ->
                  (* I don't expect this should ever happen *)
                  Log.info
                    [ Pp.textf
                        "Error adding lib %s to loc_of_lib map"
                        (Lib_name.to_string name)
                    ];
                  maps.loc_of_lib
              in
              let loc_of_pkg =
                match pkg with
                | None -> maps.loc_of_pkg
                | Some pkg_name ->
                  (match Package.Name.Map.add maps.loc_of_pkg pkg_name location with
                   | Ok l -> l
                   | Error _ ->
                     (* There will be lots of repeated packages, no problem here *)
                     maps.loc_of_pkg)
              in
              let update_fn = function
                | None -> Some (Lib_name.Map.singleton name (l, lib))
                | Some libs ->
                  (match Lib_name.Map.add libs name (l, lib) with
                   | Ok libs -> Some libs
                   | Error _ ->
                     Log.info
                       [ Pp.textf
                           "Error adding lib %s to libs_of_loc map"
                           (Lib_name.to_string name)
                       ];
                     Some libs)
              in
              let libs_of_loc =
                Ext_loc_map.update maps.libs_of_loc location ~f:update_fn
              in
              { maps with loc_of_lib; loc_of_pkg; libs_of_loc })))
  in
  let module Input = struct
    type t = Context.t * Lib_name.t list

    let equal (c1, l1) (c2, l2) = Context.equal c1 c2 && List.equal Lib_name.equal l1 l2
    let to_dyn = Dyn.pair Context.to_dyn (Dyn.list Lib_name.to_dyn)
    let hash (c, l) = Poly.hash (Context.hash c, List.hash Lib_name.hash l)
  end
  in
  Memo.create "odoc_lib_maps" ~input:(module Input) f
;;

let libs_maps_general ctx libs = Memo.exec libs_maps_def (ctx, libs)

module Classify = struct
  (* Here we classify top-level dirs in the findlib paths. They are either
     packages built by dune for which we have all the info we need -
     a [Modules.t] value - or there exists one (or more!) libraries within
     the directory that don't satisfy this, and these are labelled
     fallback directories. *)
  exception Fallback

  (* The fallback is simply a map of subdirectory to list of libraries
     found in that subdir. This is the value returned by [libs_of_local_dir] *)
  type fallback = { libs : Lib.t Lib_name.Map.t }

  let fallback_to_dyn x =
    let open Dyn in
    record [ "libs", Lib_name.Map.to_dyn Lib.to_dyn x.libs ]
  ;;

  let fallback_equal f1 f2 = Lib_name.Map.equal ~equal:Lib.equal f1.libs f2.libs

  let fallback_hash f =
    Lib_name.Map.fold f.libs ~init:0 ~f:(fun lib acc -> Poly.hash (acc, Lib.hash lib))
  ;;

  type local_dir_type =
    | Nothing
    | Dune_with_modules of (Package.Name.t * Lib.t)
    | Fallback of fallback

  (* We need to know if every single library within a findlib package dir is
     confined to its own subdirectory (ie, no two libraries are found in the
     same dir), and that we have information about the modules of every
     library within the tree. If this is not the case, we'll fall back to a
     less specific mode that simply documents the modules found within each
     dir without assigning them a library. *)
  let classify_location maps location =
    match Ext_loc_map.find maps.libs_of_loc location with
    | None ->
      Log.info
        [ Pp.textf
            "classify_local_dir: No lib at this location: %s"
            (Dyn.to_string (Dune_package.External_location.to_dyn location))
        ];
      Memo.return Nothing
    | Some libs ->
      (try
         let pkg, lib =
           match Lib_name.Map.values libs with
           | [ (dlib, lib) ] ->
             let info = Dune_package.Lib.info dlib in
             let pkg = Lib_name.package_name (Lib_info.name info) in
             (match
                let mods_opt = Lib_info.modules info in
                mods_opt, Lib_info.entry_modules info
              with
              | External (Some _), External (Ok _) -> pkg, lib
              | _ -> raise Fallback)
           | _ -> raise Fallback
         in
         Memo.return (Dune_with_modules (pkg, lib))
       with
       | Fallback ->
         let libs = Lib_name.Map.map libs ~f:snd in
         Memo.return (Fallback { libs }))
  ;;
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
          let+ mask = Dune_load.mask () in
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
            let* stdlib = stdlib_lib (Context.name ctx) in
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
    let* projects = Dune_load.projects () in
    Memo.exec valid_libs_and_packages (ctx, all, projects)
  ;;

  (* Some functions to filter various values containing libraries
     against the allowlist *)

  let filter_libs ctx ~all libs =
    let+ valid_libs, _ = get ctx ~all in
    List.filter libs ~f:(fun l -> List.mem valid_libs l ~equal:lib_equal)
  ;;

  let filter_fallback_libs ctx ~all libs =
    let+ valid_libs, _ = get ctx ~all in
    Lib_name.Map.filter libs ~f:(fun lib -> List.mem valid_libs lib ~equal:lib_equal)
  ;;

  let libs_maps ctx ~all =
    let* libs, _packages = get ctx ~all in
    let libs =
      List.filter_map
        ~f:(fun l -> if Lib.is_local l then None else Some (Lib.name l))
        libs
    in
    libs_maps_general ctx libs
  ;;

  (* It's handy for the toplevel index generation to be able to construct
     a categorized list of all the packages, the libraries and everything
     else that will end up being documented. *)
  type categorized =
    { packages : Package.Name.Set.t
    ; local : Lib.Local.t Lib_name.Map.t
    ; externals : Classify.local_dir_type Ext_loc_map.t
    }

  let empty_categorized =
    { packages = Package.Name.Set.empty
    ; local = Lib_name.Map.empty
    ; externals = Ext_loc_map.empty
    }
  ;;

  let get_categorized_memo =
    let run (ctx, all) =
      let* libs, packages = get ctx ~all in
      let init =
        Memo.return
          { empty_categorized with packages = Package.Name.Set.of_list packages }
      in
      List.fold_left libs ~init ~f:(fun cats lib ->
        let* cats = cats in
        match Lib.Local.of_lib lib with
        | Some llib ->
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
          let* maps = libs_maps ctx ~all in
          (match Lib_name.Map.find maps.loc_of_lib (Lib.name lib) with
           | None ->
             Log.info
               [ Pp.textf "No location for lib: %s" (Lib.name lib |> Lib_name.to_string) ];
             Memo.return cats
           | Some location ->
             let* classification = Classify.classify_location maps location in
             let f (old : Classify.local_dir_type option) =
               match old with
               | None -> Some classification
               | Some Nothing -> old
               | Some (Fallback _) -> old
               | Some (Dune_with_modules _) ->
                 let loc_str =
                   Dyn.to_string (Dune_package.External_location.to_dyn location)
                 in
                 Log.info
                   [ Pp.textf
                       "Duplicate 'Dune_with_modules' library found in location %s"
                       loc_str
                   ];
                 old
             in
             let externals = Ext_loc_map.update cats.externals location ~f in
             Memo.return { cats with externals }))
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

  (** [deps ctx all maps valid_libs pkg requires] returns all odoc
      dependencies of [requires]. If a package [pkg] is also specified, then
      the odoc dependencies of the package are returned - these are the odoc
      files representing mld files in the package. [maps] is obtained via
      [Valid.libs_maps] and [valid_libs] comes from [Valid.get]. *)
  val deps
    :  Context.t
    -> all:bool
    -> ext_loc_maps
    -> Lib.t list
    -> Package.Name.t option
    -> Lib.t list Resolve.t
    -> unit Action_builder.t

  (*** [setup_deps ctx all index odocs] Adds [odocs] as dependencies for [index].
    These dependencies may be used using the [deps] function *)
  val setup_deps : Context.t -> all:bool -> Index.t -> Path.Set.t -> unit Memo.t
end = struct
  let html_alias dir = Alias.make Alias0.doc_new ~dir
  let alias = Alias.make (Alias.Name.of_string ".odoc-all")

  let deps ctx ~all maps valid_libs pkg requires =
    let open Action_builder.O in
    let* libs = Resolve.read requires in
    Action_builder.deps
      (let libs, init =
         match pkg with
         | None -> libs, Dep.Set.empty
         | Some p ->
           let index = Index.of_pkg maps p in
           let init =
             Dep.Set.singleton (Dep.alias (alias ~dir:(Index.odoc_dir ctx ~all index)))
           in
           let pkg_libs =
             List.filter ~f:(fun l -> Lib.info l |> Lib_info.package = pkg) valid_libs
           in
           List.rev_append pkg_libs libs, init
       in
       List.fold_left libs ~init ~f:(fun acc (lib : Lib.t) ->
         match List.mem ~equal:lib_equal valid_libs lib with
         | false -> acc
         | true ->
           let index =
             match Lib.Local.of_lib lib with
             | None -> Index.of_external_lib maps lib
             | Some l -> Index.of_local_lib l
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
      let basename = Path.basename v.source |> Filename.remove_extension in
      sprintf "page-\"%s\"" basename
    | Module _ ->
      let basename =
        Path.basename v.source |> Filename.remove_extension |> Stdune.String.capitalize
      in
      sprintf "module-%s" basename
  ;;

  let module_name v =
    match v.ty with
    | Module _ ->
      let basename =
        Path.basename v.source |> Filename.remove_extension |> Stdune.String.capitalize
      in
      Some (Module_name.of_string_allow_invalid (Loc.none, basename))
    | _ -> None
  ;;

  let name v = Path.basename v.source |> Filename.remove_extension
  let v ~source ~odoc ~html_dir ~html_file ~ty = { source; odoc; html_dir; html_file; ty }

  let make_module ctx ~all index source ~visible =
    let basename =
      Path.basename source |> Filename.remove_extension |> Stdune.String.uncapitalize
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
    let basename = Path.basename source |> Filename.remove_extension in
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
let odoc_include_flags ctx all maps pkg requires indices =
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
            | None -> Index.of_external_lib maps lib
            | Some lib -> Index.of_local_lib lib
          in
          Index.odoc_dir ctx ~all index |> Path.build |> Path.Set.add paths)
      in
      match pkg with
      | None -> paths
      | Some p ->
        Index.odoc_dir ctx ~all (Index.of_local_package p)
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
  let ctx = Super_context.context sctx in
  let* maps = Valid.libs_maps ctx ~all in
  let+ () =
    let action_with_targets =
      let doc_dir = Path.parent_exn (Path.build (Artifact.odoc_file a)) in
      let run_odoc =
        let cmti = Artifact.source_file a in
        let iflags =
          Command.Args.memo (odoc_include_flags ctx all maps package requires indices)
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
      let file_deps =
        let open Action_builder.O in
        let* valid_libs, _ = Action_builder.of_memo (Valid.get ctx ~all) in
        Dep.deps ctx ~all maps valid_libs package requires
      in
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
let compile_requires stdlib_opt libs =
  Memo.List.map ~f:(fun l -> Lib.closure ~linking:false [ l ]) libs
  >>| Resolve.all
  >>| Resolve.map ~f:(fun requires ->
    let requires = List.flatten requires in
    let requires =
      match stdlib_opt with
      | Some l -> l :: requires
      | None -> requires
    in
    List.filter requires ~f:(fun l -> not (List.mem libs l ~equal:lib_equal)))
;;

let link_requires stdlib_opt libs =
  Lib.closure libs ~linking:false
  |> Resolve.Memo.map ~f:(fun libs ->
    match stdlib_opt with
    | None -> libs
    | Some stdlib -> stdlib :: libs)
;;

let compile_mld sctx a ~parent_opt ~quiet ~is_index ~children =
  assert (Artifact.artifact_ty a = Artifact.Mld);
  let odoc_file = Artifact.odoc_file a in
  let run_odoc =
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
      (Command.Args.A "-o"
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
  let* maps = Valid.libs_maps ctx ~all in
  let* requires =
    let* stdlib_opt = stdlib_lib (Context.name ctx) in
    link_requires stdlib_opt libs
  in
  let* deps =
    let+ valid_libs, _ = Valid.get ctx ~all in
    Dep.deps ctx ~all maps valid_libs package requires
  in
  let index_deps =
    List.map indices ~f:(fun x -> Command.Args.Hidden_deps (index_dep x))
  in
  let quiet_arg =
    if quiet then Command.Args.A "--print-warnings=false" else Command.Args.empty
  in
  Memo.parallel_iter artifacts ~f:(fun a ->
    let run_odoc =
      Odoc.run_odoc
        sctx
        ~dir:(Path.parent_exn (Path.build (Artifact.odocl_file a)))
        "link"
        ~quiet
        ~flags_for:(Some (Artifact.odoc_file a))
        (index_deps
         @ [ odoc_include_flags ctx all maps package requires indices
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
let html_generate sctx all ~search_db (a : Artifact.t) =
  let ctx = Super_context.context sctx in
  let html_output = Paths.html_root ctx ~all in
  let support_relative =
    let odoc_support_path = Paths.odoc_support ctx ~all in
    Path.reach (Path.build odoc_support_path) ~from:(Path.build html_output)
  in
  let search_args =
    Sherlodoc.odoc_args sctx ~search_db ~dir_sherlodoc_dot_js:(Index.html_dir ctx ~all [])
  in
  let run_odoc =
    Odoc.run_odoc
      sctx
      ~quiet:false
      ~dir:(Path.build html_output)
      "html-generate"
      ~flags_for:None
      [ Command.Args.A "-o"
      ; Path (Path.build html_output)
      ; search_args
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
      let odoc = Odoc.odoc_program sctx (Paths.root ctx ~all) in
      Super_context.add_rule
        sctx
        ~dir:(Paths.root ctx ~all)
        (Command.run_dyn_prog
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
let parse_odoc_deps =
  let rec getdeps cur = function
    | [] -> cur
    | x :: rest ->
      (match String.split ~on:' ' x with
       | [ m; hash ] -> getdeps ((Module_name.of_string m, hash) :: cur) rest
       | _ -> getdeps cur rest)
  in
  fun lines -> getdeps [] lines
;;

(* Here we compile all artifacts - modules and mlds. *)
let compile_odocs sctx ~all ~quiet artifacts parent libs =
  let* requires =
    let ctx = Super_context.context sctx in
    let* stdlib_opt = stdlib_lib (Context.name ctx) in
    let requires = compile_requires stdlib_opt libs in
    Resolve.Memo.bind requires ~f:(fun libs ->
      let+ libs = Valid.filter_libs ctx ~all libs in
      Resolve.return libs)
  in
  Memo.parallel_iter artifacts ~f:(fun a ->
    external_module_deps_rule sctx ~all a
    >>= function
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
      | Unix.S_REG, Some _ -> Some (Filename.remove_extension x)
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
let fallback_artifacts
  ctx
  (location : Dune_package.External_location.t)
  (libs : Lib.t Lib_name.Map.t)
  =
  let* maps = Valid.libs_maps ctx ~all:true in
  match Index.of_external_loc maps location with
  | None -> Memo.return []
  | Some index ->
    let+ mods =
      let* ocaml = Context.ocaml ctx in
      let stdlib_dir = ocaml.lib_config.Lib_config.stdlib_dir in
      let cmti_path =
        match location with
        | Absolute d -> d
        | Relative_to_findlib (dir, l) -> Path.relative dir (Path.Local.to_string l)
        | Relative_to_stdlib l -> Path.relative stdlib_dir (Path.Local.to_string l)
      in
      modules_of_dir cmti_path
      >>| List.map ~f:(fun (mod_name, (cmti_file, _)) ->
        let visible =
          Module_name.to_string mod_name |> String.contains_double_underscore |> not
        in
        Artifact.make_module ctx ~all:true index cmti_file ~visible)
    in
    [ mods, libs ]
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
  let entry_modules = Modules.With_vlib.entry_modules modules in
  modules
  |> Modules.With_vlib.drop_vlib
  |> Modules.fold ~init:[] ~f:(fun m acc ->
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
          (List.filter_map fs ~f:(function
            | `File, dst ->
              let str = Install.Entry.Dst.to_string dst in
              if Filename.check_suffix str ".mld"
              then Some (Path.relative doc_path str)
              else None
            | _ -> None))
      | _ -> None)
    |> List.concat
;;

let pkg_mlds sctx pkg =
  let* pkgs = Dune_load.packages () in
  if Package.Name.Map.mem pkgs pkg
  then Packages.mlds sctx pkg >>| List.map ~f:Path.build
  else (
    let ctx = Super_context.context sctx in
    ext_package_mlds ctx pkg)
;;

let check_mlds_no_dupes ~pkg ~mlds =
  match
    List.rev_map mlds ~f:(fun mld -> Filename.remove_extension (Path.basename mld), mld)
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

     [Sub_dir "pkg"; Top_dir Local_packages], <pkg package info>
     [Sub_dir "bar"; Sub_dir "pkg"; Top_dir Local_packages], <pkg.bar info>
     [Sub_dir "baz"; Sub_dir "pkg"; Top_dir Local_packages], <pkg.baz info>

     This will be represented by the tree:

     Br (empty,
     [ Top_dir Local_packages, Br (empty,
         [ Sub_dir "pkg", Br (<foo package info>,
           [ Sub_dir "bar", Br (<foo.bar info>, [])
           ; Sub_dir "baz", Br (<foo.baz info>, [])
     ])])])
  *)
  type artifact_info =
    { artifacts : Artifact.t list
    ; compile_libs : Lib.t list
    }

  type info =
    { artifact_sets : artifact_info list
    ; predefined_index : Path.t option
    ; libs : Artifact.t list Lib.Map.t
    ; package : Package.Name.t option
    ; is_fallback : bool
    }

  type 'a t = Br of 'a * (Index.ty * 'a t) list

  let all_artifacts info =
    List.fold_left ~init:[] info.artifact_sets ~f:(fun acc x ->
      List.rev_append x.artifacts acc)
  ;;

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
      { artifact_sets = []
      ; predefined_index = None
      ; libs = Lib.Map.empty
      ; package = None
      ; is_fallback = false
      }
    in
    let combine x y =
      let predefined_index = Option.first_some x.predefined_index y.predefined_index in
      let libs = Lib.Map.union x.libs y.libs ~f:(fun _lib x _y -> Some x) in
      let package = Option.first_some x.package y.package in
      let is_fallback = x.is_fallback || y.is_fallback in
      { artifact_sets = x.artifact_sets @ y.artifact_sets
      ; predefined_index
      ; libs
      ; package
      ; is_fallback
      }
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
    let+ _ =
      fold t ~init:[] ~f:(fun index x acc -> f index x :: acc) |> Memo.all_concurrently
    in
    ()
  ;;
end

let index_info_of_pkg_def =
  let f (sctx, all, pkg_name) =
    let* maps =
      let ctx = Super_context.context sctx in
      Valid.libs_maps ctx ~all
    in
    let index = Index.of_pkg maps pkg_name in
    let+ main_index_path, main_artifacts = pkg_artifacts sctx index pkg_name in
    let pkg_index_info =
      ( index
      , { Index_tree.libs = Lib.Map.empty
        ; artifact_sets = [ { artifacts = main_artifacts; compile_libs = [] } ]
        ; predefined_index = main_index_path
        ; package = Some pkg_name
        ; is_fallback = false
        } )
    in
    [ pkg_index_info ]
  in
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * bool * Package.Name.t

    let equal (c1, b1, n1) (c2, b2, n2) =
      Super_context.equal c1 c2 && Package.Name.equal n1 n2 && b1 = b2
    ;;

    let hash (c, b, n) = Poly.hash (Super_context.hash c, b, Package.Name.hash n)

    let to_dyn (c, b, n) =
      Dyn.Tuple [ Super_context.to_dyn c; Dyn.bool b; Package.Name.to_dyn n ]
    ;;
  end
  in
  Memo.create "index_info_of_pkg" ~input:(module Input) f
;;

let index_info_of_pkg sctx all pkg_name =
  Memo.exec index_info_of_pkg_def (sctx, all, pkg_name)
;;

let index_info_of_lib_def =
  let module Input = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * bool * Lib.t

    let equal (c1, b1, l1) (c2, b2, l2) =
      Super_context.equal c1 c2 && b1 = b2 && Lib.equal l1 l2
    ;;

    let hash (c, b, l) = Poly.hash (Super_context.hash c, b, Lib.hash l)
    let to_dyn (c, b, l) = Dyn.Tuple [ Super_context.to_dyn c; Dyn.Bool b; Lib.to_dyn l ]
  end
  in
  let f : Input.t -> _ =
    fun (sctx, all, lib) ->
    let ctx = Super_context.context sctx in
    let* maps = Valid.libs_maps ctx ~all in
    let index =
      match Lib.Local.of_lib lib with
      | Some l -> Index.of_local_lib l
      | None -> Index.of_external_lib maps lib
    in
    let+ artifacts =
      let+ modules = Dir_contents.modules_of_lib sctx (lib :> Lib.t) in
      match modules with
      | Some m -> lib_artifacts ctx all index (lib :> Lib.t) m
      | None ->
        (* Note this shouldn't happen as we've filtered out libs
           without a [Modules.t] in the classification stage. *)
        Log.info
          [ Pp.textf
              "Expecting modules info for library %s"
              (Lib.name lib |> Lib_name.to_string)
          ];
        []
    in
    let libs =
      let entry_modules =
        artifacts
        |> List.filter ~f:Artifact.is_module
        |> List.filter ~f:Artifact.is_visible
      in
      Lib.Map.singleton lib entry_modules
    in
    let package = Lib_info.package (Lib.info lib) in
    let lib_index_info =
      ( index
      , { Index_tree.libs
        ; artifact_sets = [ { artifacts; compile_libs = [ lib ] } ]
        ; predefined_index = None
        ; package
        ; is_fallback = false
        } )
    in
    match package with
    | None -> [ lib_index_info ]
    | Some pkg ->
      let pkg_index = Index.of_pkg maps pkg in
      let pkg_index_info =
        ( pkg_index
        , { Index_tree.libs
          ; artifact_sets = []
          ; predefined_index = None
          ; package = Some pkg
          ; is_fallback = false
          } )
      in
      [ lib_index_info; pkg_index_info ]
  in
  Memo.create "index_info_of_lib" ~input:(module Input) f
;;

let index_info_of_lib sctx all lib = Memo.exec index_info_of_lib_def (sctx, all, lib)

let index_info_of_external_fallback_def =
  let f (ctx, location, (fallback : Classify.fallback)) =
    let* maps = Valid.libs_maps ctx ~all:true in
    match Index.of_external_loc maps location with
    | None -> Memo.return []
    | Some index ->
      Valid.filter_fallback_libs ctx ~all:true fallback.libs
      >>= fallback_artifacts ctx location
      >>| List.map ~f:(fun (artifacts, libs) ->
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
          if Lib_name.Map.mem libs (Lib_name.of_string "stdlib")
          then
            Lib_name.Map.filter libs ~f:(fun lib ->
              Lib.name lib |> Lib_name.to_string <> "threads")
          else libs
        in
        let compile_libs = Lib_name.Map.to_list_map libs ~f:(fun _ l -> l) in
        let libs =
          Lib_name.Map.fold libs ~init:Lib.Map.empty ~f:(fun lib map ->
            match Lib.Map.add map lib [] with
            | Ok map -> map
            | Error _ -> map)
        in
        ( index
        , { Index_tree.artifact_sets = [ { artifacts; compile_libs } ]
          ; libs
          ; predefined_index = None
          ; package = None
          ; is_fallback = true
          } ))
  in
  let module Local = struct
    module Super_context = Super_context.As_memo_key

    type t = Context.t * Dune_package.External_location.t * Classify.fallback

    let equal (sctx1, l1, f1) (sctx2, l2, f2) =
      Context.equal sctx1 sctx2
      && Dune_package.External_location.compare l1 l2 = Eq
      && Classify.fallback_equal f1 f2
    ;;

    let hash =
      Tuple.T3.hash
        Context.hash
        Dune_package.External_location.hash
        Classify.fallback_hash
    ;;

    let to_dyn (sctx, l1, f) =
      Dyn.Tuple
        [ Context.to_dyn sctx
        ; Dune_package.External_location.to_dyn l1
        ; Classify.fallback_to_dyn f
        ]
    ;;
  end
  in
  Memo.create "index_info_of_external_fallback" ~input:(module Local) f
;;

let index_info_of_external_fallback sctx location fallback =
  Memo.exec index_info_of_external_fallback_def (sctx, location, fallback)
;;

(* Index generation

   The following are the functions for generating the text of the
   indexes for the three different types of level-1 indexes and the
   toplevel index. There are:

   0. The toplevel index
   1. Dune packages for which we have module info (both local and
   installed)
   2. External findlib directories
   3. Private libraries

   We actually use the same function for private libraries as for
   dune packages.
*)
let standard_index_contents b entry_modules =
  entry_modules
  |> List.sort ~compare:(fun (x, _) (y, _) -> Lib_name.compare x y)
  |> List.iter ~f:(fun (lib, modules) ->
    match modules with
    | [] -> () (* No library here! *)
    | _ ->
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
              |> String.concat ~sep:" ")))
;;

let fallback_index_contents b entry_modules artifacts =
  Printf.bprintf b "{1 Libraries}\nThe following libraries are found in this directory.\n";
  entry_modules
  |> List.sort ~compare:(fun (x, _) (y, _) -> Lib_name.compare x y)
  |> List.iter ~f:(fun (lib, _modules) ->
    Printf.bprintf b "- %s\n" (Lib_name.to_string lib));
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
     |> String.concat ~sep:" ")
;;

let default_index index tree entry_modules =
  let (Index_tree.Br (info, children)) = tree in
  let b = Buffer.create 512 in
  let main_name, subtitle =
    match index with
    | [] -> "Main index", None (* unused! *)
    | [ Index.Top_dir (Relative_to_findlib (_, dir)) ] ->
      "Opam index", Some (sprintf "Packages installed in %s" (Path.to_string dir))
    | [ Index.Top_dir Relative_to_stdlib ] -> "Packages located relative to stdlib", None
    | [ Index.Top_dir Local_packages ] -> "Local packages", None
    | [ Index.Private_lib l ] -> sprintf "Private library %s" l, None
    | xs ->
      let subdirs =
        List.filter_map
          ~f:(fun x ->
            match x with
            | Index.Sub_dir x -> Some x
            | _ -> None)
          xs
      in
      if info.Index_tree.is_fallback
      then sprintf "Directory %s" (String.concat ~sep:"/" (List.rev subdirs)), None
      else sprintf "Package %s" (String.concat ~sep:"." (List.rev subdirs)), None
  in
  Printf.bprintf b "{0 %s}\n" main_name;
  (match subtitle with
   | None -> ()
   | Some s -> Printf.bprintf b "{i %s}\n" s);
  let subindexes = List.map children ~f:(fun (ty, _) -> ty :: index) in
  if List.length children > 0
  then (
    Printf.bprintf b "{1 Sub-indexes}\n%!";
    subindexes
    |> List.sort ~compare:(fun x y -> Dyn.compare (Index.to_dyn x) (Index.to_dyn y))
    |> List.iter ~f:(fun i ->
      Printf.bprintf b "- {{!page-\"%s\"}%s}\n" (Index.mld_name i) (Index.mld_name i)));
  if info.is_fallback
  then fallback_index_contents b entry_modules (Index_tree.all_artifacts info)
  else standard_index_contents b entry_modules;
  Buffer.contents b
;;

let toplevel_index_contents t =
  let b = Buffer.create 1024 in
  Printf.bprintf b "{0 Docs}\n\n";
  let all_descendents = Index_tree.fold ~init:[] ~f:(fun idx _ acc -> idx :: acc) t in
  let sorted =
    List.sort
      ~compare:(fun x y -> Dyn.compare (Index.to_dyn x) (Index.to_dyn y))
      all_descendents
  in
  let output_indices label = function
    | [] -> ()
    | indices ->
      Printf.bprintf b "{1 %s}\n" label;
      List.iter indices ~f:(fun i ->
        Printf.bprintf b "- {!page-\"%s\"}\n" (Index.mld_name_ty i))
  in
  output_indices
    "Local Packages"
    (List.filter_map sorted ~f:(function
      | [ x; Index.Top_dir Local_packages ] -> Some x
      | _ -> None));
  output_indices
    "Switch-installed packages"
    (List.filter_map sorted ~f:(function
      | [ x; Index.Top_dir (Relative_to_findlib _) ] -> Some x
      | [ (Index.Top_dir Relative_to_stdlib as x) ] -> Some x
      | _ -> None));
  output_indices
    "Private libraries"
    (List.filter_map sorted ~f:(function
      | [ (Index.Private_lib _ as x) ] -> Some x
      | _ -> None));
  Buffer.contents b
;;

(* Here we construct the tree of all items that will end up being documented. *)
let full_tree sctx ~all =
  let ctx = Super_context.context sctx in
  let* categorized = Valid.get_categorized ctx all in
  let indexes =
    let indexes =
      Package.Name.Set.fold categorized.packages ~init:(Memo.return []) ~f:(fun pkg acc ->
        let* acc = acc in
        let+ ii = index_info_of_pkg sctx all pkg in
        List.rev_append ii acc)
    in
    Lib_name.Map.fold ~init:indexes categorized.local ~f:(fun lib acc ->
      let* acc = acc in
      let+ ii = index_info_of_lib sctx all (lib :> Lib.t) in
      List.rev_append ii acc)
  in
  Ext_loc_map.foldi ~init:indexes categorized.externals ~f:(fun loc ty acc ->
    let* acc = acc in
    match ty with
    | Dune_with_modules (_, lib) ->
      let+ ii = index_info_of_lib sctx all lib in
      List.rev_append ii acc
    | Fallback fallback ->
      let+ ii =
        index_info_of_external_fallback (Super_context.context sctx) loc fallback
      in
      List.rev_append ii acc
    | Nothing -> Memo.return acc)
  >>| Index_tree.of_index_info
;;

(* Here are the rules that operate on the Index_tree, for compiling and linking
   indexes, compiling and linking the artifacts of a package/library/fallback dir,
   and for generating the resulting HTML. *)

let hierarchical_index_rules sctx ~all (tree : Index_tree.info Index_tree.t) =
  let ctx = Super_context.context sctx in
  let rec inner index (Index_tree.Br ((ii : Index_tree.info), children) as t) =
    let mld = Artifact.index ctx ~all index in
    let extra_children =
      let subindexes = List.map children ~f:(function x, _ -> x :: index) in
      List.map ~f:(Artifact.index ctx ~all) subindexes
    in
    let quiet = false in
    let* () =
      let index_path = Index.mld_path ctx ~all index in
      match ii.predefined_index with
      | Some p -> add_rule sctx (Action_builder.symlink ~src:p ~dst:index_path)
      | None ->
        let content =
          match index with
          | [] -> toplevel_index_contents tree
          | _ ->
            Lib.Map.foldi ~init:[] ~f:(fun l x acc -> (Lib.name l, x) :: acc) ii.libs
            |> default_index index t
        in
        add_rule sctx (Action_builder.write_file index_path content)
    and* (_ : Path.Build.t) =
      let parent_opt =
        match index with
        | [] -> None
        | _ :: idx -> Some (Artifact.index ctx ~all idx)
      in
      let all_artifacts = Index_tree.all_artifacts ii in
      compile_mld
        sctx
        mld
        ~quiet
        ~parent_opt
        ~is_index:true
        ~children:(extra_children @ all_artifacts)
    and* () =
      let libs = Lib.Map.keys ii.libs in
      let package = ii.package in
      let all_descendent_artifacts =
        let all_descendent_indices =
          (* This is down to an odoc deficiency. We should remove this restriction
             once we can reference page children in odoc ref syntax. The problem
             here is that we'd like to be able to reference [{!page-pkg.page-sublib}]
             but we can't, we can only reference [{!page-sublib}] which means there's
             a danger of a clash if we have, say, library [odoc.odoc]. *)
          let cur_depth = List.length index in
          Index_tree.fold t ~init:[] ~f:(fun idx _ acc ->
            if List.length idx > cur_depth + 2 then acc else (idx @ index) :: acc)
          (* First descendent is actually this node. Remove it! *)
          |> List.rev
          |> List.tl
        in
        List.map all_descendent_indices ~f:(Artifact.index ctx ~all)
      in
      let all_artifacts = Index_tree.all_artifacts ii in
      link_odoc_rules
        sctx
        ~all
        [ mld ]
        ~package
        ~libs
        ~indices:(all_descendent_artifacts @ all_artifacts)
        ~quiet
    in
    Memo.parallel_iter ~f:(fun (x, tree) -> inner (x :: index) tree) children
  in
  inner [] tree
;;

let hierarchical_html_rules sctx all tree ~search_db =
  let ctx = Super_context.context sctx in
  Index_tree.fold ~init:(Memo.return []) tree ~f:(fun index (ii : Index_tree.info) dirs ->
    let* dirs = dirs in
    let artifacts =
      let index_artifact = Artifact.index ctx ~all:true index in
      let all_artifacts = Index_tree.all_artifacts ii in
      List.filter ~f:Artifact.is_visible (index_artifact :: all_artifacts)
    in
    let* new_dirs =
      Memo.List.filter_map artifacts ~f:(fun a -> html_generate ~search_db sctx true a)
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
  Index_tree.iter_memo tree ~f:(fun index (ii : Index_tree.info) ->
    let quiet = Index.is_external index in
    let libs = Lib.Map.keys ii.libs in
    let all_artifacts = Index_tree.all_artifacts ii in
    let* () =
      let parent = Artifact.index ctx ~all index in
      Memo.List.iter
        ~f:(fun a ->
          compile_odocs
            sctx
            ~all
            ~quiet
            a.Index_tree.artifacts
            parent
            a.Index_tree.compile_libs)
        ii.artifact_sets
    and* () =
      let package = ii.package in
      link_odoc_rules sctx ~all all_artifacts ~package ~libs ~indices:[] ~quiet
    in
    let all_deps =
      Path.Set.of_list_map all_artifacts ~f:(fun a -> Artifact.odoc_file a |> Path.build)
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
  let run_odoc =
    let ctx = Super_context.context sctx in
    let dir = Paths.odoc_support ctx ~all in
    let cmd =
      Odoc.run_odoc
        sctx
        ~quiet:false
        ~dir:(Path.build (Context.build_dir ctx))
        "support-files"
        ~flags_for:None
        [ Command.Args.A "-o"; Path (Path.build dir) ]
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
  let* search_db =
    let dir = Index.html_dir ctx ~all [] in
    let external_odocls, odocls =
      Index_tree.fold tree ~init:([], []) ~f:(fun index ii acc ->
        let externals, odocls = acc in
        let is_external = Index.is_external index in
        let index_artifact = Artifact.index ctx ~all:true index in
        let new_artifacts =
          index_artifact :: Index_tree.all_artifacts ii
          |> List.filter_map ~f:(fun a ->
            if Artifact.is_visible a then Some (Artifact.odocl_file a) else None)
        in
        if is_external
        then new_artifacts @ externals, odocls
        else externals, new_artifacts @ odocls)
    in
    Sherlodoc.search_db sctx ~dir ~external_odocls odocls
  in
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
  and+ dirs = hierarchical_html_rules sctx all tree ~search_db
  and+ () = setup_css_rule sctx ~all
  and+ () =
    let dir = Index.html_dir ctx ~all [] in
    Sherlodoc.sherlodoc_dot_js sctx ~dir
  in
  Paths.odoc_support ctx ~all :: dirs
;;

(* End of external rules *)

let gen_project_rules sctx project =
  let ctx = Super_context.context sctx in
  Dune_project.packages project
  |> Dune_lang.Package_name.Map.to_seq
  |> Memo.parallel_iter_seq ~f:(fun (_, (pkg : Package.t)) ->
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
