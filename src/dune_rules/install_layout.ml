open Import

module Library = struct
  module T = struct
    type t =
      { package : Package.Name.t
      ; name : Lib_name.t
      }

    let compare a b =
      match Package.Name.compare a.package b.package with
      | Eq -> Lib_name.compare a.name b.name
      | ordering -> ordering
    ;;

    let hash { package; name } =
      Tuple.T2.hash Package.Name.hash Lib_name.hash (package, name)
    ;;

    let to_dyn { package; name } =
      Dyn.record [ "package", Package.Name.to_dyn package; "name", Lib_name.to_dyn name ]
    ;;
  end

  include T
  include Comparable.Make (T)

  let make ~package ~name = { package; name }
  let package t = t.package
  let name t = t.name

  let repr =
    Repr.view
      Repr.(pair Package.Name.repr Lib_name.repr)
      ~to_:(fun { package; name } -> package, name)
  ;;
end

module Redirect = Library

type request =
  { packages : Package.Name.Set.t
  ; libraries : Library.Set.t
  ; redirects : Redirect.Set.t
  }

let request_equal a b =
  Package.Name.Set.equal a.packages b.packages
  && Library.Set.equal a.libraries b.libraries
  && Redirect.Set.equal a.redirects b.redirects
;;

let request_hash { packages; libraries; redirects } =
  Tuple.T3.hash
    (List.hash Package.Name.hash)
    (List.hash Library.hash)
    (List.hash Redirect.hash)
    ( Package.Name.Set.to_list packages
    , Library.Set.to_list libraries
    , Redirect.Set.to_list redirects )
;;

let request_to_dyn { packages; libraries; redirects } =
  Dyn.record
    [ "packages", Package.Name.Set.to_dyn packages
    ; "libraries", Library.Set.to_dyn libraries
    ; "redirects", Redirect.Set.to_dyn redirects
    ]
;;

type generated_entry =
  { package : Package.Name.t
  ; section : Section.t
  ; dst : Install.Entry.Dst.t
  ; contents : string Action_builder.t
  }

type library_entries =
  { install_entries : (Package.Name.t * Install.Entry.Sourced.Unexpanded.t) list
  ; generated_entries : generated_entry list
  }

type materialized_source =
  | Symlink of Path.t Install.Entry.Expanded.t
  | Contents of string Action_builder.t

type materialized_entry =
  { package : Package.Name.t
  ; section : Section.t
  ; dst : Install.Entry.Dst.t
  ; kind : Install.Entry.Expanded.kind
  ; source : materialized_source
  }

module Key : sig
  val encode : request -> string
  val decode : string -> request option
end = struct
  let reverse_table : (Digest.t, request) Table.t = Table.create (module Digest) 128

  let encode ({ packages; libraries; redirects } as request) =
    let y =
      Digest.repr
        Repr.(triple (list Package.Name.repr) (list Library.repr) (list Redirect.repr))
        ( Package.Name.Set.to_list packages
        , Library.Set.to_list libraries
        , Redirect.Set.to_list redirects )
    in
    (match Table.find reverse_table y with
     | None -> Table.set reverse_table y request
     | Some request' ->
       if not (request_equal request request')
       then
         Code_error.raise
           "Hash collision between install layout requests"
           [ "cached", request_to_dyn request'; "new", request_to_dyn request ]);
    Digest.to_string y
  ;;

  let decode s =
    match Digest.from_hex s with
    | None -> None
    | Some digest -> Table.find reverse_table digest
  ;;
end

type resolvers =
  { package_entries :
      Context_name.t -> Package.Name.t -> Install.Entry.Sourced.Unexpanded.t list Memo.t
  ; library_entries :
      Context_name.t -> Library.Set.t -> Redirect.Set.t -> library_entries Memo.t
  }

let resolvers_fdecl : resolvers Fdecl.t = Fdecl.create Dyn.opaque
let set_resolvers resolvers = Fdecl.set resolvers_fdecl resolvers

let dir ~context ~key =
  Path.Build.L.relative (Install.Context.dir ~context) [ ".packages"; key ]
;;

(* Resolve the package set's install entries against the layout root: filter
   out [Source_tree] entries, expand the rest, and key each entry by its
   materialised path under the layout. Collisions (two packages installing
   to the same destination, which can only happen in _root sections) are
   reported as user errors naming the conflicting packages and entry. *)
let compute_entries context_name root { packages; libraries; redirects } =
  let overlapping items package =
    Library.Set.to_list items
    |> List.filter ~f:(fun item -> Package.Name.Set.mem packages (package item))
  in
  let overlapping_libraries = overlapping libraries Library.package
  and overlapping_redirects = overlapping redirects Redirect.package in
  if List.is_non_empty overlapping_libraries || List.is_non_empty overlapping_redirects
  then
    Code_error.raise
      "Install layout request contains support metadata owned by explicit packages"
      [ "packages", Package.Name.Set.to_dyn packages
      ; "libraries", Dyn.list Library.to_dyn overlapping_libraries
      ; "redirects", Dyn.list Redirect.to_dyn overlapping_redirects
      ];
  let open Memo.O in
  let { package_entries; library_entries } = Fdecl.get resolvers_fdecl in
  let resolve_entry (pkg, (s : Install.Entry.Sourced.Unexpanded.t)) =
    let install_paths =
      let roots = Install.Roots.opam_from_prefix Path.root ~relative:Path.relative in
      Install.Paths.make ~relative:Path.relative ~package:pkg ~roots
    in
    let entry = s.entry in
    match entry.kind with
    | Install.Entry.Unexpanded.Source_tree -> None
    | File | Directory ->
      let relative =
        Install.Entry.relative_installed_path entry ~paths:install_paths
        |> Path.as_in_source_tree_exn
      in
      let dst = Path.Build.append_source root relative in
      let expanded =
        Install.Entry.Expanded.set_src
          (Install.Entry.Unexpanded.expand entry)
          (Path.build entry.src)
      in
      Some
        ( dst
        , { package = pkg
          ; section = expanded.section
          ; dst = expanded.dst
          ; kind = expanded.kind
          ; source = Symlink expanded
          } )
  in
  let resolve_generated { package; section; dst; contents } =
    let install_paths =
      let roots = Install.Roots.opam_from_prefix Path.root ~relative:Path.relative in
      Install.Paths.make ~relative:Path.relative ~package ~roots
    in
    let relative =
      Install.Entry.Dst.install_path install_paths section dst
      |> Path.as_in_source_tree_exn
    in
    let path = Path.Build.append_source root relative in
    ( path
    , { package
      ; section
      ; dst
      ; kind = Install.Entry.Expanded.File
      ; source = Contents contents
      } )
  in
  let* package_entries =
    Package.Name.Set.to_list packages
    |> Memo.parallel_map ~f:(fun pkg ->
      let+ entries = package_entries context_name pkg in
      List.map entries ~f:(fun entry -> pkg, entry))
    >>| List.concat
  and* { install_entries = library_entries; generated_entries } =
    library_entries context_name libraries redirects
  in
  let entries =
    List.append package_entries library_entries |> List.filter_map ~f:resolve_entry
  in
  let entries = List.append entries (List.map generated_entries ~f:resolve_generated) in
  match Path.Build.Map.of_list entries with
  | Ok m -> Memo.return m
  | Error (_, entry_a, entry_b) ->
    User_error.raise
      ~hints:[ Pp.text "Rename one of the install entries." ]
      [ Pp.textf
          "%S and %S both install %S to section %s."
          (Package.Name.to_string entry_a.package)
          (Package.Name.to_string entry_b.package)
          (Install.Entry.Dst.to_string entry_a.dst)
          (Section.to_string entry_a.section)
      ; Pp.text
          "The lib_root, share_root, and libexec_root sections install directly to the \
           section root with no per-package subdirectory, so file names must be unique \
           across the set of packages a rule depends on."
      ]
;;

let entries =
  let memo =
    Memo.create
      "install-layout-entries"
      ~input:
        (module struct
          type t = Context_name.t * request

          let equal = Tuple.T2.equal Context_name.equal request_equal
          let hash = Tuple.T2.hash Context_name.hash request_hash
          let to_dyn = Tuple.T2.to_dyn Context_name.to_dyn request_to_dyn
        end)
      (fun (context, request) ->
         let key = Key.encode request in
         let root = dir ~context ~key in
         compute_entries context root request)
  in
  fun context request -> Memo.exec memo (context, request)
;;

let files context_name request =
  let open Memo.O in
  let+ entries = entries context_name request in
  Path.Build.Map.keys entries |> List.map ~f:Path.build
;;

let deps context_name request =
  let open Action_builder.O in
  let* files = Action_builder.of_memo (files context_name request) in
  Action_builder.paths files
;;

let root context_name request = dir ~context:context_name ~key:(Key.encode request)

let env_for_request context_name request =
  let open Action_builder.O in
  let+ () = deps context_name request in
  let layout_root = root context_name request in
  let roots = Install.Roots.opam_from_prefix layout_root ~relative:Path.Build.relative in
  Install.Roots.add_to_env roots Env.empty
;;

let env context_name packages libraries redirects =
  env_for_request context_name { packages; libraries; redirects }
;;

let make_dispatch ~dir ~directory_targets subdirs f =
  let rules = Rules.collect_unit f in
  Build_config.Gen_rules.make
    ~build_dir_only_sub_dirs:
      (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir subdirs)
    ~directory_targets
    rules
;;

let gen_rules context_name ~dir rest =
  let open Memo.O in
  match rest with
  | [] ->
    Memo.return
    @@ make_dispatch
         ~dir
         ~directory_targets:Path.Build.Map.empty
         Subdir_set.all
         (fun () -> Memo.return ())
  | [ key ] ->
    (match Key.decode key with
     | None -> Memo.return Build_config.Gen_rules.no_rules
     | Some request ->
       let+ entries = entries context_name request in
       let directory_targets =
         Path.Build.Map.filter_map entries ~f:(fun entry ->
           match (entry.kind : Install.Entry.Expanded.kind) with
           | File -> None
           | Directory -> Some Loc.none)
       in
       make_dispatch ~dir ~directory_targets Subdir_set.empty (fun () ->
         Path.Build.Map.to_seq entries
         |> Memo.parallel_iter_seq ~f:(fun (dst, { kind; source; _ }) ->
           let { Action_builder.With_targets.build; targets } =
             match source, (kind : Install.Entry.Expanded.kind) with
             | Symlink { src; _ }, File -> Action_builder.symlink ~src ~dst
             | Symlink { src; _ }, Directory -> Action_builder.symlink_dir ~src ~dst
             | Contents contents, File -> Action_builder.write_file_dyn dst contents
             | Contents _, Directory ->
               Code_error.raise "Generated install layout entry is a directory" []
           in
           Rules.Produce.rule (Rule.make ~info:(Rule.Info.of_loc_opt None) ~targets build))))
  | _ :: _ :: _ ->
    Memo.return
    @@ Build_config.Gen_rules.redirect_to_parent Build_config.Gen_rules.Rules.empty
;;

module For_rocq_only = struct
  (* Rocq puts the layout's [lib] dir on [OCAMLPATH], where findlib walks
     eagerly. For a race-free, deterministic walk, every entry findlib could
     see must be a declared dep. In the opam layout, both {!Section.Lib} and
     {!Section.Libexec} are under [lib/<package>]; Dune uses Libexec for native
     OCaml plugins so that their executable bit is preserved. Bulk {!env}
     doesn't work: it also pulls in {!Section.Lib_root} entries, which include
     Rocq theory [.vo] files under
     [lib/coq/user-contrib/...]; in the same-package theory-plus-plugin case,
     that creates a build cycle (the theory rule depending on its own output
     via the layout symlink). Filtering to {!Section.Lib} and
     {!Section.Libexec} excludes root-section content (theory output) while
     keeping METAs, .cmi, .cmxs etc. — all upstream of theory compilation. *)
  let lib_root context_name packages =
    let open Action_builder.O in
    let request =
      { packages; libraries = Library.Set.empty; redirects = Redirect.Set.empty }
    in
    let* lib_paths =
      Action_builder.of_memo (entries context_name request)
      >>| Path.Build.Map.foldi ~init:[] ~f:(fun dst (entry : materialized_entry) acc ->
        match (entry.section : Section.t) with
        | Lib | Libexec -> Path.build dst :: acc
        | _ -> acc)
    in
    let+ () = Action_builder.paths lib_paths in
    let layout_root = root context_name request in
    (Install.Roots.opam_from_prefix layout_root ~relative:Path.Build.relative).lib_root
  ;;
end
