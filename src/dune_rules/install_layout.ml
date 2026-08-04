open Import

module Key : sig
  val encode : Package.Name.Set.t -> string
  val decode : string -> Package.Name.Set.t option
end = struct
  let reverse_table : (Digest.t, Package.Name.Set.t) Table.t =
    Table.create (module Digest) 128
  ;;

  let encode packages =
    let sorted = Package.Name.Set.to_list packages in
    let y = Digest.repr Repr.(list Package.Name.repr) sorted in
    (match Table.find reverse_table y with
     | None -> Table.set reverse_table y packages
     | Some packages' ->
       if not (Package.Name.Set.equal packages packages')
       then
         Code_error.raise
           "Hash collision between sets of packages"
           [ "cached", Package.Name.Set.to_dyn packages'
           ; "new", Package.Name.Set.to_dyn packages
           ]);
    Digest.to_string y
  ;;

  let decode s =
    match Digest.from_hex s with
    | None -> None
    | Some digest -> Table.find reverse_table digest
  ;;
end

let entry_resolver_fdecl
  : (Context_name.t -> Package.Name.t -> Install.Entry.Sourced.Unexpanded.t list Memo.t)
      Fdecl.t
  =
  Fdecl.create Dyn.opaque
;;

let set_entry_resolver f = Fdecl.set entry_resolver_fdecl f

let dir ~context ~key =
  Path.Build.L.relative (Install.Context.dir ~context) [ ".packages"; key ]
;;

let rocq_dir ~context ~key =
  Path.Build.L.relative (Install.Context.dir ~context) [ ".packages-rocq"; key ]
;;

(* Resolve the package set's install entries against the layout root: filter
   out [Source_tree] entries, expand the rest, and key each entry by its
   materialised path under the layout. Collisions (two packages installing
   to the same destination, which can only happen in _root sections) are
   reported as user errors naming the conflicting packages and entry. *)
let hidden_from_rocq_plugin_layout (entry : Install.Entry.Unexpanded.t) =
  let dst = Install.Entry.Dst.to_string entry.dst in
  let components = String.split dst ~on:'/' in
  Section.equal entry.section Lib_root || List.mem components "rocq.d" ~equal:String.equal
;;

let compute_entries ?(filter_entry = Fun.const true) context_name root packages =
  let open Memo.O in
  let get_entries = Fdecl.get entry_resolver_fdecl in
  Package.Name.Set.to_list packages
  |> Memo.parallel_map ~f:(fun pkg ->
    let install_paths =
      let roots = Install.Roots.opam_from_prefix Path.root ~relative:Path.relative in
      Install.Paths.make ~relative:Path.relative ~package:pkg ~roots
    in
    let+ entries = get_entries context_name pkg in
    List.filter_map entries ~f:(fun (s : Install.Entry.Sourced.Unexpanded.t) ->
      let entry = s.entry in
      match entry.kind with
      | Install.Entry.Unexpanded.Source_tree -> None
      | File | Directory ->
        if not (filter_entry entry)
        then None
        else (
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
          Some (dst, (pkg, expanded)))))
  >>| List.concat
  >>| Path.Build.Map.of_list
  >>| function
  | Ok m -> Path.Build.Map.map m ~f:snd
  | Error (_, (pkg_a, entry_a), (pkg_b, _)) ->
    User_error.raise
      ~hints:[ Pp.text "Rename one of the install entries." ]
      [ Pp.textf
          "%S and %S both install %S to section %s."
          (Package.Name.to_string pkg_a)
          (Package.Name.to_string pkg_b)
          (Install.Entry.Dst.to_string entry_a.dst)
          (Section.to_string entry_a.section)
      ; Pp.text
          "The lib_root, share_root, and libexec_root sections install directly to the \
           section root with no per-package subdirectory, so file names must be unique \
           across the set of packages a rule depends on."
      ]
;;

let entries =
  let set_hash s = List.hash Package.Name.hash (Package.Name.Set.to_list s) in
  let memo =
    Memo.create
      "install-layout-entries"
      ~input:
        (module struct
          type t = Context_name.t * Package.Name.Set.t

          let equal = Tuple.T2.equal Context_name.equal Package.Name.Set.equal
          let hash = Tuple.T2.hash Context_name.hash set_hash
          let to_dyn = Tuple.T2.to_dyn Context_name.to_dyn Package.Name.Set.to_dyn
        end)
      (fun (context, packages) ->
         let key = Key.encode packages in
         let root = dir ~context ~key in
         compute_entries context root packages)
  in
  fun context packages -> Memo.exec memo (context, packages)
;;

let files context_name packages =
  let open Memo.O in
  let+ entries = entries context_name packages in
  Path.Build.Map.keys entries |> List.map ~f:Path.build
;;

let rocq_entries =
  let set_hash s = List.hash Package.Name.hash (Package.Name.Set.to_list s) in
  let memo =
    Memo.create
      "install-layout-rocq-entries"
      ~input:
        (module struct
          type t = Context_name.t * Package.Name.Set.t

          let equal = Tuple.T2.equal Context_name.equal Package.Name.Set.equal
          let hash = Tuple.T2.hash Context_name.hash set_hash
          let to_dyn = Tuple.T2.to_dyn Context_name.to_dyn Package.Name.Set.to_dyn
        end)
      (fun (context, packages) ->
         let key = Key.encode packages in
         let root = rocq_dir ~context ~key in
         let filter_entry entry = not (hidden_from_rocq_plugin_layout entry) in
         compute_entries ~filter_entry context root packages)
  in
  fun context packages -> Memo.exec memo (context, packages)
;;

let deps context_name packages =
  let open Action_builder.O in
  let* files = Action_builder.of_memo (files context_name packages) in
  Action_builder.paths files
;;

let root context_name packages = dir ~context:context_name ~key:(Key.encode packages)

let env context_name packages =
  let open Action_builder.O in
  let+ () = deps context_name packages in
  let layout_root = root context_name packages in
  let roots = Install.Roots.opam_from_prefix layout_root ~relative:Path.Build.relative in
  Install.Roots.add_to_env roots Env.empty
;;

let make_dispatch ~dir ~directory_targets subdirs f =
  let rules = Rules.collect_unit f in
  Build_config.Gen_rules.make
    ~build_dir_only_sub_dirs:
      (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir subdirs)
    ~directory_targets
    rules
;;

let gen_rules_for_entries context_name ~dir rest ~entries:entries_of =
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
     | Some packages ->
       let+ entries = entries_of context_name packages in
       let directory_targets =
         let f (entry : Path.t Install.Entry.Expanded.t) =
           match (entry.kind : Install.Entry.Expanded.kind) with
           | File -> None
           | Directory -> Some Loc.none
         in
         Path.Build.Map.filter_map entries ~f
       in
       make_dispatch ~dir ~directory_targets Subdir_set.empty (fun () ->
         Path.Build.Map.to_seq entries
         |> Memo.parallel_iter_seq ~f:(fun (dst, { Install.Entry.kind; src; _ }) ->
           let { Action_builder.With_targets.build; targets } =
             match (kind : Install.Entry.Expanded.kind) with
             | File -> Action_builder.symlink ~src ~dst
             | Directory -> Action_builder.symlink_dir ~src ~dst
           in
           Rules.Produce.rule (Rule.make ~info:(Rule.Info.of_loc_opt None) ~targets build))))
  | _ :: _ :: _ ->
    Memo.return
    @@ Build_config.Gen_rules.redirect_to_parent Build_config.Gen_rules.Rules.empty
;;

let gen_rules context_name ~dir rest =
  gen_rules_for_entries context_name ~dir rest ~entries
;;

module For_rocq_only = struct
  (* Rocq puts this layout's [lib] dir on [OCAMLPATH], where findlib may walk
     package directories eagerly. The normal package layout can also contain
     Rocq theory outputs under [lib/<package>/rocq.d] or
     [lib/coq/user-contrib/...]. If those theory outputs are visible while
     compiling the same theory, rocqdep can report a dependency cycle through
     the theory's own installed [.glob] symlink. Use a Rocq-specific filtered
     layout: METAs and OCaml plugin files are visible, but [lib_root] entries
     and [rocq.d] theory outputs are not. *)
  let lib_root context_name packages =
    let open Action_builder.O in
    let* lib_paths =
      Action_builder.of_memo (rocq_entries context_name packages)
      >>| Path.Build.Map.keys
      >>| List.map ~f:Path.build
    in
    let+ () = Action_builder.paths lib_paths in
    let key = Key.encode packages in
    let layout_root = rocq_dir ~context:context_name ~key in
    (Install.Roots.opam_from_prefix layout_root ~relative:Path.Build.relative).lib_root
  ;;

  let gen_rules context_name ~dir rest =
    gen_rules_for_entries context_name ~dir rest ~entries:rocq_entries
  ;;
end
