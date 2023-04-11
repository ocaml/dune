open Import
open Memo.O
module Library = Dune_file.Library

module All_sctx = struct
  let all_sctx_to_entries (scontexts : Super_context.t Context_name.Map.t) :
      Install.Entry.Sourced.t list Package.Name.Map.t Context_name.Map.t Memo.t
      =
    let open Memo.O in
    let ctxs = Context_name.Map.to_list scontexts in
    let* pairs =
      Memo.sequential_map ctxs ~f:(fun (ctxn, sctx) ->
          let* packages =
            Install_rules.Stanzas_to_entries.stanzas_to_entries sctx
          in
          Memo.return (ctxn, packages))
    in
    let maps = Context_name.Map.of_list_exn pairs in
    Memo.return maps
end

module Build_path_prefix_map_dyn = struct
  open Build_path_prefix_map
  open Dyn

  let pair_to_dyn = function
    | { target; source } ->
      record [ ("target", string target); ("source", string source) ]

  let to_dyn map = list (option pair_to_dyn) map

  let bppm_compare l r =
    Option.compare
      (fun { Build_path_prefix_map.source = left; _ } { source = right; _ } ->
        String.compare left right)
      l r
end

module Abbrev_map = struct
  module Bppm = Build_path_prefix_map

  type leaf =
    { leaf_name : string
    ; leaf_targets : string list
    }

  let leaf_to_dyn { leaf_name; leaf_targets } =
    let open Dyn in
    record
      [ ("leaf_name", string leaf_name)
      ; ("leaf_targets", list string leaf_targets)
      ]

  type dir =
    { dir_name : string
    ; parent : dir option
    ; mutable leaves : leaf String.Map.t
    ; mutable children : dir String.Map.t
    }

  let rec dir_to_path (d : dir) =
    match d.parent with
    | None -> ""
    | Some parent -> Filename.concat (dir_to_path parent) d.dir_name

  let rec dir_to_dyn d =
    let open Dyn in
    record
      [ ("dir_name", string (dir_to_path d))
      ; ( "parent"
        , match d.parent with
          | None -> string "/"
          | Some p -> string (dir_to_path p) )
      ; ("leaves", String.Map.to_dyn leaf_to_dyn d.leaves)
      ; ("children", String.Map.to_dyn dir_to_dyn d.children)
      ]

  let root_to_dyn root = dir_to_dyn root

  let find_make_dir root dir_path =
    let parts = String.split dir_path ~on:'/' in
    let parts =
      match parts with
      | hd :: tl when hd = "" -> "/" :: tl
      | _ -> parts
    in
    List.fold_left parts ~init:root ~f:(fun parent part ->
        match String.Map.find parent.children part with
        | Some d -> d
        | None ->
          let new_dir =
            { dir_name = part
            ; parent = Some parent
            ; leaves = String.Map.empty
            ; children = String.Map.empty
            }
          in
          parent.children <- String.Map.add_exn parent.children part new_dir;
          new_dir)

  let build_tree root map : unit =
    List.iter map ~f:(fun opt_pair ->
        match opt_pair with
        | None -> ()
        | Some { Bppm.source; target } ->
          let src_dirname = Filename.dirname source in
          let leaf_name = Filename.basename source in
          let dir = find_make_dir root src_dirname in
          dir.leaves <-
            String.Map.update dir.leaves leaf_name ~f:(function
              | None -> Some { leaf_name; leaf_targets = [ target ] }
              | Some { leaf_name; leaf_targets } ->
                Some { leaf_name; leaf_targets = target :: leaf_targets }))

  let rec abbreviate_tree (d : dir) =
    (* The list of children is modified by the children
       so get a copy first. We are doing a post-order traversal *)
    let child_list = String.Map.to_list d.children in
    List.iter child_list ~f:(fun (_child_name, child_dir) ->
        abbreviate_tree child_dir);

    match d.parent with
    | None -> () (* Do nothing with root for the following *)
    | Some parent -> (
      match parent.parent with
      | None -> () (* Parent is root, do not make leaves in it. *)
      | Some _grandparent ->
        (* Do all promotable leaves go to the same target directory? *)
        let target_dir = ref "" in
        let max_count = ref 0 in
        let is_promotable = function
          | { leaf_name; leaf_targets = [ leaf_target ] } -> (
            let new_target = Filename.dirname leaf_target in
            Filename.basename leaf_target = leaf_name
            &&
            match String.Map.find parent.leaves d.dir_name with
            | None -> true
            | Some { leaf_name = _; leaf_targets = [ target2 ] } ->
              target2 = new_target
            | Some _ -> false)
          | _ -> false
        in
        let _target_dirs =
          String.Map.fold d.leaves ~init:String.Map.empty ~f:(fun aleaf acc ->
              match aleaf with
              | { leaf_name = _; leaf_targets = [ leaf_target ] } ->
                let new_target, promotable =
                  if is_promotable aleaf then
                    (* it is a promotable leaf. *)
                    (Filename.dirname leaf_target, 1)
                  else (leaf_target, 0)
                in

                String.Map.update acc new_target ~f:(fun arg ->
                    let result =
                      match arg with
                      | None -> Some promotable
                      | Some count -> Some (count + promotable)
                    in
                    (match result with
                    | None -> ()
                    | Some count ->
                      if count > !max_count then (
                        max_count := count;
                        target_dir := new_target));
                    result)
              | _ -> acc)
        in

        if !max_count >= 1 then (
          (* We can reduce the number of mapping by promoting the most
             promotable leaf collection. *)

          (* Get the names of the leaves to remove. *)
          let to_remove =
            String.Map.fold d.leaves ~init:[] ~f:(fun aleaf acc ->
                match aleaf with
                | { leaf_name; leaf_targets = [ leaf_target ] } ->
                  let tgt_dir = Filename.dirname leaf_target in
                  if tgt_dir = !target_dir && is_promotable aleaf then
                    leaf_name :: acc
                  else acc
                | _ -> acc)
          in
          List.iter to_remove ~f:(fun name ->
              d.leaves <- String.Map.remove d.leaves name);
          let new_leaf =
            { leaf_name = d.dir_name; leaf_targets = [ !target_dir ] }
          in
          match String.Map.find parent.leaves d.dir_name with
          | None ->
            parent.leaves <-
              String.Map.add_exn parent.leaves d.dir_name new_leaf;
            ()
          | Some { leaf_name = _leaf_name; leaf_targets } -> (
            match (leaf_targets, new_leaf.leaf_targets) with
            | [ targ1 ], [ targ2 ] when targ1 = targ2 -> ()
            | [ targ1 ], [ targ2 ] ->
              Log.info
                [ Pp.textf
                    "Missed promoting %s, new target (%s) <> existing (%s)"
                    (dir_to_path d) targ1 targ2
                ]
            | _, _ -> ()));

        let child_card = String.Map.cardinal d.children in
        let new_leaf_card = String.Map.cardinal d.leaves in
        if new_leaf_card = 0 && child_card = 0 then
          (* No longer any children or leaves. Remove as child of parent. *)
          parent.children <- String.Map.remove parent.children d.dir_name)

  let rec expand_dir (d : dir) path acc1 : Bppm.map =
    let acc2 =
      String.Map.foldi d.children ~init:acc1
        ~f:(fun (child_name : string) (child_dir : dir) acc3 ->
          let child_name = if child_name = "." then "" else child_name in
          let child_path = Filename.concat path child_name in
          expand_dir child_dir child_path acc3)
    in
    String.Map.foldi d.leaves ~init:acc2
      ~f:(fun (_leaf_name : string) { leaf_name; leaf_targets } acc4 ->
        let leaf_name = if leaf_name = "." then "" else leaf_name in
        let leaf_path = Filename.concat path leaf_name in
        List.fold_left leaf_targets ~init:acc4 ~f:(fun acc5 leaf_target ->
            Some { Bppm.source = leaf_path; target = leaf_target } :: acc5))

  let expand_tree root = expand_dir root "" []

  let abbreviate_map (map : Build_path_prefix_map.map) =
    let root =
      { dir_name = "$root"
      ; parent = None
      ; leaves = String.Map.empty
      ; children = String.Map.empty
      }
    in
    build_tree root map;
    let tree_before_dyn = root_to_dyn root in
    abbreviate_tree root;
    let tree_after_dyn = root_to_dyn root in
    let expanded = expand_tree root in
    let sorted =
      List.sort expanded ~compare:Build_path_prefix_map_dyn.bppm_compare
    in
    (sorted, tree_before_dyn, tree_after_dyn)
end

module Inverse_map = struct
  module Bppm = Build_path_prefix_map

  let invert_map (map : Bppm.map) =
    let inv_map =
      List.fold_left map ~init:[] ~f:(fun acc item ->
          Option.map item ~f:(fun { Bppm.source; target } ->
              { Bppm.source = target; target = source })
          :: acc)
    in
    let inv_map_sorted =
      List.sort inv_map ~compare:Build_path_prefix_map_dyn.bppm_compare
    in
    inv_map_sorted

  let invert_maps (abbrev_maps : Build_path_prefix_map.map String.Map.t) =
    String.Map.foldi abbrev_maps ~init:String.Map.empty
      ~f:(fun name abbrev_map acc ->
        let inverted_map = invert_map abbrev_map in
        String.Map.add_exn acc name inverted_map)
end

module Build_map = struct
  open Dune_engine
  module Bppm = Build_path_prefix_map

  let debug = ref false

  type t = Build_path_prefix_map.map String.Map.t

  let to_dyn (cmap : t) : Dyn.t =
    let open Dyn in
    record
      (String.Map.foldi cmap ~init:[] ~f:(fun ctx_name map acc ->
           (ctx_name, Build_path_prefix_map_dyn.to_dyn map) :: acc))

  let build_package_map (context : Context.t) pkg
      (entries_in : Install.Entry.Sourced.t list) : Build_path_prefix_map.map =
    let prefix_path = Path.of_string "/install_root" in
    let build_dir = Path.Build.to_string context.build_dir ^ "/" in
    List.filter_map entries_in ~f:(fun (es : Install.Entry.Sourced.t) ->
        let (e : Path.Build.t Install.Entry.t) = es.entry in
        let source = Path.Build.to_string e.src in
        if not (String.is_suffix ~suffix:".ml" source) then None
        else
          let source = String.drop_prefix_if_exists ~prefix:build_dir source in
          let target = Install.Dst.to_string e.dst in
          let target_path =
            Install.Section.Paths.get_location_from_prefix prefix_path e.section
              pkg
          in
          let target = Path.relative target_path target in
          let target = Path.to_string target in
          let source = String.drop_suffix_if_exists ~suffix:"/" source in
          let target = String.drop_suffix_if_exists ~suffix:"/" target in
          Some (Some { Build_path_prefix_map.source; target }))

  let build_context_map_full (sctx : Super_context.t) packages :
      Build_path_prefix_map.map =
    let context = Super_context.context sctx in
    let map =
      List.concat
        (Package.Name.Map.foldi packages ~init:[] ~f:(fun pkg entries acc ->
             build_package_map context pkg entries :: acc))
    in
    let install_root =
      Path.to_string context.ocaml.bin_dir |> Filename.dirname
    in
    let install_pair =
      Some
        { Build_path_prefix_map.target = "/install_root"
        ; source = install_root
        }
    in
    let build_dir_pair =
      Some { Build_path_prefix_map.target = "/workspace_root"; source = "" }
    in
    let map = build_dir_pair :: install_pair :: map in
    let map = List.sort map ~compare:Build_path_prefix_map_dyn.bppm_compare in
    map

  let all_maps_builder (scontexts : Super_context.t Context_name.Map.t)
      (ctx_to_pkgs :
        Install.Entry.Sourced.t list Package.Name.Map.t Context_name.Map.t)
      (single :
           Super_context.t
        -> Install.Entry.Sourced.t list Package.Name.Map.t
        -> Bppm.map) : Bppm.map String.Map.t =
    Context_name.Map.foldi scontexts ~init:String.Map.empty
      ~f:(fun ctxn sctx acc ->
        let pkgs = Context_name.Map.find_exn ctx_to_pkgs ctxn in
        let map = single sctx pkgs in
        let ctx_name = Context_name.to_string ctxn in
        String.Map.add_exn acc ctx_name map)

  let build_all_maps_full (scontexts : Super_context.t Context_name.Map.t)
      (ctx_to_pkgs :
        Install.Entry.Sourced.t list Package.Name.Map.t Context_name.Map.t) :
      Bppm.map String.Map.t =
    all_maps_builder scontexts ctx_to_pkgs build_context_map_full

  let build_context_map (sctx : Super_context.t) packages :
      Build_path_prefix_map.map =
    let full_map = build_context_map_full sctx packages in
    let map, _, _ = Abbrev_map.abbreviate_map full_map in
    map

  let build_all_maps (scontexts : Super_context.t Context_name.Map.t)
      (ctx_to_pkgs :
        Install.Entry.Sourced.t list Package.Name.Map.t Context_name.Map.t) :
      Bppm.map String.Map.t =
    all_maps_builder scontexts ctx_to_pkgs build_context_map

  let build_src_map_full (context : Context.t) (full_map : Bppm.map) =
    let src_dir = Sys.getcwd () in
    let build_dir =
      Filename.concat src_dir (Path.Build.to_string context.build_dir)
    in
    List.fold_left full_map ~init:([], []) ~f:(fun (accs, accn) opt_pair ->
        match opt_pair with
        | None -> (accs, None :: accn)
        | Some { Bppm.source; target } as pr ->
          if not (Filename.is_relative source) then (accs, pr :: accn)
          else
            let src_path = Filename.concat src_dir source in
            if source = "" then
              (* This is the root, put in both source and build dir. *)
              let bld_path = Filename.concat build_dir source in
              ( Some { Bppm.source = src_path; target } :: accs
              , Some { Bppm.source = bld_path; target } :: accn )
            else if Sys.file_exists src_path then
              (Some { Bppm.source = src_path; target } :: accs, accn)
            else
              let bld_path = Filename.concat build_dir source in
              (accs, Some { Bppm.source = bld_path; target } :: accn))

  let build_context_inverse_source_map (sctx : Super_context.t) packages :
      Bppm.map =
    let full_map = build_context_map_full sctx packages in
    let context = Super_context.context sctx in
    let src_map, non_src_map = build_src_map_full context full_map in
    let src_map, _, _ = Abbrev_map.abbreviate_map src_map in
    let src_map = Inverse_map.invert_map src_map in
    let non_src_map, _, _ = Abbrev_map.abbreviate_map non_src_map in
    let non_src_map = Inverse_map.invert_map non_src_map in
    let result = List.rev_append (List.rev non_src_map) src_map in
    result

  let build_inverse_source_maps (scontexts : Super_context.t Context_name.Map.t)
      (ctx_to_pkgs :
        Install.Entry.Sourced.t list Package.Name.Map.t Context_name.Map.t) :
      Bppm.map String.Map.t =
    all_maps_builder scontexts ctx_to_pkgs build_context_inverse_source_map

  let build_and_save_maps (scontexts : Super_context.t Context_name.Map.t) =
    let* sctxs_to_entries = All_sctx.all_sctx_to_entries scontexts in
    let maps = build_all_maps scontexts sctxs_to_entries in
    let deploy_maps = build_inverse_source_maps scontexts sctxs_to_entries in
    if !debug then
      Console.print
        [ Pp.text "{build_and_save_maps, {forward maps="
        ; Dyn.pp (to_dyn maps)
        ; Pp.text "}, {deploy_maps="
        ; Dyn.pp (to_dyn deploy_maps)
        ; Pp.text "}}"
        ];
    Dune_util.Build_path_prefix_map0.map_for_build_context := maps;
    Dune_util.Build_path_prefix_map0.map_for_deploy_context := deploy_maps;
    Memo.return ()
end
