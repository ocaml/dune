open Import

module Key : sig
  val encode : Filename.Set.t -> string
  val decode : string -> Filename.Set.t
end = struct
  (* [decode] is only called on digests produced by [encode] in the same
     process (deps are evaluated before paths under the layout dir are
     resolved), so the entry will always be present. Same pattern as
     [Ppx_driver.Key]. *)
  let reverse_table : (Digest.t, Filename.Set.t) Table.t =
    Table.create (module Digest) 128
  ;;

  let encode bins =
    let y =
      Digest.repr
        Repr.(list string)
        (Filename.Set.to_list bins |> List.map ~f:Filename.to_string)
    in
    Table.set reverse_table y bins;
    Digest.to_string y
  ;;

  let of_string s =
    match Digest.from_hex s with
    | Some digest -> digest
    | None -> Code_error.raise "invalid .binaries key" [ "key", Dyn.string s ]
  ;;

  let decode s =
    match Table.find reverse_table (of_string s) with
    | Some x -> x
    | None ->
      Code_error.raise
        "unknown .binaries digest (encode was not called first)"
        [ "digest", Dyn.string s ]
  ;;
end

let layout_dir ~context key =
  Path.Build.L.relative (Install.Context.dir ~context) [ ".binaries"; key ]
;;

let create context bins =
  let bins = List.map bins ~f:Filename.of_string_exn |> Filename.Set.of_list in
  let dir = Key.encode bins |> layout_dir ~context in
  ( dir
  , Filename.Set.to_list_map bins ~f:(fun name ->
      Path.build (Path.Build.relative_fname dir name)) )
;;

let symlink_rules_for_key context_name ~dir key =
  let bins = Key.decode key in
  let open Memo.O in
  let* artifacts =
    let* sctx = Super_context.find_exn context_name in
    Artifacts_db.get (Super_context.context sctx)
  in
  Filename.Set.to_list bins
  |> Memo.parallel_iter ~f:(fun name ->
    let* prog =
      Artifacts.binary
        artifacts
        ~where:Original_path
        ~dir
        ~loc:None
        (Filename.to_string name)
    in
    match prog with
    | Error _ ->
      Code_error.raise
        "Bin_layout.gen_rules: binary not found"
        [ "name", Filename.to_dyn name; "context", Context_name.to_dyn context_name ]
    | Ok src ->
      let { Action_builder.With_targets.build; targets } =
        Action_builder.symlink ~src ~dst:(Path.Build.relative_fname dir name)
      in
      Rules.Produce.rule (Rule.make ~targets build))
;;

let make_dispatch ~dir subdirs f =
  let rules = Rules.collect_unit f in
  Build_config.Gen_rules.make
    ~build_dir_only_sub_dirs:
      (Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir subdirs)
    rules
;;

let gen_rules context_name ~dir rest =
  match rest with
  | [] -> make_dispatch ~dir Subdir_set.all (fun () -> Memo.return ())
  | [ key ] ->
    make_dispatch ~dir Subdir_set.empty (fun () ->
      symlink_rules_for_key context_name ~dir key)
  | _ :: _ :: _ ->
    Build_config.Gen_rules.redirect_to_parent Build_config.Gen_rules.Rules.empty
;;
