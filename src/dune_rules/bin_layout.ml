open Import

module Entry = struct
  module T = struct
    type t =
      { lookup_name : string
      ; install_name : Filename.t
      }

    let repr =
      Repr.record
        "bin-layout-entry"
        [ Repr.field "lookup_name" Repr.string ~get:(fun t -> t.lookup_name)
        ; Repr.field "install_name" Filename.repr ~get:(fun t -> t.install_name)
        ]
    ;;

    (* CR-someday Alizter: [Filename.t] is structurally a string and uses
       [String.compare], but [Filename.repr] exposes it as a view, which
       [Repr.Poly] conservatively rejects. Make its representation structural
       so this comparison can be derived. *)
    let compare x y =
      let open Ordering.O in
      let= () = String.compare x.lookup_name y.lookup_name in
      Filename.compare x.install_name y.install_name
    ;;
  end

  include T
  include Repr.Make (T)

  include Comparable.Make (struct
      include T
      include Repr.Make (T)
    end)
end

module Key : sig
  val encode : Entry.Set.t -> string
  val decode : string -> Entry.Set.t
end = struct
  (* [decode] is only called on digests produced by [encode] in the same
     process (deps are evaluated before paths under the layout dir are
     resolved), so the entry will always be present. Same pattern as
     [Ppx_driver.Key]. *)
  let reverse_table : (Digest.t, Entry.Set.t) Table.t = Table.create (module Digest) 128

  let encode entries =
    let y = Digest.repr Repr.(list Entry.repr) (Entry.Set.to_list entries) in
    Table.set reverse_table y entries;
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

let create context ~dir bin_names =
  let open Memo.O in
  let* artifacts =
    let* sctx = Super_context.find_exn context in
    Artifacts_db.get (Super_context.context sctx)
  in
  let+ entries =
    Memo.List.filter_map bin_names ~f:(fun lookup_name ->
      Artifacts.local_binary_install_name artifacts ~dir lookup_name
      >>| Option.map ~f:(fun install_name -> { Entry.lookup_name; install_name }))
  in
  match entries with
  | [] -> None
  | _ :: _ ->
    let entries = Entry.Set.of_list entries in
    let layout_dir = Key.encode entries |> layout_dir ~context in
    Some
      ( layout_dir
      , Entry.Set.to_list_map entries ~f:(fun { Entry.lookup_name = _; install_name } ->
          Path.build (Path.Build.relative_fname layout_dir install_name)) )
;;

let symlink_rules_for_key context_name ~dir key =
  let entries = Key.decode key in
  let open Memo.O in
  let* artifacts =
    let* sctx = Super_context.find_exn context_name in
    Artifacts_db.get (Super_context.context sctx)
  in
  Entry.Set.to_list entries
  |> Memo.parallel_iter ~f:(fun ({ Entry.lookup_name; install_name } as entry) ->
    let* prog =
      Artifacts.binary artifacts ~where:Original_path ~dir ~loc:None lookup_name
    in
    match prog with
    | Error _ ->
      Code_error.raise
        "Bin_layout.gen_rules: binary not found"
        [ "entry", Entry.to_dyn entry; "context", Context_name.to_dyn context_name ]
    | Ok src ->
      let { Action_builder.With_targets.build; targets } =
        Action_builder.symlink ~src ~dst:(Path.Build.relative_fname dir install_name)
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
  | [] -> Memo.return (make_dispatch ~dir Subdir_set.all (fun () -> Memo.return ()))
  | [ key ] ->
    Memo.return
      (make_dispatch ~dir Subdir_set.empty (fun () ->
         symlink_rules_for_key context_name ~dir key))
  | _ :: _ :: _ ->
    Memo.return
      (Build_config.Gen_rules.redirect_to_parent Build_config.Gen_rules.Rules.empty)
;;
