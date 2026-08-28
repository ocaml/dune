open Import

module Entry = struct
  module T = struct
    type t =
      { original_path : Path.t
      ; install_name : Filename.t
      }

    let repr =
      Repr.record
        "bin-layout-entry"
        [ Repr.field "original_path" Path.repr ~get:(fun t -> t.original_path)
        ; Repr.field "install_name" Filename.repr ~get:(fun t -> t.install_name)
        ]
    ;;

    (* CR-someday Alizter: [Filename.t] is structurally a string and uses
       [String.compare], but [Filename.repr] exposes it as a view, which
       [Repr.Poly] conservatively rejects. The same is true for the cases in
       [Path.repr] too. Making their representation structural would allow this
       comparison to be derived. *)
    let compare x y =
      match Path.compare x.original_path y.original_path with
      | Eq -> Filename.compare x.install_name y.install_name
      | (Lt | Gt) as ordering -> ordering
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

let create context ~artifacts ~dir bin_names =
  let open Memo.O in
  let+ entries =
    Memo.List.filter_map bin_names ~f:(fun lookup_name ->
      Artifacts.local_binary artifacts ~dir lookup_name
      >>| Option.map ~f:(fun (original_path, install_name) ->
        { Entry.install_name; original_path }))
  in
  match entries with
  | [] -> None
  | _ :: _ ->
    let entries = Entry.Set.of_list entries in
    let layout_dir = Key.encode entries |> layout_dir ~context in
    Some
      ( layout_dir
      , Entry.Set.to_list_map entries ~f:(fun { Entry.install_name; _ } ->
          Path.build (Path.Build.relative_fname layout_dir install_name)) )
;;

let symlink_rules_for_key ~dir key =
  let entries = Key.decode key in
  Entry.Set.to_list entries
  |> Memo.parallel_iter ~f:(fun { Entry.install_name; original_path; _ } ->
    let { Action_builder.With_targets.build; targets } =
      Action_builder.symlink
        ~src:original_path
        ~dst:(Path.Build.relative_fname dir install_name)
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

let gen_rules ~dir rest =
  match rest with
  | [] -> make_dispatch ~dir Subdir_set.all (fun () -> Memo.return ())
  | [ key ] ->
    make_dispatch ~dir Subdir_set.empty (fun () -> symlink_rules_for_key ~dir key)
  | _ :: _ :: _ ->
    Build_config.Gen_rules.redirect_to_parent Build_config.Gen_rules.Rules.empty
;;
