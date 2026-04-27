open Import

module Key : sig
  type encoded = Digest.t

  module Decoded : sig
    type t = private { bins : string list }

    val of_bins : string list -> t
  end

  val encode : Decoded.t -> encoded
  val decode : encoded -> Decoded.t
end = struct
  type encoded = Digest.t

  module Decoded = struct
    type t = { bins : string list }

    let equal x y = List.equal String.equal x.bins y.bins
    let to_string { bins } = String.enumerate_and bins

    let of_bins bins =
      let bins = List.sort_uniq bins ~compare:String.compare in
      { bins }
    ;;
  end

  (* This mutable table is safe. [decode] is only called on digests produced
     by [encode] in the same process (deps are evaluated before paths under
     the layout dir are resolved), so the entry will always be present. An
     unknown digest indicates an invariant violation. Same pattern as
     [Ppx_driver.Key]. *)
  let reverse_table : (Digest.t, Decoded.t) Table.t = Table.create (module Digest) 128

  let encode ({ Decoded.bins } as x) =
    let y = Digest.repr Repr.(list string) bins in
    match Table.find reverse_table y with
    | None ->
      Table.set reverse_table y x;
      y
    | Some x' ->
      if Decoded.equal x x'
      then y
      else
        Code_error.raise
          "Hash collision between sets of binaries"
          [ "cached", Dyn.string (Decoded.to_string x')
          ; "new", Dyn.string (Decoded.to_string x)
          ]
  ;;

  let decode y =
    match Table.find reverse_table y with
    | Some x -> x
    | None ->
      Code_error.raise
        "unknown bin-layout digest (encode was not called first)"
        [ "digest", Dyn.string (Digest.to_string y) ]
  ;;
end

let layout_dir ~context ~key =
  Path.Build.relative (Install.Context.dir ~context) (".binaries/" ^ key)
;;

let create context bins =
  let decoded = Key.Decoded.of_bins bins in
  let digest = Key.encode decoded in
  let key = Digest.to_string digest in
  let dir = layout_dir ~context ~key in
  let { Key.Decoded.bins } = decoded in
  let files = List.map bins ~f:(fun name -> Path.build (Path.Build.relative dir name)) in
  Memo.return (dir, files)
;;

let gen_rules context_name ~dir key =
  match Digest.from_hex key with
  | None -> User_error.raise [ Pp.textf "invalid bin-layout key %S" key ]
  | Some digest ->
    let { Key.Decoded.bins } = Key.decode digest in
    let open Memo.O in
    let* artifacts =
      let* sctx = Super_context.find_exn context_name in
      Artifacts_db.get (Super_context.context sctx)
    in
    Memo.parallel_iter bins ~f:(fun name ->
      let* prog = Artifacts.binary artifacts ~where:Original_path ~dir ~loc:None name in
      match prog with
      | Error _ ->
        Code_error.raise
          "Bin_layout.gen_rules: binary not found"
          [ "name", Dyn.string name; "context", Context_name.to_dyn context_name ]
      | Ok src ->
        let dst = Path.Build.relative dir name in
        let { Action_builder.With_targets.build; targets } =
          Action_builder.symlink ~src ~dst
        in
        Rules.Produce.rule (Rule.make ~targets build))
;;
