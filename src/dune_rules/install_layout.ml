open Import

module Key : sig
  type encoded = Digest.t

  module Decoded : sig
    type t = private { packages : Package.Name.t list }

    val of_packages : Package.Name.t list -> t
  end

  val encode : Decoded.t -> encoded
  val decode : encoded -> Decoded.t
end = struct
  type encoded = Digest.t

  module Decoded = struct
    type t = { packages : Package.Name.t list }

    let equal x y = List.equal Package.Name.equal x.packages y.packages

    let to_string { packages } =
      String.enumerate_and (List.map packages ~f:Package.Name.to_string)
    ;;

    let of_packages packages =
      let packages = List.sort_uniq packages ~compare:Package.Name.compare in
      { packages }
    ;;
  end

  let reverse_table : (Digest.t, Decoded.t) Table.t = Table.create (module Digest) 128

  let encode ({ Decoded.packages } as x) =
    let y = Digest.repr Repr.(list Package.Name.repr) packages in
    match Table.find reverse_table y with
    | None ->
      Table.set reverse_table y x;
      y
    | Some x' ->
      if Decoded.equal x x'
      then y
      else
        Code_error.raise
          "Hash collision between sets of packages"
          [ "cached", Dyn.string (Decoded.to_string x')
          ; "new", Dyn.string (Decoded.to_string x)
          ]
  ;;

  let decode y =
    match Table.find reverse_table y with
    | Some x -> x
    | None ->
      Code_error.raise
        "unknown package set digest (encode was not called first)"
        [ "digest", Dyn.string (Digest.to_string y) ]
  ;;
end

type entry =
  { src : Path.t
  ; relative : Path.Source.t
  }

let entry_resolver_fdecl : (Context_name.t -> Package.Name.t -> entry list Memo.t) Fdecl.t
  =
  Fdecl.create Dyn.opaque
;;

let set_entry_resolver f = Fdecl.set entry_resolver_fdecl f

let dir ~context ~key =
  Path.Build.L.relative (Install.Context.dir ~context) [ ".packages"; key ]
;;

let compute_entries context_name root packages =
  let open Memo.O in
  let get_entries = Fdecl.get entry_resolver_fdecl in
  Memo.parallel_map packages ~f:(fun pkg ->
    let+ entries = get_entries context_name pkg in
    List.map entries ~f:(fun { src; relative } ->
      let dst = Path.Build.append_source root relative in
      src, dst, relative))
  >>| List.rev_concat
;;

let entries_memo =
  Memo.create
    "install-layout-entries"
    ~input:
      (module struct
        type t = Context_name.t * Digest.t

        let equal (a1, b1) (a2, b2) = Context_name.equal a1 a2 && Digest.equal b1 b2
        let hash (a, b) = Tuple.T2.hash Context_name.hash Digest.hash (a, b)
        let to_dyn (a, b) = Dyn.pair Context_name.to_dyn Digest.to_dyn (a, b)
      end)
    (fun (context_name, digest) ->
       let key = Digest.to_string digest in
       let root = dir ~context:context_name ~key in
       let { Key.Decoded.packages } = Key.decode digest in
       compute_entries context_name root packages)
;;

let encode_packages packages = Key.Decoded.of_packages packages |> Key.encode

let files context_name packages =
  let open Memo.O in
  let digest = encode_packages packages in
  let+ entries = Memo.exec entries_memo (context_name, digest) in
  List.map entries ~f:(fun (_, dst, _) -> Path.build dst)
;;

let lib_root context_name packages =
  let key = Digest.to_string (encode_packages packages) in
  Path.Build.relative (dir ~context:context_name ~key) "lib"
;;
