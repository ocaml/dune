open Import
module Id = Id.Make ()

type repr = ..
type _ witness = ..

module type T = sig
  type t
  type _ witness += W : t witness

  val repr : t -> repr
  val id : Id.t
  val compare : t -> t -> Ordering.t
  val hash : t -> int
end

type t = E : 'a * Package_id.t option * (module T with type t = 'a) -> t

let package (E (_, p, _)) = p

module Key = struct
  type stanza = t
  type nonrec 'a t = t -> 'a option

  let get (t : _ t) (x : stanza) = t x
end

module type S = sig
  type stanza := t
  type t
  type repr += T of t

  val make_stanza : t -> Package_id.t option -> stanza
  val decode_stanza : t Decoder.t -> stanza list Decoder.t
  val decode_stanzas : t list Decoder.t -> stanza list Decoder.t
  val key : t Key.t
end

let repr (E (a, _id, (module T))) = T.repr a

module Make (S : sig
    type t

    val compare : t -> t -> Ordering.t
    val hash : t -> int
  end) =
struct
  type repr += T of S.t

  module T = struct
    include S

    let id = Id.gen ()
    let repr x = T x

    type _ witness += W : t witness
  end

  let make_stanza (a : S.t) pkg = E (a, pkg, (module T))

  let decode_stanza d =
    let open Decoder in
    let* mask = Decoder.get Package_mask.key in
    let+ d = d in
    let package =
      match mask with
      | Some (Forbidden_packages _) | None -> None
      | Some (Inside_package id) -> Some id
    in
    [ make_stanza d package ]
  ;;

  let decode_stanzas d = Decoder.map d ~f:(List.map ~f:(fun s -> make_stanza s None))

  let key : S.t Key.t =
    fun t ->
    match repr t with
    | T x -> Some x
    | _ -> None
  ;;

  include T
end

let compare (E (x, px, (module X))) (E (y, py, (module Y))) =
  match X.W with
  | Y.W -> Tuple.T2.compare X.compare (Option.compare Package_id.compare) (x, px) (y, py)
  | _ -> Id.compare X.id Y.id
;;

let hash (E (t, p, (module T))) =
  Tuple.T3.hash Id.hash (Option.hash Package_id.hash) T.hash (T.id, p, t)
;;

let equal x y = Ordering.is_eq (compare x y)

module Parser = struct
  type nonrec t = string * t list Decoder.t
end

(* The actual latest version is defined in the rpc library. This is because rpc
   client needs to know the version of dune to use to connect.

   To upgrade the latest version of the dune language, you need to edit the file
   in the rpc library. *)
let latest_version = Dune_rpc_private.Version.latest
let since v = v, `Since v
let all_minors (major, minor) = List.init (minor + 1) ~f:(fun i -> since (major, i))

let syntax =
  Syntax.create
    ~name:"dune"
    ~desc:"the dune language"
    (List.concat [ all_minors (1, 12); all_minors (2, 9); all_minors latest_version ])
;;
