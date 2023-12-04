open Stdune

type t =
  | Ocaml of Ocaml.Mode.t
  | Melange

let equal x y =
  match x, y with
  | Ocaml o1, Ocaml o2 -> Ocaml.Mode.equal o1 o2
  | Ocaml _, _ | _, Ocaml _ -> false
  | Melange, Melange -> true
;;

let decode =
  let open Dune_sexp.Decoder in
  enum [ "byte", Ocaml Byte; "native", Ocaml Native; "melange", Melange ]
;;

let choose byte native melange = function
  | Ocaml Byte -> byte
  | Ocaml Native -> native
  | Melange -> melange
;;

let to_string = choose "byte" "native" "melange"
let encode t = Dune_sexp.Encoder.string (to_string t)

module Cm_kind = struct
  type t =
    | Ocaml of Ocaml.Cm_kind.t
    | Melange of Melange.Cm_kind.t

  let choose ocaml melange = function
    | Ocaml k -> ocaml k
    | Melange k -> melange k
  ;;

  let source = choose Ocaml.Cm_kind.source Melange.Cm_kind.source
  let ext = choose Ocaml.Cm_kind.ext Melange.Cm_kind.ext

  let cmi = function
    | Ocaml _ -> Ocaml Cmi
    | Melange _ -> Melange Cmi
  ;;

  let to_dyn =
    let open Dyn in
    function
    | Ocaml k -> variant "ocaml" [ Ocaml.Cm_kind.to_dyn k ]
    | Melange k -> variant "melange" [ Melange.Cm_kind.to_dyn k ]
  ;;

  module Map = struct
    type 'a t =
      { ocaml : 'a Ocaml.Cm_kind.Dict.t
      ; melange : 'a Melange.Cm_kind.Map.t
      }

    let get t = function
      | Ocaml k -> Ocaml.Cm_kind.Dict.get t.ocaml k
      | Melange k ->
        (match k with
         | Cmi -> t.melange.cmi
         | Cmj -> t.melange.cmj)
    ;;

    let make_all x =
      { ocaml = Ocaml.Cm_kind.Dict.make_all x; melange = Melange.Cm_kind.Map.make_all x }
    ;;
  end
end

let of_cm_kind : Cm_kind.t -> t = function
  | Ocaml (Cmi | Cmo) -> Ocaml Byte
  | Ocaml Cmx -> Ocaml Native
  | Melange (Cmi | Cmj) -> Melange
;;

module Map = struct
  let mode_equal = equal

  type 'a t =
    { ocaml : 'a Ocaml.Mode.Dict.t
    ; melange : 'a
    }

  let equal f { ocaml; melange } t : bool =
    Ocaml.Mode.Dict.equal f ocaml t.ocaml && f melange t.melange
  ;;

  let to_dyn to_dyn { ocaml; melange } =
    let open Dyn in
    record [ "ocaml", Ocaml.Mode.Dict.to_dyn to_dyn ocaml; "melange", to_dyn melange ]
  ;;

  let get t = function
    | Ocaml k -> Ocaml.Mode.Dict.get t.ocaml k
    | Melange -> t.melange
  ;;

  let map t ~f = { ocaml = Ocaml.Mode.Dict.map ~f t.ocaml; melange = f t.melange }
  let make_all x = { ocaml = Ocaml.Mode.Dict.make_both x; melange = x }

  let make ~byte ~native ~melange =
    { ocaml = Ocaml.Mode.Dict.make ~byte ~native; melange }
  ;;

  module Set = struct
    type nonrec t = bool t

    let equal = equal Bool.equal

    let to_list (t : t) =
      let l = [] in
      let l = if t.ocaml.native then Ocaml Native :: l else l in
      let l = if t.ocaml.byte then Ocaml Byte :: l else l in
      let l = if t.melange then Melange :: l else l in
      l
    ;;

    let encode t = List.map ~f:encode (to_list t)

    let of_list l =
      { ocaml =
          { byte = List.mem l (Ocaml Byte) ~equal:mode_equal
          ; native = List.mem l (Ocaml Native) ~equal:mode_equal
          }
      ; melange = List.mem l Melange ~equal:mode_equal
      }
    ;;

    let to_dyn { ocaml; melange } =
      let open Dyn in
      record [ "ocaml", Ocaml.Mode.Dict.Set.to_dyn ocaml; "melange", bool melange ]
    ;;

    let for_merlin { ocaml = { byte; native = _ }; melange } =
      match byte, melange with
      | false, true -> Melange
      | _, _ -> Ocaml Byte
    ;;
  end
end
