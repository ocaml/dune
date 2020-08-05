open! Stdune
open! Import

type t =
  | Byte
  | Native

let compare = Poly.compare

let all = [ Byte; Native ]

let decode =
  let open Dune_lang.Decoder in
  enum [ ("byte", Byte); ("native", Native) ]

let choose byte native = function
  | Byte -> byte
  | Native -> native

let to_string = choose "byte" "native"

let encode t = Dune_lang.Encoder.string (to_string t)

let to_dyn t =
  let open Dyn.Encoder in
  constr (to_string t) []

let compiled_unit_ext = choose (Cm_kind.ext Cmo) (Cm_kind.ext Cmx)

let compiled_lib_ext = choose ".cma" ".cmxa"

let plugin_ext = choose ".cma" ".cmxs"

let variant = choose Variant.byte Variant.native

let cm_kind = choose Cm_kind.Cmo Cmx

let exe_ext = choose ".bc" ".exe"

let of_cm_kind : Cm_kind.t -> t = function
  | Cmi
  | Cmo ->
    Byte
  | Cmx -> Native

module Dict = struct
  type 'a t =
    { byte : 'a
    ; native : 'a
    }

  let equal f { byte; native } t = f byte t.byte && f native t.native

  let for_all { byte; native } ~f = f byte && f native

  let to_dyn to_dyn { byte; native } =
    let open Dyn.Encoder in
    record [ ("byte", to_dyn byte); ("native", to_dyn native) ]

  let get t = function
    | Byte -> t.byte
    | Native -> t.native

  let of_func f = { byte = f ~mode:Byte; native = f ~mode:Native }

  let map2 a b ~f = { byte = f a.byte b.byte; native = f a.native b.native }

  let map t ~f = { byte = f t.byte; native = f t.native }

  let mapi t ~f = { byte = f Byte t.byte; native = f Native t.native }

  let iteri t ~f =
    f Byte t.byte;
    f Native t.native

  let make_both x = { byte = x; native = x }

  let make ~byte ~native = { byte; native }

  module Set = struct
    type nonrec t = bool t

    let to_dyn { byte; native } =
      let open Dyn.Encoder in
      record [ ("byte", bool byte); ("native", bool native) ]

    let all = { byte = true; native = true }

    let to_list t =
      let l = [] in
      let l =
        if t.native then
          Native :: l
        else
          l
      in
      let l =
        if t.byte then
          Byte :: l
        else
          l
      in
      l

    let of_list l =
      { byte = List.mem Byte ~set:l; native = List.mem Native ~set:l }

    let encode t = List.map ~f:encode (to_list t)

    let is_empty t = not (t.byte || t.native)

    let iter t ~f =
      if t.byte then f Byte;
      if t.native then f Native
  end

  module List = struct
    type nonrec 'a t = 'a list t

    let empty = { byte = []; native = [] }

    let encode f { byte; native } =
      let open Dune_lang.Encoder in
      record_fields [ field_l "byte" f byte; field_l "native" f native ]

    let decode f =
      let open Dune_lang.Decoder in
      fields
        (let+ byte = field ~default:[] "byte" (repeat f)
         and+ native = field ~default:[] "native" (repeat f) in
         { byte; native })
  end
end
