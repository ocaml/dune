open! Stdune

type t =
  | Byte
  | Native

let equal x y =
  match (x, y) with
  | Byte, Byte -> true
  | Byte, _ | _, Byte -> false
  | Native, Native -> true

let compare x y =
  match (x, y) with
  | Byte, Byte -> Eq
  | Byte, _ -> Lt
  | _, Byte -> Gt
  | Native, Native -> Eq

let all = [ Byte; Native ]

let decode =
  let open Dune_sexp.Decoder in
  enum [ ("byte", Byte); ("native", Native) ]

let choose byte native = function
  | Byte -> byte
  | Native -> native

let to_string = choose "byte" "native"

let encode t = Dune_sexp.Encoder.string (to_string t)

let to_dyn t = Dyn.variant (to_string t) []

let compiled_unit_ext = choose (Cm_kind.ext Cmo) (Cm_kind.ext Cmx)

let compiled_lib_ext = choose ".cma" ".cmxa"

let plugin_ext = choose ".cma" ".cmxs"

let variant = choose Variant.byte Variant.native

let cm_kind = choose Cm_kind.Cmo Cmx

let exe_ext = choose ".bc" ".exe"

let of_cm_kind : Cm_kind.t -> t = function
  | Cmi | Cmo -> Byte
  | Cmx -> Native

module Dict = struct
  let mode_equal = equal

  type 'a t =
    { byte : 'a
    ; native : 'a
    }

  let equal f { byte; native } t = f byte t.byte && f native t.native

  let for_all { byte; native } ~f = f byte && f native

  let to_dyn to_dyn { byte; native } =
    let open Dyn in
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

  let foldi t ~init ~f = f Native t.native @@ f Byte t.byte init

  let make_both x = { byte = x; native = x }

  let make ~byte ~native = { byte; native }

  module Set = struct
    type nonrec t = bool t

    let equal = equal Bool.equal

    let to_dyn { byte; native } =
      let open Dyn in
      record [ ("byte", bool byte); ("native", bool native) ]

    let all = { byte = true; native = true }

    let to_list t =
      let l = [] in
      let l = if t.native then Native :: l else l in
      let l = if t.byte then Byte :: l else l in
      l

    let of_list l =
      { byte = List.mem l Byte ~equal:mode_equal
      ; native = List.mem l Native ~equal:mode_equal
      }

    let encode t = List.map ~f:encode (to_list t)

    let is_empty t = not (t.byte || t.native)

    let iter_concurrently t ~f =
      let open Memo.O in
      let+ () = Memo.when_ t.byte (fun () -> f Byte)
      and+ () = Memo.when_ t.native (fun () -> f Native) in
      ()
  end

  module List = struct
    type nonrec 'a t = 'a list t

    let empty = { byte = []; native = [] }

    let encode f { byte; native } =
      let open Dune_sexp.Encoder in
      record_fields [ field_l "byte" f byte; field_l "native" f native ]

    let decode f =
      let open Dune_sexp.Decoder in
      fields
        (let+ byte = field ~default:[] "byte" (repeat f)
         and+ native = field ~default:[] "native" (repeat f) in
         { byte; native })
  end
end

module Select = struct
  type mode = t

  type nonrec t =
    | Only of t
    | All

  let compare x y =
    match (x, y) with
    | All, All -> Eq
    | All, _ -> Lt
    | _, All -> Gt
    | Only x, Only y -> compare x y

  let of_option = function
    | None -> All
    | Some m -> Only m

  let is_not_all = function
    | All -> false
    | Only _ -> true

  let equal t t' =
    match (t, t') with
    | Only m, Only m' -> equal m m'
    | All, All -> true
    | _, _ -> false

  let to_dyn t =
    let open Dyn in
    match t with
    | All -> Variant ("All", [])
    | Only m -> Variant ("Only", [ to_dyn m ])

  let encode t =
    let open Dune_sexp.Encoder in
    match t with
    | All -> string "all"
    | Only m -> encode m

  let decode =
    let open Dune_sexp.Decoder in
    let+ value = either (keyword "all") decode in
    match value with
    | Left () -> All
    | Right mode -> Only mode
end

module Map = struct
  type mode = t

  (* Here we use a Map to be able in the future to use more variants than
     just "Byte, Native or All" *)
  include Map.Make (Select)

  module Multi = struct
    include Multi

    let create_for_all_modes l = Multi.add_all empty All l

    let for_all_modes t = find t All

    let for_only ?(and_all = false) t mode =
      let all = if and_all then for_all_modes t else [] in
      List.rev_append all @@ find t (Only mode)
  end

  let encode encoder t =
    let open Dune_sexp.Encoder in
    let fields =
      foldi t ~init:[] ~f:(fun for_ files acc ->
          if List.is_empty files then acc
          else
            let field_for = field "for" Select.encode for_ in
            let field_files = field_l "files" encoder files in
            field_l "archives" Fun.id (record_fields [ field_for; field_files ])
            :: acc)
    in
    record_fields fields

  let decode decoder =
    let open Dune_sexp.Decoder in
    let+ fields =
      fields
      @@ multi_field "archives"
           (fields
              (let+ for_ = field "for" Select.decode
               and+ files = field "files" (repeat decoder) in
               (for_, files)))
    in
    of_list_exn fields
end
