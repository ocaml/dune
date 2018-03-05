open! Import

type t = Byte | Native

let all = [Byte; Native]

let t =
  let open Sexp.Of_sexp in
  enum
    [ "byte"   , Byte
    ; "native" , Native
    ]

let choose byte native = function
  | Byte   -> byte
  | Native -> native

let compiled_unit_ext = choose ".cmo" ".cmx"
let compiled_lib_ext = choose ".cma" ".cmxa"
let plugin_ext = choose ".cma" ".cmxs"

let variant = choose Variant.byte Variant.native

let cm_kind = choose Cm_kind.Cmo Cmx

let exe_ext = choose ".bc" ".exe"

let of_cm_kind : Cm_kind.t -> t = function
  | Cmi | Cmo -> Byte
  | Cmx -> Native

module Dict = struct
  type 'a t =
    { byte   : 'a
    ; native : 'a
    }

  let get t = function
    | Byte   -> t.byte
    | Native -> t.native

  let of_func f =
    { byte   = f ~mode:Byte
    ; native = f ~mode:Native
    }

  let map2 a b ~f =
    { byte   = f a.byte   b.byte
    ; native = f a.native b.native
    }

  let make_both x =
    { byte   = x
    ; native = x
    }

  module Set = struct
    type nonrec t = bool t

    let all =
      { byte   = true
      ; native = true
      }

    let to_list t =
      let l = [] in
      let l = if t.native then Native :: l else l in
      let l = if t.byte   then Byte   :: l else l in
      l

    let of_list l =
      { byte   = List.mem Byte   ~set:l
      ; native = List.mem Native ~set:l
      }

    let t sexp = of_list (Sexp.Of_sexp.list t sexp)

    let is_empty t = not (t.byte || t.native)

    let iter t ~f =
      if t.byte   then f Byte;
      if t.native then f Native
  end

  module Binary_Kind_Set = struct
    type binary_kind =
      | Executable
      | Object
      | Shared_object

    let binary_kind =
      let open Sexp.Of_sexp in
      enum
        [ "executable"    , Executable
        ; "object"        , Object
        ; "shared_object" , Shared_object
        ]

    type nonrec t = binary_kind list t

    let default =
      { byte   = [Executable]
      ; native = [Executable]
      }

    let all_binary_kind = [Executable; Object; Shared_object]
    let all =
      { byte   = all_binary_kind
      ; native = all_binary_kind
      }

    let to_list t =
      (List.map t.native ~f:(fun x -> Native,x)) @
      (List.map t.byte ~f:(fun x -> Byte,x))

    let of_list l =
      let byte, native = List.partition_map ~f:(fun (m,x) -> if m=Byte then Right x else Left x) l in
      { byte
      ; native
      }

    let t : t Sexp.Of_sexp.t =
      fun sexp ->
        let elt (sexp : Sexp.Ast.t) =
          match sexp with
          | List _ -> Sexp.Of_sexp.pair t binary_kind sexp
          | _      -> (t sexp, Executable)
        in
        of_list (Sexp.Of_sexp.list elt sexp)

    let is_empty t = t.byte = [] && t.native = []

    let iter t ~f =
      List.iter ~f (to_list t)

    let best_executable_mode t =
      if List.mem ~set:t.native Executable then Some Native
      else if List.mem ~set:t.byte Executable then Some Byte
      else None
  end


end
