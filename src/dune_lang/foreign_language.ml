open Import

module T = struct
  type t =
    | C
    | Cxx

  let repr =
    Repr.variant
      "foreign-language"
      [ Repr.case0 "C" ~test:(function
          | C -> true
          | Cxx -> false)
      ; Repr.case0 "Cxx" ~test:(function
          | Cxx -> true
          | C -> false)
      ]
  ;;

  let equal, compare = Repr.make_compare repr
  let to_dyn = Repr.to_dyn repr
end

include T

let proper_name = function
  | C -> "C"
  | Cxx -> "C++"
;;

include Comparable.Make (T)

module Dict = struct
  type 'a t =
    { c : 'a
    ; cxx : 'a
    }

  let equal f { c; cxx } t = f c t.c && f cxx t.cxx
  let c t = t.c
  let cxx t = t.cxx
  let map { c; cxx } ~f = { c = f c; cxx = f cxx }
  let mapi { c; cxx } ~f = { c = f ~language:C c; cxx = f ~language:Cxx cxx }
  let make_both a = { c = a; cxx = a }
  let make ~c ~cxx = { c; cxx }

  let get { c; cxx } = function
    | C -> c
    | Cxx -> cxx
  ;;

  let add t k v =
    match k with
    | C -> { t with c = v }
    | Cxx -> { t with cxx = v }
  ;;

  let update t k ~f =
    let v = get t k in
    add t k (f v)
  ;;

  let merge t1 t2 ~f = { c = f t1.c t2.c; cxx = f t1.cxx t2.cxx }
end

let header_extension = Filename.Extension.h

let source_extensions =
  String.Map.of_list_exn
    [ "c", (C, (1, 0)); "cpp", (Cxx, (1, 0)); "cxx", (Cxx, (1, 8)); "cc", (Cxx, (1, 10)) ]
;;

let has_foreign_extension ~fn =
  let ext = Filename.extension fn in
  if Filename.Extension.Or_empty.is_empty ext
  then false
  else (
    let ext = Filename.Extension.Or_empty.extension_exn ext in
    Filename.Extension.equal ext header_extension
    || String.Map.mem source_extensions (Filename.Extension.drop_dot ext))
;;
