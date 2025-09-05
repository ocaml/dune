open Import

module T = struct
  type t =
    | C
    | Cxx

  let compare x y =
    match x, y with
    | C, C -> Eq
    | C, _ -> Lt
    | _, C -> Gt
    | Cxx, Cxx -> Eq
  ;;

  let equal x y =
    match x, y with
    | C, C -> true
    | Cxx, Cxx -> true
    | _, _ -> false
  ;;

  let to_dyn = function
    | C -> Dyn.Variant ("C", [])
    | Cxx -> Dyn.Variant ("Cxx", [])
  ;;
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

let header_extension = ".h"

let source_extensions =
  String.Map.of_list_exn
    [ "c", (C, (1, 0)); "cpp", (Cxx, (1, 0)); "cxx", (Cxx, (1, 8)); "cc", (Cxx, (1, 10)) ]
;;

let has_foreign_extension ~fn =
  let ext = Filename.extension fn in
  ext = header_extension || String.Map.mem source_extensions (String.drop ext 1)
;;
