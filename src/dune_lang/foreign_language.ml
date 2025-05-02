open Import

type t =
  [ `C
  | `Cxx
  | `Asm
  ]

let equal x y =
  match x, y with
  | `C, `C -> true
  | `C, _ | _, `C -> false
  | `Cxx, `Cxx -> true
  | `Cxx, _ | _, `Cxx -> false
  | `Asm, `Asm -> true
  | `Asm, _ | _, `Asm -> false
  | _, _ -> false
;;

let to_dyn : t -> Dyn.t = function
  | `C -> Dyn.variant "C" []
  | `Cxx -> Dyn.variant "Cxx" []
  | `Asm -> Dyn.variant "Asm" []
;;

let proper_name = function
  | `C -> "C"
  | `Cxx -> "C++"
  | `Asm -> "Assembly"
;;

let to_string : t -> string = function
  | `C -> "c"
  | `Cxx -> "cxx"
  | `Asm -> "asm"
;;

let all = [ `C; `Cxx; `Asm ]
let decode = all |> List.map ~f:(fun x -> to_string x, x) |> Decoder.enum

module Dict = struct
  type 'a t =
    { c : 'a
    ; cxx : 'a
    }

  let equal f { c; cxx } t = f c t.c && f cxx t.cxx
  let c t = t.c
  let cxx t = t.cxx
  let map { c; cxx } ~f = { c = f c; cxx = f cxx }
  let mapi { c; cxx } ~f = { c = f ~language:`C c; cxx = f ~language:`Cxx cxx }
  let make_all a = { c = a; cxx = a }
  let make ~c ~cxx = { c; cxx }

  let get { c; cxx } = function
    | `C -> c
    | `Cxx -> cxx
  ;;

  let add t k v =
    match k with
    | `C -> { t with c = v }
    | `Cxx -> { t with cxx = v }
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
    [ "c", (`C, (1, 0))
    ; "cpp", (`Cxx, (1, 0))
    ; "cxx", (`Cxx, (1, 8))
    ; "cc", (`Cxx, (1, 10))
    ; "S", (`Asm, (3, 19))
    ; "s", (`Asm, (3, 19))
    ; "asm", (`Asm, (3, 19))
    ]
;;

let has_foreign_extension ~fn =
  let ext = Filename.extension fn in
  ext = header_extension || String.Map.mem source_extensions (String.drop ext 1)
;;
