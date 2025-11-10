open Stdune

type t =
  | Cmi
  | Cmo
  | Cmx
  | Cmt
  | Cmti

let compare x y : Ordering.t =
  match x, y with
  | Cmi, Cmi -> Eq
  | Cmi, _ -> Lt
  | _, Cmi -> Gt
  | Cmo, Cmo -> Eq
  | Cmo, _ -> Lt
  | _, Cmo -> Gt
  | Cmx, Cmx -> Eq
  | Cmx, _ -> Lt
  | _, Cmx -> Gt
  | Cmt, Cmt -> Eq
  | Cmt, _ -> Lt
  | _, Cmt -> Gt
  | Cmti, Cmti -> Eq
;;

let all = [ Cmi; Cmo; Cmx; Cmt; Cmti ]

let choose cmi cmo cmx cmt cmti = function
  | Cmi -> cmi
  | Cmo -> cmo
  | Cmx -> cmx
  | Cmt -> cmt
  | Cmti -> cmti
;;

let ext = choose ".cmi" ".cmo" ".cmx" ".cmt" ".cmti"
let source = choose Ml_kind.Intf Impl Impl Impl Intf

let to_dyn =
  let open Dyn in
  function
  | Cmi -> variant "cmi" []
  | Cmo -> variant "cmo" []
  | Cmx -> variant "cmx" []
  | Cmt -> variant "cmt" []
  | Cmti -> variant "cmti" []
;;
