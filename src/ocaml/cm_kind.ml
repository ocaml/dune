open Stdune

type t =
  | Cmi
  | Cmo
  | Cmx

let compare x y : Ordering.t =
  match x, y with
  | Cmi, Cmi -> Eq
  | Cmi, _ -> Lt
  | _, Cmi -> Gt
  | Cmo, Cmo -> Eq
  | Cmo, _ -> Lt
  | _, Cmo -> Gt
  | Cmx, Cmx -> Eq
;;

let all = [ Cmi; Cmo; Cmx ]

let choose cmi cmo cmx = function
  | Cmi -> cmi
  | Cmo -> cmo
  | Cmx -> cmx
;;

let ext = choose ".cmi" ".cmo" ".cmx"
let source = choose Ml_kind.Intf Impl Impl

module Dict = struct
  type 'a t =
    { cmi : 'a
    ; cmo : 'a
    ; cmx : 'a
    }

  let get t = function
    | Cmi -> t.cmi
    | Cmo -> t.cmo
    | Cmx -> t.cmx
  ;;

  let of_func f = { cmi = f ~cm_kind:Cmi; cmo = f ~cm_kind:Cmo; cmx = f ~cm_kind:Cmx }
  let make_all x = { cmi = x; cmo = x; cmx = x }
end

let to_dyn =
  let open Dyn in
  function
  | Cmi -> variant "cmi" []
  | Cmo -> variant "cmo" []
  | Cmx -> variant "cmx" []
;;
