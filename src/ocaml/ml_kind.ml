open Stdune

type t =
  | Impl
  | Intf

let all = [ Impl; Intf ]

let choose t ~impl ~intf =
  match t with
  | Impl -> impl
  | Intf -> intf
;;

let to_string = choose ~impl:"impl" ~intf:"intf"
let to_dyn t = Dyn.String (to_string t)

let cmt_ext = function
  | Impl -> ".cmt"
  | Intf -> ".cmti"
;;

module Dict = struct
  type 'a t =
    { impl : 'a
    ; intf : 'a
    }

  let compare f { impl; intf } t =
    let open Ordering.O in
    let= () = f impl t.impl in
    f intf t.intf
  ;;

  let get t = function
    | Impl -> t.impl
    | Intf -> t.intf
  ;;

  let of_func f = { impl = f ~ml_kind:Impl; intf = f ~ml_kind:Intf }
  let make ~impl ~intf = { impl; intf }
  let make_both x = { impl = x; intf = x }
  let map t ~f = { impl = f t.impl; intf = f t.intf }
  let mapi t ~f = { impl = f Impl t.impl; intf = f Intf t.intf }

  let iteri t ~f =
    f Impl t.impl;
    f Intf t.intf
  ;;

  let to_dyn f { impl; intf } =
    let open Dyn in
    record [ "impl", f impl; "intf", f intf ]
  ;;
end
