open Stdune

type t = Impl | Intf

let all = [Impl; Intf]

let choose impl intf = function
  | Impl -> impl
  | Intf -> intf

let suffix = choose "" "i"

let to_string = choose "impl" "intf"

let to_dyn t = Dyn.String (to_string t)

let pp fmt t = Format.pp_print_string fmt (to_string t)

let flag t = choose (Command.Args.A "-impl") (A "-intf") t

let ppx_driver_flag t = choose (Command.Args.A "--impl") (A "--intf") t

module Dict = struct
  type 'a t =
    { impl : 'a
    ; intf : 'a
    }

  let get t = function
    | Impl -> t.impl
    | Intf -> t.intf

  let of_func f =
    { impl = f ~ml_kind:Impl
    ; intf = f ~ml_kind:Intf
    }

  let make ~impl ~intf = { impl; intf }

  let make_both x = { impl = x; intf = x }

  let map t ~f = { impl = f t.impl; intf = f t.intf }
  let mapi t ~f = { impl = f Impl t.impl; intf = f Intf t.intf }

  let iteri t ~f =
    f Impl t.impl;
    f Intf t.intf

  let to_dyn f { impl; intf } =
    let open Dyn.Encoder in
    record
      [ "impl", f impl
      ; "intf", f intf
      ]
end
