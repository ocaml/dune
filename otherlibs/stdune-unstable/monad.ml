module type Basic = Monad_intf.Basic

module Make (M : Monad_intf.Basic) = struct
  include M

  let map t ~f = bind t ~f:(fun x -> return (f x))

  module O = struct
    let ( >>= ) t f = bind t ~f

    let ( >>| ) t f = map t ~f

    let ( >>> ) a b = bind a ~f:(fun () -> b)

    let ( let+ ) t f = map t ~f

    let ( and+ ) x y =
      let open M in
      x >>= fun x ->
      y >>= fun y -> return (x, y)

    let ( let* ) t f = bind t ~f

    let ( and* ) = ( and+ )
  end
end
[@@inlined always]

module Id = Make (struct
  type 'a t = 'a

  let return x = x

  let bind x ~f = f x
end)
