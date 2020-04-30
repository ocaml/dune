module Make (M : Monad_intf.S1_base) = struct
  include M

  module O = struct
    let ( let+ ) x f = M.( >>= ) x (fun x -> M.return (f x))

    let ( and+ ) x y =
      let open M in
      x >>= fun x ->
      y >>= fun y -> return (x, y)

    let ( let* ) = M.( >>= )
  end
end

module Id = Make (struct
  type 'a t = 'a

  let return x = x

  let ( >>= ) x f = f x
end)
