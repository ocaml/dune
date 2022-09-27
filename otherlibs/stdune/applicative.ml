module type Basic = Applicative_intf.Basic

module Make (A : Applicative_intf.Basic) = struct
  include A

  module O = struct
    let ( let+ ) x f = A.map x ~f

    let ( and+ ) = A.both

    let ( >>> ) x y =
      let+ () = x
      and+ y = y in
      y
  end

  let rec all xs =
    match xs with
    | [] -> return []
    | x :: xs ->
      let open O in
      let+ x = x
      and+ xs = all xs in
      x :: xs
end
[@@inline always]

module Id = struct
  include Make (struct
    type 'a t = 'a

    let return a = a

    let map x ~f = f x

    let both x y = (x, y)
  end)

  let all x = x
end
