module Id = struct
  type 'a t = 'a

  let return x = x

  let ( >>= ) x f = f x
end
