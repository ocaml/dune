module Either = struct
  type ('a, 'b) t =
    | Left of 'a
    | Right of 'b
end
