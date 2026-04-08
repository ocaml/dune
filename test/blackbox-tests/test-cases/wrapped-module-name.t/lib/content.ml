module type S = sig
  type t
  val x : t
end

module F : S = struct
  type t = int
  let x = 42
end

module D : S = struct
  type t = string
  let x = "hello"
end
