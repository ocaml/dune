module type F_sig = module type of Content.F

module Make (A : F_sig) = struct
  type u = A.t
  let value = A.x
end

module F_tools = Make (Content.F)
