module type F_sig = module type of Content.F

module Make (A : F_sig) : sig
  type u = A.t
  val value : u
end

module F_tools : sig
  type u = Content.F.t
  val value : u
end
