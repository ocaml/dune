open Import

module Id : sig
  type 'a t

  val eq : 'a t -> 'b t -> ('a, 'b) eq option
end

module type S = sig
  type t

  val id : t Id.t

  val load : Path.t -> t
  val to_string : t -> string
end

type 'a t = (module S with type t = 'a)

val eq : 'a t -> 'b t -> ('a, 'b) eq option

module Make
    (T : sig
       type t
       val t : t Sexp.To_sexp.t
       val name : string
     end)
  : S with type t = T.t
