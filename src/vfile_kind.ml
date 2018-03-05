open Import

module Id = struct
  type 'a tag = ..

  module type S = sig
    type t
    type 'a tag += X : t tag
  end

  type 'a t = (module S with type t = 'a)

  let create (type a) () =
    let module M = struct
      type t = a
      type 'a tag += X : t tag
    end in
    (module M : S with type t = a)

  let eq (type a) (type b)
        (module A : S with type t = a)
        (module B : S with type t = b)
    : (a, b) eq option =
    match A.X with
    | B.X -> Some Eq
    | _   -> None
end

module type S = sig
  type t

  val id : t Id.t

  val load : Path.t -> t
  val to_string : Path.t -> t -> string
end

type 'a t = (module S with type t = 'a)

let eq (type a) (type b)
      (module A : S with type t = a)
      (module B : S with type t = b) =
  Id.eq A.id B.id

module Make_full
    (T : sig type t end)
    (To_sexp : sig val t : Path.t -> T.t -> Sexp.t end)
    (Of_sexp : sig val t : Path.t -> Sexp.Ast.t -> T.t end)
  : S with type t = T.t =
struct
  type t = T.t

  let id = Id.create ()

  let to_string path x = To_sexp.t path x |> Sexp.to_string

  let load path =
    Of_sexp.t path (Sexp.load ~fname:(Path.to_string path) ~mode:Single)
end


module Make
    (T : sig type t end)
    (F : functor (C : Sexp.Combinators) -> sig val t : T.t C.t end)
  : S with type t = T.t =
struct
  module Of_sexp = struct
    include F(Sexp.Of_sexp)
    let t _ sexp = t sexp
  end
  module To_sexp = struct
    include F(Sexp.To_sexp)
    let t _ x = t x
  end

  include Make_full(T)(To_sexp)(Of_sexp)
end

