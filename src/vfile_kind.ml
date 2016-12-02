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

  val load : filename:string -> t
  val save : filename:string -> t -> unit
end

type 'a t = (module S with type t = 'a)

let eq (type a) (type b)
      (module A : S with type t = a)
      (module B : S with type t = b) =
  Id.eq A.id B.id

module Make_full
    (T : sig type t end)
    (To_sexp : sig val t : T.t -> Sexp.t end)
    (Of_sexp : sig val t : Sexp.t -> T.t end)
  : S with type t = T.t =
struct
  type t = T.t

  let id = Id.create ()

  let save ~filename x =
    let s = To_sexp.t x |> Sexp.to_string in
    let oc = open_out filename in
    output_string oc s;
    close_out oc

  let load ~filename =
    let sexp, _locs =
      with_lexbuf_from_file filename ~f:Sexp_lexer.single
    in
    Of_sexp.t sexp
end


module Make
    (T : sig type t end)
    (F : functor (C : Sexp.Combinators) -> sig val t : T.t C.t end)
  : S with type t = T.t =
struct
  module Of_sexp = F(Sexp.Of_sexp)
  module To_sexp = F(Sexp.To_sexp)

  include Make_full(T)(To_sexp)(Of_sexp)
end

