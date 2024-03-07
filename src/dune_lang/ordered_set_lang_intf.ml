open Stdune

module type Key = sig
  type t

  val compare : t -> t -> Ordering.t

  module Map : sig
    type key := t
    type 'a t

    val singleton : key -> 'a -> 'a t
    val empty : 'a t
    val merge : 'a t -> 'b t -> f:(key -> 'a option -> 'b option -> 'c option) -> 'c t
  end
end

module type Unordered_eval = sig
  (** Evaluate an ordered set. [standard] is the interpretation of [:standard]
      inside the DSL. *)
  type t

  module Key : Key

  (** Same as [eval] but the result is unordered *)
  val eval
    :  t
    -> parse:(loc:Loc.t -> string -> 'a)
    -> key:('a -> Key.t)
    -> standard:'a Key.Map.t
    -> 'a Key.Map.t

  (** Same as [eval] but the result is unordered *)
  val eval_loc
    :  t
    -> parse:(loc:Loc.t -> string -> 'a)
    -> key:('a -> Key.t)
    -> standard:(Loc.t * 'a) Key.Map.t
    -> (Loc.t * 'a) Key.Map.t
end

module type Action_builder = sig
  type 'a t

  val return : 'a -> 'a t
  val all : 'a t list -> 'a list t
  val read_sexp : Path.t -> Dune_sexp.Ast.t t

  val push_stack_frame
    :  human_readable_description:(unit -> User_message.Style.t Pp.t)
    -> (unit -> 'a t)
    -> 'a t

  module O : sig
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
  end

  val expand
    :  String_with_vars.t
    -> mode:(_, 'value) String_with_vars.Mode.t
    -> dir:Path.t
    -> f:Value.t list t String_with_vars.expander
    -> 'value t
end
