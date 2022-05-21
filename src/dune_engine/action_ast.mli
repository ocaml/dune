(** Generate helper functions and decoders for generic action types.

    More documentation is available in the [Action] and [Action_intf] modules.
    This module is separate to break dependency cycles *)

module type Encoder = sig
  type t

  val encode : t -> Dune_lang.t
end

module Make
    (Program : Encoder)
    (Path : Encoder)
    (Target : Encoder)
    (String : Encoder)
    (Ast : Action_intf.Ast
             with type program := Program.t
              and type path := Path.t
              and type target := Target.t
              and type string := String.t) : sig
  include module type of Ast with type t = Ast.t

  include Encoder with type t := t

  include
    Action_intf.Helpers
      with type t := t
       and type program := Program.t
       and type path := Path.t
       and type target := Target.t
       and type string := String.t
end
