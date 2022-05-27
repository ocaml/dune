(** Generate helper functions and decoders for generic action types.

    More documentation is available in the [Action] and [Action_intf] modules.
    This module is separate to break dependency cycles *)

module type Target_intf = sig
  include Dune_lang.Conv.S

  (** Needed to simplify redirections to /dev/null. In particular, this means
      that no /dev/null target is inferred *)
  val is_dev_null : t -> bool
end

module Make
    (Program : Dune_lang.Conv.S)
    (Path : Dune_lang.Conv.S)
    (Target : Target_intf)
    (String : Dune_lang.Conv.S)
    (Ast : Action_intf.Ast
             with type program := Program.t
              and type path := Path.t
              and type target := Target.t
              and type string := String.t) : sig
  include module type of Ast with type t = Ast.t

  include Dune_lang.Conv.S with type t := t

  include
    Action_intf.Helpers
      with type t := t
       and type program := Program.t
       and type path := Path.t
       and type target := Target.t
       and type string := String.t
end
