(** Build rules *)

module Spec : sig
  type _ t =
    | Unit : string list -> unit t
    | Vals : 'a Values.Spec.t -> 'a Values.t t
    | Both : string list * 'a Values.Spec.t -> 'a Values.t t
end

val rule
  :  deps:'a Spec.t
  -> targets:'b Spec.t
  -> ('a -> 'b Future.t)
  -> unit

type 'a with_dynamic_deps =
    Dyn : { deps : 'b Spec.t
          ; exec : 'b -> 'a Future.t
          } -> 'a with_dynamic_deps

val dyn_rule
  :  deps:'a Spec.t
  -> targets:'b Spec.t
  -> ('a -> 'b with_dynamic_deps)
  -> unit

(** Simple rule. [stdout_to] is automatically added to the list of targets. *)
val simple_rule
  :  deps:string list
  -> ?targets:string list
  -> ?stdout_to:string
  -> string      (** program   *)
  -> string list (** arguments *)
  -> unit

(** Do the actual build *)
val do_build : string list -> unit Future.t
