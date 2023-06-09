open Import

val sanitize_for_tests : bool ref

type options =
  { with_deps : bool
  ; with_pps : bool
  }

module Descr : sig
  module Item : sig
    type t
  end

  module Workspace : sig
    type t = Item.t list

    val to_dyn : options -> t -> Dyn.t
  end
end

module Crawl : sig
  val workspace :
       options
    -> Dune_engine.No_io.Path.Source.t list option
    -> Dune_rules.Main.build_system
    -> Context.t
    -> Descr.Workspace.t Memo.t
end

module Sanitize_for_tests : sig
  module Workspace : sig
    val sanitize : Context.t -> Descr.Item.t list -> Descr.Item.t list
  end
end

module External_lib_deps : sig
  val get : Dune_rules.Main.build_system -> Super_context.t -> Dyn.t Memo.t
end

module Preprocess : sig
  val run : Super_context.t -> string -> unit Memo.t
end

module Format : sig
  type t =
    | Sexp
    | Csexp

  val arg : t Term.t

  val print_dyn : t -> Dyn.t -> unit
end
