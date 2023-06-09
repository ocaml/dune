open Import
open Stdune

type options =
  { with_deps : bool
  ; with_pps : bool
  }

module Descr : sig
  module Item : sig
    type t

    val map_path : t -> f:(Path.t -> Path.t) -> t
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

module Format : sig
  type t =
    | Sexp
    | Csexp

  val arg : t Term.t

  val print_dyn : t -> Dyn.t -> unit
end
