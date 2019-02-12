open Stdune

module Kind : sig
  type t =
    | C
    | Cxx

  val pp : t Fmt.t

  val split_extension
    :  string
    -> dune_version:Syntax.Version.t
    -> (string * t) option

  (** [possible_fns t s] returns the possible filenames given the extension-less
      basenames [s] *)
  val possible_fns : t -> string -> dune_version:Syntax.Version.t -> string list

  module Dict : sig
    type kind
    type 'a t =
      { c : 'a
      ; cxx : 'a
      }

    val make : 'a -> 'a t

    val update : 'a t -> kind -> f:('a -> 'a) -> 'a t

    val merge : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t
  end with type kind := t
end

module Source : sig
  type t

  val kind : t -> Kind.t
  val path : t -> Path.t
  val src_dir : t -> Path.t

  val make : kind:Kind.t -> path:Path.t -> t
end

module Sources : sig
  type t = (Loc.t * Source.t) String.Map.t

  val objects : t -> dir:Path.t -> ext_obj:string -> Path.t list

  val split_by_kind : t -> t Kind.Dict.t
end
