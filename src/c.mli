open Stdune

module Kind : sig
  type t =
    | C
    | Cxx

  val to_string : t -> string

  val pp : t Fmt.t

  type split =
    | Unrecognized
    | Not_allowed_until of Syntax.Version.t
    | Recognized of string * t

  val split_extension
    :  string
    -> dune_version:Syntax.Version.t
    -> split

  (** [possible_fns t s] returns the possible filenames given the extension-less
      basenames [s] *)
  val possible_fns : t -> string -> dune_version:Syntax.Version.t -> string list

  module Dict : sig
    type kind
    type 'a t =
      { c : 'a
      ; cxx : 'a
      }

    val c : 'a t -> 'a
    val cxx : 'a t -> 'a

    val map : 'a t -> f:('a -> 'b) -> 'b t
    val mapi : 'a t -> f:(kind:kind -> 'a -> 'b) -> 'b t

    val make_both : 'a -> 'a t

    val make : c:'a -> cxx:'a -> 'a t

    val update : 'a t -> kind -> f:('a -> 'a) -> 'a t

    val merge : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

    val get : 'a t -> kind -> 'a
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
