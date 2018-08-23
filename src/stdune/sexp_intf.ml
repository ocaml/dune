module Combinators = struct
  module type S2 = sig
    type ('a, 'ctx) t

    val unit       : (unit, _) t

    val string     : (string, _) t
    (** Convert an [Atom] or a [Quoted_string] from/to a string. *)

    val int        : (int, _) t
    val float      : (float, _) t
    val bool       : (bool, _) t
    val pair       : ('a, 'ctx) t -> ('b, 'ctx) t -> ('a * 'b, 'ctx) t

    val triple
      :  ('a, 'ctx) t
      -> ('b, 'ctx) t
      -> ('c, 'ctx) t
      -> ('a * 'b * 'c, 'ctx) t

    val list       : ('a, 'ctx) t -> ('a list   , 'ctx) t
    val array      : ('a, 'ctx) t -> ('a array  , 'ctx) t
    val option     : ('a, 'ctx) t -> ('a option , 'ctx) t

    val string_set : (String.Set.t, 'ctx) t
    (** [atom_set] is a conversion to/from a set of strings representing
        atoms. *)

    val string_map : ('a, 'ctx) t -> ('a String.Map.t, 'ctx ) t
    (** [atom_map conv]: given a conversion [conv] to/from ['a], returns
        a conversion to/from a map where the keys are atoms and the
        values are of type ['a]. *)

    val string_hashtbl : ('a, 'ctx) t -> ((string, 'a) Hashtbl.t, 'ctx) t
    (** [atom_hashtbl conv] is similar to [atom_map] for hash tables. *)
  end

  module type S = sig
    type 'a t
    val unit       : unit                      t

    val string     : string                    t
    (** Convert an [Atom] or a [Quoted_string] from/to a string. *)

    val int        : int                       t
    val float      : float                     t
    val bool       : bool                      t
    val pair       : 'a t -> 'b t -> ('a * 'b) t
    val triple     : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
    val list       : 'a t -> 'a list           t
    val array      : 'a t -> 'a array          t
    val option     : 'a t -> 'a option         t

    val string_set : String.Set.t            t
    (** [atom_set] is a conversion to/from a set of strings representing atoms. *)

    val string_map : 'a t -> 'a String.Map.t   t
    (** [atom_map conv]: given a conversion [conv] to/from ['a], returns
        a conversion to/from a map where the keys are atoms and the
        values are of type ['a]. *)

    val string_hashtbl : 'a t -> (string, 'a) Hashtbl.t t
    (** [atom_hashtbl conv] is similar to [atom_map] for hash tables. *)
  end
end
