(** Package masks for decoding stanzas.

   Package masks describe the invariants that must be respected when
   associating a stanza with a package. *)

open Import

type t = private
  | Inside_package of Package_id.t
  (** The only valid package association is the argument *)
  | Forbidden_packages of
      { by_dir : Package_id.t Path.Source.Map.t
      ; by_id : Path.Source.t Package_id.Map.t
      }
  (** All of the packages in either map are forbidden and may not be attached
      to any stanza when this mask is set. *)

val validate : t -> loc:Loc.t -> Package_id.t -> (unit, User_message.t) result
val package_env : dir:Path.Source.t -> packages:Package_id.t Path.Source.Map.t -> t
val key : t Univ_map.Key.t
val decode : unit -> (t, _) Decoder.parser
val decode_pkg : Package_id.t option Decoder.t
