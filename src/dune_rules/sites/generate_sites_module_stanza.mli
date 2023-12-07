open Import

(** Stanza which generate a module for getting information from dune *)

type t =
  { loc : Loc.t
  ; module_ : Module_name.t (** name of the module to generate *)
  ; sourceroot : bool (** should the sourceroot of the project be provided *)
  ; relocatable : bool
  (** should the fact that the installation use the relocatable mode *)
  ; sites : (Loc.t * Package.Name.t) list
  (** list of the sites whose location should be given *)
  ; plugins : (Loc.t * (Package.Name.t * (Loc.t * Site.t))) list
  (** list of the sites for which a plugin system must be provided *)
  }

val decode : t Dune_sexp.Decoder.t

include Stanza.S with type t := t
