(** Loads dune files from the file system.

    Also responsible for evaluating dune files written in OCaml syntax. *)
open! Dune_engine

open! Stdune

module Dune_file : sig
  (** A fully evaluated dune file *)
  type t =
    { dir : Path.Source.t
    ; project : Dune_project.t
    ; stanzas : Dune_file.Stanzas.t
    }

  val fold_stanzas :
    t list -> init:'acc -> f:(t -> Stanza.t -> 'acc -> 'acc) -> 'acc
end

module Dune_files : sig
  (** A partially evaluated dune file. The context's ocamlc is used to evaluate
      dune files in ocaml syntax *)
  type t

  val eval : t -> context:Context.t -> Dune_file.t list Fiber.t
end

type conf = private
  { dune_files : Dune_files.t
  ; packages : Package.t Package.Name.Map.t
  ; projects : Dune_project.t list
  ; vcs : Vcs.t list
  }

(** Initialize the file tree and load all dune files. [ancestor_vcs] is the
    potential VCS repository the root is contained in. That is, not the
    repository the root directly contains. *)
val load : ancestor_vcs:Vcs.t option -> conf
