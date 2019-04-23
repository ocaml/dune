open! Stdune

module Dune_file : sig
  type t =
    { dir     : Path.Source.t
    ; project : Dune_project.t
    ; stanzas : Dune_file.Stanzas.t
    ; kind    : Dune_lang.File_syntax.t
    }
end

module Dune_files : sig
  type t

  val eval
    :  t
    -> context:Context.t
    -> Dune_file.t list Fiber.t
end

type conf = private
  { file_tree  : File_tree.t
  ; dune_files : Dune_files.t
  ; packages   : Package.t Package.Name.Map.t
  ; projects   : Dune_project.t list
  }

val load
  :  ?ignore_promoted_rules:bool
  -> unit
  -> conf
