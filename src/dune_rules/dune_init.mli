(** Initialize dune components *)
open! Dune_engine

open! Stdune

(** Supported kinds of components for initialization *)
module Kind : sig
  type t =
    | Executable
    | Library
    | Project
    | Test

  val to_string : t -> string

  (* val kind_strings : string list *)
  val commands : (string * t) list
end

(** The context in which the initialization is executed *)
module Init_context : sig
  type t =
    { dir : Path.t
    ; project : Dune_project.t
    }

  val make : string option -> t
end

(** A [Component.t] is a set of files that can be built or included as part of a
    build. *)
module Component : sig
  (** Options determining the details of a generated component *)
  module Options : sig
    module Common : sig
      type t =
        { name : Dune_lang.Atom.t
        ; libraries : Dune_lang.Atom.t list
        ; pps : Dune_lang.Atom.t list
        }
    end

    type public_name =
      | Use_name
      | Public_name of Dune_lang.Atom.t

    val public_name_to_string : public_name -> string

    module Executable : sig
      type t = { public : public_name option }
    end

    module Library : sig
      type t =
        { public : public_name option
        ; inline_tests : bool
        }
    end

    module Test : sig
      (** NOTE: no options supported yet *)
      type t = unit
    end

    module Project : sig
      module Template : sig
        type t =
          | Exec
          | Lib

        val of_string : string -> t option

        val commands : (string * t) list
      end

      (** The package manager used for a project *)
      module Pkg : sig
        type t =
          | Opam
          | Esy

        val commands : (string * t) list
      end

      type t =
        { template : Template.t
        ; inline_tests : bool
        ; pkg : Pkg.t
        }
    end

    type 'a t =
      { context : Init_context.t
      ; common : Common.t
      ; options : 'a
      }
  end

  (** The supported types of components *)
  type 'options t =
    | Executable : Options.Executable.t Options.t -> Options.Executable.t t
    | Library : Options.Library.t Options.t -> Options.Library.t t
    | Project : Options.Project.t Options.t -> Options.Project.t t
    | Test : Options.Test.t Options.t -> Options.Test.t t

  (** Create or update the component specified by the ['options t], where
      ['options] is *)
  val init : 'options t -> unit
end
