(** Initialize dune components *)

open Import

(** The context in which the initialization is executed *)
module Init_context : sig
  type t =
    { dir : Path.t
    ; project : Dune_project.t
    }

  val make : string option -> t Memo.t
end

module Public_name : sig
  type t

  val to_string : t -> string
  val of_string_user_error : Loc.t * string -> (t, User_message.t) result
  val of_name_exn : Dune_lang.Atom.t -> t
end

(** A [Component.t] is a set of files that can be built or included as part of a
    build. *)
module Component : sig
  (** Options determining the details of a generated component *)
  module Options : sig
    (** The common options shared by all components *)
    module Common : sig
      type t =
        { name : Dune_lang.Atom.t
        ; libraries : Dune_lang.Atom.t list
        ; pps : Dune_lang.Atom.t list
        }
    end

    (** Options for executable components *)
    module Executable : sig
      type t = { public : Public_name.t option }
    end

    (** Options for library components *)
    module Library : sig
      type t =
        { public : Public_name.t option
        ; inline_tests : bool
        }
    end

    (** Options for test components *)
    module Test : sig
      (** NOTE: no options supported yet *)
      type t = unit
    end

    (** Options for project components (which consist of several sub-components) *)
    module Project : sig
      (** Determines whether this is a library project or an executable project *)
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

  (** All the the supported types of components *)
  type 'options t =
    | Executable : Options.Executable.t Options.t -> Options.Executable.t t
    | Library : Options.Library.t Options.t -> Options.Library.t t
    | Project : Options.Project.t Options.t -> Options.Project.t t
    | Test : Options.Test.t Options.t -> Options.Test.t t

  (** Create or update the component specified by the ['options t], where
      ['options] is *)
  val init : 'options t -> unit
end
