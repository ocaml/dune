(** Initialize dune components *)
open! Stdune

(** Supported kinds of components for initialization *)
module Kind : sig
  type t =
    | Executable
    | Library
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
    type common =
      { name: string
      ; libraries: string list
      ; pps: string list
      }

    type executable =
      { public: string option
      }

    type library =
      { public: string option
      ; inline_tests: bool
      }

    (** NOTE: no options supported yet *)
    type test = ()

    type 'a t =
      { context : Init_context.t
      ; common : common
      ; options : 'a
      }
  end

  type 'options t =
    | Executable : Options.executable Options.t -> Options.executable t
    | Library : Options.library Options.t -> Options.library t
    | Test : Options.test Options.t -> Options.test t

  (** Create or update the component specified by the ['options t],
      where ['options] is *)
  val init : 'options t -> unit
end

val validate_component_name : string -> unit
val print_completion : Kind.t -> string -> unit
