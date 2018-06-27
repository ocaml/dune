(** Configuration parameters *)

open! Import

(** Local installation directory *)
val local_install_dir : context:string -> Path.t

val local_install_bin_dir : context:string -> Path.t
val local_install_man_dir : context:string -> Path.t
val local_install_lib_dir : context:string -> package:string -> Path.t

val dev_null : Path.t

(** When this file is present in a directory jbuilder will delete
    nothing in it if it knows to generate this file. *)
val jbuilder_keep_fname : string

(** Are we running inside an emacs shell? *)
val inside_emacs : bool

(** Are we running insinde Dune? *)
val inside_dune : bool

val default_build_profile : string

(** Jbuilder configuration *)

module Display : sig
  type t =
    | Progress (** Single interactive status line *)
    | Short    (** One line per command           *)
    | Verbose  (** Display all commands fully     *)
    | Quiet    (** Only display errors            *)

  val t : t Sexp.Of_sexp.t
  val all : (string * t) list
end

module Concurrency : sig
  type t =
    | Fixed of int
    | Auto

  val of_string : string -> (t, string) result
  val to_string : t -> string
end

module type S = sig
  type 'a field

  type t =
    { display     : Display.t     field
    ; concurrency : Concurrency.t field
    }
end

include S with type 'a field = 'a

module Partial : S with type 'a field := 'a option

val t : t Sexp.Of_sexp.t

val merge : t -> Partial.t -> t

val default : t
val user_config_file : Path.t
val load_user_config_file : unit -> t
val load_config_file : Path.t -> t

(** Set display mode to [Quiet] if it is [Progress], the output is not
    a tty and we are not running inside emacs. *)
val adapt_display : t -> output_is_a_tty:bool -> t
