(** Handling of substitutions in artifacts during promotion and installation *)

open Import

type configpath =
  | Sourceroot
  | Stdlib

(** A symbolic representation of the value to substitute to *)
type t =
  | Vcs_describe of Path.Source.t
  | Location of Section.t * Package.Name.t
  | Configpath of configpath
  | Hardcoded_ocaml_path
  | Repeat of int * string
      (** [Repeat (n, s)] evaluates to [s] repeated [n] times. This substitution
          is used for unit tests. *)

type hardcoded_ocaml_path =
  | Hardcoded of Path.t list
  | Relocatable of Path.t

type conf = private
  { get_vcs : Path.Source.t -> Vcs.t option Memo.t
  ; get_location : Section.t -> Package.Name.t -> Path.t
  ; get_config_path : configpath -> Path.t option
  ; hardcoded_ocaml_path : hardcoded_ocaml_path
        (** Initial prefix of installation when relocatable chosen *)
  ; sign_hook : (Path.t -> unit Fiber.t) option Lazy.t
        (** Called on binary after if has been edited *)
  }

val conf_of_context : Context.t option -> conf

val conf_for_install :
     relocatable:Path.t option
  -> default_ocamlpath:Path.t list
  -> stdlib_dir:Path.t
  -> roots:Path.t Install.Section.Paths.Roots.t
  -> context:Context.t
  -> conf

val conf_dummy : conf

val to_dyn : t -> Dyn.t

(** A string encoding of a substitution. The resulting string is what should be
    written inside generated source files. {!copy_file} recognise such strings
    and expand them.

    The resulting string is guaranteed to be of length at least [min_len], which
    defaults to [0]. *)
val encode : ?min_len:int -> t -> string

(** [decode s] returns the value [t] such that [encode t = s]. *)
val decode : string -> t option

(** Copy a file, performing all required substitutions. The operation is atomic,
    i.e., the contents is first copied to a temporary file in the same directory
    and then atomically renamed to [dst]. *)
val copy_file :
     conf:conf
  -> ?chmod:(int -> int)
  -> ?delete_dst_if_it_is_a_directory:bool
  -> src:Path.t
  -> dst:Path.t
  -> unit
  -> unit Fiber.t

type status =
  | Some_substitution
  | No_substitution

(** Generic version of [copy_file]. Rather than filenames, it takes an input and
    output functions. Their semantic must match the ones of the [input] and
    [output] functions from the OCaml standard library.

    [input_file] is used only for debugging purposes. It must be the name of the
    source file.

    Return whether a substitution happened. *)
val copy :
     conf:conf
  -> input_file:Path.t
  -> input:(Bytes.t -> int -> int -> int)
  -> output:(Bytes.t -> int -> int -> unit)
  -> status Fiber.t

(** Produce the string that would replace the placeholder with the given value .*)
val encode_replacement : len:int -> repl:string -> string

(** Test if a file contains a substitution placeholder. *)
val test_file : src:Path.t -> unit -> status Fiber.t
