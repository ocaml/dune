(* Build engine API

   This module exposes all of the core operations and modules that rules can use
   to manipulate the build engine.

   The api is collected into a single file to avoid exposing the internal
   [Dune_engine] types and modules to consumers, while remaining transparent
   within the API layer itself. Spiritually, each unique module in this file
   represents an interface over some concept in [Dune_engine], most typically a
   particular module from that library, and should be considered the equivalent
   of separate "files" (in the project-management sense) themselves.

   One reasonable way to read this file would be to find a module in
   [Dune_engine] that appears useful, then use a code searching tool of your
   choice to find the corresponding module in this file. *)

(* CR-someday cwong: It might be a good idea to separate these modules into
   different files and cat them together (or something somewhat more
   sophisticated) to avoid the problems that come with having such a gigantic
   file. Such an approach would probably need something special (probably some
   first-class support within dune itself?) to make error reporting at all
   reasonable. *)

open Stdune

(* CR cwong: get rid of this *)
module Import : sig
  module Log = Dune_util.Log
  module Re = Dune_re
  module Stringlike = Dune_util.Stringlike
  module Stringlike_intf = Dune_util.Stringlike_intf

  val initial_cwd : string

  val protect : f:(unit -> 'a) -> finally:(unit -> unit) -> 'a

  val protectx : 'a -> f:('a -> 'b) -> finally:('a -> unit) -> 'b

  type fail = { fail : 'a. unit -> 'a }

  (* Disable file operations to force to use the IO module *)
  val open_in : [> `Use_Io ]

  val open_in_bin : [> `Use_Io ]

  val open_in_gen : [> `Use_Io ]

  val open_out : [> `Use_Io ]

  val open_out_bin : [> `Use_Io ]

  val open_out_gen : [> `Use_Io ]

  (* We open this module at the top of module generating rules, to make sure
     they don't do Io manually *)
  module No_io : sig
    module Io : sig end
  end
end

module Context_name : sig
  open! Import

  type t

  val is_default : t -> bool

  val build_dir : t -> Path.Build.t

  val default : t

  val hash : t -> int

  val target : t -> toolchain:t -> t

  val equal : t -> t -> bool

  include Import.Stringlike_intf.S with type t := t

  module Map : Map.S with type key = t

  module Set : Set.S with type elt = t

  module Top_closure :
    Top_closure_intf.S with type key := t and type 'a monad := 'a Monad.Id.t
end

module Dpath : sig
  (* type t = ... *)
  include Dune_lang.Conv.S with type t = Path.t

  module Target_dir : sig
    type context_related =
      | Root
      | With_context of Context_name.t * Path.Source.t

    val build_dir : context_related -> Path.Build.t

    type t =
      | Install of context_related
      | Alias of context_related
      | Regular of context_related
      | Invalid of Path.Build.t

    val of_target : Path.Build.t -> t
  end

  type target_kind =
    | Regular of Context_name.t * Path.Source.t
    | Alias of Context_name.t * Path.Source.t
    | Install of Context_name.t * Path.Source.t
    | Other of Path.Build.t

  type 'build path_kind =
    | Source of Path.Source.t
    | External of Path.External.t
    | Build of 'build

  val describe_path : Path.t -> string

  (** Return the name of an alias from its stamp file *)
  val analyse_target : Path.Build.t -> target_kind

  val analyse_path : Path.t -> target_kind path_kind

  val analyse_dir : Path.t -> Target_dir.t path_kind

  module Local : sig
    val encode : dir:Path.t -> Path.t Dune_lang.Encoder.t

    val decode : dir:Path.t -> Path.t Dune_lang.Decoder.t
  end

  module External : sig
    val encode : Path.External.t Dune_lang.Encoder.t

    val decode : Path.External.t Dune_lang.Decoder.t
  end

  module Build : sig
    include Dune_lang.Conv.S with type t = Path.Build.t

    val is_dev_null : t -> bool

    val install_dir : t

    val alias_dir : t

    val is_alias_stamp_file : t -> bool
  end
end

module Stanza : sig
  (** Stanza in dune/jbuild files *)

  type t = ..

  val latest_version : Dune_lang.Syntax.Version.t

  module Parser : sig
    (** Type of stanza parser.

        Each stanza in a configuration file might produce several values of type
        [t], hence the [t list] here. *)
    type nonrec t = string * t list Dune_lang.Decoder.t
  end

  (** Syntax identifier for the Dune language. [(0, X)] correspond to the Jbuild
      language while versions from [(1, 0)] correspond to the Dune one. *)
  val syntax : Dune_lang.Syntax.t
end

module Alias : sig
  module Name : sig
    type t

    val decode : t Dune_lang.Decoder.t

    val of_string : string -> t

    val parse_string_exn : Loc.t * string -> t

    val to_string : t -> string

    val to_dyn : t -> Dyn.t

    val default : t

    val runtest : t

    val install : t

    val all : t

    val parse_local_path : Loc.t * Path.Local.t -> Path.Local.t * t

    module Map : Map.S with type key = t

    module Set : Set.S with type elt = t
  end

  type t

  val equal : t -> t -> bool

  val hash : t -> int

  val compare : t -> t -> Ordering.t

  val make : Name.t -> dir:Path.Build.t -> t

  (** The following always holds: [make (name t) ~dir:(dir t) = t] *)
  val name : t -> Name.t

  val dir : t -> Path.Build.t

  val stamp_file_dir : t -> Path.Build.t

  val to_dyn : t -> Dyn.t

  val encode : t Dune_lang.Encoder.t

  val of_user_written_path : loc:Loc.t -> Path.t -> t

  val fully_qualified_name : t -> Path.Build.t

  val default : dir:Path.Build.t -> t

  val runtest : dir:Path.Build.t -> t

  val install : dir:Path.Build.t -> t

  val doc : dir:Path.Build.t -> t

  val private_doc : dir:Path.Build.t -> t

  val lint : dir:Path.Build.t -> t

  val all : dir:Path.Build.t -> t

  val check : dir:Path.Build.t -> t

  val fmt : dir:Path.Build.t -> t

  (** Return the underlying stamp file *)
  val stamp_file : t -> Path.Build.t

  val is_standard : Name.t -> bool

  val suffix : string
end

module Predicate : sig
  (** Predicates are functions from 'a -> bool along with a uniquely identifying
      string. The uniquely identifying string allows us to safely memoize on the
      predicate *)

  type 'a t

  val equal : 'a t -> 'a t -> bool

  val compare : 'a t -> 'a t -> Ordering.t

  val hash : _ t -> int

  val encode : _ t Dune_lang.Encoder.t

  val to_dyn : _ t -> Dyn.t

  (**[create id ~f] creates a predicate defined by [f] identified uniquely with
     [id]. [id] is used to safely compare predicates for equality for
     memoization *)
  val create : id:Dyn.t Lazy.t -> f:('a -> bool) -> 'a t

  (** The predicate that evaluates to [true] for any query. *)
  val true_ : _ t

  (** The predicate that evaluates to [false] for any query. *)
  val false_ : _ t

  val test : 'a t -> 'a -> bool

  (** the user of this function must take care not to break the uniqueness of
      the underlying representation *)
  val contramap : 'a t -> f:('b -> 'a) -> map_id:(Dyn.t -> Dyn.t) -> 'b t
end

module File_selector : sig
  (** A File_selector.t is a predicate that is to be evaluated in a particular
      directory *)

  type t

  val dir : t -> Path.t

  val create : dir:Path.t -> string Predicate.t -> t

  val equal : t -> t -> bool

  val hash : t -> int

  val compare : t -> t -> Ordering.t

  val encode : t Dune_lang.Encoder.t

  (** [to_dyn] is used as a marshallable representation of [t] (to compute
      digests), so it must be injective *)
  val to_dyn : t -> Dyn.t

  val test : t -> Path.t -> bool
end

module Cram_test : sig
  type t =
    | File of Path.Source.t
    | Dir of
        { file : Path.Source.t
        ; dir : Path.Source.t
        }

  val is_cram_suffix : string -> bool

  val dyn_of_t : t -> Dyn.t

  val name : t -> string

  val script : t -> Path.Source.t
end

module Dune_lexer : sig
  (** Returns [true] if the input starts with "(* -*- tuareg -*- *)" *)
  val is_script : Lexing.lexbuf -> bool

  val eof_reached : Lexing.lexbuf -> bool
end

module Clflags : sig
  (** Command line flags *)

  (** Print dependency path in case of error *)
  val debug_dep_path : bool ref

  (** Debug the findlib implementation *)
  val debug_findlib : bool ref

  (** The command line for "Hint: try: dune external-lib-deps ..." *)
  val external_lib_deps_hint : string list ref

  val external_lib_deps_mode : bool ref

  (** Capture the output of sub-commands *)
  val capture_outputs : bool ref

  (** Always print backtraces, to help debugging dune itself *)
  val debug_backtraces : bool -> unit

  (** Print debug info about artifact substitution *)
  val debug_artifact_substitution : bool ref

  (** Command to use to diff things *)
  val diff_command : string option ref

  module Promote : sig
    type t =
      | Automatically
      | Never
  end

  (** explicit promotion mode is set *)
  val promote : Promote.t option ref

  (** Force re-running actions associated to aliases *)
  val force : bool ref

  (** Instead of terminating build after completion, watch for changes *)
  val watch : bool ref

  (** Do not print "Entering directory" messages *)
  val no_print_directory : bool ref

  (** Store original source directory in dune-package metadata *)
  val store_orig_src_dir : bool ref

  (** Always show full command on error *)
  val always_show_command_line : bool ref

  (** Promote the generated [<package>.install] files to the source tree *)
  val promote_install_files : bool ref

  (** Wether we are ignorimg rules with [(mode promote)] *)
  val ignore_promoted_rules : bool ref
end

module Opam_file : sig
  (** Parsing and interpretation of opam files *)

  open OpamParserTypes

  (** Type of opam files *)
  type t = opamfile

  (** Load a file *)
  val load : Path.t -> t

  (** Extracts a field *)
  val get_field : t -> string -> value option

  (** Parse the contents of an opam file *)
  val parse : Lexing.lexbuf -> t

  (** Parse just a value *)
  val parse_value : Lexing.lexbuf -> value

  (** Replace all [pos] value by a triplet [(fname, line, absolute_offset)] *)
  val absolutify_positions : file_contents:string -> opamfile -> opamfile

  val nopos : OpamParserTypes.pos

  val existing_variables : t -> String.Set.t

  module Create : sig
    open OpamParserTypes

    val string : string -> value

    val list : ('a -> value) -> 'a list -> value

    val string_list : string list -> value

    val normalise_field_order : (string * value) list -> (string * value) list

    val of_bindings : (string * value) list -> file:Path.t -> t
  end
end

module Section : sig
  type t =
    | Lib
    | Lib_root
    | Libexec
    | Libexec_root
    | Bin
    | Sbin
    | Toplevel
    | Share
    | Share_root
    | Etc
    | Doc
    | Stublibs
    | Man
    | Misc

  val compare : t -> t -> Ordering.t

  include Comparable_intf.S with type Key.t = t

  val enum_decoder : (string * t) list

  val all : Set.t

  val to_string : t -> string

  val of_string : string -> t option

  val parse_string : string -> (t, string) Result.t

  val decode : t Dune_lang.Decoder.t

  val encode : t Dune_lang.Encoder.t

  val to_dyn : t -> Dyn.t

  (** [true] iff the executable bit should be set for files installed in this
      location. *)
  val should_set_executable_bit : t -> bool

  module Site : sig
    type t

    include Interned_intf.S with type t := t

    include Dune_lang.Conv.S with type t := t

    module Infix : Comparator.OPS with type t = t

    include Import.Stringlike_intf.S with type t := t
  end

  val dune_site_syntax : Dune_lang.Syntax.t

  module Modulelike (S : sig
    type t

    (** The name of the module, for use in error messages. For example
        ["Lib_name"], ["Context_name"]. *)
    val module_ : string

    (** A short description of the type, for use in user-facing error messages.
        For example "context name", "library name". *)
    val description : string

    val to_string : t -> string

    (** The string is always a correct module name, except not capitalized *)
    val make : string -> t
  end) : Import.Stringlike_intf.S with type t = S.t

  val valid_format_doc : User_message.Style.t Pp.t
end

module Package : sig
  (** Information about a package defined in the workspace *)

  module Name : sig
    type t

    val opam_fn : t -> string

    val version_fn : t -> string

    include Interned_intf.S with type t := t

    include Dune_lang.Conv.S with type t := t

    module Infix : Comparator.OPS with type t = t

    include Import.Stringlike_intf.S with type t := t

    val of_opam_file_basename : string -> t option
  end

  module Id : sig
    type t

    val name : t -> Name.t

    module Set : Set.S with type elt = t

    module Map : Map.S with type key = t
  end

  module Dependency : sig
    module Op : sig
      type t =
        | Eq
        | Gte
        | Lte
        | Gt
        | Lt
        | Neq
    end

    module Constraint : sig
      module Var : sig
        type t =
          | QVar of string
          | Var of string
      end

      type t =
        | Bvar of Var.t
        | Uop of Op.t * Var.t
        | Bop of Op.t * Var.t * Var.t
        | And of t list
        | Or of t list
    end

    type t =
      { name : Name.t
      ; constraint_ : Constraint.t option
      }

    val opam_depend : t -> OpamParserTypes.value

    val to_dyn : t -> Dyn.t

    val decode : t Dune_lang.Decoder.t
  end

  module Source_kind : sig
    module Host : sig
      type kind =
        | Github
        | Bitbucket
        | Gitlab

      type t =
        { user : string
        ; repo : string
        ; kind : kind
        }

      val homepage : t -> string
    end

    type t =
      | Host of Host.t
      | Url of string

    val to_dyn : t Dyn.Encoder.t

    val to_string : t -> string

    val decode : t Dune_lang.Decoder.t
  end

  module Info : sig
    type t

    val source : t -> Source_kind.t option

    val license : t -> string option

    val authors : t -> string list option

    val homepage : t -> string option

    val bug_reports : t -> string option

    val documentation : t -> string option

    val maintainers : t -> string list option

    val empty : t

    val to_dyn : t Dyn.Encoder.t

    val decode :
         ?since:Dune_lang.Syntax.Version.t
      -> unit
      -> t Dune_lang.Decoder.fields_parser

    val superpose : t -> t -> t
  end

  type t =
    { id : Id.t
    ; loc : Loc.t
    ; synopsis : string option
    ; description : string option
    ; depends : Dependency.t list
    ; conflicts : Dependency.t list
    ; depopts : Dependency.t list
    ; info : Info.t
    ; version : string option
    ; has_opam_file : bool
    ; tags : string list
    ; deprecated_package_names : Loc.t Name.Map.t
    ; sites : Section.t Section.Site.Map.t
    }

  val name : t -> Name.t

  val dir : t -> Path.Source.t

  val file : dir:Path.t -> name:Name.t -> Path.t

  val decode : dir:Path.Source.t -> t Dune_lang.Decoder.t

  val opam_file : t -> Path.Source.t

  val meta_file : t -> Path.Source.t

  val deprecated_meta_file : t -> Name.t -> Path.Source.t

  val to_dyn : t -> Dyn.t

  val hash : t -> int

  val is_opam_file : Path.t -> bool

  (** Construct a package description from an opam file. *)
  val load_opam_file : Path.Source.t -> Name.t -> t

  val missing_deps : t -> effective_deps:Name.Set.t -> Name.Set.t
end

module Sandbox_mode : sig
  (** How to sandbox actions *)

  (** This module describes the method used to sandbox actions. Choices include:

      - not sandboxing - sandboxing by symlinking dependencies - sandboxing by
        copying dependencies *)

  type some =
    | Symlink
    | Copy

  type t = some option

  val compare : t -> t -> Ordering.t

  val equal : t -> t -> bool

  module Dict : sig
    type key = t

    type 'a t =
      { none : 'a
      ; symlink : 'a
      ; copy : 'a
      }

    val compare : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t

    val of_func : (key -> 'a) -> 'a t

    val get : 'a t -> key -> 'a
  end

  module Set : sig
    type key = t

    type t = bool Dict.t

    val equal : t -> t -> bool

    val compare : t -> t -> Ordering.t

    val of_func : (key -> bool) -> t

    val mem : t -> key -> bool

    val inter : t -> t -> t
  end

  val all : t list

  val none : t

  val symlink : t

  val copy : t

  val of_string : string -> (t, string) Result.t

  val to_string : t -> string
end

module Config : sig
  (** Configuration parameters *)

  open! Import

  (** Local installation directory *)
  val local_install_dir : context:Context_name.t -> Path.Build.t

  val local_install_lib_root : context:Context_name.t -> Path.Build.t

  val local_install_bin_dir : context:Context_name.t -> Path.Build.t

  val local_install_man_dir : context:Context_name.t -> Path.Build.t

  val local_install_lib_dir :
    context:Context_name.t -> package:Package.Name.t -> Path.Build.t

  val dev_null : Path.t

  (** When this file is present in a directory dune will delete nothing in it if
      it knows to generate this file. *)
  val dune_keep_fname : string

  (** Are we running inside an emacs shell? *)
  val inside_emacs : bool

  (** Are we running inside Dune? *)
  val inside_dune : bool

  (** Are we running in CI?. This checks the CI environment variable which is
      supported by travis, gitlab.*)
  val inside_ci : bool

  val show_full_command_on_error : unit -> bool

  (** Dune configuration *)

  module Terminal_persistence : sig
    type t =
      | Preserve
      | Clear_on_rebuild

    val all : (string * t) list

    val of_string : string -> (t, string) result

    val to_string : t -> string

    val decode : t Dune_lang.Decoder.t
  end

  module Display : sig
    type t =
      | Progress  (** Single interactive status line *)
      | Short  (** One line per command *)
      | Verbose  (** Display all commands fully *)
      | Quiet  (** Only display errors *)

    val decode : t Dune_lang.Decoder.t

    val all : (string * t) list

    (** The console backend corresponding to the selected display mode *)
    val console_backend : t -> Console.Backend.t
  end

  module Concurrency : sig
    type t =
      | Fixed of int
      | Auto

    val of_string : string -> (t, string) result

    val to_string : t -> string
  end

  module Sandboxing_preference : sig
    type t = Sandbox_mode.t list
  end

  module Caching : sig
    module Mode : sig
      type t =
        | Disabled
        | Enabled

      val all : (string * t) list

      val decode : t Dune_lang.Decoder.t

      val to_string : t -> string
    end

    module Transport : sig
      type t =
        | Daemon
        | Direct

      val all : (string * t) list

      val decode : t Dune_lang.Decoder.t
    end

    module Duplication : sig
      type t = Cache.Duplication_mode.t option

      val all : (string * t) list

      val decode : t Dune_lang.Decoder.t
    end
  end

  module type S = sig
    type 'a field

    type t =
      { display : Display.t field
      ; concurrency : Concurrency.t field
      ; terminal_persistence : Terminal_persistence.t field
      ; sandboxing_preference : Sandboxing_preference.t field
      ; cache_mode : Caching.Mode.t field
      ; cache_transport : Caching.Transport.t field
      ; cache_check_probability : float field
      ; cache_duplication : Caching.Duplication.t field
      ; cache_trim_period : int field
      ; cache_trim_size : int64 field
      }
  end

  include S with type 'a field = 'a

  module Partial : S with type 'a field := 'a option

  val decode : t Dune_lang.Decoder.t

  val merge : t -> Partial.t -> t

  val default : t

  val user_config_file : Path.t

  val load_user_config_file : unit -> t

  val load_config_file : Path.t -> t

  (** Set display mode to [Quiet] if it is [Progress], the output is not a tty
      and we are not running inside emacs. *)
  val adapt_display : t -> output_is_a_tty:bool -> t

  (** The global configuration for the process *)
  val t : unit -> t

  (** Initialises the configuration for the process *)
  val init : t -> unit

  val to_dyn : t -> Dyn.t
end

module Diff : sig
  (** Representation of (diff ...) actions *)

  module Mode : sig
    type t =
      | Binary  (** no diffing, just raw comparison *)
      | Text  (** diffing after newline normalization *)
  end

  type ('path, 'target) t =
    { optional : bool
    ; mode : Mode.t
    ; file1 : 'path
    ; file2 : 'target
    }

  val decode :
       'path Dune_lang.Decoder.t
    -> 'target Dune_lang.Decoder.t
    -> optional:bool
    -> ('path, 'target) t Dune_lang.Decoder.t

  val decode_binary :
       'path Dune_lang.Decoder.t
    -> 'target Dune_lang.Decoder.t
    -> ('path, 'target) t Dune_lang.Decoder.t

  val eq_files : (Path.t, Path.Build.t) t -> bool
end

module Glob : sig
  type t

  val equal : t -> t -> bool

  val compare : t -> t -> Ordering.t

  val hash : t -> int

  val to_dyn : t Dyn.Encoder.t

  val encode : t Dune_lang.Encoder.t

  val decode : t Dune_lang.Decoder.t

  val test : t -> string -> bool

  val filter : t -> string list -> string list

  val empty : t

  val universal : t

  val of_string_exn : Loc.t -> string -> t

  val to_pred : t -> string Predicate.t
end

module Predicate_lang : sig
  (** DSL to define sets that are defined by a membership : 'a -> bool function. *)

  type 'a t =
    | Element of 'a
    | Compl of 'a t
    | Standard
    | Union of 'a t list
    | Inter of 'a t list

  val diff : 'a t -> 'a t -> 'a t

  val inter : 'a t list -> 'a t

  val compl : 'a t -> 'a t

  val union : 'a t list -> 'a t

  val not_union : 'a t list -> 'a t

  val any : 'a t

  val decode_one : 'a Dune_lang.Decoder.t -> 'a t Dune_lang.Decoder.t

  val decode : 'a Dune_lang.Decoder.t -> 'a t Dune_lang.Decoder.t

  val encode : 'a Dune_lang.Encoder.t -> 'a t Dune_lang.Encoder.t

  val to_dyn : 'a Dyn.Encoder.t -> 'a t Dyn.Encoder.t

  val exec : 'a t -> standard:'a t -> ('a -> bool) -> bool

  val empty : 'a t

  module Glob : sig
    type glob

    type nonrec t = glob t

    val to_dyn : t -> Dyn.t

    val decode : t Dune_lang.Decoder.t

    val exec : t -> standard:t -> string -> bool

    val filter : t -> standard:t -> string list -> string list

    val of_glob : Glob.t -> t

    val of_pred : (string -> bool) -> t

    val of_string_set : String.Set.t -> t

    val true_ : t
  end
end

module Action_plugin : sig
  val syntax : Dune_lang.Syntax.t
end

(* Only exposed for the sake of [Action_ast] below. Use [Action.Inputs] and
   [Action.Outputs] instead. *)
module Action_outputs : sig
  type t =
    | Stdout
    | Stderr
    | Outputs  (** Both Stdout and Stderr *)
end

module Action_inputs : sig
  type t = Stdin
end

(* Not to be confused with [Dune_engine.Action_ast]. If you are consuming the
   build api, you almost certainly don't want to be using this signature -- see
   [Action], and the various other [Action_] modules in this file.

   This module abstracts the types [program], [path], [target] and [string] so
   as to share the same structure at differing points in the build process. For
   example *)
module type Action_ast = sig
  type program

  type path

  type target

  type string

  type t =
    | Run of program * string list
    | With_accepted_exit_codes of int Predicate_lang.t * t
    | Dynamic_run of program * string list
    | Chdir of path * t
    | Setenv of string * string * t
    (* It's not possible to use a build path here since jbuild supports
       redirecting to /dev/null. In [dune] files this is replaced with %{null} *)
    | Redirect_out of Action_outputs.t * target * t
    | Redirect_in of Action_inputs.t * path * t
    | Ignore of Action_outputs.t * t
    | Progn of t list
    | Echo of string list
    | Cat of path
    | Copy of path * target
    | Symlink of path * target
    | Copy_and_add_line_directive of path * target
    | System of string
    | Bash of string
    | Write_file of target * string
    | Rename of target * target
    | Remove_tree of target
    | Mkdir of path
    | Digest_files of path list
    | Diff of (path, target) Diff.t
    | Merge_files_into of path list * string list * target
    | No_infer of t
    | Pipe of Action_outputs.t * t list
    | Format_dune_file of Dune_lang.Syntax.Version.t * path * target
    | Cram of path
end

module Action_mapper : sig
  (** Create generic mappings between action ast's *)

  module Make (Src : Action_ast) (Dst : Action_ast) : sig
    type map =
         Src.t
      -> dir:Src.path
      -> f_program:(dir:Src.path -> Src.program -> Dst.program)
      -> f_string:(dir:Src.path -> Src.string -> Dst.string)
      -> f_path:(dir:Src.path -> Src.path -> Dst.path)
      -> f_target:(dir:Src.path -> Src.target -> Dst.target)
      -> Dst.t

    val map_one_step : map -> map

    val map : map
  end
end

module Value : sig
  type t =
    | String of string
    | Dir of Path.t
    | Path of Path.t

  val compare : t -> t -> Ordering.t

  val to_dyn : t -> Dyn.t

  val to_string : t -> dir:Path.t -> string

  val to_path : ?error_loc:Loc.t -> t -> dir:Path.t -> Path.t

  module L : sig
    val strings : string list -> t list

    (** [compare_vals ~dir a b] is a more efficient version of:

        {[
          List.compare ~compare:String.compare (to_string ~dir a)
            (to_string ~dir b)
        ]} *)
    val compare_vals : dir:Path.t -> t list -> t list -> Ordering.t

    val paths : Path.t list -> t list

    val deps_only : t list -> Path.t list

    val dirs : Path.t list -> t list

    val concat : t list -> dir:Path.t -> string

    val to_strings : t list -> dir:Path.t -> string list
  end
end

module String_with_vars : sig
  (** String with variables of the form %\{...\} or %(...)

      Variables cannot contain "%\{", "%(", ")" or "\}". For instance in "%(cat
      %\{x\})", only "%\{x\}" will be considered a variable, the rest is text. *)

  (** A sequence of text and variables. *)
  type t

  val compare_no_loc : t -> t -> Ordering.t

  val equal_no_loc : t -> t -> bool

  (** [loc t] returns the location of [t] — typically, in the [dune] file. *)
  val loc : t -> Loc.t

  val syntax_version : t -> Dune_lang.Syntax.Version.t

  val to_dyn : t Dyn.Encoder.t

  include Dune_lang.Conv.S with type t := t

  (** [t] generated by the OCaml code. The first argument should be [__POS__].
      [quoted] says whether the string is quoted ([false] by default). *)
  val virt_var : ?quoted:bool -> string * int * int * int -> string -> t

  val virt_text : string * int * int * int -> string -> t

  val make_var : ?quoted:bool -> Loc.t -> ?payload:string -> string -> t

  val make_text : ?quoted:bool -> Loc.t -> string -> t

  val make : Dune_lang.Template.t -> t

  val is_var : t -> name:string -> bool

  val has_vars : t -> bool

  (** If [t] contains no variable, returns the contents of [t]. *)
  val text_only : t -> string option

  module Mode : sig
    (** Expansion may produce either a [Single] value or [Many] values

        The caller always knows which of the contexts above it requires,
        therefore it can specify this to the expansion functions. This allows us
        to return a precise result type from the expansion, and do some
        validation to make sure we aren't expanding into multiple values in
        cases where it's not allowed. *)
    type _ t =
      | Single : Value.t t
      | Many : Value.t list t
  end

  module Var : sig
    (** Variables are of the form %\{foo\} or %\{foo:bar\}. The latter form is
        also referred to as macros. *)
    type t

    val to_dyn : t -> Dyn.t

    val name : t -> string

    val loc : t -> Loc.t

    val full_name : t -> string

    (** Variables do not have a payload. While macros always do. *)
    val payload : t -> string option

    val with_name : t -> name:string -> t

    (** Describe what this variable is *)
    val describe : t -> string
  end

  type yes_no_unknown =
    | Yes
    | No
    | Unknown of Var.t

  module Partial : sig
    type string_with_vars

    (** Result of a best effort expansion. If we managed to expand everything we
        return some ['a Mode.t t], otherwise we return a new template where all
        the variables we know about are expanded. *)
    type nonrec 'a t =
      | Expanded of 'a
      | Unexpanded of t

    val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val is_suffix : string t -> suffix:string -> yes_no_unknown

    val is_prefix : string t -> prefix:string -> yes_no_unknown

    val elim : 'a t -> exp:('a -> 'b) -> unexp:(string_with_vars -> 'b) -> 'b

    val expanded : 'a -> 'a t
  end
  with type string_with_vars := t

  type known_suffix =
    | Full of string
    | Partial of (Var.t * string)

  type known_prefix =
    | Full of string
    | Partial of (string * Var.t)

  val known_suffix : t -> known_suffix

  val known_prefix : t -> known_prefix

  val is_suffix : t -> suffix:string -> yes_no_unknown

  val is_prefix : t -> prefix:string -> yes_no_unknown

  val fold_vars : t -> init:'a -> f:(Var.t -> 'a -> 'a) -> 'a

  type 'a expander = Var.t -> Dune_lang.Syntax.Version.t -> 'a

  module type S = sig
    type 'a app

    (** [expand ~f] attempts to expand all percent forms in a template. If [f]
        returns [None] for any variable (no substitution was found), then this
        function will raise. *)
    val expand :
         t
      -> mode:'a Mode.t
      -> dir:Path.t
      -> f:Value.t list option app expander
      -> 'a app

    (** [partial_expand] does a best effort expansion of the template. If it
        fails to expand any variables, it will return [Unexpanded t] where [t]
        is the maximally expanded template. If it manages to expand everything
        [Expanded] will be returned. *)
    val partial_expand :
         t
      -> mode:'a Mode.t
      -> dir:Path.t
      -> f:Value.t list option app expander
      -> 'a Partial.t app
  end

  include S with type 'a app := 'a

  val remove_locs : t -> t
end

module Action_dune_lang : sig
  (* This module is to be used in Dune_file. It should not introduce any
     dependencies unless they're already dependencies of Dune_file *)

  (* these are included to allow this to be used with [Action_mapper] *)
  type program = String_with_vars.t

  type string = String_with_vars.t

  type path = String_with_vars.t

  type target = String_with_vars.t

  (* type t = ... *)
  include
    Action_ast
      with type program := String_with_vars.t
       and type string := String_with_vars.t
       and type path := String_with_vars.t
       and type target := String_with_vars.t

  include Dune_lang.Conv.S with type t := t

  (** Raises User_error on invalid action. *)
  val validate : loc:Loc.t -> t -> unit

  val compare_no_locs : t -> t -> Ordering.t

  val to_dyn : t -> Dyn.t

  val remove_locs : t -> t
end

module Ml_kind : sig
  type t =
    | Impl
    | Intf

  val all : t list

  val choose : t -> impl:'a -> intf:'a -> 'a

  (** "" or "i" *)
  val suffix : t -> string

  val to_string : t -> string

  val to_dyn : t -> Dyn.t

  val cmt_ext : t -> string

  module Dict : sig
    type kind = t

    type 'a t =
      { impl : 'a
      ; intf : 'a
      }

    val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t

    val get : 'a t -> kind -> 'a

    val of_func : (ml_kind:kind -> 'a) -> 'a t

    val make_both : 'a -> 'a t

    val iteri : 'a t -> f:(kind -> 'a -> unit) -> unit

    val make : impl:'a -> intf:'a -> 'a t

    val mapi : 'a t -> f:(kind -> 'a -> 'b) -> 'b t

    val map : 'a t -> f:('a -> 'b) -> 'b t
  end
  with type kind := t
end

module Dialect : sig
  (** Dialects

      A dialect is an alternative frontend to OCaml (such as ReasonML). It is
      described by a pair of file extensions, one corresponding to interfaces
      and one to implementations.

      The extensions are unique among all dialects of a given project, so that a
      given extension can be mapped back to the corresponding dialect.

      A dialect can use the standard OCaml syntax or it can specify an action to
      convert from a custom syntax to a binary OCaml syntax.

      Similarly, a dialect can specify a custom formatter to implement the \@fmt
      alias.

      When not using a custom syntax or formatting action, a dialect is nothing
      but a way to specify custom file extensions for OCaml code. *)

  type t

  val name : t -> string

  val to_dyn : t -> Dyn.t

  val decode : t Dune_lang.Decoder.t

  val extension : t -> Ml_kind.t -> string

  val preprocess : t -> Ml_kind.t -> (Loc.t * Action_dune_lang.t) option

  val format :
    t -> Ml_kind.t -> (Loc.t * Action_dune_lang.t * string list) option

  val ocaml : t

  val reason : t

  val ml_suffix : t -> Ml_kind.t -> string option

  module DB : sig
    type dialect

    type t

    val empty : t

    val add : t -> loc:Loc.t -> dialect -> t

    val find_by_name : t -> string -> dialect option

    val find_by_extension : t -> string -> (dialect * Ml_kind.t) option

    val fold : t -> init:'a -> f:(dialect -> 'a -> 'a) -> 'a

    val to_dyn : t -> Dyn.t

    val builtin : t
  end
  with type dialect := t
end

module Format_config : sig
  (** Represent the [(formatting)] field in [dune-project] files *)

  module Language : sig
    (** Dune can format either source files through external programs (ocaml and
        reason are builtin dialects) or dune files *)
    type t =
      | Dialect of string
      | Dune
  end

  type t

  val of_config :
       ext:t option
    -> dune_lang:t option
    -> version:Dune_lang.Syntax.Version.t
    -> t

  (** The syntax corresponding to the dune 1.x [(using fmt)] extension. *)
  val syntax : Dune_lang.Syntax.t

  (** Where the configuration was defined. Can be [Loc.none] if formatting is
      done by default. *)
  val loc : t -> Loc.t

  (** Should we emit formatting rules for a particular [language]? *)
  val includes : t -> Language.t -> bool

  val is_empty : t -> bool

  (** Parse arguments for the 1.x extension. *)
  val dparse_args : (t * Stanza.Parser.t list) Dune_lang.Decoder.t

  val to_dyn : t -> Dyn.t

  (** Parse the contents of the dune2 [(formatting)] option.*)
  val field :
    since:Dune_lang.Syntax.Version.t -> t option Dune_lang.Decoder.fields_parser

  val equal : t -> t -> bool
end

module Include_stanza : sig
  type context

  val in_file : Path.Source.t -> context

  val load_sexps :
    context:context -> Loc.t * string -> Dune_lang.Ast.t list * context
end

module Sub_dirs : sig
  module Status : sig
    type t =
      | Data_only
      | Normal
      | Vendored

    val to_dyn : t -> Dyn.t

    module Or_ignored : sig
      type nonrec t =
        | Ignored
        | Status of t
    end

    module Map : sig
      type status

      type 'a t =
        { data_only : 'a
        ; vendored : 'a
        ; normal : 'a
        }

      val merge : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

      val find : 'a t -> status -> 'a

      val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
    end
    with type status := t

    module Set : sig
      type t = bool Map.t

      val all : t

      val normal_only : t
    end
  end

  type subdir_stanzas = (Loc.t * Predicate_lang.Glob.t) option Status.Map.t

  val or_default : subdir_stanzas -> Predicate_lang.Glob.t Status.Map.t

  val default : Predicate_lang.Glob.t Status.Map.t

  type status_map

  val eval :
    Predicate_lang.Glob.t Status.Map.t -> dirs:string list -> status_map

  val status : status_map -> dir:string -> Status.Or_ignored.t

  module Dir_map : sig
    type t

    type per_dir =
      { sexps : Dune_lang.Ast.t list
      ; subdir_status : subdir_stanzas
      }

    val descend : t -> string -> t option

    val sub_dirs : t -> string list

    val merge : t -> t -> t

    val root : t -> per_dir
  end

  val decode : file:Path.Source.t -> Dir_map.t Dune_lang.Decoder.t
end

module Dune_project : sig
  (** dune-project files *)

  module Name : sig
    (** Invariants: - Named s -> s <> "" and s does not contain '.' or '/' -
        Anonymous p -> p is a local path in the source tree *)
    type t = private
      | Named of string
      | Anonymous of Path.Source.t

    val to_dyn : t -> Dyn.t

    val equal : t -> t -> bool

    val compare : t -> t -> Ordering.t

    (** Convert to a string that is suitable for human readable messages *)
    val to_string_hum : t -> string

    (** Convert to/from an encoded string that is suitable to use in filenames *)
    val to_encoded_string : t -> string

    val of_encoded_string : string -> t

    module Infix : Comparator.OPS with type t = t

    module Map : Map.S with type key = t
  end

  module Project_file : sig
    type t

    val to_dyn : t -> Dyn.t
  end

  type t

  module File_key : sig
    (** File_key encodes the project in a unique way to be used as part of file
        path. *)
    type t

    val to_string : t -> string

    val of_string : string -> t

    module Map : Map.S with type key = t
  end

  val to_dyn : t -> Dyn.t

  val file_key : t -> File_key.t

  val packages : t -> Package.t Package.Name.Map.t

  val version : t -> string option

  val name : t -> Name.t

  val root : t -> Path.Source.t

  val stanza_parser : t -> Stanza.t list Dune_lang.Decoder.t

  val generate_opam_files : t -> bool

  val dialects : t -> Dialect.DB.t

  val explicit_js_mode : t -> bool

  val format_config : t -> Format_config.t

  val equal : t -> t -> bool

  val hash : t -> int

  (** Return the path of the project file. *)
  val file : t -> Path.Source.t

  module Lang : sig
    (** [register id stanzas_parser] register a new language. Users will select
        this language by writing:

        {[ (lang <name> <version>) ]}

        as the first line of their [dune-project] file. [stanza_parsers] defines
        what stanzas the user can write in [dune] files. *)
    val register : Dune_lang.Syntax.t -> Stanza.Parser.t list -> unit
  end

  module Extension : sig
    type 'a t

    (** [register id parser] registers a new extension. Users will enable this
        extension by writing:

        {[ (using <name> <version> <args>) ]}

        in their [dune-project] file. [parser] is used to describe what [<args>]
        might be. *)
    val register :
         Dune_lang.Syntax.t
      -> ('a * Stanza.Parser.t list) Dune_lang.Decoder.t
      -> ('a -> Dyn.t)
      -> 'a t

    (** A simple version where the arguments are not used through
        [find_extension_args]. *)
    val register_simple :
      Dune_lang.Syntax.t -> Stanza.Parser.t list Dune_lang.Decoder.t -> unit

    (** Register experimental extensions that were deleted *)
    val register_deleted :
      name:string -> deleted_in:Dune_lang.Syntax.Version.t -> unit
  end

  (** Load a project description from the following directory. [files] is the
      set of files in this directory.

      If [infer_from_opam_files] is true and the directory contains no
      [dune-project] file but contains at least one [>package>.opam] files, then
      a project description is inferred from the opam files. *)
  val load :
       dir:Path.Source.t
    -> files:String.Set.t
    -> infer_from_opam_files:bool
    -> dir_status:Sub_dirs.Status.t
    -> t option

  (** Create an anonymous project with no package rooted at the given directory *)
  val anonymous : dir:Path.Source.t -> t

  (** "dune-project" *)
  val filename : string

  type created_or_already_exist =
    | Created
    | Already_exist

  (** Generate an appropriate project [lang] stanza *)
  val lang_stanza : unit -> string

  (** Check that the dune-project file exists and create it otherwise. *)
  val ensure_project_file_exists : t -> created_or_already_exist

  (** Default language version to use for projects that don't have a
      [dune-project] file. The default value is the latest version of the dune
      language. *)
  val default_dune_language_version : Dune_lang.Syntax.Version.t ref

  (** Set the project we are currently parsing dune files for *)
  val set :
    t -> ('a, 'k) Dune_lang.Decoder.parser -> ('a, 'k) Dune_lang.Decoder.parser

  val get_exn : unit -> (t, 'k) Dune_lang.Decoder.parser

  (** Find arguments passed to (using). [None] means that the extension was not
      written in dune-project. *)
  val find_extension_args : t -> 'a Extension.t -> 'a option

  val set_parsing_context :
    t -> 'a Dune_lang.Decoder.t -> 'a Dune_lang.Decoder.t

  val implicit_transitive_deps : t -> bool

  val dune_version : t -> Dune_lang.Syntax.Version.t

  val wrapped_executables : t -> bool

  val strict_package_deps : t -> bool

  val cram : t -> bool
end

module Hooks : sig
  (** This module deals with management of hooks that run after specific events
      (e.g. end of build). *)

  module type S = sig
    (** Register a hook called every time the event occurs. *)
    val always : (unit -> unit) -> unit

    (** Register a hook that will only be called once when the next event
        occurs. *)
    val once : (unit -> unit) -> unit

    (** Signalize the event and run all registered hooks. *)
    val run : unit -> unit
  end

  module Make () : S

  (** Every time a build ends, which includes every iteration in watch mode,
      including cancellation of build because of file changes. *)
  module End_of_build : S
end

module Dtemp : sig
  (** Temp directory used by dune processes *)

  (** This returns a build path, but we don't rely on that *)
  val file : prefix:string -> suffix:string -> Path.t

  (** Add the temp env var to the environment passed or return the initial
      environment with the temp var added. *)
  val add_to_env : Env.t -> Env.t

  (** Destroy the temporary file or directory *)
  val destroy : Temp.what -> Path.t -> unit
end

module Response_file : sig
  (** Response file support *)

  type t =
    | Not_supported
    | Zero_terminated_strings of string
        (** The argument is the command line flag, such as "-args0" *)

  (** Return whether [prog] supports a response file or not *)
  val get : prog:Path.t -> t

  (** Registers the fact that [prog] supports a response file *)
  val set : prog:Path.t -> t -> unit
end

module Persistent : sig
  (** Persistent values *)

  (** This module allows to store values on disk so that they persist after Dune
      has exited and can be re-used by the next run of Dune.

      Values are simply marshaled using the [Marshal] module and manually
      versioned. As such, it is important to remember to increase the version
      number when the type of persistent values changes.

      In the future, we hope to introduce a better mechanism so that persistent
      values are automatically versioned. *)

  module type Desc = sig
    type t

    val name : string

    val version : int
  end

  (** Create a pair of functions to write/read a persistent value to/from a
      file. [D.name] must be unique.

      In the future, we plan to add a command [dune dump <file>] that will
      pretty-print the contents of any persistent file. This command will use
      the [D.name] stored in the persistent file to locate the appropriate
      pretty printer. *)
  module Make (D : Desc) : sig
    val to_string : D.t -> string

    val dump : Path.t -> D.t -> unit

    val load : Path.t -> D.t option
  end
end

module Cached_digest : sig
  (** Digest files with caching *)

  (** Digest the contents of the following file *)
  val file : Path.t -> Digest.t

  (** The digest of the following file, if cached *)
  val peek_file : Path.t -> Digest.t option

  (** Clear the following digest from the cache *)
  val remove : Path.t -> unit

  (** Same as {!file} but forces the digest to be recomputed *)
  val refresh : Path.t -> Digest.t

  (** Same as {!refresh} remove write permissions on the file *)
  val refresh_and_chmod : Path.t -> Digest.t

  (** Update the digest for a file in the cache *)
  val set : Path.t -> Digest.t -> unit

  (** Invalidate cached timestamp *)
  val invalidate_cached_timestamps : unit -> unit
end

module Lib_name : sig
  type t

  val hash : t -> int

  include Import.Stringlike_intf.S with type t := t

  module Local : sig
    include Import.Stringlike_intf.S

    (** Description of valid library names *)
    val valid_format_doc : User_message.Style.t Pp.t

    val mangled_path_under_package : t -> string list
  end

  val compare : t -> t -> Ordering.t

  val equal : t -> t -> bool

  val of_local : Loc.t * Local.t -> t

  val to_local : Loc.t * t -> (Local.t, User_message.t) result

  val to_local_exn : t -> Local.t

  val split : t -> Package.Name.t * string list

  val package_name : t -> Package.Name.t

  val of_package_name : Package.Name.t -> t

  type analyze =
    | Public of Package.Name.t * string list
    | Private of Package.Name.t * Local.t

  val analyze : t -> analyze

  val mangled : Package.Name.t -> Local.t -> t

  module Map : Map.S with type key = t

  module Set : sig
    include Set.S with type elt = t

    val to_string_list : t -> string list
  end

  val nest : t -> t -> t
end

module Variant : sig
  (** Library variants *)
  include Interned_intf.S

  (** Library variants allow to select the implementation of a library at link
      time.

      They are directly mapped to findlib predicates. *)

  (** Well-known variants *)
  val ppx_driver : t

  val mt : t

  val mt_posix : t

  val byte : t

  val native : t

  val plugin : t

  val encode : t Dune_lang.Encoder.t

  val decode : t Dune_lang.Decoder.t
end

module Dep_path : sig
  (** Dependency path *)

  module Entry : sig
    module Lib : sig
      type t =
        { path : Path.t
        ; name : Lib_name.t
        }
    end

    module Implements_via : sig
      type t =
        | Variant of Variant.t
        | Default_for of Lib.t
    end

    type t =
      | Path of Path.t
      | Alias of Path.t
      | Library of Lib.t * Implements_via.t option
      | Executables of (Loc.t * string) list
      | Preprocess of Lib_name.t list
      | Loc of Loc.t
  end

  module Entries : sig
    type t = Entry.t list

    val pp : t -> _ Pp.t
  end

  (** Re-raise an exception and augment it's dependency path with the given
      entry. The raised exception will be wrapped. *)
  val reraise : Exn_with_backtrace.t -> Entry.t -> _

  (** Extend the required_by stack of an exception *)
  val prepend_exn : exn -> Entry.t -> exn

  (** Extract a wrapped exception *)
  val unwrap_exn : exn -> exn * Entry.t list option

  (** Apply [f] to the underlying exception. *)
  val map : f:(exn -> exn) -> exn -> exn
end

module Report_error : sig
  (** Same as {!Stdune.Report_error.report} but also print the dependency path *)
  val report : Exn_with_backtrace.t -> unit
end

module Stats : sig
  (** Collect stats during the execution of dune *)

  (** Enable stats recording *)
  val enable : string -> unit

  (** If stats recording is enabled, collect stats now *)
  val record : unit -> unit

  (** Collect data about a subprocess *)
  val with_process :
    program:string -> args:string list -> 'a Fiber.t -> 'a Fiber.t

  (** Called by the build system when a new rule is fully evaluated and ready to
      fire *)
  val new_evaluated_rule : unit -> unit
end

module Scheduler : sig
  (** Scheduling *)

  (** [go ?log ?config fiber] runs the fiber until it terminates. *)
  val go : ?config:Config.t -> (unit -> 'a Fiber.t) -> 'a

  (** Runs [once] in a loop, executing [finally] after every iteration, even if
      Fiber.Never was encountered.

      If any source files change in the middle of iteration, it gets canceled. *)
  val poll :
       ?config:Config.t
    -> once:(unit -> unit Fiber.t)
    -> finally:(unit -> unit)
    -> unit
    -> 'a

  (** [with_job_slot f] waits for one job slot (as per [-j <jobs] to become
      available and then calls [f]. *)
  val with_job_slot : (unit -> 'a Fiber.t) -> 'a Fiber.t

  (** Wait for the following process to terminate *)
  val wait_for_process : Pid.t -> Unix.process_status Fiber.t

  (** Wait for dune cache to be disconnected. Drop any other event. *)
  val wait_for_dune_cache : unit -> unit

  val set_concurrency : int -> unit Fiber.t

  (** Make the scheduler ignore next change to a certain file in watch mode.

      This is used with promoted files that are copied back to the source tree
      after generation *)
  val ignore_for_watch : Path.t -> unit

  (** Number of jobs currently running in the background *)
  val running_jobs_count : unit -> int

  (** Execute the given callback with current directory temporarily changed *)
  val with_chdir : dir:Path.t -> f:(unit -> 'a) -> 'a

  (** Notify the scheduler of a file to deduplicate from another thread *)
  val send_dedup : Cache.caching -> Cache.File.t -> unit
end

module Process : sig
  (** Running external programs *)

  (** How to handle sub-process failures *)
  type ('a, 'b) failure_mode =
    | Strict : ('a, 'a) failure_mode
        (** Fail if the process exits with anything else than [0] *)
    | Accept : int Predicate_lang.t -> ('a, ('a, int) result) failure_mode
        (** Accept the following non-zero exit codes, and return [Error code] if
            the process exists with one of these codes. *)

  module Io : sig
    (** Where to redirect stdout/stderr/stdin *)
    type input

    type output

    type 'a mode =
      | In : input mode
      | Out : output mode

    type 'a t

    val stdout : output t

    val stderr : output t

    val stdin : input t

    val null : 'a mode -> 'a t

    (** Return a buffered channel for this output. The channel is created
        lazily. *)
    val out_channel : output t -> out_channel

    (** Create a [t] representing redirecting the input or to a file or reading
        input from the file. The returned channel can only be used by a single
        call to {!run}. If you want to use it multiple times, you need to use
        [clone]. *)
    val file : Path.t -> 'a mode -> 'a t

    (** Call this when you no longer need this redirection *)
    val release : 'a t -> unit

    (** [multi_use t] returns a copy for which [release] does nothing *)
    val multi_use : 'a t -> 'a t
  end

  (** Why a Fiber.t was run *)
  type purpose =
    | Internal_job
    | Build_job of Path.Build.Set.t

  (** [run ?dir ?stdout_to prog args] spawns a sub-process and wait for its
      termination. [stdout_to] [stderr_to] are released *)
  val run :
       ?dir:Path.t
    -> ?stdout_to:Io.output Io.t
    -> ?stderr_to:Io.output Io.t
    -> ?stdin_from:Io.input Io.t
    -> ?env:Env.t
    -> ?purpose:purpose
    -> (unit, 'a) failure_mode
    -> Path.t
    -> string list
    -> 'a Fiber.t

  (** Run a command and capture its output *)
  val run_capture :
       ?dir:Path.t
    -> ?stderr_to:Io.output Io.t
    -> ?stdin_from:Io.input Io.t
    -> ?env:Env.t
    -> ?purpose:purpose
    -> (string, 'a) failure_mode
    -> Path.t
    -> string list
    -> 'a Fiber.t

  val run_capture_line :
       ?dir:Path.t
    -> ?stderr_to:Io.output Io.t
    -> ?stdin_from:Io.input Io.t
    -> ?env:Env.t
    -> ?purpose:purpose
    -> (string, 'a) failure_mode
    -> Path.t
    -> string list
    -> 'a Fiber.t

  val run_capture_lines :
       ?dir:Path.t
    -> ?stderr_to:Io.output Io.t
    -> ?stdin_from:Io.input Io.t
    -> ?env:Env.t
    -> ?purpose:purpose
    -> (string list, 'a) failure_mode
    -> Path.t
    -> string list
    -> 'a Fiber.t

  val run_capture_zero_separated :
       ?dir:Path.t
    -> ?stderr_to:Io.output Io.t
    -> ?stdin_from:Io.input Io.t
    -> ?env:Env.t
    -> ?purpose:purpose
    -> (string list, 'a) failure_mode
    -> Path.t
    -> string list
    -> 'a Fiber.t
end

module Foreign_language : sig
  (* CR-soon cwong: I am deeply unsatisfied with the refactoring that lead to
     the existence of this module, and would like to delete it. This module (and
     the last three functions) were intially part of [Foreign], but needed to be
     split separately to keep [Foreign] cleanly in the front-end. However, the
     only real backend use of these files is that [Utils] uses
     [has_foreign_extension], which could theoretically be moved to the backedn
     by itself. However, that function relies on the key set of
     [source_extensions], which ultimately pulls in the rest of this
     infrastructure. In theory, we could just hardcode that key set somewhere
     (such as in [Utils]), that would add extra maintenance burden to ensure
     that the external keyset evolves in sync with the [source_extensions] here,
     and so this module was created to hold it. *)

  type t =
    | C
    | Cxx

  val compare : t -> t -> ordering

  val equal : t -> t -> bool

  val to_dyn : t -> Dyn.t

  (** The proper name of a language, e.g. "C++" for [Cxx]. Useful for diagnostic
      messages. *)
  val proper_name : t -> string

  module Map : Map.S with type key = t

  module Dict : sig
    type language

    type 'a t =
      { c : 'a
      ; cxx : 'a
      }

    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool

    val c : 'a t -> 'a

    val cxx : 'a t -> 'a

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val mapi : 'a t -> f:(language:language -> 'a -> 'b) -> 'b t

    val make_both : 'a -> 'a t

    val make : c:'a -> cxx:'a -> 'a t

    val update : 'a t -> language -> f:('a -> 'a) -> 'a t

    val merge : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

    val get : 'a t -> language -> 'a
  end
  with type language := t

  val source_extensions : (t * (int * int)) String.Map.t

  val header_extension : string

  val has_foreign_extension : fn:string -> bool
end

module Utils : sig
  (** Utilities that can't go in [Import] *)

  (** Return the absolute path to the shell and the argument to pass it (-c or
      /c). Raise in case in cannot be found. *)
  val system_shell_exn : needed_to:string -> Path.t * string

  (** Same as [system_shell_exn] but for bash *)
  val bash_exn : needed_to:string -> Path.t

  (** Raise an error about a program not found in the PATH or in the tree *)
  val program_not_found :
    ?context:Context_name.t -> ?hint:string -> loc:Loc.t option -> string -> _

  (** Raise an error about a library not found *)
  val library_not_found : ?context:Context_name.t -> ?hint:string -> string -> _

  val install_file :
    package:Package.Name.t -> findlib_toolchain:Context_name.t option -> string

  (** Produce a line directive *)
  val line_directive : filename:string -> line_number:int -> string

  (** [local_bin dir] The directory which contains the local binaries viewed by
      rules defined in [dir] *)
  val local_bin : Path.Build.t -> Path.Build.t

  (** Pretty-printer for suggesting a given shell command to the user *)
  val pp_command_hint : string -> _ Pp.t
end

module Vcs : sig
  (** VCS handling *)

  module Kind : sig
    type t =
      | Git
      | Hg

    val of_dir_contents : String.Set.t -> t option

    val of_filename : string -> t option
  end

  type t =
    { root : Path.t
    ; kind : Kind.t
    }

  val equal : t -> t -> bool

  val to_dyn : t -> Dyn.t

  (** Nice description of the current tip *)
  val describe : t -> string Fiber.t

  (** String uniquely identifying the current head commit *)
  val commit_id : t -> string Fiber.t

  (** List of files committed in the repo *)
  val files : t -> Path.t list Fiber.t

  (** VCS commands *)
  val git : Path.t Lazy.t

  val hg : Path.t Lazy.t
end

module File_tree : sig
  (** Dune representation of the source tree *)

  open! Import

  module Dune_file : sig
    val fname : string

    val jbuild_fname : string

    type kind = private
      | Plain
      | Ocaml_script

    type t

    (** We release the memory taken by s-exps as soon as it is used, unless
        [kind = Ocaml_script]. In which case that optimization is incorrect as
        we need to re-parse in every context. *)
    val get_static_sexp_and_possibly_destroy : t -> Dune_lang.Ast.t list

    val kind : t -> kind

    val path : t -> Path.Source.t
  end

  module Dir : sig
    type t

    type error = Missing_run_t of Cram_test.t

    val cram_tests : t -> (Cram_test.t, error) result list

    val path : t -> Path.Source.t

    val files : t -> String.Set.t

    val file_paths : t -> Path.Source.Set.t

    val fold_sub_dirs :
      t -> init:'a -> f:(basename:string -> t -> 'a -> 'a) -> 'a

    val fold_dune_files :
         t
      -> init:'acc
      -> f:(basename:string option -> t -> Dune_file.t -> 'acc -> 'acc)
      -> 'acc

    val sub_dir_paths : t -> Path.Source.Set.t

    val sub_dir_names : t -> String.Set.t

    val vcs : t -> Vcs.t option

    val status : t -> Sub_dirs.Status.t

    val fold :
      t -> traverse:Sub_dirs.Status.Set.t -> init:'a -> f:(t -> 'a -> 'a) -> 'a

    (** Return the contents of the dune (or jbuild) file in this directory *)
    val dune_file : t -> Dune_file.t option

    (** Return the project this directory is part of *)
    val project : t -> Dune_project.t

    val to_dyn : t -> Dyn.t
  end

  (** [set source ~ancestor_vcs ~recognize_jbuilder_projects] set the root, the
      default VCS, and if jbuilder project will be recognized. It must be called
      before all other calls to the file tree. All of these settings can only be
      set once per dune process *)
  val init :
    ancestor_vcs:Vcs.t option -> recognize_jbuilder_projects:bool -> unit

  val root : unit -> Dir.t

  (** Traverse starting from the root and report progress in the status line *)
  val fold_with_progress :
    traverse:Sub_dirs.Status.Set.t -> init:'a -> f:(Dir.t -> 'a -> 'a) -> 'a

  val find_dir : Path.Source.t -> Dir.t option

  (** [nearest_dir t fn] returns the directory with the longest path that is an
      ancestor of [fn]. *)
  val nearest_dir : Path.Source.t -> Dir.t

  (** [nearest_vcs t fn] returns the version control system with the longest
      root path that is an ancestor of [fn]. *)

  val nearest_vcs : Path.Source.t -> Vcs.t option

  val files_of : Path.Source.t -> Path.Source.Set.t

  (** [true] iff the path is a directory *)
  val dir_exists : Path.Source.t -> bool

  (** [true] iff the path is a file *)
  val file_exists : Path.Source.t -> bool

  val find_dir_specified_on_command_line : dir:Path.Source.t -> Dir.t
end

module Sandbox_config : sig
  (** Sandboxing configuration of build rules *)

  (** This module manages the sandboxing configuration written by the user in
      dune files or inside the build description.

      The sandboxing configuration of a build rule represent what the rule
      expects in terms of sandboxing. For instance, a rule might not work
      correctly when it is not sandboxed, or the opposite. *)

  (** A set of sandbox modes in which the rule is expected to work correctly. *)
  type t = Sandbox_mode.Set.t

  val compare : t -> t -> Ordering.t

  val equal : t -> t -> bool

  (** Computes the intersection of allowed sandbox modes *)
  val inter : t -> t -> t

  val no_special_requirements : t

  val no_sandboxing : t

  val needs_sandboxing : t

  (** The default sandboxing config for actions that don't bother specifying it.

      Often this means that they don't have special requirements, but it also
      often means that we're not quite sure.

      Currently we have [default = no_special_requirements]. *)
  val default : t

  val disallow : Sandbox_mode.t -> t

  val mem : t -> Sandbox_mode.t -> bool

  module Partial : sig
    type t = bool option Sandbox_mode.Dict.t

    (** [merge] distributes across [inter] when there is no error, but it can
        detect a nonsensical configuration where [inter] can't.

        Can raise a User_error. *)
    val merge : loc:Loc.t -> t list -> Sandbox_mode.Set.t

    val no_special_requirements : t

    val no_sandboxing : t

    val needs_sandboxing : t

    val disallow : Sandbox_mode.t -> t
  end
end

module Dep : sig
  type t = private
    | Env of Env.Var.t
    | File of Path.t
    | Alias of Alias.t
    | File_selector of File_selector.t
    | Universe
    | Sandbox_config of Sandbox_config.t

  val file : Path.t -> t

  val env : Env.Var.t -> t

  val universe : t

  val file_selector : File_selector.t -> t

  val alias : Alias.t -> t

  val sandbox_config : Sandbox_config.t -> t

  val compare : t -> t -> Ordering.t

  type eval_pred = File_selector.t -> Path.Set.t

  module Trace : sig
    type t
  end

  module Set : sig
    include Set.S with type elt = t

    val has_universe : t -> bool

    val sandbox_config : t -> Sandbox_config.t

    val source_tree : Path.t -> t

    val of_files : Path.t list -> t

    val of_files_set : Path.Set.t -> t

    val paths : t -> eval_pred:eval_pred -> Path.Set.t

    val encode : t -> Dune_lang.t

    val trace :
         t
      -> sandbox_mode:Sandbox_mode.t
      -> env:Env.t
      -> eval_pred:eval_pred
      -> Trace.t

    val add_paths : t -> Path.Set.t -> t

    val parallel_iter : t -> f:(elt -> unit Fiber.t) -> unit Fiber.t

    val parallel_iter_files :
      t -> f:(Path.t -> unit Fiber.t) -> eval_pred:eval_pred -> unit Fiber.t

    val dirs : t -> Path.Set.t
  end
end

(* see [Action_ast] above *)
module type Action_helpers = sig
  type program

  type path

  type target

  type string

  type t

  val run : program -> string list -> t

  val chdir : path -> t -> t

  val setenv : string -> string -> t -> t

  val with_stdout_to : target -> t -> t

  val with_stderr_to : target -> t -> t

  val with_outputs_to : target -> t -> t

  val with_stdin_from : path -> t -> t

  val ignore_stdout : t -> t

  val ignore_stderr : t -> t

  val ignore_outputs : t -> t

  val progn : t list -> t

  val echo : string list -> t

  val cat : path -> t

  val copy : path -> target -> t

  val symlink : path -> target -> t

  val copy_and_add_line_directive : path -> target -> t

  val system : string -> t

  val bash : string -> t

  val write_file : target -> string -> t

  val rename : target -> target -> t

  val remove_tree : target -> t

  val mkdir : path -> t

  val digest_files : path list -> t

  val diff : ?optional:bool -> ?mode:Diff.Mode.t -> path -> target -> t

  val format_dune_file :
    version:Dune_lang.Syntax.Version.t -> path -> target -> t
end

module Action : sig
  open! Import
  module Outputs = Action_outputs
  module Inputs = Action_inputs

  (** result of the lookup of a program, the path to it or information about the
      failure and possibly a hint how to fix it *)
  module Prog : sig
    module Not_found : sig
      type t = private
        { context : Context_name.t
        ; program : string
        ; hint : string option
        ; loc : Loc.t option
        }

      val create :
           ?hint:string
        -> context:Context_name.t
        -> program:string
        -> loc:Loc.t option
        -> unit
        -> t

      val raise : t -> _
    end

    type t = (Path.t, Not_found.t) result

    val to_dyn : t -> Dyn.t

    val ok_exn : t -> Path.t
  end

  include
    Action_ast
      with type program := Prog.t
      with type path := Path.t
      with type target := Path.Build.t
      with type string := string

  module Ast :
    Action_ast
      with type program = Prog.t
      with type path = Path.t
      with type target = Path.Build.t
      with type string = string
      with type t = t

  include
    Action_helpers
      with type program := Prog.t
      with type path := Path.t
      with type target := Path.Build.t
      with type string := string
      with type t := t

  val decode : t Dune_lang.Decoder.t

  module For_shell : sig
    include
      Action_ast
        with type program := string
        with type path := string
        with type target := string
        with type string := string

    val encode : t Dune_lang.Encoder.t
  end

  (** Convert the action to a format suitable for printing *)
  val for_shell : t -> For_shell.t

  (** Return the list of directories the action chdirs to *)
  val chdirs : t -> Path.Set.t

  (** The empty action that does nothing. *)
  val empty : t

  (** Checks, if action contains a [Dynamic_run]. *)
  val is_dynamic : t -> bool

  (** Ast where programs are not yet looked up in the PATH *)
  module Unresolved : sig
    type action = t

    module Program : sig
      type t =
        | This of Path.t
        | Search of Loc.t option * string

      val of_string : dir:Path.t -> loc:Loc.t option -> string -> t
    end

    include
      Action_ast
        with type program := Program.t
        with type path := Path.t
        with type target := Path.Build.t
        with type string := string

    val resolve : t -> f:(Loc.t option -> string -> Path.t) -> action
  end
  with type action := t

  (** Return a sandboxed version of an action. It takes care of preparing deps
      in the sandbox, but it does not copy the targets back out. It's the
      responsibility of the caller to do that. *)
  val sandbox :
       t
    -> sandboxed:(Path.Build.t -> Path.Build.t)
    -> mode:Sandbox_mode.some
    -> deps:Dep.Set.t
    -> eval_pred:Dep.eval_pred
    -> t

  type is_useful =
    | Clearly_not
    | Maybe

  (** Whether it makes sense to run the action inside a sandbox because it could
      have harmful side effects, to ensure it only consumes declared
      dependencies and it does not produce undeclared targets.

      Eg. it is maybe useful to sandbox an arbitrary shell command, but not a
      directory creation. *)
  val is_useful_to_sandbox : t -> is_useful

  (** Whether it makes sense to lookup the target in the distributed cache.

      Eg. there is no point in trying to fetch the result of a local file copy
      from the distributed cache, as we already have the file locally. *)
  val is_useful_to_distribute : t -> is_useful

  (** Whether it is useful to promote the rule to the cache.

      Eg. a file copy should be cached so we benefit from hardlink
      deduplication, but an action creating a symlink should not since the cache
      will reject it anyway. *)
  val is_useful_to_memoize : t -> is_useful
end

module Build_context : sig
  (* For the purpose of avoiding dependency cycles between the rules and the
     engine.

     This name could probably be chosen to be a bit more informative. *)

  open! Import

  type t = private
    { name : Context_name.t
    ; build_dir : Path.Build.t
    ; env : Env.t
    ; host : t option
    ; stdlib_dir : Path.t
    ; default_ocamlpath : Path.t list
    }

  val create :
       name:Context_name.t
    -> build_dir:Path.Build.t
    -> env:Env.t
    -> host:t option
    -> stdlib_dir:Path.t
    -> default_ocamlpath:Path.t list
    -> t
end

module Format_dune_lang : sig
  type dune_file =
    | OCaml_syntax of Loc.t
    | Sexps of Dune_lang.Cst.t list

  (** Read a file into its concrete syntax *)
  val parse_file : Path.t option -> dune_file

  (** Write the formatted concrete syntax to the file at [path] *)
  val write_file :
       version:Dune_lang.Syntax.Version.t
    -> path:Path.t
    -> Dune_lang.Cst.t list
    -> unit

  (** Reformat a dune file. [None] in [input] corresponds to stdin. [None] in
      [output] corresponds to stdout. *)
  val format_file :
       version:Dune_lang.Syntax.Version.t
    -> input:Path.t option
    -> output:Path.t option
    -> unit

  (** Pretty-print a list of toplevel s-expressions *)
  val pp_top_sexps :
    version:Dune_lang.Syntax.Version.t -> Dune_lang.Cst.t list -> _ Pp.t
end

module Print_diff : sig
  (** Diff two files that are expected not to match. *)
  val print : ?skip_trailing_cr:bool -> Path.t -> Path.t -> _ Fiber.t
end

module Promotion : sig
  module File : sig
    type t

    val to_dyn : t -> Dyn.t

    (** Register an intermediate file to promote. The build path may point to
        the sandbox and the file will be moved to the staging area. *)
    val register_intermediate :
      source_file:Path.Source.t -> correction_file:Path.Build.t -> unit

    (** Register file to promote where the correction file is a dependency of
        the current action (rather than an intermediate file). [correction_file]
        refers to a path in the build dir, not in the sandbox (it can point to
        the sandbox, but the sandbox root will be stripped). *)
    val register_dep :
      source_file:Path.Source.t -> correction_file:Path.Build.t -> unit
  end

  (** Promote all registered files if [!Clflags.auto_promote]. Otherwise dump
      the list of registered files to [_build/.to-promote]. *)
  val finalize : unit -> unit

  (** Describe what files should be promoted. The second argument of [These] is
      a function that is called on files that cannot be promoted. *)
  type files_to_promote =
    | All
    | These of Path.Source.t list * (Path.Source.t -> unit)

  val promote_files_registered_in_last_run : files_to_promote -> unit
end

(* CR cwong: one day, we'll move [Cram] entirely into the front end and get rid
   of this. *)
module Action_exec : sig
  (* For registering the cram_exec function. *)
  val cram_run : (env:Env.t -> script:Path.t -> unit Fiber.t) Fdecl.t
end

module Install : sig
  (** Opam install file *)

  module Dune_section = Section

  module Dst : sig
    type t

    val to_string : t -> string
  end

  (** Location for installation, containing the sections relative to the current
      package, and sites of possibly other packages *)
  module Section_with_site : sig
    type t =
      | Section of Section.t
      | Site of
          { pkg : Package.Name.t
          ; site : Section.Site.t
          }

    val to_string : t -> string

    (* val parse_string : string -> (t, string) Result.t *)

    val decode : t Dune_lang.Decoder.t

    val to_dyn : t -> Dyn.t
  end

  module Section : sig
    type t = Section.t

    module Set : Set.S with type elt = t

    val to_string : t -> string

    val parse_string : string -> (t, string) Result.t

    val decode : t Dune_lang.Decoder.t

    val to_dyn : t -> Dyn.t

    module Paths : sig
      type section = t

      type t

      val make :
           package:Package.Name.t
        -> destdir:Path.t
        -> ?libdir:Path.t
        -> ?mandir:Path.t
        -> unit
        -> t

      val install_path : t -> section -> Dst.t -> Path.t

      val get : t -> section -> Path.t

      val get_local_location :
        Context_name.t -> section -> Package.Name.t -> Path.t
    end
    with type section := t
  end

  module Entry : sig
    type 'src t = private
      { src : 'src
      ; dst : Dst.t
      ; section : Section.t
      }

    val adjust_dst :
         src:string String_with_vars.Partial.t
      -> dst:string option
      -> section:Section.t
      -> Dst.t

    val make : Section.t -> ?dst:string -> Path.Build.t -> Path.Build.t t

    val make_with_site :
         Section_with_site.t
      -> ?dst:string
      -> (pkg:Package.Name.t -> site:Dune_section.Site.t -> Section.t)
      -> Path.Build.t
      -> Path.Build.t t

    val set_src : _ t -> 'src -> 'src t

    val relative_installed_path : _ t -> paths:Section.Paths.t -> Path.t

    val add_install_prefix :
      Path.Build.t t -> paths:Section.Paths.t -> prefix:Path.t -> Path.Build.t t
  end

  (** Same as Entry, but the destination can be in the site of a package *)
  module Entry_with_site : sig
    type 'src t =
      { src : 'src
      ; dst : Dst.t
      ; section : Section_with_site.t
      }
  end

  val files : Path.Build.t Entry.t list -> Path.Set.t

  val gen_install_file : Path.Build.t Entry.t list -> string

  val load_install_file : Path.t -> Path.t Entry.t list
end

module Artifact_substitution : sig
  (** Handling of substitutions in artifacts during promotion and installation *)

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
        (** [Repeat (n, s)] evaluates to [s] repeated [n] times. This
            substitution is used for unit tests. *)

  type hardcoded_ocaml_path =
    | Hardcoded of Path.t list
    | Relocatable of Path.t

  type conf = private
    { get_vcs : Path.Source.t -> Vcs.t option
    ; get_location : Section.t -> Package.Name.t -> Path.t
    ; get_config_path : configpath -> Path.t option
    ; hardcoded_ocaml_path : hardcoded_ocaml_path
          (** Initial prefix of installation when relocatable chosen *)
    }

  val conf_of_context : Build_context.t option -> conf

  val conf_for_install :
       relocatable:bool
    -> default_ocamlpath:Path.t list
    -> stdlib_dir:Path.t
    -> prefix:Path.t
    -> libdir:Path.t option
    -> mandir:Path.t option
    -> conf

  val conf_dummy : conf

  val to_dyn : t -> Dyn.t

  (** A string encoding of a substitution. The resulting string is what should
      be written inside generated source files. {!copy_file} recognise such
      strings and expand them.

      The resulting string is guaranteed to be of length at least [min_len],
      which defaults to [0]. *)
  val encode : ?min_len:int -> t -> string

  (** [decode s] returns the value [t] such that [encode t = s]. *)
  val decode : string -> t option

  (** Copy a file, performing all required substitutions *)
  val copy_file :
       conf:conf
    -> ?chmod:(int -> int)
    -> src:Path.t
    -> dst:Path.t
    -> unit
    -> unit Fiber.t

  (** Generic version of [copy_file]. Rather than filenames, it takes an input
      and output functions. Their semantic must match the ones of the [input]
      and [output] functions from the OCaml standard library.

      [input_file] is used only for debugging purposes. It must be the name of
      the source file. *)
  val copy :
       conf:conf
    -> input_file:Path.t
    -> input:(Bytes.t -> int -> int -> int)
    -> output:(Bytes.t -> int -> int -> unit)
    -> unit Fiber.t

  (** Produce the string that would replace the placeholder with the given value
      .*)
  val encode_replacement : len:int -> repl:string -> string
end

module Static_deps : sig
  open! Import

  (** A simple wrapper around [Deps.t], where some dependencies are recorded as
      [rule_deps] and other as [action_deps]. Action dependencies are
      dependencies the external commands are expected to access, and rule
      dependencies are dependencies needed in order to compute the action to
      execute as well as its dependencies. *)

  type t =
    { rule_deps : Dep.Set.t
    ; action_deps : Dep.Set.t
    }

  val to_dyn : t -> Dyn.t

  (** No dependencies. *)
  val empty : t

  (** Union of dependencies. *)
  val union : t -> t -> t

  (** The paths to both rule and action dependencies. *)
  val paths : t -> eval_pred:Dep.eval_pred -> Path.Set.t
end

module Build : sig
  (** The build description *)

  open! Import

  (** Type of values allowed to be labeled *)
  type label = ..

  type 'a t

  include Applicative_intf.S1 with type 'a t := 'a t

  module With_targets : sig
    type 'a build

    type nonrec 'a t =
      { build : 'a t
      ; targets : Path.Build.Set.t
      }

    val map_build : 'a t -> f:('a build -> 'b build) -> 'b t

    val return : 'a -> 'a t

    val add : 'a t -> targets:Path.Build.t list -> 'a t

    val map : 'a t -> f:('a -> 'b) -> 'b t

    val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

    val write_file_dyn : Path.Build.t -> string t -> Action.t t

    val all : 'a t list -> 'a list t

    val of_result_map :
      'a Or_exn.t -> f:('a -> 'b t) -> targets:Path.Build.t list -> 'b t

    (** [memoize name t] is a build description that behaves like [t] except
        that its result is computed only once. *)
    val memoize : string -> 'a t -> 'a t

    module O : sig
      val ( >>> ) : unit t -> 'a t -> 'a t

      val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

      val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
    end
  end
  with type 'a build := 'a t

  (** This function should be called before analysing build expressions using
      [static_deps], [lib_deps] or [exec], which all require some file system
      information. *)
  val set_file_system_accessors :
       file_exists:(Path.t -> bool)
    -> eval_pred:(File_selector.t -> Path.Set.t)
    -> unit

  (** Add a set of targets to a build description, turning a target-less
      [Build.t] into [Build.With_targets.t]. *)
  val with_targets : 'a t -> targets:Path.Build.t list -> 'a With_targets.t

  (** Create a value of [With_targets.t] with the empty set of targets. *)
  val with_no_targets : 'a t -> 'a With_targets.t

  val return : 'a -> 'a t

  val map : 'a t -> f:('a -> 'b) -> 'b t

  val map2 : 'a t -> 'b t -> f:('a -> 'b -> 'c) -> 'c t

  val ignore : 'a t -> unit t

  val all : 'a t list -> 'a list t

  val all_unit : unit t list -> unit t

  (** Delay a static computation until the description is evaluated *)
  val delayed : (unit -> 'a) -> 'a t

  val or_exn : 'a Or_exn.t t -> 'a t

  (** CR-someday diml: this API is not great, what about:

      {[
        module Action_with_deps : sig
          type t
          val add_file_dependency : t -> Path.t -> t
        end

        (** Same as
            [t >>> arr (fun x -> Action_with_deps.add_file_dependency x p)]
            but better as [p] is statically known *)

        val record_dependency
          :  Path.t
          -> ('a, Action_with_deps.t) t
          -> ('a, Action_with_deps.t) t
      ]} *)

  (** [path p] records [p] as a file that is read by the action produced by the
      build description. *)
  val path : Path.t -> unit t

  val dep : Dep.t -> unit t

  val deps : Dep.Set.t -> unit t

  val dyn_deps : ('a * Dep.Set.t) t -> 'a t

  val paths : Path.t list -> unit t

  val path_set : Path.Set.t -> unit t

  (** Evaluate a predicate against all targets and record all the matched files
      as dependencies of the action produced by the build description. *)
  val paths_matching : loc:Loc.t -> File_selector.t -> Path.Set.t t

  (** [paths_existing paths] will require as dependencies the files that
      actually exist. *)
  val paths_existing : Path.t list -> unit t

  (** [env_var v] records [v] as an environment variable that is read by the
      action produced by the build description. *)
  val env_var : string -> unit t

  val alias : Alias.t -> unit t

  (** Compute the set of source of all files present in the sub-tree starting at
      [dir] and record them as dependencies. *)
  val source_tree : dir:Path.t -> Path.Set.t t

  (** Record dynamic dependencies *)
  val dyn_paths : ('a * Path.t list) t -> 'a t

  val dyn_paths_unit : Path.t list t -> unit t

  val dyn_path_set : ('a * Path.Set.t) t -> 'a t

  val dyn_path_set_reuse : Path.Set.t t -> Path.Set.t t

  (** [catch t ~on_error] evaluates to [on_error exn] if exception [exn] is
      raised during the evaluation of [t]. *)
  val catch : 'a t -> on_error:(exn -> 'a) -> 'a t

  (** [contents path] returns a description that when run will return the
      contents of the file at [path]. *)
  val contents : Path.t -> string t

  (** [lines_of path] returns a description that when run will return the
      contents of the file at [path] as a list of lines. *)
  val lines_of : Path.t -> string list t

  (** [strings path] is like [lines_of path] except each line is unescaped using
      the OCaml conventions. *)
  val strings : Path.t -> string list t

  (** Load an S-expression from a file *)
  val read_sexp : Path.t -> Dune_lang.Ast.t t

  (** Evaluates to [true] if the file is present on the file system or is the
      target of a rule. It doesn't add the path as dependency *)
  val file_exists : Path.t -> bool t

  (** [if_file_exists p ~then ~else] is a description that behaves like [then_]
      if [file_exists p] evaluates to [true], and [else_] otherwise. *)
  val if_file_exists : Path.t -> then_:'a t -> else_:'a t -> 'a t

  (** [filter_existing_files p] is a build description which keep only the
      existing files. The files are not registered as dynamic dependencies. *)
  val filter_existing_files : ('a * Path.Set.t) t -> ('a * Path.Set.t) t

  (** Always fail when executed. We pass a function rather than an exception to
      get a proper backtrace *)
  val fail : fail -> _ t

  val of_result : 'a t Or_exn.t -> 'a t

  val of_result_map : 'a Or_exn.t -> f:('a -> 'b t) -> 'b t

  (** [memoize name t] is a build description that behaves like [t] except that
      its result is computed only once. *)
  val memoize : string -> 'a t -> 'a t

  (** Create a file with the given contents. *)
  val write_file : Path.Build.t -> string -> Action.t With_targets.t

  val write_file_dyn : Path.Build.t -> string t -> Action.t With_targets.t

  val copy : src:Path.t -> dst:Path.Build.t -> Action.t With_targets.t

  val copy_and_add_line_directive :
    src:Path.t -> dst:Path.Build.t -> Action.t With_targets.t

  val symlink : src:Path.t -> dst:Path.Build.t -> Action.t With_targets.t

  val create_file : Path.Build.t -> Action.t With_targets.t

  (** Merge a list of actions accumulating the sets of their targets. *)
  val progn : Action.t With_targets.t list -> Action.t With_targets.t

  val label : label -> unit t

  (** {1 Analysis} *)

  (** Compute static dependencies of a build description. *)
  val static_deps : _ t -> Static_deps.t

  (** Compute static library dependencies of a build description. *)
  val fold_labeled : _ t -> init:'acc -> f:(label -> 'acc -> 'acc) -> 'acc

  (** {1 Execution} *)

  (** Execute a build description. Returns the result and the set of dynamic
      dependencies discovered during execution. *)
  val exec : 'a t -> 'a * Dep.Set.t

  (**/**)

  val paths_for_rule : Path.Set.t -> unit t
end

module Dir_set : sig
  (** Potentially infinite sets of directories *)

  (** Type of potentially infinite sets of directories. Not all sets can be
      represented, only ones that can be efficiently inspected. *)
  type 'w t

  (** [mem t p] is [true] if and only if [p] is in [t] *)
  val mem : 'w t -> 'w Path.Local_gen.t -> bool

  (** [here t] is the same as [mem t Path.Build.root] but more efficient. *)
  val here : 'w t -> bool

  (** The empty set *)
  val empty : 'w t

  (** The set of all possible directories *)
  val universal : 'w t

  (** [trivial b] is such that for all path [p]:

      {[ mem (trivial b) p = b ]}

      i.e. [trivial false] is [empty] and [trivial true] is [universal]. *)
  val trivial : bool -> 'w t

  val is_empty : 'w t -> bool

  val is_universal : 'w t -> bool

  (** [descend t comp] is the set [t'] such that for all path [p], [p] is in
      [t'] iff [comp/p] is in [t]. [comp] must be a path component, i.e. without
      directory separator characters. *)
  val descend : 'w t -> string -> Path.Local.w t

  (** [exceptions t] is the set of all bindings of the form [(comp, t')] such
      that:

      - [t' = descend t comp] - [t' <> trivial (default t)]

      Sets of directories for which [exceptions t] is not finite cannot be
      represented by this module. *)
  val exceptions : 'w t -> Path.Local.w t String.Map.t

  (** Default membership value for paths that are neither empty nor part of the
      exceptions. I.e. for all non-empty path [p] whose first component is not
      in [exceptions t], [mem t p = default t]. *)
  val default : 'w t -> bool

  val create :
       default:bool
    -> here:bool
    -> exceptions:Path.Unspecified.w t String.Map.t
    -> Path.Unspecified.w t

  (** [singleton p] is the set containing only [p] *)
  val singleton : 'w Path.Local_gen.t -> 'w t

  (** [subtree p] is the set of all directories that are descendant of [p]. *)
  val subtree : 'w Path.Local_gen.t -> 'w t

  val is_subset : 'w t -> of_:'w t -> bool

  val union : 'w t -> 'w t -> 'w t

  val union_all : 'w t list -> 'w t

  val inter : 'w t -> 'w t -> 'w t

  val inter_all : 'w t list -> 'w t

  val diff : 'w t -> 'w t -> 'w t

  val negate : 'w t -> 'w t

  val to_dyn : 'w t -> Dyn.t

  val forget_root : 'w t -> Path.Unspecified.w t

  type toplevel_subdirs =
    | Infinite
    | Finite of String.Set.t

  val toplevel_subdirs : 'w t -> toplevel_subdirs

  val of_list : 'w Path.Local_gen.t list -> 'w t

  val just_the_root : 'w t
end

module Rule : sig
  (** Representation of rules *)

  open! Import

  module Info : sig
    type t =
      | From_dune_file of Loc.t
      | Internal
      | Source_file_copy

    val of_loc_opt : Loc.t option -> t
  end

  module Promote : sig
    module Lifetime : sig
      type t =
        | Unlimited  (** The promoted file will be deleted by [dune clean] *)
        | Until_clean
    end

    module Into : sig
      type t =
        { loc : Loc.t
        ; dir : string
        }
    end

    type t =
      { lifetime : Lifetime.t
      ; into : Into.t option
      ; only : Predicate_lang.Glob.t option
      }
  end

  module Mode : sig
    type t =
      | Standard  (** Only use this rule if the source files don't exist. *)
      | Fallback  (** Silently promote the targets to the source tree. *)
      | Promote of Promote.t
          (** Just ignore the source files entirely. This is for cases where the
              targets are promoted only in a specific context, such as for
              .install files. *)
      | Ignore_source_files
  end

  module Id : sig
    type t

    val compare : t -> t -> Ordering.t

    module Map : Map.S with type key = t

    module Set : Set.S with type elt = t
  end

  type t = private
    { id : Id.t
    ; context : Build_context.t option
    ; env : Env.t option
    ; action : Action.t Build.With_targets.t
    ; mode : Mode.t
    ; locks : Path.t list
    ; info : Info.t
    ; (* Directory where all the targets are produced. *) dir : Path.Build.t
    }

  module Set : Set.S with type elt = t

  val equal : t -> t -> bool

  val hash : t -> int

  val to_dyn : t -> Dyn.t

  val make :
       ?sandbox:Sandbox_config.t
    -> ?mode:Mode.t
    -> context:Build_context.t option
    -> env:Env.t option
    -> ?locks:Path.t list
    -> ?info:Info.t
    -> Action.t Build.With_targets.t
    -> t

  val with_prefix : t -> build:unit Build.t -> t

  val loc : t -> Loc.t

  val effective_env : t -> Env.t

  (** [find_source_dir rule] is the closest source directory corresponding to
      rule.dir. Eg. [src/dune] for a rule with dir
      [_build/default/src/dune/.dune.objs]. *)
  val find_source_dir : t -> File_tree.Dir.t
end

module Rules : sig
  (** A collection of rules across a known finite set of directories *)

  (** Represent a set of rules producing files in a given directory *)
  module Dir_rules : sig
    type t

    val empty : t

    val union : t -> t -> t

    type alias_action =
      { stamp : Digest.t
      ; action : Action.t Build.With_targets.t
      ; locks : Path.t list
      ; context : Build_context.t
      ; env : Env.t option
      ; loc : Loc.t option
      }

    module Alias_spec : sig
      type t =
        { deps : Path.Set.t
        ; dyn_deps : Path.Set.t Build.t
        ; actions : alias_action Appendable_list.t
        }
    end

    (** A ready to process view of the rules of a directory *)
    type ready =
      { rules : Rule.t list
      ; aliases : Alias_spec.t Alias.Name.Map.t
      }

    val consume : t -> ready

    val is_subset : t -> of_:t -> bool

    val is_empty : t -> bool

    val to_dyn : t -> Dyn.t
  end

  (** A value of type [t] holds a set of rules for multiple directories *)
  type t

  val to_map : t -> Dir_rules.t Path.Build.Map.t

  module Produce : sig
    (* CR-someday aalekseyev: the below comments are not quite right *)

    (** Add a rule to the system. This function must be called from the
        [gen_rules] callback. All the target of the rule must be in the same
        directory.

        Assuming that [gen_rules ~dir:a] calls [add_rule r] where [r.dir] is
        [b], one of the following assumption must hold:

        - [a] and [b] are the same - [gen_rules ~dir:b] calls [load_dir ~dir:a]

        The call to [load_dir ~dir:a] from [gen_rules ~dir:b] declares a
        directory dependency from [b] to [a]. There must be no cyclic directory
        dependencies. *)
    val rule : Rule.t -> unit

    module Alias : sig
      type t = Alias.t

      (** [add_deps store alias ?dyn_deps deps] arrange things so that all
          [dyn_deps] and [deps] are built as part of the build of alias [alias]. *)
      val add_deps : t -> ?dyn_deps:Path.Set.t Build.t -> Path.Set.t -> unit

      (** [add_action store alias ~stamp action] arrange things so that [action]
          is executed as part of the build of alias [alias]. [stamp] is any
          S-expression that is unique and persistent S-expression. *)
      val add_action :
           t
        -> context:Build_context.t
        -> env:Env.t option
        -> loc:Loc.t option
        -> ?locks:Path.t list
        -> stamp:_
        -> Action.t Build.With_targets.t
        -> unit
    end
  end

  val implicit_output : t Memo.Implicit_output.t

  val empty : t

  val union : t -> t -> t

  val produce_dir : dir:Path.Build.t -> Dir_rules.t -> unit

  val produce_dir' : dir:Path.t -> Dir_rules.t -> unit

  val produce : t -> unit

  val produce_opt : t option -> unit

  val is_subset : t -> of_:t -> bool

  val map_rules : t -> f:(Rule.t -> Rule.t) -> t

  val collect : (unit -> 'a) -> 'a * t

  val collect_unit : (unit -> unit) -> t

  val collect_opt : (unit -> 'a) -> 'a * t option

  (** returns [Dir_rules.empty] for non-build paths *)
  val find : t -> Path.t -> Dir_rules.t
end

module Build_system : sig
  (** Build rules *)

  open! Import

  (** {1 Setup} *)

  (** {2 Creation} *)

  type caching =
    { cache : (module Cache.Caching)
    ; check_probability : float
    }

  (** Initializes the build system. This must be called first. *)
  val init :
       contexts:Build_context.t list
    -> ?caching:caching
    -> sandboxing_preference:Sandbox_mode.t list
    -> unit
    -> unit

  val reset : unit -> unit

  module Subdir_set : sig
    type t =
      | All
      | These of String.Set.t

    val empty : t

    val union : t -> t -> t

    val union_all : t list -> t

    val mem : t -> string -> bool
  end

  type extra_sub_directories_to_keep = Subdir_set.t

  module Context_or_install : sig
    type t =
      | Install of Context_name.t
      | Context of Context_name.t

    val to_dyn : t -> Dyn.t
  end

  (** Set the rule generators callback. There must be one callback per build
      context name.

      Each callback is used to generate the rules for a given directory in the
      corresponding build context. It receives the directory for which to
      generate the rules and the split part of the path after the build context.
      It must return an additional list of sub-directories to keep. This is in
      addition to the ones that are present in the source tree and the ones that
      already contain rules.

      It is expected that [f] only generate rules whose targets are descendant
      of [dir].

      [init] can generate rules in any directory, so it's always called. *)
  val set_rule_generators :
       init:(unit -> unit)
    -> gen_rules:
         (   Context_or_install.t
          -> (dir:Path.Build.t -> string list -> extra_sub_directories_to_keep)
             option)
    -> unit

  (** Set the list of VCS repositiories contained in the source tree *)
  val set_vcs : Vcs.t list -> unit Fiber.t

  (** All other functions in this section must be called inside the rule
      generator callback. *)

  (** {2 Primitive for rule generations} *)

  (** [prefix_rules t prefix ~f] Runs [f] and adds [prefix] as a dependency to
      all the rules generated by [f] *)
  val prefix_rules : unit Build.t -> f:(unit -> 'a) -> 'a

  (** [eval_pred t \[glob\]] returns the list of files in
      [File_selector.dir glob] that matches [File_selector.predicate glob]. The
      list of files includes the list of targets. *)
  val eval_pred : File_selector.t -> Path.Set.t

  (** Returns the set of targets in the given directory. *)
  val targets_of : dir:Path.t -> Path.Set.t

  (** Load the rules for this directory. *)
  val load_dir : dir:Path.t -> unit

  (** Sets the package assignment *)
  val set_packages : (Path.Build.t -> Package.Id.Set.t) -> unit

  (** Assuming [files] is the list of files in [_build/install] that belong to
      package [pkg], [package_deps t pkg files] is the set of direct package
      dependencies of [package]. *)
  val package_deps : Package.t -> Path.Set.t -> Package.Id.Set.t Build.t

  (** {2 Aliases} *)

  module Alias : sig
    type t = Alias.t

    (** Alias for all the files in [_build/install] that belong to this package *)
    val package_install : context:Build_context.t -> pkg:Package.t -> t

    (** [dep t = Build.path (stamp_file t)] *)
    val dep : t -> unit Build.t

    (** Implements [@@alias] on the command line *)
    val dep_multi_contexts :
         dir:Path.Source.t
      -> name:Alias.Name.t
      -> contexts:Context_name.t list
      -> unit Build.t

    (** Implements [(alias_rec ...)] in dependency specification *)
    val dep_rec : t -> loc:Loc.t -> unit Build.t

    (** Implements [@alias] on the command line *)
    val dep_rec_multi_contexts :
         dir:Path.Source.t
      -> name:Alias.Name.t
      -> contexts:Context_name.t list
      -> unit Build.t
  end

  (** {1 Building} *)

  (** All the functions in this section must be called outside the rule
      generator callback. *)

  (** Do the actual build *)
  val do_build : request:'a Build.t -> 'a Fiber.t

  (** {2 Other queries} *)

  val is_target : Path.t -> bool

  val static_deps_of_request : 'a Build.t -> Path.Set.t

  val rules_for_transitive_closure : Path.Set.t -> Rule.t list

  val contexts : unit -> Build_context.t Context_name.Map.t

  (** List of all buildable targets. *)
  val all_targets : unit -> Path.Build.Set.t

  (** The set of files that were created in the source tree and need to be
      deleted. *)
  val files_in_source_tree_to_delete : unit -> Path.Set.t

  (** {2 Build rules} *)

  (** A fully evaluated rule. *)
  module Evaluated_rule : sig
    type t = private
      { id : Rule.Id.t
      ; dir : Path.Build.t
      ; deps : Dep.Set.t
      ; targets : Path.Build.Set.t
      ; context : Build_context.t option
      ; action : Action.t
      }
  end

  (** Return the list of fully evaluated rules used to build the given targets.
      If [recursive] is [true], also include the rules needed to build the
      transitive dependencies of the targets. *)
  val evaluate_rules :
    recursive:bool -> request:unit Build.t -> Evaluated_rule.t list Fiber.t

  val get_cache : unit -> caching option
end
