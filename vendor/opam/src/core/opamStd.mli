(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Generic stdlib functions (String, List, Option, Sys submodules...) *)

(** {2 Signatures and functors} *)

(** Sets with extended interface and infix operators *)
module type SET = sig

  include Set.S

  val map: (elt -> elt) -> t -> t

  val is_singleton: t -> bool

  (** Returns one element, assuming the set is a singleton. Raises [Not_found]
      on an empty set, [Failure] on a non-singleton. *)
  val choose_one : t -> elt

  val choose_opt: t -> elt option

  val of_list: elt list -> t
  val to_list_map: (elt -> 'b) -> t -> 'b list
  val to_string: t -> string
  val to_json: t OpamJson.encoder
  val of_json: t OpamJson.decoder
  val find: (elt -> bool) -> t -> elt
  val find_opt: (elt -> bool) -> t -> elt option

  (** Raises Failure in case the element is already present *)
  val safe_add: elt -> t -> t

  (** Accumulates the resulting sets of a function of elements until a fixpoint
      is reached *)
  val fixpoint: (elt -> t) -> t -> t

  (** [map_reduce f op t] applies [f] to every element of [t] and combines the
      results using associative operator [op]. Raises [Invalid_argument] on an
      empty set, or returns [default] if it is defined. *)
  val map_reduce: ?default:'a -> (elt -> 'a) -> ('a -> 'a -> 'a) -> t -> 'a

  module Op : sig
    val (++): t -> t -> t (** Infix set union *)

    val (--): t -> t -> t (** Infix set difference *)

    val (%%): t -> t -> t (** Infix set intersection *)
  end

end

(** Maps with extended interface *)
module type MAP = sig

  include Map.S

  val to_string: ('a -> string) -> 'a t  -> string
  val to_json: 'a OpamJson.encoder -> 'a t OpamJson.encoder
  val of_json: 'a OpamJson.decoder -> 'a t OpamJson.decoder
  val keys: 'a t -> key list
  val values: 'a t -> 'a list
  val find_opt: key -> 'a t -> 'a option
  val choose_opt: 'a t -> (key * 'a) option

  (** A key will be in the union of [m1] and [m2] if it is appears
      either [m1] or [m2], with the corresponding value. If a key
      appears in both [m1] and [m2], then the resulting value is built
      using the function given as argument. *)
  val union: ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t

  val is_singleton: 'a t -> bool

  val of_list: (key * 'a) list -> 'a t

  (** Raises Failure in case the element is already present *)
  val safe_add: key -> 'a -> 'a t -> 'a t

  (** [update k f zero map] updates the binding of [k] in [map] using function
      [f], applied to the current value bound to [k] or [zero] if none *)
  val update: key -> ('a -> 'a) -> 'a -> 'a t -> 'a t

  (** [map_reduce f op t] applies [f] to every binding of [t] and combines the
      results using associative operator [op]. Raises [Invalid_argument] on an
      empty map, or returns [default] if it is defined. *)
  val map_reduce:
    ?default:'b -> (key -> 'a -> 'b) -> ('b -> 'b -> 'b) -> 'a t -> 'b

  val filter_map: (key -> 'a -> 'b option) -> 'a t -> 'b t
end

(** A signature for handling abstract keys and collections thereof *)
module type ABSTRACT = sig

  type t

  val compare: t -> t -> int
  val equal: t -> t -> bool
  val of_string: string -> t
  val to_string: t -> string
  val to_json: t OpamJson.encoder
  val of_json: t OpamJson.decoder

  module Set: SET with type elt = t
  module Map: MAP with type key = t
end

(** A basic implementation of ABSTRACT using strings *)
module AbstractString : ABSTRACT with type t = string

(** {3 Generators for set and map modules with printers} *)

module type OrderedType = sig
  include Set.OrderedType
  val to_string: t -> string
  val to_json: t OpamJson.encoder
  val of_json: t OpamJson.decoder
end

module Set: sig
  module Make (S: OrderedType): SET with type elt = S.t
end

module Map: sig
  module Make (S: OrderedType): MAP with type key = S.t
end


(** {2 Integer collections} *)

(** Map of ints *)
module IntMap: MAP with type key = int

(** Set of ints *)
module IntSet: SET with type elt = int


(** {2 Utility modules extending the standard library on base types} *)

module Option: sig
  val map: ('a -> 'b) -> 'a option -> 'b option

  val iter: ('a -> unit) -> 'a option -> unit

  val default: 'a -> 'a option -> 'a

  val default_map: 'a option -> 'a option -> 'a option

  val replace : ('a -> 'b option) -> 'a option -> 'b option

  val map_default: ('a -> 'b) -> 'b -> 'a option -> 'b

  val compare: ('a -> 'a -> int) -> 'a option -> 'a option -> int

  val equal: ('a -> 'a -> bool) -> 'a option -> 'a option -> bool

  val to_string: ?none:string -> ('a -> string) -> 'a option -> string

  val to_list: 'a option -> 'a list

  val some: 'a -> 'a option

  val none: 'a -> 'b option

  (** [of_Not_found f x] calls [f x], catches [Not_found] and returns [None] *)
  val of_Not_found: ('a -> 'b) -> 'a -> 'b option

  module Op: sig
    val (>>=): 'a option -> ('a -> 'b option) -> 'b option
    val (>>|): 'a option -> ('a -> 'b) -> 'b option
    val (>>+): 'a option -> (unit -> 'a option) -> 'a option
    val (+!): 'a option -> 'a -> 'a
    val (++): 'a option -> 'a option -> 'a option
  end
end

module List : sig

  val cons: 'a -> 'a list -> 'a list

  (** Convert list items to string and concat. [sconcat_map sep f x] is equivalent
      to String.concat sep (List.map f x) but tail-rec. *)
  val concat_map:
    ?left:string -> ?right:string -> ?nil:string -> ?last_sep:string ->
    string -> ('a -> string) -> 'a list -> string

  (** Like [List.find], but returning option instead of raising *)
  val find_opt: ('a -> bool) -> 'a list -> 'a option

  val to_string: ('a -> string) -> 'a list -> string

  (** Removes consecutive duplicates in a list *)
  val remove_duplicates: ('a -> 'a -> bool) -> 'a list -> 'a list

  (** Sorts the list, removing duplicates *)
  val sort_nodup: ('a -> 'a -> int) -> 'a list -> 'a list

  (** Filter and map *)
  val filter_map: ('a -> 'b option) -> 'a list -> 'b list

  (** Retrieves [Some] values from a list *)
  val filter_some: 'a option list -> 'a list

  (** Returns the first non-[None] value returned by the passed function on the
      elements of the passed list.
      @raise Not_found if all of them yield [None] *)
  val find_map: ('a -> 'b option) -> 'a list -> 'b

  (** Like [find_map], but returns [Some _] if succeeded and [None] if failed. *)
  val find_map_opt: ('a -> 'b option) -> 'a list -> 'b option

  (** Insert a value in an ordered list *)
  val insert: ('a -> 'a -> int) -> 'a -> 'a list -> 'a list

  (** Inserts a value at the given index (starting from 0) in the list (start or
      end if index < 0 or > length respectively). Not tail-recursive *)
  val insert_at: int -> 'a -> 'a list -> 'a list

  (** Like [List.assoc] with an equality function. *)
  val assoc: ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b

  (** Like [assoc], but returning option instead of raising [Not_found] *)
  val assoc_opt: ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b option

  (** Like [assoc], but as an option, and also returns the list with the
      binding removed, e.g. equivalent to [(assoc_opt x l, remove_assoc x l)]
      (but tail-recursive and more efficient) *)
  val pick_assoc:
    ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> 'b option * ('a * 'b) list

  (** Like [assoc], but returns a boolean instead of associated value *)
  val mem_assoc: ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> bool

  (** [remove_assoc eq k l] removes first association of [k] from list [l]
      (tail-recursive). *)
  val remove_assoc:
    ('a -> 'a -> bool) -> 'a -> ('a * 'b) list -> ('a * 'b) list

  (** [update_assoc key value list] updates the first value bound to [key] in
      the associative list [list], or appends [(key, value)] if the key is not
      bound. *)
  val update_assoc:
    ('a -> 'a -> bool) -> 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list

  (** Like [pick_assoc], but with a test function that takes a list element *)
  val pick: ('a -> bool) -> 'a list -> 'a option * 'a list

  (** Like [List.fold_left], but also performs [List.map] at the same time *)
  val fold_left_map: ('s -> 'a -> ('s * 'b)) -> 's -> 'a list -> 's * 'b list
end

module String : sig

  (** {3 Collections} *)

  module Map: MAP with type key = string

  module Set: SET with type elt = string

  (** Set of string sets *)
  module SetSet: SET with type elt = Set.t

  (** Map of string sets *)
  module SetMap: MAP with type key = Set.t

  (** {3 Checks} *)

  val starts_with: prefix:string -> string -> bool
  val ends_with: suffix:string -> string -> bool
  val for_all: (char -> bool) -> string -> bool
  val contains_char: string -> char -> bool
  val contains: sub:string -> string -> bool
  val exact_match: Dune_re.re -> string -> bool
  val find_from: (char -> bool) -> string -> int -> int

  (** Like [String.compare], but with lowercase/uppercase variants ordered next
      to each other (still considered not equal though) *)
  val compare_case: string -> string -> int

  (** {3 Manipulation} *)

  val map: (char -> char) -> string -> string
  val strip: string -> string
  val strip_right: string -> string
  val sub_at: int -> string -> string
  val remove_prefix: prefix:string -> string -> string
  val remove_suffix: suffix:string -> string -> string

  (** [is_prefix_of from full str] returns true if [str] if a prefix of [full],
      with at least [from] first characters *)
  val is_prefix_of: from:int -> full:string -> string -> bool

  (** {4 Transformations} *)

  (** Cut a string at the first occurrence of the given char *)
  val cut_at: string -> char -> (string * string) option

  (** Same as [cut_at], but starts from the right *)
  val rcut_at: string -> char -> (string * string) option

  (** Split a string at occurrences of a given characters. Empty strings are
      skipped. *)
  val split: string -> char -> string list

  (** The same as [split], but keep empty strings (leading, trailing or between
      contiguous delimiters) *)
  val split_delim: string -> char -> string list

  val fold_left: ('a -> char -> 'a) -> 'a -> string -> 'a

  val is_hex: string -> bool

end

module Format : sig

  (** {4 Querying information} *)

  (** Returns the length of the string in terminal chars, ignoring ANSI color
      sequences from OpamConsole.colorise *)
  val visual_length: string -> int

  (** {4 Text formatting functions} *)

  (** Truncates the string to not visually get over [width] columns *)
  val cut_at_visual: string -> int -> string

  (** left indenting. [~visual] can be used to indent eg. ANSI colored
      strings and should correspond to the visible characters of s *)
  val indent_left: string -> ?visual:string -> int -> string

  val indent_right: string -> ?visual:string -> int -> string

  (** Pads fields in a table with spaces for alignment. *)
  val align_table: string list list -> string list list

  (** Cut long lines in string according to the terminal width *)
  val reformat:
    ?start_column:int -> ?indent:int -> ?width:int -> string -> string

  (** Convert a list of items to string as a dashed list (already reformats
      supposes no additional left margin: don't use within OpamConsole.error or
      similar) *)
  val itemize: ?bullet:string -> ('a -> string) -> 'a list -> string

  (** Display a pretty list: ["x";"y";"z"] -> "x, y and z".
      "and" can be changed by specifying [last] *)
  val pretty_list: ?last:string -> string list -> string

  (** Splits a list of strings so that it can be printed as a table that should
      fit on screen *)
  val as_aligned_table: ?width:int -> string list -> string list list
end

module Exn : sig

  (** To use when catching default exceptions: ensures we don't catch fatal errors
      like C-c. try-with should _always_ (by decreasing order of preference):
      - either catch specific exceptions
      - or re-raise the same exception (preferably with [Exn.finalise])
      - or call this function on the caught exception *)
  val fatal: exn -> unit

  (** Register a backtrace for when you need to process a finalizer (that
      internally uses exceptions) and then re-raise the same exception.
      To be printed by pretty_backtrace. *)
  val register_backtrace: exn -> unit

  (** Return a pretty-printed backtrace *)
  val pretty_backtrace: exn -> string

  (** Runs the given finaliser, then reraises the given exception, while
      preserving backtraces (when the OCaml version permits, e.g. >= 4.05.0) *)
  val finalise: exn -> (unit -> unit) -> 'a

  (** Execute the given continuation, then run the finaliser before returning
      the result. If an exception is raised, call [finalise] with the given
      finaliser. *)
  val finally: (unit -> unit) -> (unit -> 'a) -> 'a

end

(** {2 Manipulation and query of environment variables} *)

module Env : sig

  (** {3 Generic functions} *)

  (** Remove from a c-separated list of string the ones with the given prefix *)
  val reset_value: prefix:string -> char -> string -> string list

  (** split a c-separated list of string in two according to the first
      occurrences of the string with the given [prefix]. The list of
      elements occurring before is returned in reverse order. If there are
      other elements with the same [prefix] they are kept in the second list.
  *)
  val cut_value: prefix:string -> char -> string -> string list * string list

  (** Utility function for shell single-quoted strings. In most shells,
      backslash escapes are not allowed and a single quote needs to be replaced
      by [quote double-quote quote double-quote quote] (close the single-quoted
      literal, put the single quote in a double-quoted literal, and reopen a
      single-quoted literal). fish is the exception and should set
      [using_backslashes] to escape both quotes and backslashes using
      backslashes *)
  val escape_single_quotes: ?using_backslashes:bool -> string -> string

  (** Utility function for PowerShell strings. *)
  val escape_powershell: string -> string

  (** {3 Environment variable handling} *)

  (** Environment variable names. Windows has complicated semantics for
      environment variables. The retrieval functions are case insensitive, but
      it's "legal" for the environment block to contain entries which differ
      only by case. If environment variables are set entirely using CRT or Win32
      API functions, then there isn't usually a problem, the issue arises when
      creating a program where the environment block is instead passed. In this
      model, it's very easy to end up with two bindings in the same block. When
      dealing with Windows programs, this will mostly be transparent, but it's a
      problem with Cygwin which actively allows "duplicate" entries which differ
      by case only and implements Posix semantics on top of this. The problem is
      constantly with us thanks to the use of PATH on Unix, and Path on Windows!
      opam tries to ensure that environment variables are looked up according to
      the OS semantics (so case insensitively on Windows) and OpamEnv goes to
      some trouble to ensure that updates to environment variables are case
      preserving (i.e. PATH+=foo gets transformed to Path+=foo if Path exists
      in the environment block).

      Key to this is not accidentally treating environment variable names as
      strings, without using the appropriate comparison functions. Name.t
      represents environment variable names as private strings, providing
      comparison operators to handle them, and still allowing the possibility
      to coerce them to strings.
      *)
  module Name : sig
    include ABSTRACT with type t = private string

    val equal_string: t -> string -> bool

  end

  val get: string -> string

  val getopt: string -> string option

  val getopt_full: Name.t -> Name.t * string option

  val list: unit -> (Name.t * string) list
  val raw_env: unit -> string Array.t
  val cyg_env: string -> string Array.t
end

(** {2 System query and exit handling} *)

module Sys : sig

  (** {3 Querying} *)

  (** true if stdout is bound to a terminal *)
  val tty_out : bool

  (** true if stdin is bound to a terminal *)
  val tty_in : bool

  (** Queried lazily, but may change on SIGWINCH *)
  val terminal_columns : unit -> int

  (** The user's home directory. Queried lazily *)
  val home: unit -> string

  (** The /etc directory *)
  val etc: unit -> string

  (** The system directory (Windows only) *)
  val system: unit -> string

  type os = Darwin
          | Linux
          | FreeBSD
          | OpenBSD
          | NetBSD
          | DragonFly
          | Cygwin
          | Win32
          | Unix
          | Other of string

  (** Queried lazily *)
  val os: unit -> os

  (** The output of the command "uname", with the given argument. Memoised. *)
  val uname: string -> string option

  (** Append .exe (only if missing) to executable filenames on Windows *)
  val executable_name : string -> string

  (** The different families of shells we know about *)
  type powershell_host = Powershell_pwsh | Powershell
  type shell = SH_sh | SH_bash | SH_zsh | SH_csh | SH_fish
    | SH_pwsh of powershell_host | SH_cmd

  (** List of all supported shells *)
  val all_shells : shell list

  (** Guess the shell compat-mode *)
  val guess_shell_compat: unit -> shell

  (** Guess the location of .profile. Returns None if the shell doesn't
      support the concept of a .profile file. *)
  val guess_dot_profile: shell -> string option

  (** The separator character used in the PATH variable (varies depending on
      OS) *)
  val path_sep: char

  (** Splits a PATH-like variable separated with [path_sep]. More involved than
      it seems, because there may be quoting on Windows. By default, it returns
      the path cleaned (remove trailing, leading, contiguous delimiters).
      Optional argument [clean] permits to keep those empty strings. *)
  val split_path_variable: ?clean:bool -> string -> string list

  (** For native Windows builds, returns [`Cygwin] if the command is a Cygwin-
      compiled executable, [`Msys2] if the command is a MSYS2-compiled
      executable, and [`Tainted of [ `Msys2 | `Cygwin ]] if the command links
      to a library which is itself Cygwin- or MSYS2-compiled, or [`Native]
      otherwise.

      Note that this returns [`Native] on a Cygwin-build of opam!

      Both cygcheck and an unqualified command will be resolved if necessary
      using the current PATH. *)
  val get_windows_executable_variant: cygbin:string option ->
    string -> [ `Native | `Cygwin | `Tainted of [ `Msys2 | `Cygwin] | `Msys2 ]

  (** Determines if cygcheck in given cygwin binary directory comes from a
      Cygwin or MSYS2 installation. Determined by analysing the cygpath command
      found with it. *)
  val is_cygwin_cygcheck : cygbin:string option -> bool

  (** For native Windows builds, returns [`Cygwin] if the command is a Cygwin-
      or Msys2- compiled executable, and [`CygLinked] if the command links to a
      library which is itself Cygwin/Msys2-compiled, or [`Native] otherwise.

      Note that this returns [`Native] on a Cygwin-build of opam!

      Both cygcheck and an unqualified command will be resolved using the
      current PATH. *)
  val get_cygwin_variant: cygbin:string option -> string -> [ `Native | `Cygwin | `CygLinked ]

  (** Returns true if [get_cygwin_variant] is [`Cygwin] *)
  val is_cygwin_variant: cygbin:string option -> string -> bool

  (** {3 Exit handling} *)

  (** Like Stdlib.at_exit but with the possibility to call manually
      (eg. before exec()) *)
  val at_exit: (unit -> unit) -> unit

  (** Calls the functions registered in at_exit. Unneeded if exiting normally *)
  val exec_at_exit: unit -> unit

  (** Indicates intention to exit the program with given exit code *)
  exception Exit of int

  (** Indicates intention to exec() the given command (parameters as per
      [Unix.execvpe]), after proper finalisations. It's the responsibility of
      the main function to catch this, call [exec_at_exit], and
      [Unix.execvpe]. *)
  exception Exec of string * string array * string array

  (** Raises [Exit i] *)
  (* val exit: int -> 'a *)

  type exit_reason =
    [ `Success | `False | `Bad_arguments | `Not_found | `Aborted | `Locked
    | `No_solution | `File_error | `Package_operation_error | `Sync_error
    | `Configuration_error | `Solver_failure | `Internal_error
    | `User_interrupt ]

  val exit_codes : (exit_reason * int) list

  val get_exit_code : exit_reason -> int

  (** Raises [Exit], with the code associated to the exit reason *)
  val exit_because: exit_reason -> 'a

  (**/**)

  type warning_printer =
    {mutable warning : 'a . ('a, unit, string, unit) format4 -> 'a}
  val set_warning_printer : warning_printer -> unit
end

(** {2 Windows-specific functions} *)
module Win32 : sig
  (** Win32 Registry Hives and Values *)
  module RegistryHive : sig
    val to_string : OpamStubs.registry_root -> string
    val of_string : string -> OpamStubs.registry_root
  end

  val set_parent_pid : int32 -> unit
  (** Change which the pid written to by {!parent_putenv}. This function cannot
      be called after [parent_putenv]. *)

  val parent_putenv : string -> string -> bool
  (** Update an environment variable in the parent (i.e. shell) process's
      environment. *)

  val persistHomeDirectory : string -> unit
  (** [persistHomeDirectory value] sets the HOME environment variable in this
      and the parent process and also persists the setting to the user's
      registry and broadcasts the change to other processes. *)
end

(** {2 General use infix function combinators} *)

module Op: sig

  (** Function application (with lower priority) (predefined in OCaml 4.01+) *)
  val (@@): ('a -> 'b) -> 'a -> 'b

  (** Pipe operator -- reverse application (predefined in OCaml 4.01+) *)
  val (|>): 'a -> ('a -> 'b) -> 'b

  (** Function composition : (f @* g) x =~ f (g x) *)
  val (@*): ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c

  (** Reverse function composition : (f @> g) x =~ g (f x) *)
  val (@>): ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c

end

(** {2 Helper functions to initialise configuration from the environment} *)

module Config : sig

  type env_var = string

  type when_ = [ `Always | `Never | `Auto ]
  type when_ext = [ `Extended | when_ ]
  type answer = [ `unsafe_yes | `all_yes | `all_no | `ask ]
  type yes_answer = [ `unsafe_yes | `all_yes ]

  (* Parse a envrionement variable boolean value *)
  val bool_of_string: string -> bool option

  val env: (string -> 'a) -> string -> 'a option

  val env_bool: env_var -> bool option

  val env_int: env_var -> int option

  type level = int
  (* Like [env_int], but accept boolean values for 0 and 1 *)
  val env_level: env_var -> level option

  type sections = int option String.Map.t
  val env_sections: env_var -> sections option

  val env_string: env_var -> string option

  val env_float: env_var -> float option

  val env_when: env_var -> when_ option

  val env_when_ext: env_var -> when_ext option

  val resolve_when: auto:(bool Lazy.t) -> when_ -> bool

  val env_answer: env_var -> answer option

  module type Sig = sig

    (** Read-only record type containing the lib's configuration options *)
    type t

    (** Type of functions with optional arguments for setting each of [t]'s
        fields, similarly named, and returning ['a] *)
    type 'a options_fun

    (** The default values of the options to use at startup *)
    val default: t

    (** Use to update any option in a [t], using the optional arguments of
        [options_fun]. E.g. [set opts ?option1:1 ?option4:"x" ()] *)
    val set: t -> (unit -> t) options_fun

    (** Same as [set], but passes the result to a continuation, allowing
        argument stacking *)
    val setk: (t -> 'a) -> t -> 'a options_fun

    (** The global reference containing the currently set library options.
        Access using [OpamXxxConfig.(!r.field)]. *)
    val r: t ref

    (** Updates the currently set options in [r] according to the optional
        arguments *)
    val update: ?noop:_ -> (unit -> unit) options_fun

    (** Sets the options, reading the environment to get default values when
        unspecified *)
    val init: ?noop:_ -> (unit -> unit) options_fun

    (** Sets the options like [init], but returns the given value (for arguments
        stacking) *)
    val initk: 'a -> 'a options_fun

  end

  (* Opam environment variables handling *)
  module E : sig
    type t = ..
    type t += REMOVED
    val find: (t -> 'a option) -> 'a
    (* Lazy *)
    val value: (t -> 'a option) -> (unit -> 'a option)
    (* Not lazy *)
    val value_t: (t -> 'a option) -> 'a option
    val update: t -> unit
    val updates: t list -> unit
  end

end

(** {2 Polymorphic comparison functions}
    We use this module in opam codebase to flag polymorphic comparison usage.
*)
module Compare : sig
  val compare: 'a -> 'a -> int
  val equal: 'a -> 'a -> bool
  val (=): 'a -> 'a -> bool
  val (<>): 'a -> 'a -> bool
  val (<): 'a -> 'a -> bool
  val (>): 'a -> 'a -> bool
  val (<=): 'a -> 'a -> bool
  val (>=): 'a -> 'a -> bool
end
