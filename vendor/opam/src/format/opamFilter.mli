(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2019 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Formulas on variables, as used in opam files build scripts

    Filters are a small language of formulas over strings and booleans used for
    conditions and text replacements. It has relational operators over strings
    (using version-number comparison), And, Or and Not boolean operations,
    dynamic casting (using strings "true" and "false"), and string
    interpolation. Variables are resolved using a user function returning an
    option, undefined values are propagated.

    String interpolation uses the syntax [%{identifier}%]

    Identifiers have the form
    {v [package:]var[?str_if_true:str_if_false_or_undef] v}

    The last optional part specifies a conversion from boolean to static strings.

    The syntax [pkg1+pkg2+pkgn:var] is allowed as a shortcut to
    [pkg1:var & pkg2:var & pkgn:var].

    The special variable [pkg:enable] is allowed as a shortcut for
    [pkg:installed?enable:disable]
*)


open OpamTypes

(** Pretty-print
    [custom ?context ?paren filter] is a function that permit to customise part
    of filter pretty-printing. *)
val to_string:
  ?custom:(context:[> `And | `Defined | `Not | `Or | `Relop ] ->
           paren:(?cond:bool -> string -> string) ->
           filter -> string option) ->
  filter -> string

(** Folds on the tree of a filter *)
val fold_down_left: ('a -> filter -> 'a) -> 'a -> filter -> 'a

(** Maps on all nodes of a filter, bottom-up *)
val map_up: (filter -> filter) -> filter -> filter

(** Returns all the variables appearing in a filter (including the ones within
    string interpolations *)
val variables: filter -> full_variable list

(** Type of filter environment. *)
type env = full_variable -> variable_contents option

(** The type of filter idents with (optionally multiple) qualifying package
    names and optional string converter. Package name [None] encodes the
    self-reference [_] *)
type fident = name option list * variable * (string * string) option

(** Maps on all variables appearing in a filter. The case where package
    variables are renamed differently and appear in a filter ident of the form
    [%{pkg1+pkg2:var}%] is not supported and raises [Invalid_argument]. *)
val map_variables: (full_variable -> full_variable) -> filter -> filter

(** Same limitation as [map_variables] *)
val map_variables_in_string:
  (full_variable -> full_variable) -> string -> string

(** Does not handle rewriting the variables to different names (which can't be
    expressed with a [fident] anymore), and raises [Invalid_argument] *)
val map_variables_in_fident:
  (full_variable -> full_variable) -> fident -> fident

(** Distributes the negations to apply only to atoms *)
val distribute_negations: ?neg:bool -> filter -> filter

(** Rewrites string interpolations within a string. [default] is applied to the
    fident string (e.g. what's between [%{] and [}%]) when the expansion is
    undefined. If unspecified, this raises [Failure].

    With [partial], [default] defaults to the identity, and is otherwise
    expected to return a fident. In this case, the returned string is supposed
    to be expanded again (expansion results are escaped, escapes are otherwise
    kept). This makes the function idempotent *)
val expand_string:
  ?partial:bool -> ?default:(string -> string) -> env -> string -> string

(** Returns the (beginning, end) offsets and substrings of any unclosed [%{]
    expansions *)
val unclosed_expansions: string -> ((int * int) * string) list

(** Computes the value of a filter. May raise [Failure] if [default] isn't
    provided *)
val eval: ?default:variable_contents -> env -> filter -> variable_contents

(** Like [eval] but casts the result to a bool. Raises [Invalid_argument] if
    not a valid bool and no default supplied. *)
val eval_to_bool: ?default:bool -> env -> filter -> bool

(** Same as [eval_to_bool], but takes an option as filter and returns always
    [true] on [None], [false] when the filter is [Undefined]. This is the
    most common behaviour for using "filters" for filtering *)
val opt_eval_to_bool: env -> filter option -> bool

(** Like [eval] but casts the result to a string *)
val eval_to_string: ?default:string -> env -> filter -> string

(** Reduces what can be, keeps the rest unchanged *)
val partial_eval: env -> filter -> filter

(** Wraps a full_variable into a fident accessor *)
val ident_of_var: full_variable -> fident

(** A fident accessor directly referring a variable with the given name *)
val ident_of_string: string -> fident

(** Resolves a filter ident. Like [eval], may raise Failure if no default is
    provided *)
val ident_value: ?default:variable_contents -> env -> fident -> variable_contents

(** Like [ident_value], but casts the result to a string *)
val ident_string: ?default:string -> env -> fident -> string

(** Like [ident_value], but casts the result to a bool *)
val ident_bool: ?default:bool -> env -> fident -> bool

(** Rewrites [basename].in to [basename], expanding interpolations.
    If the first line begins ["opam-version:"], assumes that expansion of
    variables within strings should be properly escaped. In particular, this
    means that Windows paths should expand correctly when generating .config
    files. *)
val expand_interpolations_in_file: env -> basename -> unit


(** Processes filters evaluation in a command list: parameter expansion and
    conditional filtering *)
val commands: env -> command list -> string list list

(** Process a simpler command, without filters *)
val single_command: env -> arg list -> string list

(** Extracts variables appearing in a list of commands *)
val commands_variables: command list -> full_variable list

(** Converts a generic formula to a filter, given a converter for atoms *)
val of_formula: ('a -> filter) -> 'a generic_formula -> filter

(** Resolves the filter in a filtered formula, reducing to a pure formula.

    [default] is the assumed result for undefined filters. If a version filter
    doesn't resolve to a valid version, the constraint is dropped unless
    [default_version] is specified.

    May raise, as other filter functions, if [default] is not provided and
    filters don't resolve. *)
val filter_formula:
  ?default_version:version -> ?default:bool ->
  env -> filtered_formula -> formula

(** Reduces according to what is defined in [env], and returns the simplified
    formula *)
val partial_filter_formula: env -> filtered_formula -> filtered_formula

(** A more generic formula reduction function, that takes a "partial resolver"
    as argument *)
val gen_filter_formula:
  ('a -> [< `True | `False | `Formula of 'b OpamTypes.generic_formula ]) ->
  ('c * 'a) OpamFormula.formula ->
  ('c * 'b OpamTypes.generic_formula) OpamFormula.formula


val string_of_filtered_formula: filtered_formula -> string

val variables_of_filtered_formula: filtered_formula -> full_variable list

(** Resolves the build, post, test, doc, dev flags in a filtered formula
    (which is supposed to have been pre-processed to remove switch and global
    variables). [default] determines the behaviour on undefined filters, and the
    function may raise if it is undefined. If a constraint resolves to an
    invalid version, it is dropped, or replaced with [default_version] if
    specified. If test, doc or dev are unspecified, they are assumed to be
    filtered out already and encountering them will raise an assert. *)
val filter_deps:
  build:bool -> post:bool -> ?test:bool -> ?doc:bool -> ?dev_setup:bool ->
  ?dev:bool -> ?default_version:version -> ?default:bool -> filtered_formula ->
  formula

(** The environment used in resolving the dependency filters, as per
    [filter_deps]. *)
val deps_var_env:
  build:bool -> post:bool -> ?test:bool -> ?doc:bool -> ?dev_setup:bool ->
  ?dev:bool ->
  env

(** Like [OpamFormula.simplify_version_formula], but on filtered formulas
    (filters are kept unchanged, but put in front) *)
val simplify_extended_version_formula:
  condition -> condition option

val atomise_extended:
  filtered_formula ->
  (OpamPackage.Name.t * (filter * (relop * filter) option))
    OpamFormula.formula

(* Uses [OpamFormula.sort] to sort on names, and sort version formulas with
   [simplify_extended_version_formula]. *)
val sort_filtered_formula:
  ((name * condition) -> (name * condition) -> int) -> filtered_formula ->
  filtered_formula
