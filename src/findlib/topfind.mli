(* $Id: topfind.mli,v 1.3 2001/10/12 20:17:40 gerd Exp $
 * ----------------------------------------------------------------------
 *
 *)

(** Load packages from toploops (including scripts) *)

val predicates : string list ref
  (** The list of predicates used for package loading *)

val add_predicates : string list -> unit
  (** Adds predicates to the list of predicates *)

val syntax : string -> unit
  (** Emulates the [-syntax] option *)

val standard_syntax : unit -> unit
  (** Adds predicates that select the standard syntax. Same as
   * [syntax "camlp4o"]
   *)

val revised_syntax : unit -> unit
  (** Adds predicates that select the revised syntax. Same as 
   * [syntax "camlp4r"] 
   *)

val don't_load : string list -> unit
  (** The packages named in pkglist are added to the list of packages which
   * are already loaded.
   *)

val don't_load_deeply : string list -> unit
  (** The packages named in pkglist and all direct and indirect ancestors
   * are added to the list of packages which are already loaded.
   *)

val load : string list -> unit
  (** The packages from the passed package list are loaded, from left to
   * right, but packages that have already been loaded are left out.
   *)

val load_deeply : string list -> unit
  (** The packages from the passed package list and all direct or indirect 
   * ancestors are loaded in topological order. Packages that have already
   * been loaded are left out.
   *)

val reset : unit -> unit
  (** All entries in the list of loaded packages that have been added by
   * [load] or [load_deeply] functions are removed from this list. This
   * means that if you execute the same [load] or [load_deeply] functions
   * again, the packages will be reloaded.
   *)

val announce : unit -> unit
  (** Output the startup message *)


(** {1 Directives}
 *
 * This module also defines the following directives for the toploop:
 *
 * - [#require "<package>"]
 *   loads the package (and if necessary the prerequisites of the package)
 * - [#camlp4o]
 *   loads camlp4 and selects standard syntax
 * - [#camlp4r]
 *   loads camlp4 and selects revised syntax
 * - [#list]
 *   lists the available packages (calls external command "ocamlfind")
 * - [#thread]
 *   enables multi-threading if possible
 * - [#predicates "p1,p2,..."]
 *   adds these predicates
 *)
