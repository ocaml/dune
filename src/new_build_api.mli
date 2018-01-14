(** Dune build scripts *)

(** This module defines the type of Dune build scripts.

    The way Dune works is by producing a build script from looking at
    the file systems and reading user configuration files. This build
    script is then evuluated lazily in order to produce the requested
    targets.

    Build scripts are dynamic and can adapt to external changes such
    as files and directories being modified, however it is encouraged
    to write as much as possible using static constructions, as
    detailed later. The reason for that is that when executing a static
    script, Dune can partially see the future and what effects the
    script is going to have on the file system. This allows Dune to
    guide the build in order to go as fast as possible.

    Dune makes the assumption that build scripts are pure. In
    particular, it is not allowed to use global variables or access the
    file system directly. Instead, one must use the primitives declared
    in this module.
*)

(** The [t] data type represent the type of build scripts. ['a] is the
    type of values that are provided to the script at execution time
    and ['b] is the type of values it produces. *)
type ('a, 'b) t

(** [arr f] is the simplest form of build scripts. [f] is an OCaml
    function that will be executed when the build script is
    evaluated.

    Warning: [f] must not access global variables or files.
*)
val arr : ('a -> 'b) -> ('a, 'b) t

(** [return] is the same as [arr (fun x -> x)]. *)
val return : ('a, 'a) t

(** [const v] is a node that always evaluate to the same value. *)
val const : 'a -> (unit, 'a) t

module O : sig
  (** The following operators allow to compose [t] values in order to
      build complex build scripts. *)

  val ( >>> ) : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
  val ( ^>> ) : ('a -> 'b) -> ('b, 'c) t -> ('a, 'c) t
  val ( >>^ ) : ('a, 'b) t -> ('b -> 'c) -> ('a, 'c) t

  val ( <<< ) : ('b, 'c) t -> ('a, 'b) t -> ('a, 'c) t
  val ( ^<< ) : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val ( <<^ ) : ('b, 'c) t -> ('a -> 'b) -> ('a, 'c) t

  val ( *** ) : ('a, 'b) t -> ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  val ( &&& ) : ('a, 'b) t -> ('a, 'c) t -> ('a, 'b * 'c) t
end

module Ext : sig
  type ('a, 'b) build = ('a, 'b) t

  (** An [Ext.t] value represent an external action. For instance a
      command to run. *)
  type t =
    { deps   : Path.Set.t  (** List of files that we'll be read when
                               execution [acton]. *)
    ; locks  : Path.t list (** List of lock files that must be held
                               while [action] is running. *)
    ; action : Action.t    (** The actual action to execute. *)
    }

  (** [add_dep path] is the same as:

      {[
        arr (fun ext ->
          { ext with deps = Path.Set.add path deps })
      ]}

      However, because [path] is given statically, Dune is able to
      know earlier about this dependency and can parallelize the build
      better.

      Note that [add_dep] can only be called inside a call to [ext].
      I.e. the following code is valid:

      {[
        Biild.ext ~targets (Build.Ext.add_dep path <<< t)
      ]}

      but this one isn't:

      {[
        t >>> Build.Ext.add_dep path >>>
        Biild.ext ~targets Build.return
      ]}
  *)
  val add_dep : Path.t -> (t, t) build
end with type ('a, 'b) build := ('a, 'b) t

(** Declares an external action. [targets] is the set of files the
    action is expected to write when executed. *)
val ext : targets:Path.Set.t -> ('a, Ext.t) t -> ('a, unit) t

(** [dynamic t] introduces dynamicity in build scripts. This allow to
    use values that are computed while the script is being executed in
    places where only static values are accepted.

    For instance [ext] only accept static values for
    [targets]. i.e. all targets must be known at the point the call to
    [ext] appears in the source code. By using [dynamic], you can use
    computed values for [targets].

    Note that Dune cannot see past [dynamic] nodes, so at execution
    time it might have to eagerly evaluate script up to certain dynamic
    nodes to see what follows. As a result, it is recommended to only
    use [dynamic] when absolutely necessary. *)
val dynamic : ('a, ('a, 'b) t) t -> ('a, 'b) t

(** {2 IOs} *)

(** Read a file. *)
val read : Path.t -> (unit, string) t

(** [files_of ~dir] evaluates to the list of files in [dir]. *)
val files_of : dir:Path.t -> (unit, Path.Set.t) t

(** [files_matching ~dir re] computes the set of files matching a
    certain regular expression in [dir]. This is the same as:

    {[
      fiels_of ~dir >>^ Path.Set.filter ~f:(Re.execp re)
    ]}

    However, [files_matching] produces build scripts that behave better
    in polling mode.
*)
val files_matching : dir:Path.t -> Re.re -> (unit, Path.Set.t) t

(** Same as [files_of ~dir >>^ Path.Set.mem path] but behaves better
    in polling mode. *)
val file_exists : Path.t -> (unit, bool) t

(** List of sub-directories of the given direcrtory. *)
val sub_dirs : dir:Path.t -> (unit, string list) t

(** {2 Variables} *)

(** As said previously, it is not allowed to use global variables
    inside build script. Instead Dune offers an alternative mechanism
    to share computed values between different part of a build script.

    It is essentially the same as writing out a value to a file and
    later reading this file. The only difference is that the value is
    not actually written out and is kept in memory. The command line
    option [--debug-vars] forces all variables to be written out.
*)

(** Type of variables. *)
module Type : sig
  type 'a t

  (** [create to_sexp] creates a new variable type. [to_sexp] is only
      used when [--debug-vars] is used. *)
  val create : ('a -> Sexp.t) -> 'a t
end

(** [set_var path type] set the value of variable [path]. *)
val set_var : Path.t -> 'a Type.t -> ('a, unit) t

(** [get_var path type] reads the value of a variable. *)
val get_var : Path.t -> 'a Type.t -> (unit, 'a) t

(** {2 Partitions} *)

(** To improve performances, the user can partition the file system
    into section. Sections allows Dune to quickly locate what part of a
    build script can generate a certain file.
*)

(** [define_section ~dir ~limits t] defines a section of the file system.

    The section is defined as the set of directories [d] such that:

    - [Path.is_descendant d ~of_:dir]
    - [List.iter limits ~f:(fun p -> not (Path.is_descendant d ~of_:p))]

    Whenever a section is defined, the following must hold:

    - All external actions producing files in this section must be part of [t]
    - All external actions in [t] must only produce files in this section only
*)
val define_section
  :  dir:Path.t
  -> limits:Path.t list
  -> ('a, 'b) t Lazy.t
  -> ('a, 'b) t
