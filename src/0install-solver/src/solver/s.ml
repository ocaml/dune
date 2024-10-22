(* Copyright (C) 2013, Thomas Leonard
   See the README file for details, or visit http://0install.net. *)

(** Some useful abstract module types. *)

module type Monad = sig
  type 'a t

  val return : 'a -> 'a t

  module List : sig
    val iter : ('a -> unit t) -> 'a list -> unit t
    val iter2 : ('a -> 'b -> unit t) -> 'a list -> 'b list -> unit t
  end

  module Seq : sig
    val parallel_map : ('a -> 'b t) -> 'a Seq.t -> 'b Seq.t t
  end

  module O : sig
    val (>>|) : 'a t -> ('a -> 'b) -> 'b t
    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    val (let+) : 'a t -> ('a -> 'b) -> 'b t
    val (let*) : 'a t -> ('a -> 'b t) -> 'b t
  end
end

module type CORE_MODEL = sig
  (** To use the solver with a particular packaging system (e.g. 0install), you need
      to provide an implementation of this module to map your system's concepts on to
      the solver's. *)

  module Role : sig
    (** A role that needs to be filled by a single implementation.
        If two dependencies require the same role then they will both
        get the same implementation.
        This could be the package name, although 0install also includes
        whether you want source code or a binary in the role. *)
    type t
    val pp : Format.formatter -> t -> unit
    val compare : t -> t -> int
  end

  (** An [impl] is something that can fill a [Role.t] (e.g. a particular version of
      a package). *)
  type impl

  (** A [command] is an entry-point provided by an implementation.
      Using a command may require extra dependencies (for example, a "test" command
      might depend on a test runner). *)
  type command

  (** An identifier for a command within a role.
      Note: It might not be necessary to use any commands - we could instead
      treat the command name as an optional part of the role, and treat each
      command as a separate impl instead. *)
  type command_name = private string

  (** A dependency indicates that an impl or command requires another role to be filled. *)
  type dependency

  type dep_info = {
    dep_role : Role.t;

    (* If the dependency is [`Essential] then its role must be filled.
       Otherwise, we just prefer to fill it if possible.
       A [`Restricts] dependency does not cause the solver to try to fill a role, it just
       adds restrictions if it is used for some other reason. *)
    dep_importance : [ `Essential | `Recommended | `Restricts ];

    (* Any commands on [dep_role] required by this dependency. *)
    dep_required_commands : command_name list;
  }

  (* The top-level requirements from the user. *)
  type requirements = {
    role : Role.t;
    command : command_name option;
  }

  (** Get an implementation's dependencies.
    
     The dependencies should be ordered with the most important first.
     The solver will prefer to select the best possible version of an earlier
     dependency, even if that means selecting a worse version of a later one
     ([restricts_only] dependencies are ignored for this).
    
     An implementation or command can also bind to itself.
     e.g. "test" command that requires its own "run" command.
     We also return all such required commands. *)
  val requires : Role.t -> impl -> dependency list * command_name list

  val dep_info : dependency -> dep_info

  (** As [requires], but for commands. *)
  val command_requires : Role.t -> command -> dependency list * command_name list

  val get_command : impl -> command_name -> command option
end

module type SOLVER_INPUT = sig
  type 'a monad
  (** This defines what the solver sees (hiding the raw XML, etc). *)

  include CORE_MODEL

  (** Information provided to the solver about a role. *)
  type role_information = {
    replacement : Role.t option;  (** Another role that conflicts with this one. *)
    impls : impl list;            (** Candidates to fill the role. *)
  }

  (** A restriction limits which implementations can fill a role. *)
  type restriction

  (** The solver will avoid selections with mixed machine groups.
      This is useful if e.g. the CPU supports 32-bit and 64-bit programs,
      but we can't mix them in a single process. The string simply has
      to be unique for each group (e.g. "32" and "64"). *)
  type machine_group = private string

  val pp_impl : Format.formatter -> impl -> unit
  val pp_command : Format.formatter -> command -> unit

  (** The list of candidates to fill a role. *)
  val implementations : Role.t -> role_information monad

  (** Restrictions on how the role is filled *)
  val restrictions : dependency -> restriction list
  val meets_restriction : impl -> restriction -> bool

  val machine_group : impl -> machine_group option

  (** There can be only one implementation in each conflict class. *)
  type conflict_class = private string
  val conflict_class : impl -> conflict_class list

  (** {2 The following are used for diagnostics only} *)

  (** The reason why the model rejected an implementation before it got to the solver.
      For example, the implementation was a Windows binary but the host is Linux. *)
  type rejection

  (** Get the candidates which were rejected for a role (and not passed to the solver),
      as well as any general notes and warnings not tied to a particular impl. *)
  val rejects : Role.t -> ((impl * rejection) list * string list) monad

  (** Used to sort the results. *)
  val compare_version : impl -> impl -> int
  val pp_version : Format.formatter -> impl -> unit

  (** Get any user-specified restrictions affecting a role.
      These are used to filter out implementations before they get to the solver. *)
  val user_restrictions : Role.t -> restriction option

  (** A detailed identifier for the implementation. In 0install, this is the version
      number and part of the hash. *)
  val pp_impl_long : Format.formatter -> impl -> unit

  val format_machine : impl -> string
  val string_of_restriction : restriction -> string
  val describe_problem : impl -> rejection -> string

  (** A dummy implementation, used to get diagnostic information if the solve fails.
      It satisfies all requirements, even conflicting ones. *)
  val dummy_impl : impl
end

module type SELECTIONS = sig
  (** Some selections previously produced by a solver. *)

  include CORE_MODEL

  module RoleMap : Map.S with type key = Role.t

  type t

  val to_map : t -> impl RoleMap.t
  val get_selected : Role.t -> t -> impl option
  val selected_commands : impl -> command_name list
  val requirements : t -> requirements
end

module type SOLVER_RESULT = sig
  (** The result of running the solver.
      Unlike the plain [SELECTIONS] type, this type can relate the selections back
      to the solver inputs, which is useful to provide diagnostics and the GUI. *)

  module Input : SOLVER_INPUT

  include SELECTIONS with
    module Role = Input.Role and
    type command = Input.command and
    type dependency = Input.dependency and
    type command_name = Input.command_name and
    type dep_info = Input.dep_info and
    type requirements = Input.requirements

  val unwrap : impl -> Input.impl

  (** Get diagnostics-of-last-resort. *)
  val explain : t -> Role.t -> string
end
