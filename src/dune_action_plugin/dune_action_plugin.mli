module V1 : sig
  (** Applicative and monadic interface for declaring dependencies.

      This module is intended to be used as an interface for declaring
      dependencies of a computation. Dependencies can be declared dynamically -
      the list of dependencies can depend on previous dependencies.

      Note: Monadic "bind" is provided, but it can be very costly. It's called
      [stage] to discourage people from overusing it. When dune decides that
      the action needs to be re-run, it runs stages one by one, and starts a
      process from scratch for every stage. So a linear chain of binds leads to
      a linear number of program re-runs, and therefore overall quadratic time
      complexity. This also means that using non-deterministic mutable state
      can lead to surprising results. *)

  module Path = Path

  type 'a t

  (** {1:monadic_interface Applicative/monadic interface} *)

  (** [return a] creates a pure computation resulting in [a]. *)
  val return : 'a -> 'a t

  (** If [at] is a computation resulting in [a] then [map at ~f] is a
      computation resulting in [f a]. *)
  val map : 'a t -> f:('a -> 'b) -> 'b t

  (** If [at] is a computation resulting in [a] and [bt] is computation
      resulting in [b] then [both at bt] is a computation resulting in [(a,
      b)]. *)
  val both : 'a t -> 'b t -> ('a * 'b) t

  (** If [at] is a computation resulting in value of type ['a] and [f] is a
      function taking value of type ['a] and returning a computation [bt] then
      [stage a ~f] is a computation that is equivalent to staging computation
      [bt] after computation [at].

      Note: This is a monadic "bind" function. This function is costly so
      different name was chosen to discourage excessive use. *)
  val stage : 'a t -> f:('a -> 'b t) -> 'b t

  (** {1 Syntax sugar for applicative subset} *)

  (** Syntax sugar for applicative subset of the interface. Syntax sugar for
      [stage] is not provided to prevent accidential use.*)
  module O : sig
    (** {[ let+ a = g in h ]} is equivalent to {[ map g ~f:(fun a -> g) ]}. *)
    val ( let+ ) : 'a t -> ('a -> 'b) -> 'b t

    (** {[ let+ a1 = g1 and+ a2 = g2 in h ]} is equivalent to {[ both g1 g2 |>
        map ~f:(fun (a1, a2) -> g) ]}. *)
    val ( and+ ) : 'a t -> 'b t -> ('a * 'b) t
  end

  (** {1:interaction Declaring dependencies and interacting with filesystem} *)

  (** [read_file ~path:file] returns a computation depending on a [file] to be
      run and resulting in a file content. *)
  val read_file : path:Path.t -> string t

  (** [write_file ~path:file ~data] returns a computation that writes [data] to
      a [file].

      Note: [file] must be declared as a target in dune build file. *)
  val write_file : path:Path.t -> data:string -> unit t

  (** [read_directory ~path:directory] returns a computation depending on a
      listing of a [directory] and all source and target files contained in
      that directory. Computation will result in a directory listing. *)
  val read_directory : path:Path.t -> string list t

  (** {1:running Running the computation} *)

  (** Runs the computation. This function never returns. *)
  val run : unit t -> 'a
end

(* [Private] module should only be used by dune itself. Its stability will not
   be maintained by the future library releases. *)
module Private : sig
  module Protocol = Protocol

  val do_run : unit V1.t -> unit

  module Execution_error : sig
    exception E of string
  end
end
