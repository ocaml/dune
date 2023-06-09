open Import
open Stdune

(** Common library for "dune describe" style commands. This module contains two
    modules:

    - [Desc] which is a description of a workspace that can be serialised.
    - [Format] which provides outputs serialisation support for a command. *)

(** The module [Descr] is a typed representation of the description of a
    workspace, that is provided by the ``dune describe workspace`` command.

    Each sub-module contains a [to_dyn] function, that translates the
    descriptors to a value of type [Dyn.t].

    The typed representation aims at precisely describing the structure of the
    information computed by ``dune describe``, and hopefully make users' life
    easier in decoding the S-expressions into meaningful contents. *)
module Descr : sig
  (** Option flags for what to do while crawling the workspace *)
  type options =
    { with_deps : bool
          (** whether to compute direct dependencies between modules *)
    ; with_pps : bool
          (** whether to include the dependencies to ppx-rewriters (that are
              used at compile time) *)
    }

  (** [dyn_path p] converts a path to a value of type [Dyn.t]. Remark: this is
      different from Path.to_dyn, that produces extra tags from a variant
      datatype. *)
  val dyn_path : Path.t -> Dyn.t

  (** Description of the dependencies of a module *)
  module Mod_deps : sig
    type t =
      { for_intf : Dune_rules.Module_name.t list
            (** direct module dependencies for the interface *)
      ; for_impl : Dune_rules.Module_name.t list
            (** direct module dependencies for the implementation *)
      }

    (** Conversion to the [Dyn.t] type *)
    val to_dyn : t -> Dyn.t
  end

  (** Description of modules *)
  module Mod : sig
    type t =
      { name : Dune_rules.Module_name.t  (** name of the module *)
      ; impl : Path.t option  (** path to the .ml file, if any *)
      ; intf : Path.t option  (** path to the .mli file, if any *)
      ; cmt : Path.t option  (** path to the .cmt file, if any *)
      ; cmti : Path.t option  (** path to the .cmti file, if any *)
      ; module_deps : Mod_deps.t  (** direct module dependencies *)
      }

    (** Conversion to the [Dyn.t] type *)
    val to_dyn : options -> t -> Dyn.t
  end

  (** Description of executables *)
  module Exe : sig
    type t =
      { names : string list  (** names of the executable *)
      ; requires : Digest.t list
            (** list of direct dependencies to libraries, identified by their
                digests *)
      ; modules : Mod.t list
            (** list of the modules the executable is composed of *)
      ; include_dirs : Path.t list  (** list of include directories *)
      }

    val map_path : t -> f:(Path.t -> Path.t) -> t

    (** Conversion to the [Dyn.t] type *)
    val to_dyn : options -> t -> Dyn.t
  end

  (** Description of libraries *)
  module Lib : sig
    type t =
      { name : Lib_name.t  (** name of the library *)
      ; uid : Digest.t  (** digest of the library *)
      ; local : bool  (** whether this library is local *)
      ; requires : Digest.t list
            (** list of direct dependendies to libraries, identified by their
                digests *)
      ; source_dir : Path.t
            (** path to the directory that contains the sources of this library *)
      ; modules : Mod.t list
            (** list of the modules the executable is composed of *)
      ; include_dirs : Path.t list  (** list of include directories *)
      }

    val map_path : t -> f:(Path.t -> Path.t) -> t

    (** Conversion to the [Dyn.t] type *)
    val to_dyn : options -> t -> Dyn.t
  end

  (** Description of items: executables, or libraries *)
  module Item : sig
    type t =
      | Executables of Exe.t
      | Library of Lib.t
      | Root of Path.t
      | Build_context of Path.t

    val map_path : t -> f:(Path.t -> Path.t) -> t

    (** Conversion to the [Dyn.t] type *)
    val to_dyn : options -> t -> Dyn.t
  end

  (** Description of a workspace: a list of items *)
  module Workspace : sig
    type t = Item.t list

    (** Conversion to the [Dyn.t] type *)
    val to_dyn : options -> t -> Dyn.t
  end
end

module Format : sig
  type t =
    | Sexp
    | Csexp

  val arg : t Term.t

  val print_dyn : t -> Dyn.t -> unit
end
