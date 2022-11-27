open Import

module Bin : sig
  type t

  val bin_dir_basename : Filename.t

  (** [local_bin dir] The directory which contains the local binaries viewed by
      rules defined in [dir] *)
  val local_bin : Path.Build.t -> Path.Build.t

  (** A named artifact that is looked up in the PATH if not found in the tree If
      the name is an absolute path, it is used as it. *)
  val binary :
    t -> ?hint:string -> loc:Loc.t option -> string -> Action.Prog.t Memo.t

  val binary_available : t -> string -> bool Memo.t

  module Local : sig
    type t

    val equal : t -> t -> bool

    val create : Path.Build.Set.t -> t
  end

  val create : context:Context.t -> local_bins:Local.t -> t

  val add_binaries : t -> dir:Path.Build.t -> File_binding.Expanded.t list -> t
end

module Public_libs : sig
  type t =
    { context : Context.t
    ; public_libs : Lib.DB.t
    }

  (** [file_of_lib t ~from ~lib ~file] returns the path to a file in the
      directory of the given library. *)
  val file_of_lib :
    t -> loc:Loc.t -> lib:Lib_name.t -> file:string -> Path.t Resolve.Memo.t
end

type t = private
  { public_libs : Public_libs.t
  ; bin : Bin.t
  }

val create : Context.t -> public_libs:Lib.DB.t -> local_bins:Bin.Local.t -> t
