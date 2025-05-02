open Import

module Source : sig
  type kind =
    | Stubs of Foreign_stubs.t
    | Ctypes of Ctypes_field.t

  (** A foreign source file that has a [path] and all information of the
    corresponding [Foreign.Stubs.t] declaration. *)
  type t

  val kind : t -> kind
  val language : t -> Foreign_language.t
  val mode : t -> Mode.Select.t
  val path : t -> Path.Build.t

  (** The name of the corresponding object file; for example, [name] for a
      source file [some/path/name.cpp] of [name_mode] if the stub is
      mode-specific. *)
  val object_name : t -> Filename.t

  (** The name of the corresponding object file without the mode suffix. This is
      useful for messages where the internally suffixed name would be confusing. *)
  val user_object_name : t -> Filename.t

  val make : kind -> path:Path.Build.t -> t
end
