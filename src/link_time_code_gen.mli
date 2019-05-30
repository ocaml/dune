(** {1 Handle link time code generation} *)

open Stdune

type t =
  { to_link : Lib.Lib_and_module.t list
  ; force_linkall : bool
  }

(** Generate link time code for special libraries such as [findlib.dynload] *)
val handle_special_libs
  :  Compilation_context.t
  -> package:Package.t option
  -> t Or_exn.t
