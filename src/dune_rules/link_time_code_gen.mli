(** {1 Handle link time code generation} *)

type t = Link_time_code_gen_type.t =
  { to_link : Lib_flags.Lib_and_module.L.t
  ; force_linkall : bool
  }

(** Generate link time code for special libraries such as [findlib.dynload] *)
val handle_special_libs : Compilation_context.t -> t Resolve.t Memo.t
