open! Import

type t =
  { to_link : Lib_flags.Lib_and_module.L.t
  ; force_linkall : bool
  }
