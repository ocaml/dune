include Dune_filesystem_stubs.Unix_error

module Detailed = struct
  include Dune_filesystem_stubs.Unix_error.Detailed

  let to_dyn (error, syscall, arg) =
    Dyn.Record
      [ "error", String (Unix.error_message error)
      ; "syscall", String syscall
      ; "arg", String arg
      ]
  ;;

  let pp ?(prefix = "") unix_error = Pp.verbatim (prefix ^ to_string_hum unix_error)
end
