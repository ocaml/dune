open Sexp.Of_sexp

module Context = struct
  type t =
    { coverage: string
    ; covered_libs : Ordered_set_lang.t
    }

  let t =
    record (
      field "coverage" string >>= fun coverage ->
      field "covered_libs" ~default:Ordered_set_lang.standard
        Ordered_set_lang.t >>= fun covered_libs ->
      return
        { coverage
        ; covered_libs
        }
    )
end
