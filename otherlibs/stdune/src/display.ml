module T = struct
  type t =
    | Quiet
    | Short
    | Verbose

  let repr =
    Repr.variant
      "display"
      [ Repr.case0 "Quiet" ~test:(function
          | Quiet -> true
          | Short | Verbose -> false)
      ; Repr.case0 "Short" ~test:(function
          | Short -> true
          | Quiet | Verbose -> false)
      ; Repr.case0 "Verbose" ~test:(function
          | Verbose -> true
          | Quiet | Short -> false)
      ]
  ;;
end

include T
include Repr.Make (T)
include Repr.Poly (T)
