open Import

type t =
  | Batch
  | Watch of int

let repr =
  Repr.variant
    "Run_id.t"
    [ Repr.case0 "Batch" ~test:(function
        | Batch -> true
        | Watch _ -> false)
    ; Repr.case "Watch" Repr.int ~proj:(function
        | Batch -> None
        | Watch i -> Some i)
    ]
;;

include Repr.Poly (struct
    type nonrec t = t

    let repr = repr
  end)

let to_int = function
  | Batch -> 0
  | Watch n -> n
;;
