module T = struct
  type t =
    | Preserve
    | Clear_on_rebuild
    | Clear_on_rebuild_and_flush_history

  let repr =
    Repr.variant
      "terminal-persistence"
      [ Repr.case0 "Preserve" ~test:(function
          | Preserve -> true
          | Clear_on_rebuild | Clear_on_rebuild_and_flush_history -> false)
      ; Repr.case0 "Clear_on_rebuild" ~test:(function
          | Clear_on_rebuild -> true
          | Preserve | Clear_on_rebuild_and_flush_history -> false)
      ; Repr.case0 "Clear_on_rebuild_and_flush_history" ~test:(function
          | Clear_on_rebuild_and_flush_history -> true
          | Preserve | Clear_on_rebuild -> false)
      ]
  ;;
end

include T

let all =
  [ "preserve", Preserve
  ; "clear-on-rebuild", Clear_on_rebuild
  ; "clear-on-rebuild-and-flush-history", Clear_on_rebuild_and_flush_history
  ]
;;

include Repr.Make (T)

include Repr.Poly (struct
    type nonrec t = t

    let repr = repr
  end)
