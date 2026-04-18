open Stdune

module Reproducibility_check = struct
  (* CR-someday amokhov: Add [Check_and_repair] to rewrite cache entries if they
     disagree with the check. *)
  type t =
    | Skip
    | Check_with_probability of float
    | Check

  let repr =
    Repr.variant
      "reproducibility-check"
      [ Repr.case0 "Skip" ~test:(function
          | Skip -> true
          | Check_with_probability _ | Check -> false)
      ; Repr.case "Check_with_probability" Repr.float ~proj:(function
          | Check_with_probability p -> Some p
          | Skip | Check -> None)
      ; Repr.case0 "Check" ~test:(function
          | Check -> true
          | Skip | Check_with_probability _ -> false)
      ]
  ;;

  let equal a b =
    match a, b with
    | Skip, Skip | Check, Check -> true
    | Check_with_probability a, Check_with_probability b -> Float.equal a b
    | _, _ -> false
  ;;

  let sample = function
    | Skip -> false
    | Check_with_probability p -> Random.float 1. < p
    | Check -> true
  ;;

  let to_dyn = Repr.to_dyn repr

  let check_with_probability ?loc p =
    let error () =
      User_error.raise
        ?loc
        [ Pp.text "The reproducibility check probability must be in the range [0, 1]." ]
    in
    if p < 0.
    then error ()
    else if p = 0.
    then Skip
    else if p < 1.
    then Check_with_probability p
    else if p = 1.
    then Check
    else error ()
  ;;
end

type t =
  | Disabled
  | Enabled of
      { storage_mode : Mode.t
      ; reproducibility_check : Reproducibility_check.t
      }
