open Import
open Exported_types

type t =
  | Waiting
  | In_progress of
      { complete : int
      ; remaining : int
      }
  | Failed
  | Interrupted
  | Success

let sexp =
  let open Conv in
  let waiting = constr "waiting" unit (fun () -> Waiting) in
  let failed = constr "failed" unit (fun () -> Failed) in
  let in_progress =
    let complete = field "complete" (required int) in
    let remaining = field "remaining" (required int) in
    constr
      "in_progress"
      (record (both complete remaining))
      (fun (complete, remaining) -> In_progress { complete; remaining })
  in
  let interrupted = constr "interrupted" unit (fun () -> Interrupted) in
  let success = constr "success" unit (fun () -> Success) in
  let constrs =
    List.map ~f:econstr [ waiting; failed; interrupted; success ]
    @ [ econstr in_progress ]
  in
  let serialize = function
    | Waiting -> case () waiting
    | In_progress { complete; remaining } -> case (complete, remaining) in_progress
    | Failed -> case () failed
    | Interrupted -> case () interrupted
    | Success -> case () success
  in
  sum constrs serialize
;;

let to_progress : t -> Progress.t = function
  | Waiting -> Waiting
  | In_progress { complete; remaining } -> In_progress { complete; remaining; failed = 0 }
  | Failed -> Failed
  | Interrupted -> Interrupted
  | Success -> Success
;;

let of_progress : Progress.t -> t = function
  | Waiting -> Waiting
  | In_progress { complete; remaining; failed = _ } -> In_progress { complete; remaining }
  | Failed -> Failed
  | Interrupted -> Interrupted
  | Success -> Success
;;
