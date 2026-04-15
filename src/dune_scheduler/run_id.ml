open Import

type t =
  | Batch
  | Watch of int

let to_int = function
  | Batch -> 0
  | Watch n -> n
;;

module State = struct
  type nonrec t =
    | Batch_not_started
    | Batch_started
    | Watch of int

  let create ~watch_mode = if watch_mode then Watch 1 else Batch_not_started

  let is_watch = function
    | Batch_not_started | Batch_started -> false
    | Watch _ -> true
  ;;

  let next_to_start = function
    | Batch_not_started -> Batch
    | Batch_started ->
      Code_error.raise "batch mode may not emit more than one build run id" []
    | Watch n -> Watch n
  ;;

  let start = function
    | Batch_not_started -> Batch_started, Batch
    | Batch_started -> Code_error.raise "batch mode may not start more than one build" []
    | Watch n -> Watch (n + 1), Watch n
  ;;
end
