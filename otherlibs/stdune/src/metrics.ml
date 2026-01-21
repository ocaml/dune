let enabled = ref false
let enable () = enabled := true

module Timer = struct
  module Measure = struct
    type t =
      { cumulative_time : Time.Span.t
      ; count : int
      }
  end

  type t =
    { start_time : Time.t
    ; tag : string
    ; mutable stopped : bool
    }

  let aggregate = ref String.Map.empty
  let aggregated_timers () = !aggregate

  let update_aggregate { start_time; tag; stopped } =
    if not stopped
    then
      aggregate
      := String.Map.update !aggregate tag ~f:(function
           | Some { Measure.cumulative_time; count } ->
             Some
               { Measure.cumulative_time =
                   Time.Span.add cumulative_time (Time.diff (Time.now ()) start_time)
               ; count = count + 1
               }
           | None ->
             Some
               { Measure.cumulative_time = Time.diff (Time.now ()) start_time; count = 1 })
  ;;

  let start tag = { start_time = Time.now (); tag; stopped = false }

  let stop t =
    match !enabled with
    | false ->
      Code_error.raise "Tried to stop a previously stopped timer" [ "tag", String t.tag ]
    | true ->
      update_aggregate t;
      t.stopped <- true
  ;;

  let record tag ~f =
    match !enabled with
    | false -> f ()
    | true ->
      let start_time = Time.now () in
      Exn.protect ~f ~finally:(fun () ->
        update_aggregate { tag; start_time; stopped = false })
  ;;
end

let reset () = Timer.aggregate := String.Map.empty
