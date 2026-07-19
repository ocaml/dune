open Import

let parallelism ~build_duration ~process_time =
  match
    match Time.Span.compare build_duration Time.Span.zero with
    | Eq | Lt -> None
    | Gt ->
      (try Some (Time.Span.to_secs process_time /. Time.Span.to_secs build_duration) with
       | _ -> None)
  with
  | None -> ""
  | Some s -> sprintf "%.1fx" s
;;

let format =
  let section = function
    | "" -> ""
    | s -> "[" ^ s ^ "]"
  in
  fun ~build_duration ~process_time ->
    let parallelism = parallelism ~build_duration ~process_time in
    let duration = sprintf "%.1fs" (Time.Span.to_secs build_duration) in
    [ section duration; section parallelism ]
    |> List.filter ~f:(function
      | "" -> false
      | _ -> true)
    |> String.concat ~sep:" "
;;

let format_now started_at =
  format
    ~build_duration:(Time.diff (Time.now ()) started_at)
    ~process_time:(Metrics.Build.process_time ())
;;
