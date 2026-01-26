open Stdune
open Dune_action_trace
module List = ListLabels

let () =
  let now = Time.now () |> Time.to_ns in
  let args = ref [] in
  let () =
    let current_key = ref None in
    Array.to_list Sys.argv
    |> List.tl
    |> List.iter ~f:(fun v ->
      if String.starts_with ~prefix:"-" v
      then current_key := Some (String.drop v 1)
      else (
        match !current_key with
        | None -> failwith "missing field name"
        | Some k -> args := (k, v) :: !args))
  in
  let args = !args in
  let ctx = Context.create ~name:"dune-tests" in
  let duration =
    match List.assoc_opt "duration" args with
    | None -> None
    | Some s -> Some (Time.Span.of_secs (float_of_string s))
  in
  let category = List.assoc "cat" args in
  let name = List.assoc "name" args in
  let args =
    List.filter_map args ~f:(fun (k, v) ->
      if k = "cat" || k = "name" || k = "duration" then None else Some (k, Csexp.Atom v))
  in
  match duration with
  | None ->
    Context.emit ctx (Event.instant ~args ~category ~name ~time_in_nanoseconds:now ())
  | Some duration ->
    let duration_in_nanoseconds = Time.Span.to_ns duration in
    Context.emit
      ctx
      (Event.span
         ~args
         ~category
         ~name
         ~start_in_nanoseconds:now
         ~duration_in_nanoseconds
         ())
;;
