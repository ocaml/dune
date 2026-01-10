module List = ListLabels

module Private = struct
  let trace_dir_env_var = "DUNE_ACTION_TRACE_DIR"
end

module Event = struct
  type t = Csexp.t

  module Arg = struct
    let string s = Csexp.Atom s
    let float x = string (string_of_float x)
    let list xs = Csexp.List xs
    let record xs = List.map xs ~f:(fun (k, v) -> list [ string k; v ])
    let time ts = float ts
    let span span = float span
  end

  let base ~name cat : Csexp.t list = [ Atom cat; Atom name ]

  let instant ?(args = []) ~category ~name ~time_in_seconds () =
    Csexp.List (base ~name category @ [ Arg.time time_in_seconds ] @ Arg.record args)
  ;;

  let span ?(args = []) ~category ~name ~start_in_seconds ~duration_in_seconds () =
    Csexp.List
      (base ~name category
       @ [ Csexp.List [ Arg.time start_in_seconds; Arg.span duration_in_seconds ] ]
       @ Arg.record args)
  ;;
end

module Sys = struct
  let[@warning "-32"] mkdir _ = failwith "unsupported"

  include Sys
end

module Context = struct
  type state =
    | Open of out_channel
    | Closed

  type t =
    | Disabled
    | Enabled of state ref

  let create ~name =
    match Sys.getenv_opt Private.trace_dir_env_var with
    | None -> Disabled
    | Some dir ->
      (match
         match Sys.mkdir dir 0o777 with
         | () -> `Ok
         | exception Sys_error _ when Sys.is_directory dir -> `Ok
         | exception _ -> `Failure
       with
       | `Failure -> Disabled
       | `Ok ->
         Enabled (ref (Open (Filename.open_temp_file ~temp_dir:dir name ".csexp" |> snd))))
  ;;

  let is_enabled = function
    | Disabled -> false
    | Enabled x ->
      (match !x with
       | Open _ -> true
       | Closed -> false)
  ;;

  let emit t event =
    match t with
    | Disabled -> ()
    | Enabled state ->
      (match !state with
       | Closed -> failwith "unable to log event because context is closed"
       | Open chan -> Csexp.to_channel chan event)
  ;;

  let close t =
    match t with
    | Disabled -> ()
    | Enabled state ->
      (match !state with
       | Closed -> ()
       | Open chan ->
         state := Closed;
         close_out chan)
  ;;
end
