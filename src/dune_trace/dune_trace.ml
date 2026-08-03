open Stdune
module Category = Category
module Event = Event
module File_watcher_event = File_watcher_event
module Out = Out

type ownership =
  | Owned of Path.t
  | Borrowed

type global =
  { out : Out.t
  ; ownership : ownership
  }

let global = ref None

let reset_alloc_profile () =
  Option.iter !global ~f:(fun { out; _ } -> Option.iter (Out.alloc out) ~f:Alloc.reset)
;;

let capture_alloc_profile kind =
  match
    match !global with
    | None -> None
    | Some { out; _ } -> Out.alloc out
  with
  | None -> None
  | Some alloc ->
    let { Alloc.minor; major; promoted } = Alloc.snapshot alloc in
    let phase, run_id =
      match kind with
      | `Build run_id -> `Build, Some run_id
      | `Exit -> `Exit, None
    in
    Some (Event.alloc_summary ~phase ~run_id ~minor ~major ~promoted)
;;

let at_exit =
  At_exit.at_exit Global_lock.at_exit (fun () ->
    match !global with
    | None -> ()
    | Some { out; ownership = Borrowed } ->
      Out.emit_runtime out;
      let alloc_summary = capture_alloc_profile `Exit in
      Option.iter (Out.alloc out) ~f:Alloc.stop;
      Option.iter alloc_summary ~f:(Out.emit out);
      Out.close out
    | Some { out; ownership = Owned path } ->
      Out.emit_runtime out;
      let alloc_summary = capture_alloc_profile `Exit in
      Option.iter (Out.alloc out) ~f:Alloc.stop;
      Option.iter alloc_summary ~f:(Out.emit out);
      Out.emit out (Event.exit ());
      Out.close out;
      (match Env.(get initial Dune_action_trace.Private.trace_dir_env_var) with
       | None -> ()
       | Some dir ->
         let dir = Path.of_string dir in
         Path.mkdir_p dir;
         let dst =
           Path.relative
             (Temp.temp_dir ~parent_dir:dir ~prefix:"dune" ~suffix:"trace")
             "trace.csexp"
         in
         Io.copy_file ~src:path ~dst ()))
;;

let set_global_impl ~ownership out =
  if Option.is_some !global then Code_error.raise "global stats have been set" [];
  global := Some { out; ownership }
;;

let set_global t ~path = set_global_impl ~ownership:(Owned path) t

let set_global_inherited_fd ?(common_args = []) fd =
  Fd.set_close_on_exec fd;
  let out = Out.create (`Fd fd) in
  set_global_impl ~ownership:Borrowed out;
  Event.Event.set_common_args common_args
;;

let global () = Option.map !global ~f:(fun { out; _ } -> out)

let duplicate_global_fd () =
  if Sys.win32
  then None
  else (
    match global () with
    | None -> None
    | Some out ->
      let fd = Fd.dup (Out.fd out) in
      Fd.clear_close_on_exec fd;
      Some fd)
;;

let always_emit event =
  match global () with
  | None -> ()
  | Some out -> Out.emit out event
;;

let emit ?buffered cat f =
  match global () with
  | None -> ()
  | Some out -> if Category.Set.mem (Out.cats out) cat then Out.emit ?buffered out (f ())
;;

let flush () =
  match global () with
  | None -> ()
  | Some out -> Out.flush out
;;

let emit_runtime () = Option.iter (global ()) ~f:Out.emit_runtime

let emit_all ?buffered cat f =
  match global () with
  | None -> ()
  | Some out ->
    if Category.Set.mem (Out.cats out) cat
    then List.iter (f ()) ~f:(Out.emit ?buffered out)
;;

let enabled cat =
  match global () with
  | None -> false
  | Some s -> Category.Set.mem (Out.cats s) cat
;;

module Private = struct
  module Fd_count = Fd_count
  module Buffer = Buffer
end
