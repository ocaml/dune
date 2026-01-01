open Stdune
module Category = Category
module Event = Event
module Out = Out

let global = ref None

let () =
  at_exit (fun () ->
    match !global with
    | None -> ()
    | Some t ->
      Out.emit t (Event.exit ());
      Out.close t)
;;

let set_global t =
  if Option.is_some !global then Code_error.raise "global stats have been set" [];
  global := Some t
;;

let global () = !global

let always_emit event =
  match global () with
  | None -> ()
  | Some out -> Out.emit out event
;;

let emit cat f =
  match global () with
  | None -> ()
  | Some out -> if Category.Set.mem out.cats cat then Out.emit out (f ())
;;

let emit_all cat f =
  match global () with
  | None -> ()
  | Some out -> if Category.Set.mem out.cats cat then List.iter (f ()) ~f:(Out.emit out)
;;

module Private = struct
  module Fd_count = Fd_count
  module Buffer = Buffer
end
