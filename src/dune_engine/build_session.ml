open Import
open Fiber.O

(* Per-build context that holds state for a single build execution.
   This allows multiple builds to run concurrently without interfering
   with each other. *)

module Progress = struct
  type t =
    { number_of_rules_discovered : int
    ; number_of_rules_executed : int
    ; number_of_rules_failed : int
    }

  let init =
    { number_of_rules_discovered = 0
    ; number_of_rules_executed = 0
    ; number_of_rules_failed = 0
    }
  ;;
end

type t =
  { id : Build_id.t
  ; progress : Progress.t Fiber.Svar.t
  ; errors : Build_system_error.Set.t Fiber.Svar.t
  ; pending_targets : Targets.t ref
  ; finalize_hooks : (unit -> unit Fiber.t) Queue.t
  }

let create () =
  { id = Build_id.create ()
  ; progress = Fiber.Svar.create Progress.init
  ; errors = Fiber.Svar.create Build_system_error.Set.empty
  ; pending_targets = ref Targets.empty
  ; finalize_hooks = Queue.create ()
  }
;;

let id t = t.id

(* Progress tracking *)

let get_progress t = Fiber.return (Fiber.Svar.read t.progress)
let set_progress t progress = Fiber.Svar.write t.progress progress

let update_progress t ~f =
  let* current = get_progress t in
  set_progress t (f current)
;;

let incr_rules_discovered t =
  update_progress t ~f:(fun p ->
    { p with number_of_rules_discovered = p.number_of_rules_discovered + 1 })
;;

let incr_rules_executed t =
  update_progress t ~f:(fun p ->
    { p with number_of_rules_executed = p.number_of_rules_executed + 1 })
;;

let incr_rules_failed t =
  update_progress t ~f:(fun p ->
    { p with number_of_rules_failed = p.number_of_rules_failed + 1 })
;;

(* Error tracking *)

let get_errors t = Fiber.return (Fiber.Svar.read t.errors)

let add_errors t error_list =
  let* current = get_errors t in
  let updated = List.fold_left error_list ~init:current ~f:Build_system_error.Set.add in
  Fiber.Svar.write t.errors updated
;;

let reset_errors t = Fiber.Svar.write t.errors Build_system_error.Set.empty

(* Hooks *)

let add_finalize_hook t hook = Queue.push t.finalize_hooks hook

let run_finalize_hooks t =
  let open Fiber.O in
  let hooks = Queue.to_list t.finalize_hooks in
  Queue.clear t.finalize_hooks;
  let* results =
    Fiber.parallel_map hooks ~f:(fun hook -> Fiber.collect_errors (fun () -> hook ()))
  in
  let exns =
    List.filter_map results ~f:(function
      | Ok () -> None
      | Error exns -> Some exns)
    |> List.concat
  in
  match exns with
  | [] -> Fiber.return ()
  | [ { Exn_with_backtrace.exn = User_error.E _ as e; backtrace = _ } ] -> raise e
  | exns ->
    Code_error.raise
      "finalize hooks failed"
      [ "build_id", Build_id.to_dyn t.id
      ; "exns", Dyn.list Exn_with_backtrace.to_dyn exns
      ]
;;

(* Pending targets *)

let add_pending_targets t targets =
  t.pending_targets := Targets.combine !(t.pending_targets) targets
;;

let remove_pending_targets t targets =
  t.pending_targets := Targets.diff !(t.pending_targets) targets
;;

let cleanup_pending_targets t =
  let targets = !(t.pending_targets) in
  t.pending_targets := Targets.empty;
  Targets.iter targets ~file:Path.Build.unlink_no_err ~dir:(fun p ->
    Path.rm_rf (Path.build p))
;;

let to_dyn t =
  let open Dyn in
  record
    [ "id", Build_id.to_dyn t.id; "pending_targets", Targets.to_dyn !(t.pending_targets) ]
;;
