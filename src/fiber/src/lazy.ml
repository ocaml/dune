open Stdune
open Core
open Core.O

(** State of a lazy computation. Note that for efficiency, we have a separate variant for
    successes ([Done]). We don't have one for errors since it doesn't seem worth it; we
    get those from the [Ivar] instead. *)
type 'a state =
  | (* Finished successfully *)
    Done of 'a
  | (* Might still be running *)
    Running of ('a, Exn_with_backtrace.t list) Result.t Ivar.t
  | (* Hasn't been forced yet *)
    Init of (unit -> 'a Core.t)

type 'a t = 'a state ref

let of_value x = ref (Done x)
let unit = ref (Done ())
let create f = ref (Init f)

let prep t =
  let v = Ivar.create () in
  t := Running v;
  v
;;

let execute t v f =
  let* r = collect_errors f in
  (match r with
   | Ok x -> t := Done x
   | Error _ -> ());
  Ivar.fill v r
;;

let read v =
  let* r = Ivar.read v in
  match r with
  | Ok x -> return x
  | Error exns ->
    (* Subsequent computations will always force the appendable list into a proper list
       and therefore not share this work. For now, this doesn't matter to us. *)
    reraise_all exns
;;

let force t =
  let* () = return () in
  match !t with
  | Done x -> return x
  | Running v -> read v
  | Init f ->
    let v = prep t in
    let* () = execute t v f in
    read v
;;

let is_value t =
  match !t with
  | Done _ -> true
  | Running _ | Init _ -> false
;;

let force_all_unit =
  let stop () = end_of_fiber in
  (* Fork all computations that haven't been forced yet. Note that this should be
     substantially more efficient that [parallel_map ~f:force] since we ignore
     computations which have already been forced. *)
  let start ts =
    sequential_iter ts ~f:(fun t ->
      match !t with
      | Done _ | Running _ -> return ()
      | Init f ->
        let v = prep t in
        fun k -> fork (fun () -> (execute t v f) stop) k)
  in
  (* Wait for all computations, collecting all exceptions.  *)
  (* CR-someday rgrinberg: use [Appendable.t] for [acc] rather than [Appendable.t option]. *)
  let rec collect acc = function
    | [] -> return acc
    | t :: ts ->
      (match !t with
       | Done _ -> collect acc ts
       | Running v ->
         let* r = Ivar.read v in
         (match r with
          | Ok _ -> collect acc ts
          | Error exns ->
            let exns =
              match acc with
              | None -> exns
              | Some exns' -> exns @ exns'
            in
            collect (Some exns) ts)
       | Init _ ->
         (* We forked all computations previously so this should be impossible. *)
         assert false)
  in
  fun ts ->
    let* () = start ts in
    let* r = collect None ts in
    match r with
    | None -> return ()
    | Some exns -> reraise_all exns
;;
