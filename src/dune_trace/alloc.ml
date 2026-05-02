open Stdune

module Memprof = struct
  [@@@ocaml.warning "-69-32-34"]

  type t = unit
  type allocation_source = |

  type allocation =
    { n_samples : int
    ; size : int
    ; source : allocation_source
    ; callstack : Printexc.raw_backtrace
    }

  type ('major, 'minor) tracker =
    { alloc_minor : allocation -> 'minor option
    ; alloc_major : allocation -> 'major option
    ; promote : 'minor -> 'major option
    ; dealloc_minor : 'minor -> unit
    }

  let null_tracker =
    { alloc_minor = (fun _ -> None)
    ; alloc_major = (fun _ -> None)
    ; promote = (fun _ -> None)
    ; dealloc_minor = (fun _ -> ())
    }
  ;;

  let start ~sampling_rate:_ ?callstack_size:_ (_ : (_, _) tracker) = ()
end

[%%if ocaml_version >= (5, 4, 0)]

open Gc

[%%else]

(* The [alloc] field is never mutated if it's disabled *)
[@@@ocaml.warning "-69"]

[%%endif]

let sampling_rate = 1e-4
let callstack_size = 10
let top_entry_count = 10

module Trace = struct
  type t =
    | Unknown
    | Trace of Printexc.raw_backtrace_slot array

  let equal t t' =
    match t, t' with
    | Unknown, Unknown -> true
    | Trace slots, Trace slots' ->
      let len = Array.length slots in
      Int.equal len (Array.length slots')
      &&
      let rec loop i = i = len || (Poly.equal slots.(i) slots'.(i) && loop (i + 1)) in
      loop 0
    | Unknown, Trace _ | Trace _, Unknown -> false
  ;;

  let hash = function
    | Unknown -> 0
    | Trace slots ->
      Array.fold_left slots ~init:(Array.length slots) ~f:(fun acc slot ->
        acc * 65599 lxor Poly.hash slot)
  ;;

  let to_dyn = function
    | Unknown -> Dyn.string "<unknown>"
    | Trace _ -> Dyn.opaque ()
  ;;
end

module Trace_table = Hashtbl.Make (Trace)

type tracked_minor =
  { trace : Trace.t
  ; n_samples : int
  }

type heap =
  { mutable total_samples : int
  ; mutable by_trace : int Trace_table.t
  }

type t =
  { mutex : Mutex.t
  ; minor : heap
  ; major : heap
  ; promoted : heap
  ; mutable profile : Memprof.t option
  }

let create_heap () = { total_samples = 0; by_trace = Trace_table.create 64 }

let create () =
  { mutex = Mutex.create ()
  ; minor = create_heap ()
  ; major = create_heap ()
  ; promoted = create_heap ()
  ; profile = None
  }
;;

let place_of_slot slot =
  let name = Printexc.Slot.name slot in
  match Printexc.Slot.location slot with
  | None -> Option.value name ~default:"<unknown>"
  | Some { filename; line_number; start_char; _ } ->
    (match name with
     | None -> sprintf "%s:%d:%d" filename line_number start_char
     | Some name -> sprintf "%s:%d:%d %s" filename line_number start_char name)
;;

let trace_to_strings = function
  | Trace.Unknown -> [ "<unknown>" ]
  | Trace slots ->
    Array.to_list slots
    |> List.map ~f:(fun slot -> Printexc.convert_raw_backtrace_slot slot |> place_of_slot)
;;

let trace_of_callstack callstack =
  let length = Printexc.raw_backtrace_length callstack in
  let rec add_inlined slot remaining acc =
    if remaining = 0
    then acc, remaining
    else (
      let acc, remaining = slot :: acc, remaining - 1 in
      if remaining = 0
      then acc, remaining
      else (
        match Printexc.get_raw_backtrace_next_slot slot with
        | None -> acc, remaining
        | Some slot -> add_inlined slot remaining acc))
  in
  let rec loop i remaining acc =
    if i = length || remaining = 0
    then (
      match List.rev acc with
      | [] -> Trace.Unknown
      | slots -> Trace (Array.of_list slots))
    else (
      let acc, remaining =
        add_inlined (Printexc.get_raw_backtrace_slot callstack i) remaining acc
      in
      loop (i + 1) remaining acc)
  in
  loop 0 callstack_size []
;;

let record_sample t heap ~trace ~n_samples =
  Mutex.protect t.mutex (fun () ->
    heap.total_samples <- heap.total_samples + n_samples;
    match Trace_table.find heap.by_trace trace with
    | None -> Trace_table.set heap.by_trace trace n_samples
    | Some samples -> Trace_table.set heap.by_trace trace (samples + n_samples))
;;

let tracker t =
  { Memprof.null_tracker with
    alloc_minor =
      (fun { Memprof.n_samples; callstack; _ } ->
        let trace = trace_of_callstack callstack in
        record_sample t t.minor ~trace ~n_samples;
        Some { trace; n_samples })
  ; alloc_major =
      (fun { Memprof.n_samples; callstack; _ } ->
        let trace = trace_of_callstack callstack in
        record_sample t t.major ~trace ~n_samples;
        None)
  ; promote =
      (fun { trace; n_samples } ->
        record_sample t t.promoted ~trace ~n_samples;
        None)
  }
;;

let start () =
  let t = create () in
  let profile = Memprof.start ~sampling_rate ~callstack_size (tracker t) in
  t.profile <- Some profile;
  t
;;

[%%if ocaml_version >= (5, 4, 0)]

let stop t =
  Option.iter t.profile ~f:(fun profile ->
    Memprof.stop ();
    Memprof.discard profile;
    t.profile <- None)
;;

[%%else]

let stop t = t.profile <- None

[%%endif]

let estimated_words_of_samples samples =
  int_of_float ((float_of_int samples /. sampling_rate) +. 0.5)
;;

let take_top_entries entries =
  let rec take acc n = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> take (x :: acc) (n - 1) xs
  in
  take [] top_entry_count entries
;;

let insert_top_entry entry entries =
  let _, samples = entry in
  let rec insert = function
    | [] -> [ entry ]
    | ((_, samples') as entry') :: entries ->
      if samples > samples' then entry :: entry' :: entries else entry' :: insert entries
  in
  insert entries |> take_top_entries
;;

let top_entries by_trace =
  Trace_table.foldi by_trace ~init:[] ~f:(fun trace samples entries ->
    insert_top_entry (trace, samples) entries)
  |> List.map ~f:(fun (trace, samples) ->
    let estimated_words = estimated_words_of_samples samples in
    ({ trace = trace_to_strings trace; estimated_words; samples } : Event.alloc_entry))
;;

let summary_of_heap total_samples by_trace =
  let total_words = estimated_words_of_samples total_samples in
  ({ total_words; total_samples; top = top_entries by_trace } : Event.alloc_heap)
;;

let swap t =
  let fresh_minor = Trace_table.create 64 in
  let fresh_major = Trace_table.create 64 in
  let fresh_promoted = Trace_table.create 64 in
  Mutex.protect t.mutex (fun () ->
    let minor_total_samples = t.minor.total_samples in
    let minor_by_trace = t.minor.by_trace in
    let major_total_samples = t.major.total_samples in
    let major_by_trace = t.major.by_trace in
    let promoted_total_samples = t.promoted.total_samples in
    let promoted_by_trace = t.promoted.by_trace in
    t.minor.total_samples <- 0;
    t.minor.by_trace <- fresh_minor;
    t.major.total_samples <- 0;
    t.major.by_trace <- fresh_major;
    t.promoted.total_samples <- 0;
    t.promoted.by_trace <- fresh_promoted;
    ( minor_total_samples
    , minor_by_trace
    , major_total_samples
    , major_by_trace
    , promoted_total_samples
    , promoted_by_trace ))
;;

type snapshot =
  { minor : Event.alloc_heap
  ; major : Event.alloc_heap
  ; promoted : Event.alloc_heap
  }

let snapshot t =
  let ( minor_total_samples
      , minor_by_trace
      , major_total_samples
      , major_by_trace
      , promoted_total_samples
      , promoted_by_trace )
    =
    swap t
  in
  let minor = summary_of_heap minor_total_samples minor_by_trace in
  let major = summary_of_heap major_total_samples major_by_trace in
  let promoted = summary_of_heap promoted_total_samples promoted_by_trace in
  { minor; major; promoted }
;;

let reset t =
  ignore
    (swap t : int * int Trace_table.t * int * int Trace_table.t * int * int Trace_table.t)
;;
