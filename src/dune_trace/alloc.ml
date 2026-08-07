open Stdune

module Memprof = struct
  [@@@ocaml.warning "-69-32-34"]

  type t = unit
  type allocation_source = |

  let string_of_allocation_source : allocation_source -> string = function
    | _ -> .
  ;;

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

[%%if ocaml_version >= (5, 4, 0) && not_defined_permissive oxcaml]

open Gc

[%%else]

(* The [alloc] field is never mutated if it's disabled *)
[@@@ocaml.warning "-69"]

[%%endif]

module Config = struct
  type t =
    { sampling_rate : float
    ; callstack_size : int
    ; top_entry_count : int
    }

  let default = { sampling_rate = 1e-4; callstack_size = 10; top_entry_count = 10 }

  let invalid input message =
    User_error.raise [ Pp.textf "invalid DUNE_TRACE_ALLOC value %S: %s" input message ]
  ;;

  let positive_int input name value =
    match Int.of_string value with
    | Some value when value > 0 -> value
    | None | Some _ -> invalid input (sprintf "%s must be a positive integer" name)
  ;;

  let sampling_rate input value =
    match Float.of_string value with
    | Some value when value > 0.0 && value <= 1.0 -> value
    | None | Some _ -> invalid input "rate must be greater than 0 and at most 1"
  ;;

  let parse input =
    String.split_on_char input ~sep:','
    |> List.fold_left ~init:default ~f:(fun config field ->
      match String.lsplit2 (String.trim field) ~on:'=' with
      | None -> invalid input "expected a comma-separated list of NAME=VALUE fields"
      | Some (name, value) ->
        let name = String.trim name in
        let value = String.trim value in
        (match name with
         | "rate" -> { config with sampling_rate = sampling_rate input value }
         | "stack" -> { config with callstack_size = positive_int input "stack" value }
         | "top" -> { config with top_entry_count = positive_int input "top" value }
         | _ -> invalid input (sprintf "unknown field %S" name)))
  ;;

  let get () =
    match Sys.getenv_opt "DUNE_TRACE_ALLOC" with
    | None -> default
    | Some input -> parse input
  ;;
end

module Raw_backtrace_slot = struct
  module T = struct
    type t = Printexc.raw_backtrace_slot

    let repr = Repr.abstract (fun _ -> Dyn.opaque ())
  end

  include T
  include Repr.Make (T)

  (* Raw backtrace slots are documented as admitting polymorphic equality and
     hashing, but their representation is abstract. *)
  let equal = Poly.equal
  let hash = Poly.hash
end

module Frame = struct
  module T = struct
    type t =
      | Unknown
      | Known of string

    let repr =
      Repr.variant
        "allocation-frame"
        [ Repr.case0 "Unknown" ~test:(function
            | Unknown -> true
            | Known _ -> false)
        ; Repr.case "Known" Repr.string ~proj:(function
            | Known frame -> Some frame
            | Unknown -> None)
        ]
    ;;
  end

  include T
  include Repr.Make (T)
  include Repr.Poly (T)

  let to_string = function
    | Unknown -> "<unknown>"
    | Known frame -> frame
  ;;

  let of_slot slot =
    let name = Printexc.Slot.name slot in
    match Printexc.Slot.location slot with
    | None ->
      (match name with
       | None -> Unknown
       | Some name -> Known name)
    | Some { filename; line_number; start_char; _ } ->
      let frame =
        match name with
        | None -> sprintf "%s:%d:%d" filename line_number start_char
        | Some name -> sprintf "%s:%d:%d %s" filename line_number start_char name
      in
      Known frame
  ;;

  let of_slot cache slot =
    Table.find_or_add cache slot ~f:(fun slot ->
      Printexc.convert_raw_backtrace_slot slot |> of_slot)
  ;;

  let nearest_known cache slots =
    let rec loop index =
      if index = Array.length slots
      then Unknown
      else (
        match of_slot cache (Array.get slots index) with
        | Unknown -> loop (index + 1)
        | Known _ as frame -> frame)
    in
    loop 0
  ;;
end

module Trace = struct
  module T = struct
    type t =
      | Unknown
      | Trace of Raw_backtrace_slot.t array

    let repr =
      Repr.variant
        "allocation-trace"
        [ Repr.case0 "Unknown" ~test:(function
            | Unknown -> true
            | Trace _ -> false)
        ; Repr.case "Trace" (Repr.array Raw_backtrace_slot.repr) ~proj:(function
            | Unknown -> None
            | Trace slots -> Some slots)
        ]
    ;;
  end

  include T
  include Repr.Make (T)

  let to_strings trace ~frame_cache =
    match trace with
    | Unknown -> [ Frame.to_string Frame.Unknown ]
    | Trace slots ->
      Array.to_list slots
      |> List.map ~f:(fun slot -> Frame.of_slot frame_cache slot |> Frame.to_string)
  ;;

  let of_callstack callstack ~callstack_size =
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
        | [] -> Unknown
        | slots -> Trace (Array.of_list slots))
      else (
        let acc, remaining =
          add_inlined (Printexc.get_raw_backtrace_slot callstack i) remaining acc
        in
        loop (i + 1) remaining acc)
    in
    loop 0 callstack_size []
  ;;
end

module Key = struct
  module T = struct
    type t =
      { source : string
      ; trace : Trace.t
      }

    let repr =
      Repr.record
        "allocation-key"
        [ Repr.field "source" Repr.string ~get:(fun { source; _ } -> source)
        ; Repr.field "trace" Trace.repr ~get:(fun { trace; _ } -> trace)
        ]
    ;;
  end

  include T
  include Repr.Make (T)

  let equal = Poly.equal
  let hash = Poly.hash
end

module Frame_key = struct
  module T = struct
    type t =
      { source : string
      ; frame : Frame.t
      }

    let repr =
      Repr.record
        "allocation-frame-key"
        [ Repr.field "source" Repr.string ~get:(fun { source; _ } -> source)
        ; Repr.field "frame" Frame.repr ~get:(fun { frame; _ } -> frame)
        ]
    ;;
  end

  include T
  include Repr.Make (T)
  include Repr.Poly (T)
end

type tracked_minor =
  { key : Key.t
  ; n_samples : int
  }

type heap_table = (Key.t, int) Table.t

type heap =
  { mutable total_samples : int
  ; mutable by_key : heap_table
  }

type gc_counters =
  { minor_words : float
  ; major_words : float
  ; promoted_words : float
  }

let gc_counters () =
  let stat = Gc.quick_stat () in
  { minor_words = stat.minor_words
  ; major_words = stat.major_words
  ; promoted_words = stat.promoted_words
  }
;;

type t =
  { config : Config.t
  ; mutex : Mutex.t
  ; minor : heap
  ; major : heap
  ; promoted : heap
  ; mutable gc_baseline : gc_counters
  ; mutable profile : Memprof.t option
  }

let create_heap () = { total_samples = 0; by_key = Table.create (module Key) 64 }

let create config =
  { config
  ; mutex = Mutex.create ()
  ; minor = create_heap ()
  ; major = create_heap ()
  ; promoted = create_heap ()
  ; gc_baseline = gc_counters ()
  ; profile = None
  }
;;

let add_samples table key samples =
  let previous = Option.value (Table.find table key) ~default:0 in
  Table.set table key (previous + samples)
;;

let record_sample t heap ~key ~n_samples =
  Mutex.protect t.mutex (fun () ->
    heap.total_samples <- heap.total_samples + n_samples;
    add_samples heap.by_key key n_samples)
;;

let key_of_allocation { Memprof.source; callstack; _ } ~callstack_size =
  let source = Memprof.string_of_allocation_source source in
  let trace = Trace.of_callstack callstack ~callstack_size in
  { Key.source; trace }
;;

let tracker t =
  let { Config.callstack_size; _ } = t.config in
  { Memprof.null_tracker with
    alloc_minor =
      (fun ({ Memprof.n_samples; _ } as allocation) ->
        let key = key_of_allocation allocation ~callstack_size in
        record_sample t t.minor ~key ~n_samples;
        Some { key; n_samples })
  ; alloc_major =
      (fun ({ Memprof.n_samples; _ } as allocation) ->
        let key = key_of_allocation allocation ~callstack_size in
        record_sample t t.major ~key ~n_samples;
        None)
  ; promote =
      (fun { key; n_samples } ->
        record_sample t t.promoted ~key ~n_samples;
        None)
  }
;;

let start () =
  let config = Config.get () in
  let t = create config in
  let { Config.sampling_rate; callstack_size; _ } = config in
  let profile = Memprof.start ~sampling_rate ~callstack_size (tracker t) in
  t.profile <- Some profile;
  t.gc_baseline <- gc_counters ();
  t
;;

[%%if ocaml_version >= (5, 4, 0) && not_defined_permissive oxcaml]

let stop t =
  Option.iter t.profile ~f:(fun profile ->
    Memprof.stop ();
    Memprof.discard profile;
    t.profile <- None)
;;

[%%else]

let stop t = t.profile <- None

[%%endif]

let estimated_words_of_samples samples ~sampling_rate =
  int_of_float ((float_of_int samples /. sampling_rate) +. 0.5)
;;

let take_top_entries entries ~top_entry_count =
  let rec take acc n = function
    | _ when n <= 0 -> List.rev acc
    | [] -> List.rev acc
    | x :: xs -> take (x :: acc) (n - 1) xs
  in
  take [] top_entry_count entries
;;

let ranked_entries table ~top_entry_count =
  Table.to_list table
  |> List.sort ~compare:(fun (_, samples) (_, samples') -> Int.compare samples' samples)
  |> take_top_entries ~top_entry_count
;;

let top_entries by_key ~frame_cache ~sampling_rate ~top_entry_count =
  ranked_entries by_key ~top_entry_count
  |> List.map ~f:(fun ({ Key.source; trace }, samples) ->
    let estimated_words = estimated_words_of_samples samples ~sampling_rate in
    let trace = Trace.to_strings trace ~frame_cache in
    { Event.source; trace; estimated_words; samples })
;;

let ranked_frames table ~sampling_rate ~top_entry_count =
  ranked_entries table ~top_entry_count
  |> List.map ~f:(fun ({ Frame_key.source; frame }, samples) ->
    let estimated_words = estimated_words_of_samples samples ~sampling_rate in
    { Event.source; frame = Frame.to_string frame; estimated_words; samples })
;;

let site_entries by_key ~frame_cache ~sampling_rate ~top_entry_count =
  let by_site = Table.create (module Frame_key) 64 in
  Table.iteri by_key ~f:(fun { Key.source; trace } samples ->
    let frame =
      match trace with
      | Trace.Unknown -> Frame.Unknown
      | Trace slots -> Frame.nearest_known frame_cache slots
    in
    add_samples by_site { Frame_key.source; frame } samples);
  ranked_frames by_site ~sampling_rate ~top_entry_count
;;

let frame_entries by_key ~frame_cache ~sampling_rate ~top_entry_count =
  let by_frame = Table.create (module Frame_key) 64 in
  Table.iteri by_key ~f:(fun { Key.source; trace } samples ->
    match trace with
    | Trace.Unknown ->
      add_samples by_frame { Frame_key.source; frame = Frame.Unknown } samples
    | Trace slots ->
      let seen = ref [] in
      Array.iter slots ~f:(fun slot ->
        let frame = Frame.of_slot frame_cache slot in
        if not (List.mem !seen frame ~equal:Frame.equal)
        then (
          seen := frame :: !seen;
          add_samples by_frame { Frame_key.source; frame } samples)));
  ranked_frames by_frame ~sampling_rate ~top_entry_count
;;

let source_entries by_key ~sampling_rate =
  let by_source = Table.create (module String) 4 in
  Table.iteri by_key ~f:(fun { Key.source; _ } samples ->
    add_samples by_source source samples);
  Table.to_list by_source
  |> List.sort ~compare:(fun (source, samples) (source', samples') ->
    match Int.compare samples' samples with
    | Eq -> String.compare source source'
    | ordering -> ordering)
  |> List.map ~f:(fun (source, samples) ->
    let estimated_words = estimated_words_of_samples samples ~sampling_rate in
    ({ source; estimated_words; samples } : Event.alloc_source))
;;

let summary_of_heap total_samples by_key (config : Config.t) ~frame_cache =
  let { Config.sampling_rate; top_entry_count; _ } = config in
  let total_words = estimated_words_of_samples total_samples ~sampling_rate in
  { Event.total_words
  ; total_samples
  ; by_source = source_entries by_key ~sampling_rate
  ; by_site = site_entries by_key ~frame_cache ~sampling_rate ~top_entry_count
  ; by_frame = frame_entries by_key ~frame_cache ~sampling_rate ~top_entry_count
  ; top = top_entries by_key ~frame_cache ~sampling_rate ~top_entry_count
  }
;;

type swap_result =
  { previous_gc : gc_counters
  ; current_gc : gc_counters
  ; minor_total_samples : int
  ; minor_by_key : heap_table
  ; major_total_samples : int
  ; major_by_key : heap_table
  ; promoted_total_samples : int
  ; promoted_by_key : heap_table
  }

let swap t =
  let fresh_minor = Table.create (module Key) 64 in
  let fresh_major = Table.create (module Key) 64 in
  let fresh_promoted = Table.create (module Key) 64 in
  let minor_total_samples = ref 0 in
  let minor_by_key = ref fresh_minor in
  let major_total_samples = ref 0 in
  let major_by_key = ref fresh_major in
  let promoted_total_samples = ref 0 in
  let promoted_by_key = ref fresh_promoted in
  let previous_gc = ref t.gc_baseline in
  let current_gc = ref t.gc_baseline in
  let swap_under_mutex () =
    previous_gc := t.gc_baseline;
    minor_total_samples := t.minor.total_samples;
    minor_by_key := t.minor.by_key;
    major_total_samples := t.major.total_samples;
    major_by_key := t.major.by_key;
    promoted_total_samples := t.promoted.total_samples;
    promoted_by_key := t.promoted.by_key;
    t.gc_baseline <- !current_gc;
    t.minor.total_samples <- 0;
    t.minor.by_key <- fresh_minor;
    t.major.total_samples <- 0;
    t.major.by_key <- fresh_major;
    t.promoted.total_samples <- 0;
    t.promoted.by_key <- fresh_promoted
  in
  (* [gc_counters] allocates and can trigger a Memprof callback, so it must run
     before taking [t.mutex]. *)
  current_gc := gc_counters ();
  Mutex.protect t.mutex swap_under_mutex;
  (* Constructing this record can allocate and trigger a Memprof callback, which
     attempts to acquire [t.mutex], so it must happen after releasing the mutex. *)
  { previous_gc = !previous_gc
  ; current_gc = !current_gc
  ; minor_total_samples = !minor_total_samples
  ; minor_by_key = !minor_by_key
  ; major_total_samples = !major_total_samples
  ; major_by_key = !major_by_key
  ; promoted_total_samples = !promoted_total_samples
  ; promoted_by_key = !promoted_by_key
  }
;;

type snapshot =
  { config : Event.alloc_config
  ; exact : Event.alloc_exact
  ; minor : Event.alloc_heap
  ; major : Event.alloc_heap
  ; promoted : Event.alloc_heap
  }

let exact_gc_delta ~previous ~current =
  let promoted_words = current.promoted_words -. previous.promoted_words in
  let major_words =
    current.major_words -. previous.major_words -. promoted_words |> Stdlib.max 0.0
  in
  let round words = int_of_float (words +. 0.5) in
  { Event.minor_words = round (current.minor_words -. previous.minor_words)
  ; major_words = round major_words
  ; promoted_words = round promoted_words
  }
;;

let snapshot t =
  let { previous_gc
      ; current_gc
      ; minor_total_samples
      ; minor_by_key
      ; major_total_samples
      ; major_by_key
      ; promoted_total_samples
      ; promoted_by_key
      }
    =
    swap t
  in
  let exact = exact_gc_delta ~previous:previous_gc ~current:current_gc in
  let { Config.sampling_rate; callstack_size; top_entry_count } = t.config in
  let config = { Event.sampling_rate; callstack_size; top_entry_count } in
  let frame_cache = Table.create (module Raw_backtrace_slot) 64 in
  let minor = summary_of_heap minor_total_samples minor_by_key t.config ~frame_cache in
  let major = summary_of_heap major_total_samples major_by_key t.config ~frame_cache in
  let promoted =
    summary_of_heap promoted_total_samples promoted_by_key t.config ~frame_cache
  in
  { config; exact; minor; major; promoted }
;;

let reset t = ignore (swap t : swap_result)
