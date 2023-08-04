open Stdune

type ('float, 'int) t =
  { elapsed_time : 'float
  ; user_cpu_time : 'float
  ; system_cpu_time : 'float
  ; minor_words : 'float
  ; promoted_words : 'float
  ; major_words : 'float
  ; minor_collections : 'int
  ; major_collections : 'int
  ; heap_words : 'int
  ; heap_chunks : 'int
  ; live_words : 'int
  ; live_blocks : 'int
  ; free_words : 'int
  ; free_blocks : 'int
  ; largest_free : 'int
  ; fragments : 'int
  ; compactions : 'int
  ; top_heap_words : 'int
  ; stack_size : 'int
  }

let make (times : Proc.Times.t) (gc : Gc.stat) =
  (* We default to 0 for the other processor times since they are rarely None in
     pracice. *)
  let { Proc.Resource_usage.user_cpu_time; system_cpu_time } =
    Option.value
      times.resource_usage
      ~default:{ user_cpu_time = 0.; system_cpu_time = 0. }
  in
  { elapsed_time = times.elapsed_time
  ; user_cpu_time
  ; system_cpu_time
  ; minor_words = gc.minor_words
  ; promoted_words = gc.promoted_words
  ; major_words = gc.major_words
  ; minor_collections = gc.minor_collections
  ; major_collections = gc.major_collections
  ; heap_words = gc.heap_words
  ; heap_chunks = gc.heap_chunks
  ; live_words = gc.live_words
  ; live_blocks = gc.live_blocks
  ; free_words = gc.free_words
  ; free_blocks = gc.free_blocks
  ; largest_free = gc.largest_free
  ; fragments = gc.fragments
  ; compactions = gc.compactions
  ; top_heap_words = gc.top_heap_words
  ; stack_size = gc.stack_size
  }
;;

let map ~f ~g (metrics : ('float, 'int) t) : ('float_, 'int_) t =
  { elapsed_time = f metrics.elapsed_time
  ; user_cpu_time = f metrics.user_cpu_time
  ; system_cpu_time = f metrics.system_cpu_time
  ; minor_words = f metrics.minor_words
  ; promoted_words = f metrics.promoted_words
  ; major_words = f metrics.major_words
  ; minor_collections = g metrics.minor_collections
  ; major_collections = g metrics.major_collections
  ; heap_words = g metrics.heap_words
  ; heap_chunks = g metrics.heap_chunks
  ; live_words = g metrics.live_words
  ; live_blocks = g metrics.live_blocks
  ; free_words = g metrics.free_words
  ; free_blocks = g metrics.free_blocks
  ; largest_free = g metrics.largest_free
  ; fragments = g metrics.fragments
  ; compactions = g metrics.compactions
  ; top_heap_words = g metrics.top_heap_words
  ; stack_size = g metrics.stack_size
  }
;;

(** Turns a list of records into a record of lists. *)
let unzip (metrics : ('float, 'int) t list) : ('float list, 'int list) t =
  List.fold_left
    metrics
    ~init:
      { elapsed_time = []
      ; user_cpu_time = []
      ; system_cpu_time = []
      ; minor_words = []
      ; promoted_words = []
      ; major_words = []
      ; minor_collections = []
      ; major_collections = []
      ; heap_words = []
      ; heap_chunks = []
      ; live_words = []
      ; live_blocks = []
      ; free_words = []
      ; free_blocks = []
      ; largest_free = []
      ; fragments = []
      ; compactions = []
      ; top_heap_words = []
      ; stack_size = []
      }
    ~f:(fun acc x ->
      { elapsed_time = x.elapsed_time :: acc.elapsed_time
      ; user_cpu_time = x.user_cpu_time :: acc.user_cpu_time
      ; system_cpu_time = x.system_cpu_time :: acc.system_cpu_time
      ; minor_words = x.minor_words :: acc.minor_words
      ; promoted_words = x.promoted_words :: acc.promoted_words
      ; major_words = x.major_words :: acc.major_words
      ; minor_collections = x.minor_collections :: acc.minor_collections
      ; major_collections = x.major_collections :: acc.major_collections
      ; heap_words = x.heap_words :: acc.heap_words
      ; heap_chunks = x.heap_chunks :: acc.heap_chunks
      ; live_words = x.live_words :: acc.live_words
      ; live_blocks = x.live_blocks :: acc.live_blocks
      ; free_words = x.free_words :: acc.free_words
      ; free_blocks = x.free_blocks :: acc.free_blocks
      ; largest_free = x.largest_free :: acc.largest_free
      ; fragments = x.fragments :: acc.fragments
      ; compactions = x.compactions :: acc.compactions
      ; top_heap_words = x.top_heap_words :: acc.top_heap_words
      ; stack_size = x.stack_size :: acc.stack_size
      })
  |> map ~f:List.rev ~g:List.rev
;;
