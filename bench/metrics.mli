open Stdune

(** [('float, 'int) t] is a record of metrics about the current process. It
    includes timing information and information available from [Gc.stat]. It is
    polymorphic in the type of field values to allow for the definition of
    [unzip] functions which make serialisation easier. *)
type ('float, 'int) t =
  { elapsed_time : 'float
  (** Real time elapsed since the process started and the process
      finished. *)
  ; user_cpu_time : 'float
  (** The amount of CPU time spent in user mode during the process. Other
      processes and blocked time are not included. *)
  ; system_cpu_time : 'float
  (** The amount of CPU time spent in kernel mode during the process.
      Similar to user time, other processes and time spent blocked by
      other processes are not counted. *)
  ; minor_words : 'float
  (** Number of words allocated in the minor heap since the program was
      started. *)
  ; promoted_words : 'float
  (** Number of words that have been promoted from the minor to the major
      heap since the program was started. *)
  ; major_words : 'float
  (** Number of words allocated in the major heap since the program was
      started. *)
  ; minor_collections : 'int
  (** Number of minor collections since the program was started. *)
  ; major_collections : 'int
  (** Number of major collection cycles completed since the program was
      started. *)
  ; heap_words : 'int (** Total size of the major heap, in words. *)
  ; heap_chunks : 'int
  (** Number of contiguous pieces of memory that make up the major heap. *)
  ; live_words : 'int
  (** Number of words of live data in the major heap, including the header
      words. *)
  ; live_blocks : 'int (** Number of live blocks in the major heap. *)
  ; free_words : 'int (** Number of words in the free list. *)
  ; free_blocks : 'int (** Number of blocks in the free list. *)
  ; largest_free : 'int (** Size (in words) of the largest block in the free list. *)
  ; fragments : 'int
  (** Number of wasted words due to fragmentation. These are 1-words free
      blocks placed between two live blocks. They are not available for
      allocation. *)
  ; compactions : 'int (** Number of heap compactions since the program was started. *)
  ; top_heap_words : 'int (** Maximum size reached by the major heap, in words. *)
  ; stack_size : 'int (** Current size of the stack, in words. *)
  }

(** [make t gc] creates a new metrics record from the given [t] and [gc]
    information. *)
val make : Proc.Times.t -> Gc.stat -> (float, int) t

(** [map ~f ~g m] applies [f] to the float fields and [g] to the int fields of
    [m]. *)
val map
  :  f:('float -> 'float_)
  -> g:('int -> 'int_)
  -> ('float, 'int) t
  -> ('float_, 'int_) t

(** [unzip m] takes a list of metrics [m] and returns a records with the lists
    of values for each field. This is particularly convenient when serialising
    to json. *)
val unzip : ('float, 'int) t list -> ('float list, 'int list) t
