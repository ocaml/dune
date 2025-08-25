(** Raw bindings for LMDB. *)

type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t 

val version : string * int * int * int


(** {2 Exceptions} *)

exception Exists
exception Map_full
exception Error of int
external strerror : int -> string = "mdbs_strerror"


(** {2 Flags} *)

(** Operations on sets of flags. *)
module type Flags = sig
  type t
  (** The type of a set of flags *)

  external ( + ) : t -> t -> t = "%orint"
  (** [a + b] is the {e union} of flag sets [a] and [b].
      This corresponds to a bitwise {e or} on C bitfields. *)

  external ( * ) : t -> t -> t = "%andint"
  (** [a * b] is the intersection of flag sets a and b.
      This corresponds to a bitwise {e and} on C bitfields. *)

  val test : t -> t -> bool
  (** [test a b] is [true] only if [a] is a subset of [b].
      This corresponds to [a & b == a] for C bitfields. *)

  val unset : t -> t -> t
  (** [unset a b] removes flags [a] from flag set [b].
      This corresponds to [a & ~b] for C bitfields. *)

  external eq : t -> t -> bool = "%equal"
  (** [eq a b] The equals relation. *)

  external of_int : int -> t   = "%identity"
  external to_int : t -> int   = "%identity"

  val none : t
  (** [none] The empty set of flags. *)
end

module Flags : Flags


(** {2 Environment} *)

type env
module EnvFlags :
  sig
    include Flags
    val fixed_map : t
    val no_subdir : t
      (** Create the environment not in an existing directory,
          but create the data file with exactly the filename given to {!Env.create}.
          The lock file will have "-lock" appended.
      *)

    val no_sync : t
    val read_only : t
    val no_meta_sync : t
    val write_map : t
    val map_async : t
    val no_tls : t
    val no_lock : t
    val no_read_ahead : t
    val no_mem_init : t
  end
module CopyFlags :
  sig
    include Flags
    val compact : t
  end
external env_create : unit -> env = "mdbs_env_create"
external env_open :
  env -> string -> EnvFlags.t -> int -> unit
  = "mdbs_env_open"
external env_close : env -> unit = "mdbs_env_close"
external env_set_mapsize : env -> int -> unit
  = "mdbs_env_set_mapsize"
external env_set_maxdbs : env -> int -> unit
  = "mdbs_env_set_maxdbs"
external env_set_maxreaders : env -> int -> unit
  = "mdbs_env_set_maxreaders"
external env_copy : env -> string -> CopyFlags.t -> unit
  = "mdbs_env_copy2"
external env_copyfd :
  env -> Unix.file_descr -> CopyFlags.t -> unit
  = "mdbs_env_copyfd2"
external env_set_flags :
  env -> EnvFlags.t -> bool -> unit
  = "mdbs_env_set_flags"
external env_get_flags : env -> EnvFlags.t
  = "mdbs_env_get_flags"
external env_get_path : env -> string
  = "mdbs_env_get_path"
external env_get_fd : env -> Unix.file_descr
  = "mdbs_env_get_fd"
external env_sync : env -> bool -> unit = "mdbs_env_sync"
external env_get_maxreaders : env -> int
  = "mdbs_env_get_maxreaders"
external env_get_maxkeysize : env -> int
  = "mdbs_env_get_maxkeysize"
external reader_list : env -> (string -> int) -> int
  = "mdbs_reader_list"
external reader_check : env -> int = "mdbs_reader_check"
type stat = {
  psize : int;
  depth : int;
  branch_pages : int;
  leaf_pages : int;
  overflow_pages : int;
  entries : int;
}
external env_stat : env -> stat = "mdbs_env_stat"
type envinfo =
  { map_addr : int
  (** To recover the actual address this integer needs to be shifted to the
      left by one bit. A 31 bit integer may overflow. *)
  ; map_size : int
  ; last_pgno : int
  ; last_txnid : int
  ; max_readers : int
  ; num_readers : int
  }
external env_info : env -> envinfo = "mdbs_env_info"


(** {2 Transaction} *)

type txn
external txn_env : txn -> env = "mdbs_txn_env"
external txn_begin :
  env -> txn option -> EnvFlags.t -> txn
  = "mdbs_txn_begin"
external txn_commit : txn -> unit = "mdbs_txn_commit"
external txn_abort : txn -> unit = "mdbs_txn_abort"


(** {2 Dbi} *)

type dbi
val invalid_dbi : dbi
module DbiFlags :
  sig
    include Flags
    val reverse_key : t
    val dup_sort : t
    val integer_key : t
    val dup_fixed : t
    val integer_dup : t
    val reverse_dup : t
    val create : t
  end
module PutFlags :
  sig
    include Flags
    val no_overwrite : t
    (** Raise {!exception: Exists} if the key already exists no matter whether the map
        supports duplicates.
    *)

    val no_dup_data : t
    (** Only for maps supporting duplicates: Don't add the value to an already
        existing key if this value is already part of this key.
    *)

    val append : t
    (** Add a key that is greater than any existing key.
        Used to efficiently add sorted data.
    *)

    val append_dup : t
    (** Add value to key that is greater than any existing value of this key.
        Used to efficiently add sorted values to a key.
    *)

    val current : t
    val reserve : t
    val multiple : t
  end
module Block_option :
  sig
    type +'a t
    val none : 'a t
    external some_unsafe : 'a -> 'a t = "%identity"
    external get_unsafe : 'a t -> 'a = "%identity"
    val is_some : 'a -> bool
    val is_none : 'a -> bool
    val some : 'a -> 'a t
    val get_exn : 'a t -> 'a
  end
external dbi_open :
  txn -> string option -> DbiFlags.t -> dbi
  = "mdbs_dbi_open"
external dbi_close : env -> dbi -> unit
  = "mdbs_dbi_close"
external dbi_flags : txn -> dbi -> DbiFlags.t
  = "mdbs_dbi_flags"
external dbi_stat : txn -> dbi -> stat
  = "mdbs_stat"
external drop : txn -> dbi -> bool -> unit
  = "mdbs_drop"
external get :
  txn -> dbi -> bigstring -> bigstring
  = "mdbs_get"
external put :
  txn ->
  dbi -> bigstring -> bigstring -> PutFlags.t -> unit = "mdbs_put"
external put_reserve :
  txn ->
  dbi -> bigstring -> int -> PutFlags.t -> bigstring = "mdbs_put"
external del :
  txn ->
  dbi -> bigstring -> bigstring Block_option.t -> unit = "mdbs_del"
external cmp :
  txn -> dbi -> bigstring -> bigstring -> int
  = "mdbs_cmp"
external dcmp :
  txn -> dbi -> bigstring -> bigstring -> int
  = "mdbs_dcmp"


(** {2 Cursor} *)

type cursor
module Ops :
  sig
    type t
    val first : t
    val first_dup : t
    val get_both : t
    val get_both_range : t
    val get_current : t
    val get_multiple : t
    val last : t
    val last_dup : t
    val next : t
    val next_dup : t
    val next_multiple : t
    val next_nodup : t
    val prev : t
    val prev_dup : t
    (* let prev_multiple  = prev_multiple - only since lmdb 0.9.19 *)
    val prev_nodup : t
    val set : t
    val set_key : t
    val set_range : t
  end
external cursor_open : txn -> dbi -> cursor
  = "mdbs_cursor_open"
external cursor_close : cursor -> unit
  = "mdbs_cursor_close"
external cursor_put :
  cursor ->
  bigstring -> bigstring -> PutFlags.t -> unit = "mdbs_cursor_put"
external cursor_put_reserve :
  cursor ->
  bigstring -> int -> PutFlags.t -> bigstring = "mdbs_cursor_put"
external cursor_del : cursor -> PutFlags.t -> unit
  = "mdbs_cursor_del"
external cursor_get :
  cursor ->
  bigstring Block_option.t ->
  bigstring Block_option.t -> Ops.t -> bigstring * bigstring
  = "mdbs_cursor_get"
external cursor_count : cursor -> int
  = "mdbs_cursor_count"


(** {2 Internal} *)

val sizeof_int : int
val sizeof_size_t : int
