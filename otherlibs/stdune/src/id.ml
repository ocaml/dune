module type S = sig
  type t [@@immediate]

  include Comparable_intf.S with type key := t
  module Table : Hashtbl.S with type key = t

  module Hashset : sig
    type id := t
    type t

    val create : unit -> t
    val is_empty : t -> bool
    val add : t -> id -> unit
    val mem : t -> id -> bool
    val to_dyn : t -> Dyn.t
  end

  val gen : unit -> t
  val peek : unit -> t
  val to_int : t -> int
  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val hash : t -> int
  val to_dyn : t -> Dyn.t
end

module Make () : S = struct
  include Int
  module Table = Hashtbl.Make (Int)

  module Array = struct
    type nonrec t = Bytes.t

    let words = Sys.word_size / 8
    let[@inline] length t = Bytes.length t / words
    let[@inline] unsafe_get t i = Int64.to_int (Bytes.get_int64_ne t (i * words))
    let[@inline] unsafe_set t i x = Bytes.set_int64_ne t (i * words) (Int64.of_int x)

    let[@inline] make len x =
      let t = Bytes.create (len * words) in
      for i = 0 to length t - 1 do
        unsafe_set t i x
      done;
      t
    ;;

    let[@inline] make_absent len = Bytes.make (len * words) '\255'

    let fold_left t ~init ~f =
      let init = ref init in
      for i = 0 to length t - 1 do
        init := f !init (unsafe_get t i)
      done;
      !init
    ;;
  end

  module Hashset = struct
    (* A specialized hash table that makes the following trade-offs:

       - Open addresing. Bucketing is quite memory intensive and dune is already
         a memory hog.

       - No boxing for empty slots. We make use of the fact that id's are never
         negative to achieve this.

       - No saving of the hash. Recomputing the hash for id's is a no-op.
    *)

    type nonrec table =
      { mutable table : Array.t
      ; mutable size : int
      }

    type t = table Option.Unboxed.t ref

    let init t =
      if Option.Unboxed.is_none !t
      then t := Option.Unboxed.some { size = 0; table = Array.make 0 (-1) };
      Option.Unboxed.value_exn !t
    ;;

    let[@inline] should_grow t =
      let slots = Array.length t.table in
      slots = 0 || (t.size > 0 && slots / t.size < 2)
    ;;

    let absent = -1

    let () =
      let x = Array.make_absent 1 in
      assert (Array.unsafe_get x 0 = absent)
    ;;

    let create () = ref Option.Unboxed.none

    let[@inline] index_of_offset slots index i =
      let i = index + !i in
      if i >= slots then i - slots else i
    ;;

    let add t x =
      let hash = hash x in
      let slots = Array.length t.table in
      let index = hash land (slots - 1) in
      let inserting = ref true in
      let i = ref 0 in
      while !inserting do
        let idx = index_of_offset slots index i in
        let elem = Array.unsafe_get t.table idx in
        if elem = absent
        then (
          Array.unsafe_set t.table idx x;
          inserting := false)
        else incr i
      done;
      t.size <- t.size + 1
    ;;

    let resize t =
      let old_table = t.table in
      let slots = Array.length old_table in
      let table = Array.make_absent (if slots = 0 then 1 else slots lsl 1) in
      t.table <- table;
      for i = 0 to slots - 1 do
        let elem = Array.unsafe_get old_table i in
        if elem <> absent then add t elem
      done
    ;;

    let add t x =
      let t = init t in
      if should_grow t then resize t;
      add t x
    ;;

    let[@inline] is_empty t =
      let t = !t in
      if Option.Unboxed.is_none t
      then true
      else (
        let t = Option.Unboxed.value_exn t in
        t.size = 0)
    ;;

    let mem t x =
      let t = !t in
      if Option.Unboxed.is_none t || (Option.Unboxed.value_exn t).size = 0
      then false
      else (
        let t = Option.Unboxed.value_exn t in
        let hash = hash x in
        let slots = Array.length t.table in
        let index = hash land (slots - 1) in
        let i = ref 0 in
        let found = ref false in
        while (not !found) && !i < slots do
          let idx = index_of_offset slots index i in
          let elem = Array.unsafe_get t.table idx in
          if Int.equal elem x
          then found := true
          else if Int.equal elem absent
          then i := slots
          else incr i
        done;
        !found)
    ;;

    let to_dyn t =
      let { table; size } = init t in
      let table =
        Array.fold_left table ~init:[] ~f:(fun acc i ->
          if i = absent then acc else i :: acc)
        |> List.rev
        |> Stdlib.Array.of_list
        |> Dyn.array Dyn.int
      in
      Dyn.record [ "table", table; "size", Dyn.int size ]
    ;;
  end

  let next = ref 0

  let gen () =
    let v = !next in
    next := v + 1;
    v
  ;;

  let peek () = !next
  let to_int x = x
end
