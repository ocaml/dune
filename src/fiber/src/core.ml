open Stdune

type ('a, 'b) fork_and_join_state =
  | Nothing_yet
  | Got_a of 'a
  | Got_b of 'b

(* Fiber computations are explicit nodes. Callbacks supplied by callers remain functions,
   while the Fiber-created computation spine is interpreted by the scheduler. *)
type _ t =
  | Return_t : 'a -> 'a t
  | Never_t : 'a t
  | Map_t : 'a t * ('a -> 'b) -> 'b t
  | Map2_t : 'a t * ('a -> 'b) * ('b -> 'c) -> 'c t
  | Map3_t : 'a t * ('a -> 'b) * ('b -> 'c) * ('c -> 'd) -> 'd t
  | Bind_t : 'a t * ('a -> 'b t) -> 'b t
  | Thunk_t : (unit -> 'a t) -> 'a t
  | Thunk_apply_t : ('a -> 'b t) * 'a -> 'b t
  | With_error_handler_t : (unit -> 'a t) * (Exn_with_backtrace.t -> Nothing.t t) -> 'a t
  | Map_reduce_errors_t :
      (module Monoid with type t = 'a) * (Exn_with_backtrace.t -> 'a t) * (unit -> 'b t)
      -> ('b, 'a) result t
  | Suspend_t : ('a k -> unit) -> 'a t
  | Resume_t : 'a k * 'a -> unit t
  | Reraise_all_t : Exn_with_backtrace.t list -> 'a t
  | Ivar_read_t : 'a ivar -> 'a t
  | Ivar_fill_t : 'a ivar * 'a -> unit t
  | Get_var_t : 'a Var_map.Key.t -> 'a t
  | Set_var_t : 'a Var_map.Key.t * 'a * (unit -> 'b t) -> 'b t
  | Update_var_t : 'a Var_map.Key.t * ('a -> 'a) * (unit -> 'b t) -> 'b t
  | Get_apply_t : 'a Var_map.Key.t * ('a -> 'b -> 'c t) * 'b -> 'c t
  | Get_apply_map_t : 'a Var_map.Key.t * ('a -> 'b -> 'c) * 'b -> 'c t
  | Set_apply_t : 'a Var_map.Key.t * 'a * ('b -> 'c t) * 'b -> 'c t
  | Update_apply_t : 'a Var_map.Key.t * ('a -> 'a) * ('b -> 'c t) * 'b -> 'c t
  (* Pair internal operations with their state without allocating a closure. *)
  | Primitive_t : ('a -> 'b continuation -> eff) * 'a -> 'b t
  | Primitive2_t : ('a -> 'b -> 'c continuation -> eff) * 'a * 'b -> 'c t
  | Primitive3_t : ('a -> 'b -> 'c -> 'd continuation -> eff) * 'a * 'b * 'c -> 'd t
  | Primitive4_t :
      ('a -> 'b -> 'c -> 'd -> 'e continuation -> eff) * 'a * 'b * 'c * 'd
      -> 'e t

(* Keep arbitrary callbacks as functions, but represent the frames introduced by common
   fiber combinators directly. *)
and 'a continuation =
  | Function : ('a -> eff) -> 'a continuation
  | Map : ('a -> 'b) * 'b continuation -> 'a continuation
  | Map2 : ('a -> 'b) * ('b -> 'c) * 'c continuation -> 'a continuation
  | Map3 : ('a -> 'b) * ('b -> 'c) * ('c -> 'd) * 'd continuation -> 'a continuation
  | Bind : ('a -> 'b t) * 'b continuation -> 'a continuation
  | Apply : ('a -> 'b -> 'c t) * 'b * 'c continuation -> 'a continuation
  | Apply_map : ('a -> 'b -> 'c) * 'b * 'c continuation -> 'a continuation
  | Unwind_to : 'a continuation -> 'a continuation
  | Unwind_map_reduce_to : ('a, 'b) result continuation -> 'a continuation
  | End : unit continuation
  | Parallel_unit_complete : int ref * unit continuation -> unit continuation
  | Map_reduce_complete :
      'a ref * int ref * ('a -> 'a -> 'a) * 'a continuation
      -> 'a continuation
  | Array_map_complete :
      'a array ref * int * int * int ref * 'a array continuation
      -> 'a continuation
  | Fork_join_left :
      ('a, 'b) fork_and_join_state ref * ('a * 'b) continuation
      -> 'a continuation
  | Fork_join_right :
      ('a, 'b) fork_and_join_state ref * ('a * 'b) continuation
      -> 'b continuation
  | Resume_many : unit k list * unit continuation -> unit continuation
  | Unreachable : Nothing.t continuation
  | Never_called : 'a continuation
  | Accumulate_error : ('result, 'errors) map_reduce_context' -> 'errors continuation

and eff =
  | Run : 'a t * 'a continuation -> eff
  | Read_ivar : 'a ivar * 'a continuation -> eff
  | Fill_ivar : 'a ivar * 'a * unit continuation -> eff
  | Suspend : ('a k -> unit) * 'a continuation -> eff
  | Resume : 'a k * 'a * unit continuation -> eff
  | Get_var : 'a Var_map.Key.t * 'a continuation -> eff
  | Set_var : 'a Var_map.Key.t * 'a * (unit -> 'b t) * 'b continuation -> eff
  | Update_var : 'a Var_map.Key.t * ('a -> 'a) * (unit -> 'b t) * 'b continuation -> eff
  | Set_var_apply : 'a Var_map.Key.t * 'a * ('b -> 'c t) * 'b * 'c continuation -> eff
  | Update_var_apply :
      'a Var_map.Key.t * ('a -> 'a) * ('b -> 'c t) * 'b * 'c continuation
      -> eff
  | With_error_handler :
      (Exn_with_backtrace.t -> Nothing.t t) * (unit -> 'a t) * 'a continuation
      -> eff
  | Unwind : 'a continuation * 'a -> eff
  | Map_reduce_errors :
      (module Monoid with type t = 'a)
      * (Exn_with_backtrace.t -> 'a t)
      * (unit -> 'b t)
      * ('b, 'a) result continuation
      -> eff
  | Unwind_map_reduce : 'a continuation * 'a -> eff
  | End_of_map_reduce_error_handler : (_, _) map_reduce_context' -> eff
  | End_of_fiber of unit
  | Never of unit
  (* Add a dummy unit argument to [End_of_fiber] and [Never] so that all
     constructors are boxed, which removes a branch in the pattern match. *)
  | Fork : eff * work -> eff
  | Reraise : Exn_with_backtrace.t -> eff
  | Reraise_all : Exn_with_backtrace.t list -> eff
  | Toplevel_exception : Exn_with_backtrace.t -> eff
  | Done of value

and work =
  | Function_work of (unit -> eff)
  | Nfork_work : 'a * 'a list * ('a -> eff) -> work
  | Nforki_work : int * 'a * 'a list * (int -> 'a -> eff) -> work
  | Nfork_seq_work : int ref * 'a * 'a Seq.t * ('a -> eff) -> work
  | Nfork_array_work : 'a array * int * ('a -> eff) -> work
  | Apply_thunk_work : (unit -> 'a t) * 'a continuation -> work
  | Eval_work : 'a t * 'a continuation -> work
  | Continue_work : unit continuation -> work

and 'a ivar = { mutable state : ('a, [ `Full | `Empty ]) ivar_state }

and ('a, _) ivar_state =
  | Full : 'a -> ('a, [> `Full ]) ivar_state
  | Empty : ('a, [> `Empty ]) ivar_state
  | Empty_with_readers :
      context * 'a continuation * ('a, [ `Empty ]) ivar_state
      -> ('a, [> `Empty ]) ivar_state

and value = ..

and context =
  { parent : context
  ; on_error : error_handler
  ; vars : Var_map.t
  ; map_reduce_context : map_reduce_context
  }

and error_handler =
  | Top_level_error
  | Handle_error of context * (Exn_with_backtrace.t -> Nothing.t t)
  | Collect_errors

and ('a, 'b) map_reduce_context' =
  { ctx : context
  ; k : ('a, 'b) result continuation
  ; on_error : Exn_with_backtrace.t -> 'b t
  ; combine : 'b -> 'b -> 'b
  ; mutable ref_count : int
  ; mutable errors : 'b
  }

(* map_reduce_context *)
and map_reduce_context =
  | Map_reduce_context : (_, _) map_reduce_context' -> map_reduce_context

and 'a k =
  { run : 'a continuation
  ; ctx : context
  }

let rec continue : type a. a continuation -> a -> eff =
  fun k x ->
  match k with
  | Function f -> f x
  | Map (f, k) -> continue k (f x)
  | Map2 (f, g, k) -> continue k (g (f x))
  | Map3 (f, g, h, k) -> continue k (h (g (f x)))
  | Bind (f, k) -> Run (f x, k)
  | Apply (f, y, k) -> Run (f x y, k)
  | Apply_map (f, y, k) -> continue k (f x y)
  | Unwind_to k -> Unwind (k, x)
  | Unwind_map_reduce_to k -> Unwind_map_reduce (k, Ok x)
  | End -> End_of_fiber ()
  | Parallel_unit_complete (running, k) ->
    decr running;
    if !running = 0 then continue k () else End_of_fiber ()
  | Map_reduce_complete (current, running, combine, k) ->
    current := combine !current x;
    decr running;
    if !running = 0 then continue k !current else End_of_fiber ()
  | Array_map_complete (results, len, i, running, k) ->
    let a =
      match !results with
      | [||] ->
        let a = Array.make len x in
        results := a;
        a
      | a ->
        a.(i) <- x;
        a
    in
    decr running;
    if !running = 0 then continue k a else End_of_fiber ()
  | Fork_join_left (state, k) ->
    (match !state with
     | Nothing_yet ->
       state := Got_a x;
       End_of_fiber ()
     | Got_a _ -> assert false
     | Got_b b -> continue k (x, b))
  | Fork_join_right (state, k) ->
    (match !state with
     | Nothing_yet ->
       state := Got_b x;
       End_of_fiber ()
     | Got_a a -> continue k (a, x)
     | Got_b _ -> assert false)
  | Resume_many (suspended, k) ->
    (match suspended with
     | [] -> continue k ()
     | suspended :: rest -> Resume (suspended, (), Resume_many (rest, k)))
  | Unreachable -> Nothing.unreachable_code x
  | Never_called -> assert false
  | Accumulate_error map_reduce_context ->
    map_reduce_context.errors <- map_reduce_context.combine map_reduce_context.errors x;
    End_of_map_reduce_error_handler map_reduce_context
;;

let return x = Return_t x
let bind t ~f = Bind_t (t, f)

let map t ~f =
  match t with
  | Map_t (t, g) -> Map2_t (t, g, f)
  | Map2_t (t, g, h) -> Map3_t (t, g, h, f)
  | t -> Map_t (t, f)
;;

let with_error_handler f ~on_error = With_error_handler_t (f, on_error)
let map_reduce_errors m ~on_error f = Map_reduce_errors_t (m, on_error, f)
let suspend f = Suspend_t f
let resume suspended x = Resume_t (suspended, x)
let end_of_fiber = End_of_fiber ()
let never = Never_t

let apply f x =
  try f x with
  | exn ->
    let exn = Exn_with_backtrace.capture exn in
    Reraise exn
;;

let apply2 f x y =
  try f x y with
  | exn ->
    let exn = Exn_with_backtrace.capture exn in
    Reraise exn
;;

let rec eval : type a. a t -> a continuation -> eff =
  fun t k ->
  match t with
  | Return_t x -> continue k x
  | Never_t -> Never ()
  | Map_t (t, f) -> eval t (Map (f, k))
  | Map2_t (t, f, g) -> eval t (Map2 (f, g, k))
  | Map3_t (t, f, g, h) -> eval t (Map3 (f, g, h, k))
  | Bind_t (t, f) -> eval t (Bind (f, k))
  | Thunk_t f -> eval (f ()) k
  | Thunk_apply_t (f, x) -> eval (f x) k
  | With_error_handler_t (f, on_error) -> With_error_handler (on_error, f, k)
  | Map_reduce_errors_t (m, on_error, f) -> Map_reduce_errors (m, on_error, f, k)
  | Suspend_t f -> Suspend (f, k)
  | Resume_t (suspended, x) -> Resume (suspended, x, k)
  | Reraise_all_t exns ->
    (match exns with
     | [] -> Never ()
     | [ exn ] -> Exn_with_backtrace.reraise exn
     | _ -> Reraise_all exns)
  | Ivar_read_t ivar ->
    (match ivar.state with
     | Full x -> continue k x
     | Empty_with_readers _ | Empty -> Read_ivar (ivar, k))
  | Ivar_fill_t (ivar, x) ->
    (match ivar.state with
     | Empty ->
       ivar.state <- Full x;
       continue k ()
     | Full _ | Empty_with_readers _ -> Fill_ivar (ivar, x, k))
  | Get_var_t key -> Get_var (key, k)
  | Set_var_t (key, value, f) -> Set_var (key, value, f, k)
  | Update_var_t (key, f, body) -> Update_var (key, f, body, k)
  | Get_apply_t (key, f, x) -> Get_var (key, Apply (f, x, k))
  | Get_apply_map_t (key, f, x) -> Get_var (key, Apply_map (f, x, k))
  | Set_apply_t (key, value, f, x) -> Set_var_apply (key, value, f, x, k)
  | Update_apply_t (key, f, body, x) -> Update_var_apply (key, f, body, x, k)
  | Primitive_t (f, x) -> f x k
  | Primitive2_t (f, x, y) -> f x y k
  | Primitive3_t (f, x, y, z) -> f x y z k
  | Primitive4_t (f, w, x, y, z) -> f w x y z k
;;

let primitive f x = Primitive_t (f, x)
let primitive2 f x y = Primitive2_t (f, x, y)
let primitive3 f x y z = Primitive3_t (f, x, y, z)
let primitive4 f w x y z = Primitive4_t (f, w, x, y, z)

let apply_t f x k =
  try eval (f x) k with
  | exn -> Reraise (Exn_with_backtrace.capture exn)
;;

let apply_t2 f x y k =
  try eval (f x y) k with
  | exn -> Reraise (Exn_with_backtrace.capture exn)
;;

let rec nfork x l f =
  match l with
  | [] -> f x
  | y :: l ->
    (* Manually inline [fork] because the compiler is unfortunately
       not getting rid of the closures. *)
    (match apply f x with
     | End_of_fiber () -> nfork y l f
     | eff -> Fork (eff, Nfork_work (y, l, f)))
;;

let rec nforki_from i x l f =
  match l with
  | [] -> f i x
  | y :: l ->
    (match apply2 f i x with
     | End_of_fiber () -> nforki_from (i + 1) y l f
     | eff -> Fork (eff, Nforki_work (i + 1, y, l, f)))
;;

let nforki x l f = nforki_from 0 x l f

let rec nfork_seq left_over x (seq : _ Seq.t) f =
  match seq () with
  | Nil -> f x
  | Cons (y, seq) ->
    incr left_over;
    (match apply f x with
     | End_of_fiber () -> nfork_seq left_over y seq f
     | eff -> Fork (eff, Nfork_seq_work (left_over, y, seq, f)))
;;

let rec nfork_array a i f =
  if i = Array.length a - 1
  then f a.(i)
  else (
    match apply f a.(i) with
    | End_of_fiber () -> nfork_array a (i + 1) f
    | eff -> Fork (eff, Nfork_array_work (a, i + 1, f)))
;;

let run_work = function
  | Function_work f -> f ()
  | Nfork_work (x, l, f) -> nfork x l f
  | Nforki_work (i, x, l, f) -> nforki_from i x l f
  | Nfork_seq_work (left_over, x, seq, f) -> nfork_seq left_over x seq f
  | Nfork_array_work (a, i, f) -> nfork_array a i f
  | Apply_thunk_work (f, k) -> apply_t f () k
  | Eval_work (t, k) -> eval t k
  | Continue_work k -> continue k ()
;;

let run_parallel_iter_seq (seq : _ Seq.t) f k =
  match seq () with
  | Nil -> continue k ()
  | Cons (x, seq) ->
    let left_over = ref 1 in
    let complete = Parallel_unit_complete (left_over, k) in
    let f' x = apply_t f x complete in
    nfork_seq left_over x seq f'
;;

let parallel_iter_seq seq ~f = primitive2 run_parallel_iter_seq seq f

let run_map_reduce_seq (seq : _ Seq.t) f empty combine k =
  match seq () with
  | Nil -> continue k empty
  | Cons (x, seq) ->
    let current = ref empty in
    let running = ref 1 in
    let complete = Map_reduce_complete (current, running, combine, k) in
    let f' x = apply_t f x complete in
    nfork_seq running x seq f'
;;

let map_reduce_seq seq ~f ~empty ~combine =
  primitive4 run_map_reduce_seq seq f empty combine
;;

let run_map_reduce_array a f empty combine k =
  match Array.length a with
  | 0 -> continue k empty
  | len ->
    let current = ref empty in
    let running = ref len in
    let complete = Map_reduce_complete (current, running, combine, k) in
    let f' x = apply_t f x complete in
    nfork_array a 0 f'
;;

let map_reduce_array a ~f ~empty ~combine =
  primitive4 run_map_reduce_array a f empty combine
;;

let run_map_reduce l f empty combine k =
  match l with
  | [] -> continue k empty
  | x :: l ->
    let current = ref empty in
    let running = ref (List.length l + 1) in
    let complete = Map_reduce_complete (current, running, combine, k) in
    let f' x = apply_t f x complete in
    nfork x l f'
;;

let map_reduce l ~f ~empty ~combine = primitive4 run_map_reduce l f empty combine

let run_fork_and_join fa fb k =
  let state = ref Nothing_yet in
  let ka = Fork_join_left (state, k) in
  let kb = Fork_join_right (state, k) in
  match apply_t fa () ka with
  | End_of_fiber () -> apply_t fb () kb
  | eff -> Fork (eff, Apply_thunk_work (fb, kb))
;;

let fork_and_join fa fb = primitive2 run_fork_and_join fa fb

let run_fork_and_join_unit fa fb k =
  let state = ref Nothing_yet in
  let pair = Map (snd, k) in
  let ka = Fork_join_left (state, pair) in
  let kb = Fork_join_right (state, pair) in
  match apply_t fa () ka with
  | End_of_fiber () -> apply_t fb () kb
  | eff -> Fork (eff, Apply_thunk_work (fb, kb))
;;

let fork_and_join_unit fa fb = primitive2 run_fork_and_join_unit fa fb

let rec length_and_rev l len acc =
  match l with
  | [] -> len, acc
  | x :: l -> length_and_rev l (len + 1) (x :: acc)
;;

let length_and_rev l = length_and_rev l 0 []
let reraise_all l = Reraise_all_t l

module Ivar = struct
  type 'a t = 'a ivar

  let create () = { state = Empty }
  let read t = Ivar_read_t t
  let fill t x = Ivar_fill_t (t, x)
  let create_full a = { state = Full a }

  let peek t =
    match t.state with
    | Empty | Empty_with_readers _ -> None
    | Full x -> Some x
  ;;
end

module Var = struct
  let get (key : 'a Var_map.Key.t) : 'a t = Get_var_t key

  let set (key : 'a Var_map.Key.t) (value : 'a) (fiber : unit -> 'b t) : 'b t =
    Set_var_t (key, value, fiber)
  ;;

  let get_exn (key : 'a option Var_map.Key.t) : 'a t =
    map (get key) ~f:(function
      | None -> failwith "Fiber.Var.get_exn"
      | Some value -> value)
  ;;

  let update (key : 'a Var_map.Key.t) ~(f : 'a -> 'a) (fiber : unit -> 'b t) : 'b t =
    Update_var_t (key, f, fiber)
  ;;

  let get_apply (key : 'a Var_map.Key.t) (f : 'a -> 'b -> 'c t) (x : 'b) : 'c t =
    Get_apply_t (key, f, x)
  ;;

  let get_apply_map (key : 'a Var_map.Key.t) (f : 'a -> 'b -> 'c) (x : 'b) : 'c t =
    Get_apply_map_t (key, f, x)
  ;;

  let set_apply (key : 'a Var_map.Key.t) (value : 'a) (f : 'b -> 'c t) (x : 'b) : 'c t =
    Set_apply_t (key, value, f, x)
  ;;

  let update_apply (key : 'a Var_map.Key.t) ~(f : 'a -> 'a) (g : 'b -> 'c t) (x : 'b)
    : 'c t
    =
    Update_apply_t (key, f, g, x)
  ;;

  include Var_map.Key
end

let of_thunk f = Thunk_t f
let of_thunk_apply f x = Thunk_apply_t (f, x)

module O = struct
  let ( >>> ) a b = bind a ~f:(fun () -> b)
  let ( >>= ) t f = bind t ~f
  let ( >>| ) t f = map t ~f
  let ( let+ ) = ( >>| )
  let ( let* ) = ( >>= )
  let ( and* ) a b = fork_and_join (fun () -> a) (fun () -> b)
  let ( and+ ) = ( and* )
end

open O

let both a b =
  let* x = a in
  let* y = b in
  return (x, y)
;;

let sequential_map l ~f =
  let rec loop l acc =
    match l with
    | [] -> return (List.rev acc)
    | x :: l ->
      let* x = f x in
      loop l (x :: acc)
  in
  loop l []
;;

let sequential_iter l ~f =
  let rec loop l =
    match l with
    | [] -> return ()
    | x :: l ->
      let* () = f x in
      loop l
  in
  loop l
;;

let run_parallel_iter l f k =
  match l with
  | [] -> continue k ()
  | [ x ] -> apply_t f x k
  | x :: l ->
    let len = List.length l + 1 in
    let left_over = ref len in
    let complete = Parallel_unit_complete (left_over, k) in
    let f' x = apply_t f x complete in
    nfork x l f'
;;

let parallel_iter l ~f = primitive2 run_parallel_iter l f

let run_parallel_array_of_list_map' x l f k =
  let len = List.length l + 1 in
  let left_over = ref len in
  let results = ref [||] in
  let f' i x = apply_t f x (Array_map_complete (results, len, i, left_over, k)) in
  nforki x l f'
;;

let run_parallel_array_of_list_map l f k =
  match l with
  | [] -> continue k [||]
  | [ x ] -> apply_t f x (Map ((fun x -> [| x |]), k))
  | x :: l -> run_parallel_array_of_list_map' x l f k
;;

let parallel_array_of_list_map l ~f = primitive2 run_parallel_array_of_list_map l f

let run_parallel_map l f k =
  match l with
  | [] -> continue k []
  | [ x ] -> apply_t f x (Map ((fun x -> [ x ]), k))
  | x :: l -> run_parallel_array_of_list_map' x l f (Map (Array.to_list, k))
;;

let parallel_map l ~f = primitive2 run_parallel_map l f
let all = sequential_map ~f:Fun.id
let all_concurrently = parallel_map ~f:Fun.id
let all_concurrently_unit l = parallel_iter l ~f:Fun.id

let rec sequential_iter_seq (seq : _ Seq.t) ~f =
  match seq () with
  | Nil -> return ()
  | Cons (x, seq) ->
    let* () = f x in
    sequential_iter_seq seq ~f
;;

let parallel_iter_set
      (type a s)
      (module S : Set.S with type elt = a and type t = s)
      set
      ~(f : a -> unit t)
  =
  parallel_iter_seq (S.to_seq set) ~f
;;

module Make_parallel_map (S : sig
    type 'a t
    type key

    val empty : _ t
    val is_empty : _ t -> bool
    val to_list : 'a t -> (key * 'a) list
    val mapi : 'a t -> f:(key -> 'a -> 'b) -> 'b t
  end) =
struct
  let parallel_map t ~f =
    if S.is_empty t
    then return S.empty
    else
      let+ a = parallel_array_of_list_map (S.to_list t) ~f:(fun (k, v) -> f k v) in
      let pos = ref 0 in
      S.mapi t ~f:(fun _ _ ->
        let i = !pos in
        pos := i + 1;
        a.(i))
  ;;
end
[@@inline always]

let rec repeat_while : 'a. f:('a -> 'a option t) -> init:'a -> unit t =
  fun ~f ~init ->
  let* result = f init in
  match result with
  | None -> return ()
  | Some init -> repeat_while ~f ~init
;;

module Exns = Monoid.Appendable_list (Exn_with_backtrace)

let collect_errors f =
  let+ res =
    map_reduce_errors
      (module Exns)
      f
      ~on_error:(fun e -> return (Appendable_list.singleton e))
  in
  match res with
  | Ok x -> Ok x
  | Error l -> Error (Appendable_list.to_list l)
;;

let finalize f ~finally =
  let* res1 = collect_errors f in
  let* res2 = collect_errors finally in
  let res =
    match res1, res2 with
    | Ok x, Ok () -> Ok x
    | Error l, Ok _ | Ok _, Error l -> Error l
    | Error l1, Error l2 -> Error (l1 @ l2)
  in
  match res with
  | Ok x -> return x
  | Error l -> reraise_all l
;;
