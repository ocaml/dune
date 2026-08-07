open Stdune

type 'a t = 'a continuation -> eff

(* Keep arbitrary callbacks as functions, but represent the frames introduced by common
   fiber combinators directly. *)
and 'a continuation =
  | Function : ('a -> eff) -> 'a continuation
  | Effect : eff continuation
  | Map : ('a -> 'b) * 'b continuation -> 'a continuation
  | Map2 : ('a -> 'b) * ('b -> 'c) * 'c continuation -> 'a continuation
  | Map3 : ('a -> 'b) * ('b -> 'c) * ('c -> 'd) * 'd continuation -> 'a continuation
  | Bind : ('a -> 'b t) * 'b continuation -> 'a continuation
  | Apply : ('a -> 'b -> 'c t) * 'b * 'c continuation -> 'a continuation
  | Apply_map : ('a -> 'b -> 'c) * 'b * 'c continuation -> 'a continuation
  | Unwind_to : 'a continuation -> 'a continuation
  | Unwind_map_reduce_to : ('a, 'b) result continuation -> 'a continuation

and eff =
  | Read_ivar : 'a ivar * 'a continuation -> eff
  | Fill_ivar : 'a ivar * 'a * unit continuation -> eff
  | Suspend : ('a k -> unit) * 'a continuation -> eff
  | Resume : 'a k * 'a * unit continuation -> eff
  | Get_var : 'a Var_map.Key.t * 'a continuation -> eff
  | Set_var : 'a Var_map.Key.t * 'a * unit continuation -> eff
  | Update_var : 'a Var_map.Key.t * ('a -> 'a) * unit continuation -> eff
  | With_error_handler : (Exn_with_backtrace.t -> Nothing.t t) * unit continuation -> eff
  | Unwind : 'a continuation * 'a -> eff
  | Map_reduce_errors :
      (module Monoid with type t = 'a)
      * (Exn_with_backtrace.t -> 'a t)
      * (unit -> eff)
      * ('b, 'a) result continuation
      -> eff
  | Unwind_map_reduce : 'a continuation * 'a -> eff
  | End_of_map_reduce_error_handler : (_, _) map_reduce_context' -> eff
  | End_of_fiber of unit
  | Never of unit
  (* Add a dummy unit argument to [End_of_fiber] and [Never] so that all
     constructors are boxed, which removes a branch in the pattern match. *)
  | Fork : eff * (unit -> eff) -> eff
  | Reraise : Exn_with_backtrace.t -> eff
  | Reraise_all : Exn_with_backtrace.t list -> eff
  | Toplevel_exception : Exn_with_backtrace.t -> eff
  | Done of value

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
  ; on_error : Exn_with_backtrace.t k
  ; vars : Var_map.t
  ; map_reduce_context : map_reduce_context
  }

and ('a, 'b) map_reduce_context' =
  { k : ('a, 'b) result k
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
  | Effect -> x
  | Map (f, k) -> continue k (f x)
  | Map2 (f, g, k) -> continue k (g (f x))
  | Map3 (f, g, h, k) -> continue k (h (g (f x)))
  | Bind (f, k) -> f x k
  | Apply (f, y, k) -> f x y k
  | Apply_map (f, y, k) -> continue k (f x y)
  | Unwind_to k -> Unwind (k, x)
  | Unwind_map_reduce_to k -> Unwind_map_reduce (k, Ok x)
;;

let return x k = continue k x
let bind t ~f k = t (Bind (f, k))

let map t ~f k =
  match k with
  | Map (g, k) -> t (Map2 (f, g, k))
  | Map2 (g, h, k) -> t (Map3 (f, g, h, k))
  | k -> t (Map (f, k))
;;

let with_error_handler f ~on_error k =
  With_error_handler (on_error, Function (fun () -> f () (Unwind_to k)))
;;

let map_reduce_errors m ~on_error f k =
  Map_reduce_errors (m, on_error, (fun () -> f () (Unwind_map_reduce_to k)), k)
;;

let suspend f k = Suspend (f, k)
let resume suspended x k = Resume (suspended, x, k)
let end_of_fiber = End_of_fiber ()
let never _k = Never ()

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

let[@inline always] fork a b =
  match apply a () with
  | End_of_fiber () -> b ()
  | eff -> Fork (eff, b)
;;

let rec nfork x l f =
  match l with
  | [] -> f x
  | y :: l ->
    (* Manually inline [fork] because the compiler is unfortunately
       not getting rid of the closures. *)
    (match apply f x with
     | End_of_fiber () -> nfork y l f
     | eff -> Fork (eff, fun () -> nfork y l f))
;;

let rec nforki i x l f =
  match l with
  | [] -> f i x
  | y :: l ->
    (match apply2 f i x with
     | End_of_fiber () -> nforki (i + 1) y l f
     | eff -> Fork (eff, fun () -> nforki (i + 1) y l f))
;;

let nforki x l f = nforki 0 x l f

let rec nfork_seq left_over x (seq : _ Seq.t) f =
  match seq () with
  | Nil -> f x
  | Cons (y, seq) ->
    incr left_over;
    (match apply f x with
     | End_of_fiber () -> nfork_seq left_over y seq f
     | eff -> Fork (eff, fun () -> nfork_seq left_over y seq f))
;;

let rec nfork_array a i f =
  if i = Array.length a - 1
  then f a.(i)
  else (
    match apply f a.(i) with
    | End_of_fiber () -> nfork_array a (i + 1) f
    | eff -> Fork (eff, fun () -> nfork_array a (i + 1) f))
;;

let parallel_iter_seq (seq : _ Seq.t) ~f k =
  match seq () with
  | Nil -> continue k ()
  | Cons (x, seq) ->
    let left_over = ref 1 in
    let f x =
      f
        x
        (Function
           (fun () ->
             decr left_over;
             if !left_over = 0 then continue k () else end_of_fiber))
    in
    nfork_seq left_over x seq f
;;

let map_reduce_seq (seq : _ Seq.t) ~f ~empty ~combine k =
  match seq () with
  | Nil -> continue k empty
  | Cons (x, seq) ->
    let current = ref empty in
    let running = ref 1 in
    let f x =
      f
        x
        (Function
           (fun y ->
             current := combine !current y;
             decr running;
             if !running = 0 then continue k !current else end_of_fiber))
    in
    nfork_seq running x seq f
;;

let map_reduce_array a ~f ~empty ~combine k =
  match Array.length a with
  | 0 -> continue k empty
  | len ->
    let current = ref empty in
    let running = ref len in
    let f x =
      f
        x
        (Function
           (fun y ->
             current := combine !current y;
             decr running;
             if !running = 0 then continue k !current else end_of_fiber))
    in
    nfork_array a 0 f
;;

let map_reduce l ~f ~empty ~combine k =
  match l with
  | [] -> continue k empty
  | x :: l ->
    let current = ref empty in
    let running = ref (List.length l + 1) in
    let f x =
      f
        x
        (Function
           (fun y ->
             current := combine !current y;
             decr running;
             if !running = 0 then continue k !current else end_of_fiber))
    in
    nfork x l f
;;

type ('a, 'b) fork_and_join_state =
  | Nothing_yet
  | Got_a of 'a
  | Got_b of 'b

let fork_and_join fa fb k =
  let state = ref Nothing_yet in
  let ka a =
    match !state with
    | Nothing_yet ->
      state := Got_a a;
      end_of_fiber
    | Got_a _ -> assert false
    | Got_b b -> continue k (a, b)
  and kb b =
    match !state with
    | Nothing_yet ->
      state := Got_b b;
      end_of_fiber
    | Got_a a -> continue k (a, b)
    | Got_b _ -> assert false
  in
  let ka = Function ka in
  let kb = Function kb in
  match apply2 fa () ka with
  | End_of_fiber () -> fb () kb
  | eff -> Fork (eff, fun () -> fb () kb)
;;

let fork_and_join_unit fa fb k =
  let state = ref Nothing_yet in
  match
    apply2
      fa
      ()
      (Function
         (fun () ->
           match !state with
           | Nothing_yet ->
             state := Got_a ();
             end_of_fiber
           | Got_a _ -> assert false
           | Got_b b -> continue k b))
  with
  | End_of_fiber () -> fb () k
  | eff ->
    Fork
      ( eff
      , fun () ->
          fb
            ()
            (Function
               (fun b ->
                 match !state with
                 | Nothing_yet ->
                   state := Got_b b;
                   end_of_fiber
                 | Got_a () -> continue k b
                 | Got_b _ -> assert false)) )
;;

let rec length_and_rev l len acc =
  match l with
  | [] -> len, acc
  | x :: l -> length_and_rev l (len + 1) (x :: acc)
;;

let length_and_rev l = length_and_rev l 0 []

let reraise_all l _k =
  match l with
  | [] -> Never ()
  | [ exn ] -> Exn_with_backtrace.reraise exn
  | _ -> Reraise_all l
;;

module Ivar = struct
  type 'a t = 'a ivar

  let create () = { state = Empty }

  let read t k =
    match t.state with
    | Full x -> continue k x
    | Empty_with_readers _ | Empty -> Read_ivar (t, k)
  ;;

  let fill t x k =
    match t.state with
    | Empty ->
      t.state <- Full x;
      continue k ()
    | Full _ | Empty_with_readers _ -> Fill_ivar (t, x, k)
  ;;

  let create_full a = { state = Full a }

  let peek t =
    match t.state with
    | Empty | Empty_with_readers _ -> None
    | Full x -> Some x
  ;;
end

module Var = struct
  let get (key : 'a Var_map.Key.t) : 'a t = fun k -> Get_var (key, k)

  let set (key : 'a Var_map.Key.t) (value : 'a) (fiber : unit -> 'b t) : 'b t =
    fun k -> Set_var (key, value, Function (fun () -> fiber () (Unwind_to k)))
  ;;

  let get_exn (key : 'a option Var_map.Key.t) : 'a t =
    map (get key) ~f:(function
      | None -> failwith "Fiber.Var.get_exn"
      | Some value -> value)
  ;;

  let update (key : 'a Var_map.Key.t) ~(f : 'a -> 'a) (fiber : unit -> 'b t) : 'b t =
    fun k -> Update_var (key, f, Function (fun () -> fiber () (Unwind_to k)))
  ;;

  let get_apply (key : 'a Var_map.Key.t) (f : 'a -> 'b -> 'c t) (x : 'b) : 'c t =
    fun k -> Get_var (key, Apply (f, x, k))
  ;;

  let get_apply_map (key : 'a Var_map.Key.t) (f : 'a -> 'b -> 'c) (x : 'b) : 'c t =
    fun k -> Get_var (key, Apply_map (f, x, k))
  ;;

  let set_apply (key : 'a Var_map.Key.t) (value : 'a) (f : 'b -> 'c t) (x : 'b) : 'c t =
    fun k -> Set_var (key, value, Function (fun () -> f x (Unwind_to k)))
  ;;

  let update_apply (key : 'a Var_map.Key.t) ~(f : 'a -> 'a) (g : 'b -> 'c t) (x : 'b)
    : 'c t
    =
    fun k -> Update_var (key, f, Function (fun () -> g x (Unwind_to k)))
  ;;

  include Var_map.Key
end

let of_thunk f k = f () k
let of_thunk_apply f x k = f x k

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

let parallel_iter l ~f k =
  match l with
  | [] -> continue k ()
  | [ x ] -> f x k
  | x :: l ->
    let len = List.length l + 1 in
    let left_over = ref len in
    let f x =
      f
        x
        (Function
           (fun () ->
             decr left_over;
             if !left_over = 0 then continue k () else end_of_fiber))
    in
    nfork x l f
;;

let parallel_array_of_list_map' x l ~f k =
  let len = List.length l + 1 in
  let left_over = ref len in
  let results = ref [||] in
  let f i x =
    f
      x
      (Function
         (fun y ->
           let a =
             match !results with
             | [||] ->
               let a = Array.make len y in
               results := a;
               a
             | a ->
               a.(i) <- y;
               a
           in
           decr left_over;
           if !left_over = 0 then continue k a else end_of_fiber))
  in
  nforki x l f
;;

let parallel_array_of_list_map l ~f k =
  match l with
  | [] -> continue k [||]
  | [ x ] -> f x (Map ((fun x -> [| x |]), k))
  | x :: l -> parallel_array_of_list_map' x l ~f k
;;

let parallel_map l ~f k =
  match l with
  | [] -> continue k []
  | [ x ] -> f x (Map ((fun x -> [ x ]), k))
  | x :: l -> parallel_array_of_list_map' x l ~f (Map (Array.to_list, k))
;;

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
