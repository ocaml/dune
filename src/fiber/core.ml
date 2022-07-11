open! Stdune

type 'a t = ('a -> eff) -> eff

and eff =
  | Read_ivar : 'a ivar * ('a -> eff) -> eff
  | Fill_ivar : 'a ivar * 'a * (unit -> eff) -> eff
  | Suspend : ('a k -> unit) * ('a -> eff) -> eff
  | Resume : 'a k * 'a * (unit -> eff) -> eff
  | Get_var : 'a Univ_map.Key.t * ('a option -> eff) -> eff
  | Set_var : 'a Univ_map.Key.t * 'a * (unit -> eff) -> eff
  | Unset_var : 'a Univ_map.Key.t * (unit -> eff) -> eff
  | With_error_handler :
      (Exn_with_backtrace.t -> Nothing.t t) * (unit -> eff)
      -> eff
  | Unwind : ('a -> eff) * 'a -> eff
  | Map_reduce_errors :
      (module Monoid with type t = 'a)
      * (Exn_with_backtrace.t -> 'a t)
      * (unit -> eff)
      * (('b, 'a) result -> eff)
      -> eff
  | Unwind_map_reduce : ('a -> eff) * 'a -> eff
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
      context * ('a -> eff) * ('a, [ `Empty ]) ivar_state
      -> ('a, [> `Empty ]) ivar_state

and value = ..

and context =
  { parent : context
  ; on_error : Exn_with_backtrace.t k
  ; vars : Univ_map.t
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
  { run : 'a -> eff
  ; ctx : context
  }

let return x k = k x

let bind t ~f k = t (fun x -> f x k)

let map t ~f k = t (fun x -> k (f x))

let with_error_handler f ~on_error k =
  With_error_handler (on_error, fun () -> f () (fun x -> Unwind (k, x)))

let map_reduce_errors m ~on_error f k =
  Map_reduce_errors
    (m, on_error, (fun () -> f () (fun x -> Unwind_map_reduce (k, Ok x))), k)

let suspend f k = Suspend (f, k)

let resume suspended x k = Resume (suspended, x, k)

let end_of_fiber = End_of_fiber ()

let never _k = Never ()

let apply f x =
  try f x
  with exn ->
    let exn = Exn_with_backtrace.capture exn in
    Reraise exn

let apply2 f x y =
  try f x y
  with exn ->
    let exn = Exn_with_backtrace.capture exn in
    Reraise exn

let[@inlined always] fork a b =
  match apply a () with
  | End_of_fiber () -> b ()
  | eff -> Fork (eff, b)

let rec nfork x l f =
  match l with
  | [] -> f x
  | y :: l -> (
    (* Manuall inline [fork] manually because the compiler is unfortunately not
       getting rid of the closures. *)
    match apply f x with
    | End_of_fiber () -> nfork y l f
    | eff -> Fork (eff, fun () -> nfork y l f))

let rec nforki i x l f =
  match l with
  | [] -> f i x
  | y :: l -> (
    match apply2 f i x with
    | End_of_fiber () -> nforki (i + 1) y l f
    | eff -> Fork (eff, fun () -> nforki (i + 1) y l f))

let nforki x l f = nforki 0 x l f

let rec nfork_seq left_over x (seq : _ Seq.t) f =
  match seq () with
  | Nil -> f x
  | Cons (y, seq) -> (
    incr left_over;
    match apply f x with
    | End_of_fiber () -> nfork_seq left_over y seq f
    | eff -> Fork (eff, fun () -> nfork_seq left_over y seq f))

let parallel_iter_seq (seq : _ Seq.t) ~f k =
  match seq () with
  | Nil -> k ()
  | Cons (x, seq) ->
    let left_over = ref 1 in
    let f x =
      f x (fun () ->
          decr left_over;
          if !left_over = 0 then k () else end_of_fiber)
    in
    nfork_seq left_over x seq f

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
    | Got_b b -> k (a, b)
  and kb b =
    match !state with
    | Nothing_yet ->
      state := Got_b b;
      end_of_fiber
    | Got_a a -> k (a, b)
    | Got_b _ -> assert false
  in
  match apply2 fa () ka with
  | End_of_fiber () -> fb () kb
  | eff -> Fork (eff, fun () -> fb () kb)

let fork_and_join_unit fa fb k =
  let state = ref Nothing_yet in
  match
    apply2 fa () (fun () ->
        match !state with
        | Nothing_yet ->
          state := Got_a ();
          end_of_fiber
        | Got_a _ -> assert false
        | Got_b b -> k b)
  with
  | End_of_fiber () -> fb () k
  | eff ->
    Fork
      ( eff
      , fun () ->
          fb () (fun b ->
              match !state with
              | Nothing_yet ->
                state := Got_b b;
                end_of_fiber
              | Got_a () -> k b
              | Got_b _ -> assert false) )

let rec length_and_rev l len acc =
  match l with
  | [] -> (len, acc)
  | x :: l -> length_and_rev l (len + 1) (x :: acc)

let length_and_rev l = length_and_rev l 0 []

let reraise_all l _k =
  match l with
  | [] -> Never ()
  | [ exn ] -> Exn_with_backtrace.reraise exn
  | _ -> Reraise_all l

module Ivar = struct
  type 'a t = 'a ivar

  let create () = { state = Empty }

  let read t k = Read_ivar (t, k)

  let fill t x k = Fill_ivar (t, x, k)

  let peek t k =
    k
      (match t.state with
      | Empty | Empty_with_readers _ -> None
      | Full x -> Some x)
end

module Var = struct
  include Univ_map.Key

  let get var k = Get_var (var, k)

  let get_exn var =
    map (get var) ~f:(function
      | None -> failwith "Fiber.Var.get_exn"
      | Some value -> value)

  let set var x f k = Set_var (var, x, fun () -> f () (fun x -> Unwind (k, x)))

  let unset var f k = Unset_var (var, fun () -> f () (fun x -> Unwind (k, x)))

  let create () = create ~name:"var" (fun _ -> Dyn.string "var")
end

let of_thunk f k = f () k

module O = struct
  let ( >>> ) a b k = a (fun () -> b k)

  let ( >>= ) t f k = t (fun x -> f x k)

  let ( >>| ) t f k = t (fun x -> k (f x))

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

let sequential_map l ~f =
  let rec loop l acc =
    match l with
    | [] -> return (List.rev acc)
    | x :: l ->
      let* x = f x in
      loop l (x :: acc)
  in
  loop l []

let sequential_iter l ~f =
  let rec loop l =
    match l with
    | [] -> return ()
    | x :: l ->
      let* () = f x in
      loop l
  in
  loop l

let parallel_iter l ~f k =
  match l with
  | [] -> k ()
  | [ x ] -> f x k
  | x :: l ->
    let len = List.length l + 1 in
    let left_over = ref len in
    let f x =
      f x (fun () ->
          decr left_over;
          if !left_over = 0 then k () else end_of_fiber)
    in
    nfork x l f

let parallel_array_of_list_map' x l ~f k =
  let len = List.length l + 1 in
  let left_over = ref len in
  let results = ref [||] in
  let f i x =
    f x (fun y ->
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
        if !left_over = 0 then k a else end_of_fiber)
  in
  nforki x l f

let parallel_array_of_list_map l ~f k =
  match l with
  | [] -> k [||]
  | [ x ] -> f x (fun x -> k [| x |])
  | x :: l -> parallel_array_of_list_map' x l ~f k

let parallel_map l ~f k =
  match l with
  | [] -> k []
  | [ x ] -> f x (fun x -> k [ x ])
  | x :: l -> parallel_array_of_list_map' x l ~f (fun a -> k (Array.to_list a))

let all = sequential_map ~f:Fun.id

let all_concurrently = parallel_map ~f:Fun.id

let all_concurrently_unit l = parallel_iter l ~f:Fun.id

let rec sequential_iter_seq (seq : _ Seq.t) ~f =
  match seq () with
  | Nil -> return ()
  | Cons (x, seq) ->
    let* () = f x in
    sequential_iter_seq seq ~f

let parallel_iter_set (type a s)
    (module S : Set.S with type elt = a and type t = s) set ~(f : a -> unit t) =
  parallel_iter_seq (S.to_seq set) ~f

module Make_map_traversals (Map : Map.S) = struct
  let parallel_iter t ~f =
    parallel_iter_seq (Map.to_seq t) ~f:(fun (k, v) -> f k v)

  let parallel_map t ~f =
    if Map.is_empty t then return Map.empty
    else
      let+ a =
        parallel_array_of_list_map (Map.to_list t) ~f:(fun (k, v) -> f k v)
      in
      let pos = ref 0 in
      Map.mapi t ~f:(fun _ _ ->
          let i = !pos in
          pos := i + 1;
          a.(i))
end
[@@inline always]

let rec repeat_while : 'a. f:('a -> 'a option t) -> init:'a -> unit t =
 fun ~f ~init ->
  let* result = f init in
  match result with
  | None -> return ()
  | Some init -> repeat_while ~f ~init

let collect_errors f =
  let module Exns = Monoid.Appendable_list (Exn_with_backtrace) in
  let+ res =
    map_reduce_errors
      (module Exns)
      f
      ~on_error:(fun e -> return (Appendable_list.singleton e))
  in
  match res with
  | Ok x -> Ok x
  | Error l -> Error (Appendable_list.to_list l)

let finalize f ~finally =
  let* res1 = collect_errors f in
  let* res2 = collect_errors finally in
  let res =
    match (res1, res2) with
    | Ok x, Ok () -> Ok x
    | Error l, Ok _ | Ok _, Error l -> Error l
    | Error l1, Error l2 -> Error (l1 @ l2)
  in
  match res with
  | Ok x -> return x
  | Error l -> reraise_all l
