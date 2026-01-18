open Import

type 'a eval_mode =
  | Lazy : Dep.Set.t eval_mode
  | Eager : Dep.Facts.t eval_mode

module Deps_or_facts = struct
  let empty : type m. m eval_mode -> m = function
    | Lazy -> Dep.Set.empty
    | Eager -> Dep.Facts.empty
  ;;

  let return : type a m. a -> m eval_mode -> a * m = fun a mode -> a, empty mode

  let union : type m. m eval_mode -> m -> m -> m =
    fun mode a b ->
    match mode with
    | Lazy -> Dep.Set.union a b
    | Eager -> Dep.Facts.union a b
  ;;

  let union_all : type m. m eval_mode -> m list -> m =
    fun mode list ->
    match mode with
    | Lazy -> Dep.Set.union_all list
    | Eager -> Dep.Facts.union_all list
  ;;
end

type 'a memoized =
  { lazy_ : ('a * Dep.Set.t) Memo.Lazy.t Lazy.t
  ; eager : ('a * Dep.Facts.t) Memo.Lazy.t
  }

type ('input, 'output) memo =
  { lazy_ : ('input, 'output * Dep.Set.t) Memo.Table.t Lazy.t
  ; eager : ('input, 'output * Dep.Facts.t) Memo.Table.t Lazy.t
  }

module T = struct
  type _ t =
    | Return : 'a -> 'a t
    | Map : 'a t * ('a -> 'b) -> 'b t
    | Bind : 'a t * ('a -> 'b t) -> 'b t
    | Both : 'a t * 'b t -> ('a * 'b) t
    | All : 'a t list -> 'a list t
    | All_unit : unit t list -> unit t
    | Of_memo : 'a Memo.t -> 'a t
    | Record :
        { res : 'a
        ; deps : Dep.Set.t
        ; f : Dep.t -> Dep.Fact.t Memo.t
        }
        -> 'a t
    | Record_success : unit Memo.t -> unit t
    | Memoize : 'a memoized -> 'a t
    | Goal : 'a t -> 'a t
    | Exec_memo : ('i, 'o) memo * 'i -> 'o t
    | Push_stack_frame : (unit -> User_message.Style.t Pp.t) * (unit -> 'a t) -> 'a t
    | List_map : 'a list * ('a -> 'b t) -> 'b list t
    | List_concat_map : 'a list * ('a -> 'b list t) -> 'b list t

  let return x = Return x
  let map t ~f = Map (t, f)
  let bind t ~f = Bind (t, f)
  let both x y = Both (x, y)
  let all xs = All xs
  let all_unit xs = All_unit xs
  let of_memo memo = Of_memo memo
  let record res deps ~f = Record { res; deps; f }
  let record_success memo = Record_success memo
  let exec_memo m i = Exec_memo (m, i)
  let goal t = Goal t

  module O = struct
    let ( >>> ) a b = map (both a b) ~f:snd
    let ( >>= ) t f = bind t ~f
    let ( >>| ) t f = map t ~f
    let ( and+ ) = both
    let ( and* ) = both
    let ( let+ ) t f = map t ~f
    let ( let* ) t f = bind t ~f
  end
end

include T

let force_memoized : type a m. m eval_mode -> a memoized -> (a * m) Memo.t =
  fun mode { lazy_; eager } ->
  match mode with
  | Lazy -> Memo.Lazy.force (Lazy.force lazy_)
  | Eager -> Memo.Lazy.force eager
;;

let rec eval : type a m. a t -> m eval_mode -> (a * m) Memo.t =
  fun t mode ->
  match t with
  | Return x -> Memo.return (Deps_or_facts.return x mode)
  | Map (t, f) ->
    let open Memo.O in
    let+ x, deps = eval t mode in
    f x, deps
  | Bind (t, f) ->
    let open Memo.O in
    let* x, deps1 = eval t mode in
    let+ y, deps2 = eval (f x) mode in
    y, Deps_or_facts.union mode deps1 deps2
  | Both (a, b) ->
    let open Memo.O in
    let+ (a, deps_a), (b, deps_b) =
      Memo.fork_and_join (fun () -> eval a mode) (fun () -> eval b mode)
    in
    (a, b), Deps_or_facts.union mode deps_a deps_b
  | All ts ->
    let open Memo.O in
    let+ res = Memo.parallel_map ts ~f:(fun t -> eval t mode) in
    let res, deps = List.split res in
    res, Deps_or_facts.union_all mode deps
  | All_unit ts ->
    let open Memo.O in
    let+ res = Memo.parallel_map ts ~f:(fun t -> eval t mode) in
    let deps = List.map res ~f:snd in
    (), Deps_or_facts.union_all mode deps
  | Of_memo memo ->
    let open Memo.O in
    let+ x = memo in
    x, Deps_or_facts.empty mode
  | Record { res; deps; f } ->
    (match mode with
     | Lazy -> Memo.return (res, deps)
     | Eager ->
       let open Memo.O in
       let+ facts = Dep.Facts.record_facts deps ~f in
       res, facts)
  | Record_success memo ->
    (match mode with
     | Lazy -> Memo.return ((), Dep.Set.empty)
     | Eager ->
       let open Memo.O in
       let+ () = memo in
       (), Dep.Facts.empty)
  | Memoize memoized -> force_memoized mode memoized
  | Goal t ->
    let open Memo.O in
    let+ a, _ = eval t mode in
    a, Deps_or_facts.empty mode
  | Exec_memo (m, i) -> exec_memo_eval m i mode
  | Push_stack_frame (human_readable_description, f) ->
    Memo.push_stack_frame ~human_readable_description (fun () -> eval (f ()) mode)
  | List_map (l, f) ->
    let open Memo.O in
    let+ res = Memo.parallel_map l ~f:(fun x -> eval (f x) mode) in
    let res, deps = List.split res in
    res, Deps_or_facts.union_all mode deps
  | List_concat_map (l, f) ->
    let open Memo.O in
    let+ res = Memo.parallel_map l ~f:(fun x -> eval (f x) mode) in
    let res, deps = List.split res in
    List.concat res, Deps_or_facts.union_all mode deps

and exec_memo_eval : type i o m. (i, o) memo -> i -> m eval_mode -> (o * m) Memo.t =
  fun memo i mode ->
  match mode with
  | Lazy -> Memo.exec (Lazy.force memo.lazy_) i
  | Eager -> Memo.exec (Lazy.force memo.eager) i
;;

let memoize ?cutoff name t =
  let lazy_ =
    lazy
      (let cutoff =
         Option.map cutoff ~f:(fun equal x y -> Tuple.T2.equal equal Dep.Set.equal x y)
       in
       Memo.lazy_ ?cutoff ~name:(name ^ "(lazy)") (fun () -> eval t Lazy))
  in
  let eager =
    let cutoff =
      Option.map cutoff ~f:(fun equal x y -> Tuple.T2.equal equal Dep.Facts.equal x y)
    in
    Memo.lazy_ ?cutoff ~name (fun () -> eval t Eager)
  in
  Memoize { lazy_; eager }
;;

module Monad_instance = struct
  module List = struct
    include Monad.List (T)

    let map l ~f = List_map (l, f)
    let concat_map l ~f = List_concat_map (l, f)
  end
end

module List = Monad_instance.List

open struct
  module List = Stdune.List
end

let evaluate_and_collect_deps t = eval t Lazy
let evaluate_and_collect_facts t = eval t Eager

let create_memo name ~input ?cutoff ?human_readable_description f =
  let lazy_ =
    lazy
      (let cutoff =
         Option.map cutoff ~f:(fun f (a, deps1) (b, deps2) ->
           f a b && Dep.Set.equal deps1 deps2)
       in
       let name = name ^ "(lazy)" in
       Memo.create name ~input ?cutoff ?human_readable_description (fun x ->
         eval (f x) Lazy))
  and eager =
    lazy
      (let cutoff =
         Option.map cutoff ~f:(fun f (a, facts1) (b, facts2) ->
           f a b && Dep.Facts.equal facts1 facts2)
       in
       Memo.create name ~input ?cutoff ?human_readable_description (fun x ->
         eval (f x) Eager))
  in
  { lazy_; eager }
;;

let push_stack_frame ~human_readable_description f =
  Push_stack_frame (human_readable_description, f)
;;

module Expert = struct
  let record_dep_on_source_file_exn res ?loc (src_path : Path.Source.t) =
    let path : Path.t = Path.source src_path in
    let dep = Dep.file path in
    let f _ =
      let open Memo.O in
      let+ digest =
        Fs_memo.file_digest_exn ?loc (Path.Outside_build_dir.In_source_dir src_path)
      in
      Dep.Fact.file path digest
    in
    record res (Dep.Set.singleton dep) ~f
  ;;
end
