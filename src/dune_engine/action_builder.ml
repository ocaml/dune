open Import

type 'a eval_mode =
  | Lazy : Dep.Set.t eval_mode
  | Eager : Dep.Facts.t eval_mode

type 'a thunk = { f : 'm. 'm eval_mode -> ('a * 'm) Memo.t } [@@unboxed]

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

module T = struct
  open Memo.O

  module M = struct
    type 'a t = 'a thunk

    let return x = { f = (fun mode -> Memo.return (Deps_or_facts.return x mode)) }

    let map t ~f =
      { f =
          (fun mode ->
            let+ x, deps = t.f mode in
            f x, deps)
      }
    ;;

    let bind t ~f =
      { f =
          (fun mode ->
            let* x, deps1 = t.f mode in
            let+ y, deps2 = (f x).f mode in
            y, Deps_or_facts.union mode deps1 deps2)
      }
    ;;
  end

  include M

  let both x y =
    { f =
        (fun mode ->
          let+ x, deps1 = x.f mode
          and+ y, deps2 = y.f mode in
          (x, y), Deps_or_facts.union mode deps1 deps2)
    }
  ;;

  let all xs =
    { f =
        (fun mode ->
          let+ res = Memo.parallel_map xs ~f:(fun x -> x.f mode) in
          let res, facts = List.split res in
          res, Deps_or_facts.union_all mode facts)
    }
  ;;

  let of_memo memo =
    { f =
        (fun mode ->
          let+ x = memo in
          x, Deps_or_facts.empty mode)
    }
  ;;

  let record res (deps : Dep.Set.t) ~f =
    let f : type m. m eval_mode -> (_ * m) Memo.t =
      fun mode ->
      let open Memo.O in
      match mode with
      | Lazy -> Memo.return (res, deps)
      | Eager ->
        let+ facts = Dep.Facts.record_facts deps ~f in
        res, facts
    in
    { f }
  ;;

  let record_success memo =
    let f : type m. m eval_mode -> (unit * m) Memo.t =
      fun mode ->
      let open Memo.O in
      match mode with
      | Lazy -> Memo.return ((), Dep.Set.empty)
      | Eager ->
        let+ () = memo in
        (), Dep.Facts.empty
    in
    { f }
  ;;

  module Expert = struct
    let record_dep_on_source_file_exn res ?loc (src_path : Path.Source.t) =
      let f : type m. m eval_mode -> (_ * m) Memo.t =
        fun mode ->
        let (path : Path.t) = Path.source src_path in
        let dep = Dep.file path in
        match mode with
        | Lazy -> Memo.return (res, Dep.Set.singleton dep)
        | Eager ->
          let open Memo.O in
          let+ digest =
            Fs_memo.file_digest_exn ?loc (Path.Outside_build_dir.In_source_dir src_path)
          in
          res, Dep.Facts.singleton dep (Dep.Fact.file path digest)
      in
      { f }
    ;;
  end

  module O = struct
    let ( >>> ) a b =
      { f =
          (fun mode ->
            let+ ((), deps_a), (b, deps_b) =
              Memo.fork_and_join (fun () -> a.f mode) (fun () -> b.f mode)
            in
            b, Deps_or_facts.union mode deps_a deps_b)
      }
    ;;

    let ( >>= ) t f = bind t ~f
    let ( >>| ) t f = map t ~f
    let ( and+ ) = both
    let ( and* ) = both
    let ( let+ ) t f = map t ~f
    let ( let* ) t f = bind t ~f
  end

  module List = struct
    include Monad.List (struct
        include M
        include Monad.Make (M)
      end)

    let map l ~f =
      { f =
          (fun mode ->
            let+ res = Memo.parallel_map l ~f:(fun x -> (f x).f mode) in
            let res, deps = List.split res in
            res, Deps_or_facts.union_all mode deps)
      }
    ;;

    let concat_map l ~f =
      { f =
          (fun mode ->
            let+ res = Memo.parallel_map l ~f:(fun x -> (f x).f mode) in
            let res, deps = List.split res in
            List.concat res, Deps_or_facts.union_all mode deps)
      }
    ;;
  end
end

include T
open O

open struct
  module List = Stdune.List
end

let evaluate_and_collect_deps t = t.f Lazy
let evaluate_and_collect_facts t = t.f Eager

let force_lazy_or_eager
  : type a m.
    m eval_mode
    -> (a * Dep.Set.t) Memo.Lazy.t Lazy.t
    -> (a * Dep.Facts.t) Memo.Lazy.t
    -> (a * m) Memo.t
  =
  fun mode lazy_ eager ->
  match mode with
  | Lazy -> Memo.Lazy.force (Lazy.force lazy_)
  | Eager -> Memo.Lazy.force eager
;;

let memoize ?cutoff name t =
  let lazy_ : ('a * Dep.Set.t) Memo.Lazy.t Lazy.t =
    lazy
      (let cutoff =
         Option.map cutoff ~f:(fun equal x y -> Tuple.T2.equal equal Dep.Set.equal x y)
       in
       Memo.lazy_ ?cutoff ~name:(name ^ "(lazy)") (fun () -> t.f Lazy))
  in
  (* Unlike [lazy_], [eager] doesn't have the outer [Lazy.t] wrapper because most [Eager]
     nodes end up getting forced during every build. *)
  let eager : ('a * Dep.Facts.t) Memo.Lazy.t =
    let cutoff =
      Option.map cutoff ~f:(fun equal x y -> Tuple.T2.equal equal Dep.Facts.equal x y)
    in
    Memo.lazy_ ?cutoff ~name (fun () -> t.f Eager)
  in
  { f = (fun mode -> force_lazy_or_eager mode lazy_ eager) }
;;

let map2 x y ~f =
  let+ x = x
  and+ y = y in
  f x y
;;

let all_unit (xs : unit t list) =
  { f =
      (fun mode ->
        let open Memo.O in
        let+ res = Memo.parallel_map xs ~f:(fun x -> x.f mode) in
        let deps = List.map res ~f:snd in
        (), Deps_or_facts.union_all mode deps)
  }
;;

type ('input, 'output) memo =
  { lazy_ : ('input, 'output * Dep.Set.t) Memo.Table.t Lazy.t
  ; eager : ('input, 'output * Dep.Facts.t) Memo.Table.t Lazy.t
  }

let create_memo name ~input ?cutoff ?human_readable_description f =
  let lazy_ =
    lazy
      (let cutoff =
         Option.map cutoff ~f:(fun f (a, deps1) (b, deps2) ->
           f a b && Dep.Set.equal deps1 deps2)
       in
       let name = name ^ "(lazy)" in
       Memo.create name ~input ?cutoff ?human_readable_description (fun x -> (f x).f Lazy))
  and eager =
    lazy
      (let cutoff =
         Option.map cutoff ~f:(fun f (a, facts1) (b, facts2) ->
           f a b && Dep.Facts.equal facts1 facts2)
       in
       Memo.create name ~input ?cutoff ?human_readable_description (fun x ->
         (f x).f Eager))
  in
  { lazy_; eager }
;;

let exec_memo : type i o m. (i, o) memo -> i -> m eval_mode -> (o * m) Memo.t =
  fun memo i mode ->
  match mode with
  | Lazy -> Memo.exec (Lazy.force memo.lazy_) i
  | Eager -> Memo.exec (Lazy.force memo.eager) i
;;

let exec_memo m i = { f = (fun mode -> exec_memo m i mode) }

let goal t =
  { f =
      (fun mode ->
        let open Memo.O in
        let+ a, _facts_are_irrelevant_for_goals = t.f mode in
        a, Deps_or_facts.empty mode)
  }
;;

let push_stack_frame ~human_readable_description f =
  { f =
      (fun mode ->
        Memo.push_stack_frame ~human_readable_description (fun () -> (f ()).f mode))
  }
;;
