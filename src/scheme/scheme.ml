open Stdune
open Memo.O
module Dir_set = Dune_engine.Dir_set

module T = struct
  type 'rules t =
    | Empty
    | Union of 'rules t * 'rules t
    | Approximation of Path.Build.w Dir_set.t * 'rules t
    | Finite of 'rules Path.Build.Map.t
    | Thunk of (unit -> 'rules t Memo.t)
end

include T

module Evaluated : sig
  type 'rules t

  val union : union_rules:('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
  val empty : unit -> 'a t
  val restrict : Path.Build.w Dir_set.t -> 'a t Memo.Lazy.t -> 'a t Memo.t
  val finite : union_rules:('a -> 'a -> 'a) -> 'a Path.Build.Map.t -> 'a t
  val get_rules : 'a t -> dir:Path.Build.t -> ('a option * String.Set.t) Memo.t
end = struct
  type 'rules t =
    { by_child : 'rules t Memo.Lazy.t String.Map.t
    ; rules_here : 'rules option Memo.Lazy.t
    }

  let empty () = { by_child = String.Map.empty; rules_here = Memo.Lazy.of_val None }

  let descend t dir =
    match String.Map.find t.by_child dir with
    | None -> Memo.return (empty ())
    | Some res -> Memo.Lazy.force res
  ;;

  let rec union ~union_rules x y =
    { by_child =
        String.Map.union x.by_child y.by_child ~f:(fun _key data1 data2 ->
          Some
            (Memo.Lazy.create ~name:"scheme-union" (fun () ->
               let+ x = Memo.Lazy.force data1
               and+ y = Memo.Lazy.force data2 in
               union ~union_rules x y)))
    ; rules_here =
        Memo.lazy_ ~name:"union-rules-here" (fun () ->
          let+ x = Memo.Lazy.force x.rules_here
          and+ y = Memo.Lazy.force y.rules_here in
          Option.merge x y ~f:union_rules)
    }
  ;;

  let rec restrict (dirs : Path.Local.w Dir_set.t) (t : _ Memo.Lazy.t) : _ t Memo.t =
    let rules_here =
      if Dir_set.here dirs
      then
        Memo.Lazy.create ~name:"restrict-rules-here" (fun () ->
          let* t = Memo.Lazy.force t in
          Memo.Lazy.force t.rules_here)
      else Memo.Lazy.of_val None
    in
    let+ by_child =
      match Dir_set.default dirs with
      | true ->
        (* This is forcing the lazy potentially too early if the directory the
           user is interested in is not actually in the set. We're not fully
           committed to supporting this case though, anyway. *)
        let+ t = Memo.Lazy.force t in
        String.Map.mapi t.by_child ~f:(fun dir v ->
          Memo.lazy_ ~name:"restrict-by-child-default" (fun () ->
            restrict (Dir_set.descend dirs dir) v))
      | false ->
        Dir_set.exceptions dirs
        |> String.Map.mapi ~f:(fun dir v ->
          Memo.lazy_ ~name:"restrict-by-child-non-default-outer" (fun () ->
            restrict
              v
              (Memo.lazy_ ~name:"restrict-by-child-non-default-inner" (fun () ->
                 let* t = Memo.Lazy.force t in
                 descend t dir))))
        |> Memo.return
    in
    { rules_here; by_child }
  ;;

  let restrict dirs t = restrict (Dir_set.forget_root dirs) t

  let singleton =
    let rec go rules = function
      | [] -> { by_child = String.Map.empty; rules_here = Memo.Lazy.of_val (Some rules) }
      | x :: xs ->
        { by_child = String.Map.singleton x (Memo.Lazy.of_val (go rules xs))
        ; rules_here = Memo.Lazy.of_val None
        }
    in
    fun path rules -> go rules (Path.Build.explode path)
  ;;

  let finite ~union_rules m =
    Path.Build.Map.foldi m ~init:(empty ()) ~f:(fun path rules acc ->
      union ~union_rules (singleton path rules) acc)
  ;;

  let get_rules =
    let rec loop dir t =
      match dir with
      | [] -> Memo.return t
      | x :: dir -> descend t x >>= loop dir
    in
    fun t ~dir ->
      let* t = loop (Path.Build.explode dir) t in
      let+ rules = Memo.Lazy.force t.rules_here in
      rules, String.Set.of_keys t.by_child
  ;;
end

let evaluate ~union_rules =
  let rec loop ~env = function
    | Empty -> Memo.return (Evaluated.empty ())
    | Thunk f -> f () >>= loop ~env
    | Union (x, y) ->
      let+ x = loop ~env x
      and+ y = loop ~env y in
      Evaluated.union ~union_rules x y
    | Approximation (paths, rules) ->
      if (not (Dir_set.is_subset paths ~of_:env))
         && not (Dir_set.is_subset (Dir_set.negate paths) ~of_:env)
      then
        Code_error.raise
          "inner [Approximate] specifies a set such that neither it, nor its negation, \
           are a subset of directories specified by the outer [Approximate]."
          [ "inner", Dir_set.to_dyn paths; "outer", Dir_set.to_dyn env ]
      else (
        let paths = Dir_set.inter paths env in
        Evaluated.restrict
          paths
          (Memo.lazy_ ~name:"evaluate-restrict" (fun () -> loop ~env:paths rules)))
    | Finite rules ->
      (match
         Path.Build.Map.foldi rules ~init:[] ~f:(fun p _ acc ->
           if Dir_set.mem env p then acc else p :: acc)
       with
       | [] -> ()
       | violations ->
         Code_error.raise
           "Scheme attempted to generate rules in a directory it promised not to touch"
           [ "directories", (Dyn.list Path.Build.to_dyn) violations ]);
      Memo.return (Evaluated.finite ~union_rules rules)
  in
  fun t -> loop ~env:Dir_set.universal t
;;

let all l = List.fold_left ~init:Empty ~f:(fun x y -> Union (x, y)) l
let evaluate t ~union = evaluate ~union_rules:union t
