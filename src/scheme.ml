open! Stdune

open Scheme_intf

module Path = Path.Build

module Gen' = struct
  include Gen
  module Evaluated = struct
    type 'rules t = {
      by_child : 'rules t Memo.Lazy.t String.Map.t;
      rules_here : 'rules Memo.Lazy.t;
    }
  end
end
open Gen'

module Make(Rules : sig
    type t
    val empty : t
    val union : t -> t -> t
  end) = struct

  type 'rules t_gen = 'rules Gen.t =
    | Empty
    | Union of 'rules t_gen * 'rules t_gen
    | Approximation of Dir_set.t * 'rules t_gen
    | Finite of 'rules Path.Map.t
    | Thunk of (unit -> 'rules t_gen)

  type t = Rules.t Gen.t

  module Evaluated = struct
    type 'rules t_gen = 'rules Evaluated.t = {
      by_child : 'rules t_gen Memo.Lazy.t String.Map.t;
      rules_here : 'rules Memo.Lazy.t;
    }
    type t = Rules.t t_gen

    let empty =
      { by_child = String.Map.empty;
        rules_here = Memo.Lazy.of_val Rules.empty;
      }

    let descend t dir =
      match String.Map.find t.by_child dir with
      | None -> empty
      | Some res -> Memo.Lazy.force res

    let rec union ~union_rules x y =
      {
        by_child =
          String.Map.union x.by_child y.by_child
            ~f:(fun _key data1 data2 -> Some (
              Memo.Lazy.map2 data1 data2
                ~f:(fun x y -> union ~union_rules x y)));
        rules_here =
          Memo.Lazy.map2 x.rules_here y.rules_here ~f:union_rules
      }

    let union = union ~union_rules:Rules.union

    let rec restrict (dirs : Dir_set.t) t : t =
      {
        rules_here =
          (if Dir_set.here dirs then
             Memo.Lazy.bind t ~f:(fun t -> t.rules_here)
           else
             Memo.Lazy.of_val Rules.empty);
        by_child =
          let dirs_children = Dir_set.children dirs in
          (match Dir_set.Children.default dirs_children with
           | true ->
             (* This is forcing the lazy potentially too early if the directory
                the user is interested in is not actually in the set.  We're not
                fully committed to supporting this case though, anyway. *)
             String.Map.mapi (Memo.Lazy.force t).by_child
               ~f:(fun dir v ->
                 Memo.lazy_ (fun () ->
                   restrict
                     (Dir_set.descend dirs dir)
                     v))
           | false ->
             String.Map.mapi (Dir_set.Children.exceptions dirs_children)
               ~f:(fun dir v ->
                 Memo.lazy_ (fun () ->
                   restrict
                     v
                     (Memo.lazy_ (fun () ->
                        descend (Memo.Lazy.force t) dir)))));
      }

    let singleton path (rules : Rules.t) =
      let rec go = function
        | [] ->
          { by_child = String.Map.empty; rules_here = Memo.Lazy.of_val rules; }
        | x :: xs ->
          {
            by_child = String.Map.singleton x (Memo.Lazy.of_val (go xs));
            rules_here = Memo.Lazy.of_val Rules.empty;
          }
      in
      go (Path.explode path)

    let finite m =
      Path.Map.to_list m
      |> List.map ~f:(fun (path, rules) ->
        singleton path rules)
      |> List.fold_left ~init:empty ~f:union

  end

  let rec evaluate ~env = function
    | Empty -> Evaluated.empty
    | Union (x, y) -> Evaluated.union (evaluate ~env x) (evaluate ~env y)
    | Approximation (paths, rules) ->
      if
        not (Dir_set.is_subset paths ~of_:env)
        && not (Dir_set.is_subset (Dir_set.negate paths) ~of_:env)
      then
        raise (Exn.code_error
                 "inner [Approximate] specifies a set such that neither it, \
                  nor its negation, are a subset of directories specified by \
                  the outer [Approximate]."
                 [
                   "inner", (Dir_set.to_sexp paths);
                   "outer", (Dir_set.to_sexp env);
                 ])
      else
        let paths = Dir_set.intersect paths env in
        Evaluated.restrict paths
          (Memo.lazy_ (fun () -> evaluate ~env:paths rules))
    | Finite rules -> Evaluated.finite rules
    | Thunk f -> evaluate ~env (f ())

  let all l = List.fold_left ~init:Empty ~f:(fun x y -> Union (x, y)) l

  module For_tests = struct
    (* [collect_rules_simple] is oversimplified in two ways:
       - it does not share the work of scheme flattening, so repeated lookups do
         repeated work
       - it does not check that approximations are correct

       If approximations are not correct, it will honor the approximation.
       So approximations act like views that prevent the rules from being seen
       rather than from being declared in the first place.
    *)
    let collect_rules_simple =
      let rec go (t : t) ~dir =
        match t with
        | Empty -> Rules.empty
        | Union (a, b) -> Rules.union(go a ~dir) (go b ~dir)
        | Approximation (dirs, t) ->
          (match Dir_set.mem dirs dir with
           | true -> go t ~dir
           | false -> Rules.empty)
        | Finite rules ->
          (match Path.Map.find rules dir with
           | None -> Rules.empty
           | Some rule -> rule)
        | Thunk f ->
          go (f ()) ~dir
      in
      go

  end

  let get_rules : Evaluated.t -> dir:Path.t -> Rules.t =
    fun t ~dir ->
      let dir = Path.explode dir in
      let t = List.fold_left dir ~init:t ~f:Evaluated.descend in
      Memo.Lazy.force t.rules_here

  let evaluate = evaluate ~env:Dir_set.universal

end

module Rules_scheme = Make(struct
    type t = unit -> unit
    let empty = (fun () -> ())
    let union f g () = f (); g ()
  end)

include Rules_scheme

module Gen = struct
  module For_tests = struct

    let instrument ~print =
      let print path suffix =
        print (String.concat (List.rev path @ [suffix]) ~sep:":")
      in
      let rec go ~path t = match t with
        | Gen.Empty -> Gen.Empty
        | Union (t1, t2) ->
          Union (go ~path:("l"::path) t1, go ~path:("r"::path) t2)
        | Approximation (dirs, rules) ->
          let path = "t" :: path in
          Approximation (dirs, go ~path rules)
        | Finite m -> Finite m
        | Thunk t ->
          Thunk (fun () ->
            print path "thunk";
            t ())
      in
      go ~path:[]
  end
end
