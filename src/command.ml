open! Stdune
open Import

type static = Arg_spec.static
type dynamic = Arg_spec.dynamic

type _ t =
  | A        : string -> _ t
  | As       : string list -> _ t
  | S        : 'a t list -> 'a t
  | Concat   : string * 'a t list  -> 'a t
  | Dep      : Path.t -> _ t
  | Deps     : Path.t list -> _ t
  | Target   : Path.t -> dynamic t
  | Path     : Path.t -> _ t
  | Paths    : Path.t list -> _ t
  | Hidden_deps    : Dep.Set.t -> _ t
  | Hidden_targets : Path.t list -> dynamic t
  | Dyn      : static t Build.s -> dynamic t
  | Fail     : fail -> _ t

let rec from_arg_spec : type a b. a Build.s -> (a, b) Arg_spec.t -> b t =
  fun  arg (spec : (_, b) Arg_spec.t) -> match spec with
    | Arg_spec.A s  -> A s
    | Arg_spec.As l -> As l
    | Arg_spec.S ts -> S (List.map ts ~f:(from_arg_spec arg))
    | Arg_spec.Concat (sep, ts) -> Concat (sep, (List.map ts ~f:(from_arg_spec arg)))
    | Arg_spec.Dep fn -> Dep fn
    | Arg_spec.Deps fns -> Deps fns
    | Arg_spec.Target t -> Target t
    | Arg_spec.Path fn -> Path fn
    | Arg_spec.Paths fns -> Paths fns
    | Arg_spec.Hidden_deps l -> Hidden_deps l
    | Arg_spec.Hidden_targets l -> Hidden_targets l
    | Arg_spec.Dyn f -> Dyn (Build.S.map arg (fun x -> from_arg_spec (Build.return ()) (f x)))
    | Arg_spec.Fail f -> Fail f

let rec add_targets ts acc =
  List.fold_left ts ~init:acc ~f:(fun acc t ->
    match t with
    | Target fn  -> fn :: acc
    | Hidden_targets fns -> List.rev_append fns acc
    | S ts
    | Concat (_, ts) -> add_targets ts acc
    | _ -> acc)

let expand ~dir ts =
  let run_loop t =
    let static_deps = ref Dep.Set.empty in
    let add_dep path = static_deps := Dep.Set.add !static_deps (Dep.file path) in
    let rec loop_static : static t -> string list  = function
      | A s  -> [s]
      | As l -> l
      | Dep fn ->
        add_dep fn;
        [Path.reach fn ~from:dir]
      | Path fn -> [Path.reach fn ~from:dir]
      | Deps fns ->
        List.map fns ~f:(fun fn ->
          add_dep fn;
          Path.reach ~from:dir fn)
      | Paths fns ->
        List.map fns ~f:(Path.reach ~from:dir)
      | S ts -> List.concat_map ts ~f:loop_static
      | Concat (sep, ts) -> [String.concat ~sep (loop_static (S ts))]
      | Hidden_deps l ->
        static_deps := Dep.Set.union !static_deps l;
        []
      | Fail f -> f.fail ()
    in
    let res = loop_static t in
    (res, !static_deps)
  in
  let rec loop = function
    | A s  -> Build.return [s]
    | As l -> Build.return l
    | Dep fn -> Build.S.map (Build.path fn) (fun () -> [Path.reach fn ~from:dir])
    | Path fn -> Build.return [Path.reach fn ~from:dir]
    | Deps fns -> Build.S.map (Build.paths fns) (fun () -> List.map fns ~f:(Path.reach ~from:dir))
    | Paths fns -> Build.return (List.map fns ~f:(Path.reach ~from:dir))
    | S ts -> Build.S.map (Build.all (List.map ts ~f:loop)) List.concat
    | Concat (sep, ts) -> Build.S.map (loop (S ts)) (fun x -> [(String.concat ~sep x)])
    | Target fn -> Build.return [Path.reach fn ~from:dir]
    | Dyn dyn -> Build.S.dyn_deps (Build.S.map dyn run_loop)
    | Fail f -> Build.fail f
    | Hidden_deps deps -> Build.S.map (Build.deps deps) (fun () -> [])
    | Hidden_targets _ -> Build.return []
  in
  loop (S ts)

let dep_prog = function
  | Ok p    -> Build.path p
  | Error _ -> Build.return ()

let prog_and_args ?(dir=Path.root) prog args =
  Build.S.seq (dep_prog prog) (
    Build.S.map (expand ~dir args) (fun args -> (prog, args))
  )

let run ~dir ?stdout_to prog args =
  let targets = add_targets args (Option.to_list stdout_to) in
  Build.S.seq (Build.declare_targets (Path.Set.of_list targets)) (
    Build.S.map (prog_and_args ~dir prog args) (fun (prog, args) ->
      let action : Action.t = Run (prog, args) in
      let action =
        match stdout_to with
        | None      -> action
        | Some path -> Redirect (Stdout, path, action)
      in
      Action.Chdir (dir, action)
    )
  )

let quote_args =
  let rec loop quote = function
    | [] -> []
    | arg :: args -> quote :: arg :: loop quote args
  in
  fun quote args -> As (loop quote args)

let fail e = Fail { fail = fun _ -> raise e }

let of_result = function
  | Ok x -> x
  | Error e -> fail e

let of_result_map res ~f =
  match res with
  | Ok    x -> f x
  | Error e -> fail e
