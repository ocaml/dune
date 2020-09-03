open! Dune_engine
open! Stdune
open Import

module Args0 = struct
  type static = Static

  type dynamic = Dynamic

  type expand = dir:Path.t -> (string list * Dep.Set.t, fail) result

  (* Debugging tip: if you changed this file and Dune got broken in a weird way
     it's probably because of the [Fail] constructor. *)
  type _ t =
    | A : string -> _ t
    | As : string list -> _ t
    | S : 'a t list -> 'a t
    | Concat : string * 'a t list -> 'a t
    | Dep : Path.t -> _ t
    | Deps : Path.t list -> _ t
    | Target : Path.Build.t -> dynamic t
    | Path : Path.t -> _ t
    | Paths : Path.t list -> _ t
    | Hidden_deps : Dep.Set.t -> _ t
    | Hidden_targets : Path.Build.t list -> dynamic t
    | Dyn : static t Build.t -> dynamic t
    | Fail : fail -> _ t
    | Expand : expand -> _ t

  let dyn args = Dyn (Build.map args ~f:(fun x -> As x))

  let empty = S []
end

open Args0

let expand_static ~dir t =
  let static_deps = ref Dep.Set.empty in
  let exception Fail of fail in
  let add_dep path = static_deps := Dep.Set.add !static_deps (Dep.file path) in
  let rec loop_static : static t -> string list = function
    | A s -> [ s ]
    | As l -> l
    | Dep fn ->
      add_dep fn;
      [ Path.reach fn ~from:dir ]
    | Path fn -> [ Path.reach fn ~from:dir ]
    | Deps fns ->
      List.map fns ~f:(fun fn ->
          add_dep fn;
          Path.reach ~from:dir fn)
    | Paths fns -> List.map fns ~f:(Path.reach ~from:dir)
    | S ts -> List.concat_map ts ~f:loop_static
    | Concat (sep, ts) -> [ String.concat ~sep (loop_static (S ts)) ]
    | Hidden_deps l ->
      static_deps := Dep.Set.union !static_deps l;
      []
    | Fail f -> raise (Fail f)
    | Expand f -> (
      match f ~dir with
      | Error e -> raise (Fail e)
      | Ok (args, deps) ->
        static_deps := Dep.Set.union !static_deps deps;
        args )
  in
  match loop_static t with
  | exception Fail fail -> Error fail
  | res -> Ok (res, !static_deps)

let expand_static_exn ~dir t =
  match expand_static ~dir t with
  | Error e -> e.fail ()
  | Ok res -> res

let expand ~dir ts =
  let rec loop : _ -> _ Build.With_targets.t = function
    | A s -> Build.With_targets.return [ s ]
    | As l -> Build.With_targets.return l
    | Dep fn ->
      Build.with_no_targets
        (Build.map (Build.path fn) ~f:(fun () -> [ Path.reach fn ~from:dir ]))
    | Path fn -> Build.With_targets.return [ Path.reach fn ~from:dir ]
    | Deps fns ->
      Build.with_no_targets
        (Build.map (Build.paths fns) ~f:(fun () ->
             List.map fns ~f:(Path.reach ~from:dir)))
    | Paths fns ->
      Build.With_targets.return (List.map fns ~f:(Path.reach ~from:dir))
    | S ts ->
      Build.With_targets.map
        (Build.With_targets.all (List.map ts ~f:loop))
        ~f:List.concat
    | Concat (sep, ts) ->
      Build.With_targets.map (loop (S ts)) ~f:(fun x ->
          [ String.concat ~sep x ])
    | Target fn ->
      Build.with_targets ~targets:[ fn ]
        (Build.return [ Path.reach (Path.build fn) ~from:dir ])
    | Dyn dyn ->
      Build.with_no_targets
        (Build.dyn_deps (Build.map dyn ~f:(expand_static_exn ~dir)))
    | Fail f -> Build.with_no_targets (Build.fail f)
    | Hidden_deps deps ->
      Build.with_no_targets (Build.map (Build.deps deps) ~f:(fun () -> []))
    | Hidden_targets fns -> Build.with_targets ~targets:fns (Build.return [])
    | Expand f ->
      Build.with_no_targets
        ( match f ~dir with
        | Error e -> Build.fail e
        | Ok (args, deps) ->
          let open Build.O in
          Build.deps deps >>> Build.return args )
  in
  loop (S ts)

let dep_prog = function
  | Ok p -> Build.path p
  | Error _ -> Build.return ()

let prog_and_args ?(dir = Path.root) prog args =
  let open Build.With_targets.O in
  Build.with_no_targets (dep_prog prog)
  >>> Build.With_targets.map (expand ~dir args) ~f:(fun args -> (prog, args))

let run ~dir ?stdout_to prog args =
  Build.With_targets.map (prog_and_args ~dir prog args) ~f:(fun (prog, args) ->
      let action : Action.t = Run (prog, args) in
      let action =
        match stdout_to with
        | None -> action
        | Some path -> Redirect_out (Stdout, path, action)
      in
      Action.Chdir (dir, action))
  |> Build.With_targets.add ~targets:(Option.to_list stdout_to)

let quote_args =
  let rec loop quote = function
    | [] -> []
    | arg :: args -> quote :: arg :: loop quote args
  in
  fun quote args -> As (loop quote args)

let fail e = Fail { fail = (fun _ -> raise e) }

let of_result = function
  | Ok x -> x
  | Error e -> fail e

let of_result_map res ~f =
  match res with
  | Ok x -> f x
  | Error e -> fail e

module Args = struct
  include Args0

  let memo t =
    let memo =
      Memo.create_hidden "Command.Args.memo"
        ~input:(module Path)
        Sync
        (fun dir -> expand_static ~dir t)
    in
    Expand (fun ~dir -> Memo.exec memo dir)
end

module Ml_kind = struct
  let flag t = Ml_kind.choose ~impl:(Args.A "-impl") ~intf:(A "-intf") t

  let ppx_driver_flag t =
    Ml_kind.choose ~impl:(Args.A "--impl") ~intf:(A "--intf") t
end
