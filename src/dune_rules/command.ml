open Import

module Args0 = struct
  type without_targets = [ `Others ]

  type any =
    [ `Others
    | `Targets
    ]

  type expand = dir:Path.t -> string list Action_builder.t

  type _ t =
    | A : string -> _ t
    | As : string list -> _ t
    | S : 'a t list -> 'a t
    | Concat : string * 'a t list -> 'a t
    | Dep : Path.t -> _ t
    | Deps : Path.t list -> _ t
    | Target : Path.Build.t -> [> `Targets ] t
    | Path : Path.t -> _ t
    | Paths : Path.t list -> _ t
    | Hidden_deps : Dep.Set.t -> _ t
    | Hidden_targets : Path.Build.t list -> [> `Targets ] t
    | Dyn : without_targets t Action_builder.t -> _ t
    | Expand : expand -> _ t

  let dyn args = Dyn (Action_builder.map args ~f:(fun x -> As x))
  let empty = S []

  let as_any : without_targets t -> any t = function
    | A _ as x -> (x :> any t)
    | As _ as x -> (x :> any t)
    (* We can't convince the type checker to do a cast, so we force it.
       See the discussion in https://github.com/ocaml/dune/pull/10278 to see why
       the pattern match is optimized away. *)
    | S _ as x -> Obj.magic x
    | Concat _ as x -> Obj.magic x
    | Dep _ as x -> (x :> any t)
    | Deps _ as x -> (x :> any t)
    | Path _ as x -> (x :> any t)
    | Paths _ as x -> (x :> any t)
    | Hidden_deps _ as x -> (x :> any t)
    | Dyn _ as x -> (x :> any t)
    | Expand _ as x -> (x :> any t)
  ;;
end

open Args0

let rec expand
  : type a. dir:Path.t -> a t -> string Appendable_list.t Action_builder.With_targets.t
  =
  fun ~dir t ->
  match t with
  | A s -> Appendable_list.singleton s |> Action_builder.With_targets.return
  | As l -> Appendable_list.of_list l |> Action_builder.With_targets.return
  | Dep fn ->
    Action_builder.path fn
    |> Action_builder.map ~f:(fun () ->
      Appendable_list.singleton (Path.reach fn ~from:dir))
    |> Action_builder.with_no_targets
  | Path fn ->
    Path.reach fn ~from:dir
    |> Appendable_list.singleton
    |> Action_builder.With_targets.return
  | Deps fns ->
    Action_builder.paths fns
    |> Action_builder.map ~f:(fun () ->
      Appendable_list.of_list @@ List.map fns ~f:(Path.reach ~from:dir))
    |> Action_builder.with_no_targets
  | Paths fns ->
    List.map fns ~f:(Path.reach ~from:dir)
    |> Appendable_list.of_list
    |> Action_builder.With_targets.return
  | S ts ->
    List.map ts ~f:(expand ~dir)
    |> Action_builder.With_targets.all
    |> Action_builder.With_targets.map ~f:Appendable_list.concat
  | Concat (sep, ts) ->
    expand ~dir (S ts)
    |> Action_builder.With_targets.map ~f:(fun x ->
      Appendable_list.to_list x |> String.concat ~sep |> Appendable_list.singleton)
  | Target fn ->
    Path.build fn
    |> Path.reach ~from:dir
    |> Appendable_list.singleton
    |> Action_builder.return
    |> Action_builder.with_file_targets ~file_targets:[ fn ]
  | Dyn dyn ->
    Action_builder.bind dyn ~f:(expand_no_targets ~dir) |> Action_builder.with_no_targets
  | Hidden_deps deps ->
    Action_builder.deps deps
    |> Action_builder.map ~f:(fun () -> Appendable_list.empty)
    |> Action_builder.with_no_targets
  | Hidden_targets fns ->
    Action_builder.return Appendable_list.empty
    |> Action_builder.with_file_targets ~file_targets:fns
  | Expand f ->
    f ~dir
    |> Action_builder.map ~f:Appendable_list.of_list
    |> Action_builder.with_no_targets

and expand_no_targets ~dir (t : without_targets t) =
  let { Action_builder.With_targets.build; targets } = expand ~dir t in
  assert (Targets.is_empty targets);
  build
;;

let dep_prog = function
  | Ok p -> Action_builder.path p
  | Error _ -> Action_builder.return ()
;;

let run_dyn_prog ~dir ?sandbox ?stdout_to prog args =
  Action_builder.With_targets.add
    ~file_targets:(Option.to_list stdout_to)
    (let open Action_builder.With_targets.O in
     let+ prog =
       Action_builder.with_no_targets
       @@
       let open Action_builder.O in
       let* prog = prog in
       let+ () = dep_prog prog in
       prog
     and+ args = expand ~dir (S args) in
     let action =
       let action = Action.Run (prog, Appendable_list.to_immutable_array args) in
       match stdout_to with
       | None -> action
       | Some path -> Action.with_stdout_to path action
     in
     Action.chdir dir action |> Action.Full.make ?sandbox)
;;

let run ~dir ?sandbox ?stdout_to prog args =
  run_dyn_prog ~dir ?sandbox ?stdout_to (Action_builder.return prog) args
;;

let run' ~dir prog args =
  let open Action_builder.O in
  let+ () = dep_prog prog
  and+ args = expand_no_targets ~dir (S args) in
  Action.Run (prog, Appendable_list.to_immutable_array args)
  |> Action.chdir dir
  |> Action.Full.make
;;

let quote_args =
  let rec loop quote = function
    | [] -> []
    | arg :: args -> quote :: arg :: loop quote args
  in
  fun quote args -> As (loop quote args)
;;

module Args = struct
  include Args0

  let memo t =
    let memo =
      Action_builder.create_memo
        "Command.Args.memo"
        ~input:(module Path)
        ~cutoff:(List.equal String.equal)
        (fun dir ->
           expand_no_targets ~dir t |> Action_builder.map ~f:Appendable_list.to_list)
    in
    Expand (fun ~dir -> Action_builder.exec_memo memo dir)
  ;;
end

module Ml_kind = struct
  let flag t = Ml_kind.choose ~impl:(Args.A "-impl") ~intf:(A "-intf") t
  let ppx_driver_flag t = Ml_kind.choose ~impl:(Args.A "--impl") ~intf:(A "--intf") t
end

let expand ~dir args =
  expand ~dir args |> Action_builder.With_targets.map ~f:Appendable_list.to_list
;;

let expand_no_targets ~dir t =
  expand_no_targets ~dir t |> Action_builder.map ~f:Appendable_list.to_list
;;
