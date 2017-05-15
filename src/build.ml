open Import

module Pset = Path.Set

module Vspec = struct
  type 'a t = T : Path.t * 'a Vfile_kind.t -> 'a t
end

module Prog_spec = struct
  type 'a t =
    | Dep of Path.t
    | Dyn of ('a -> Path.t)
end

type lib_dep_kind =
  | Optional
  | Required
type lib_deps = lib_dep_kind String_map.t

let merge_lib_dep_kind a b =
  match a, b with
  | Optional, Optional -> Optional
  | _ -> Required

module Repr = struct
  type ('a, 'b) t =
    | Arr : ('a -> 'b) -> ('a, 'b) t
    | Targets : Path.t list -> ('a, 'a) t
    | Store_vfile : 'a Vspec.t -> ('a, Action.t) t
    | Compose : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
    | First : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
    | Second : ('a, 'b) t -> ('c * 'a, 'c * 'b) t
    | Split : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
    | Fanout : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t
    | Paths : Pset.t -> ('a, 'a) t
    | Paths_glob : Path.t * Re.re -> ('a, 'a) t
    (* The reference gets decided in Build_interpret.deps *)
    | If_file_exists : Path.t * ('a, 'b) if_file_exists_state ref -> ('a, 'b) t
    | Contents : Path.t -> ('a, string) t
    | Lines_of : Path.t -> ('a, string list) t
    | Vpath : 'a Vspec.t -> (unit, 'a) t
    | Dyn_paths : ('a, Path.t list) t -> ('a, 'a) t
    | Record_lib_deps : Path.t * lib_deps -> ('a, 'a) t
    | Fail : fail -> (_, _) t
    | Memo : 'a memo -> (unit, 'a) t

  and 'a memo =
    { name          : string
    ; t             : (unit, 'a) t
    ; mutable state : 'a memo_state
    }

  and 'a memo_state =
    | Unevaluated
    | Evaluating
    | Evaluated of 'a

  and ('a, 'b) if_file_exists_state =
    | Undecided of ('a, 'b) t * ('a, 'b) t
    | Decided   of bool * ('a, 'b) t

  let get_if_file_exists_exn state =
    match !state with
    | Decided (_, t) -> t
    | Undecided _ -> code_errorf "Build.get_if_file_exists_exn: got undecided"
end
include Repr
let repr t = t

let merge_lib_deps a b =
  String_map.merge a b ~f:(fun _ a b ->
    match a, b with
    | None, None -> None
    | x, None | None, x -> x
    | Some a, Some b -> Some (merge_lib_dep_kind a b))

let arr f = Arr f
let return x = Arr (fun () -> x)

let record_lib_deps_simple ~dir lib_deps =
  Record_lib_deps (dir, lib_deps)

let record_lib_deps ~dir ~kind lib_deps =
  Record_lib_deps
    (dir,
     List.concat_map lib_deps ~f:(function
       | Jbuild_types.Lib_dep.Direct s -> [(s, kind)]
       | Select { choices; _ } ->
         List.concat_map choices ~f:(fun c ->
           String_set.elements c.Jbuild_types.Lib_dep.required
           |> List.map ~f:(fun d -> (d, Optional))))
     |> String_map.of_alist_reduce ~f:merge_lib_dep_kind)

module O = struct
  let ( >>> ) a b =
    match a, b with
    | Arr a, Arr b -> Arr (fun x -> (b (a x)))
    | _ -> Compose (a, b)

  let ( >>^ ) t f = t >>> arr f
  let ( ^>> ) f t = arr f >>> t

  let ( *** ) a b = Split (a, b)
  let ( &&& ) a b = Fanout (a, b)
end
open O

let first t = First t
let second t = Second t
let fanout a b = Fanout (a, b)
let fanout3 a b c =
  let open O in
  (a &&& (b &&& c))
  >>>
  arr (fun (a, (b, c)) -> (a, b, c))

let rec all = function
  | [] -> arr (fun _ -> [])
  | t :: ts ->
    t &&& all ts
    >>>
    arr (fun (x, y) -> x :: y)

let path p = Paths (Pset.singleton p)
let paths ps = Paths (Pset.of_list ps)
let path_set ps = Paths ps
let paths_glob ~dir re = Paths_glob (dir, re)
let vpath vp = Vpath vp
let dyn_paths t = Dyn_paths t

let contents p = Contents p
let lines_of p = Lines_of p

let if_file_exists p ~then_ ~else_ =
  If_file_exists (p, ref (Undecided (then_, else_)))

let file_exists p =
  if_file_exists p
    ~then_:(arr (fun _ -> true))
    ~else_:(arr (fun _ -> false))

let file_exists_opt p t =
  if_file_exists p
    ~then_:(t >>^ fun x -> Some x)
    ~else_:(arr (fun _ -> None))

let fail ?targets x =
  match targets with
  | None -> Fail x
  | Some l -> Targets l >>> Fail x

let memoize name t =
  Memo { name; t; state = Unevaluated }

let files_recursively_in ~dir ~file_tree =
  let prefix_with, dir =
    match Path.extract_build_context_dir dir with
    | None -> (Path.root, dir)
    | Some (ctx_dir, src_dir) -> (ctx_dir, src_dir)
  in
  path_set (File_tree.files_recursively_in file_tree dir ~prefix_with)

let store_vfile spec = Store_vfile spec

let get_prog (prog : _ Prog_spec.t) =
  match prog with
  | Dep p -> path p >>> arr (fun _ -> p)
  | Dyn f -> arr f >>> dyn_paths (arr (fun x -> [x]))

let prog_and_args ~dir prog args =
  Paths (Arg_spec.add_deps args Pset.empty)
  >>>
  (get_prog prog &&&
   (arr (Arg_spec.expand ~dir args)
    >>>
    dyn_paths (arr (fun (_args, deps) -> Path.Set.elements deps))
    >>>
    arr fst))

let run ~context ?(dir=context.Context.build_dir) ?stdout_to ?(extra_targets=[])
      prog args =
  let extra_targets =
    match stdout_to with
    | None -> extra_targets
    | Some fn -> fn :: extra_targets
  in
  let targets = Arg_spec.add_targets args extra_targets in
  prog_and_args ~dir prog args
  >>>
  Targets targets
  >>^  (fun (prog, args) ->
    let action : Action.Mini_shexp.t = Run (prog, args) in
    let action =
      match stdout_to with
      | None      -> action
      | Some path -> Redirect (Stdout, path, action)
    in
    { Action.
      dir
    ; context = Some context
    ; action
    })

let action ~context ?(dir=context.Context.build_dir) ~targets action =
  Targets targets
  >>^ fun () ->
  { Action. context = Some context; dir; action  }

let action_dyn ~context ?(dir=context.Context.build_dir) ~targets () =
  Targets targets
  >>^ fun action ->
  { Action. context = Some context; dir; action  }

let action_context_independent ?(dir=Path.root) ~targets action =
  Targets targets
  >>^ fun () ->
  { Action. context = None; dir; action  }

let update_file fn s =
  action_context_independent ~targets:[fn] (Update_file (fn, s))

let update_file_dyn fn =
  Targets [fn]
  >>^ fun s ->
  { Action.
    context = None
  ; dir     = Path.root
  ; action  = Update_file (fn, s)
  }

let copy ~src ~dst =
  path src >>>
  action_context_independent ~targets:[dst] (Copy (src, dst))

let symlink ~src ~dst =
  path src >>>
  action_context_independent ~targets:[dst] (Symlink (src, dst))

let create_file fn =
  action_context_independent ~targets:[fn] (Create_file fn)

let and_create_file fn =
  Targets [fn]
  >>^ fun (action : Action.t) ->
  { action with
    action = Progn [action.action; Create_file fn]
  }

(*
   {[
     let progn ts =
       all ts >>^ fun (actions : Action.t list) ->
       match actions with
       | [] ->
         { Action.
           context = None
         ; dir     = Path.root
         ; action  = Progn []
         }
       | first :: rest ->
         let rest =
           List.map rest ~f:(fun a ->
             (match first.context, a.context with
              | None, None -> ()
              | Some c1, Some c2 when c1.name = c2.name -> ()
              | _ ->
                Sexp.code_error "Build.progn"
                  [ "actions", Sexp.To_sexp.list Action.sexp_of_t actions ]);
             if first.dir = a.dir then
               a.action
             else
               Chdir (a.dir, a.action))
         in
         { first with action = Progn (first :: rest) }
   ]}
*)
