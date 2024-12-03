open Import
include Dune_util.Action
module Ext = Action_intf.Ext

module type T = sig
  type t
end

include Action_intf.Exec

module Make
    (Program : T)
    (Path : T)
    (Target : T)
    (String : T)
    (Extension : T)
    (Ast : Action_intf.Ast
           with type program := Program.t
           with type path := Path.t
           with type target := Target.t
           with type string := String.t
            and type ext := Extension.t) =
struct
  include Ast

  let run prog args = Run (prog, Array.Immutable.of_list args)
  let chdir path t = Chdir (path, t)
  let setenv var value t = Setenv (var, value, t)

  let with_stdout_to ?(perm = File_perm.Normal) path t =
    Redirect_out (Stdout, path, perm, t)
  ;;

  let with_stderr_to ?(perm = File_perm.Normal) path t =
    Redirect_out (Stderr, path, perm, t)
  ;;

  let with_outputs_to ?(perm = File_perm.Normal) path t =
    Redirect_out (Outputs, path, perm, t)
  ;;

  let with_stdin_from path t = Redirect_in (Stdin, path, t)
  let ignore_stdout t = Ignore (Stdout, t)
  let ignore_stderr t = Ignore (Stderr, t)
  let ignore_outputs t = Ignore (Outputs, t)
  let progn ts = Progn ts
  let concurrent ts = Concurrent ts
  let echo s = Echo s
  let cat ps = Cat ps
  let copy a b = Copy (a, b)
  let symlink a b = Symlink (a, b)
  let bash s = Bash s
  let write_file ?(perm = File_perm.Normal) p s = Write_file (p, perm, s)
  let rename a b = Rename (a, b)
  let remove_tree path = Remove_tree path
  let mkdir path = Mkdir path
  let needed_deps xs = Needed_deps xs
end

module Prog = struct
  module Not_found = struct
    type t =
      { context : Context_name.t
      ; program : string
      ; hint : string option
      ; loc : Loc.t option
      }

    let create ?hint ~context ~program ~loc () = { hint; context; program; loc }

    let raise { context; program; hint; loc } =
      raise (User_error.E (Utils.program_not_found_message ?hint ~loc ~context program))
    ;;

    let to_dyn { context; program; hint; loc = _ } =
      let open Dyn in
      record
        [ "context", Context_name.to_dyn context
        ; "program", string program
        ; "hint", option string hint
        ]
    ;;
  end

  type t = (Path.t, Not_found.t) result

  let to_dyn t = Result.to_dyn Path.to_dyn Not_found.to_dyn t

  let ok_exn = function
    | Ok s -> s
    | Error e -> Not_found.raise e
  ;;
end

module Encode_ext = struct
  type t =
    (module Ext.Instance
       with type target = Stdune.Path.Build.t
        and type path = Stdune.Path.t)
end

module type Ast =
  Action_intf.Ast
  with type program = Prog.t
  with type path = Path.t
  with type target = Path.Build.t
  with type string = String.t
   and type ext = Encode_ext.t

module rec Ast : Ast = Ast
include Make (Prog) (Stdune.Path) (Stdune.Path.Build) (String) (Encode_ext) (Ast)

include Monoid.Make (struct
    type nonrec t = t

    let empty = Progn []

    let combine a b =
      match a, b with
      | Progn [], x | x, Progn [] -> x
      | Progn xs, Progn ys -> Progn (xs @ ys)
      | x, y -> Progn [ x; y ]
    ;;
  end)

type string = String.t

module For_shell = struct
  module type Ast =
    Action_intf.Ast
    with type program = string
    with type path = string
    with type target = string
    with type string = string
    with type ext = Sexp.t

  module rec Ast : Ast = Ast
  include Make (String) (String) (String) (String) (Sexp) (Ast)
end

module Relativise = Action_mapper.Make (Ast) (For_shell.Ast)

let for_shell t =
  let rec loop t ~dir ~f_program ~f_string ~f_path ~f_target ~f_ext =
    match t with
    | Symlink (src, dst) ->
      let src =
        match Path.Build.parent dst with
        | None -> Path.to_string src
        | Some from -> Path.reach ~from:(Path.build from) src
      in
      let dst = Path.reach ~from:dir (Path.build dst) in
      For_shell.Symlink (src, dst)
    | t ->
      Relativise.map_one_step loop t ~dir ~f_program ~f_string ~f_path ~f_target ~f_ext
  in
  let f_path ~dir x = Path.reach x ~from:dir in
  let f_target ~dir x = Path.reach (Path.build x) ~from:dir in
  loop
    t
    ~dir:Path.root
    ~f_string:(fun ~dir:_ x -> x)
    ~f_path
    ~f_target
    ~f_ext:(fun ~dir (module A) ->
      A.Spec.encode
        A.v
        (fun p -> Sexp.Atom (f_path p ~dir))
        (fun p -> Sexp.Atom (f_target p ~dir)))
    ~f_program:(fun ~dir x ->
      match x with
      | Ok p -> Path.reach p ~from:dir
      | Error e -> e.program)
;;

let fold_one_step t ~init:acc ~f =
  match t with
  | Chdir (_, t)
  | Setenv (_, _, t)
  | Redirect_out (_, _, _, t)
  | Redirect_in (_, _, t)
  | Ignore (_, t)
  | With_accepted_exit_codes (_, t) -> f acc t
  | Progn l | Pipe (_, l) | Concurrent l -> List.fold_left l ~init:acc ~f
  | Run _
  | Dynamic_run _
  | Echo _
  | Cat _
  | Copy _
  | Symlink _
  | Hardlink _
  | Bash _
  | Write_file _
  | Rename _
  | Remove_tree _
  | Mkdir _
  | Extension _
  | Needed_deps _ -> acc
;;

include Action_mapper.Make (Ast) (Ast)

(* TODO change [chdir] to use [Path.Build.t] in the directory. This will be
   easier once [Action.t] is split off from the dune lang's action *)
let chdirs =
  let rec loop acc t =
    let acc =
      match t with
      | Chdir (dir, _) ->
        (match Path.as_in_build_dir dir with
         | None ->
           Code_error.raise "chdir outside the build directory" [ "dir", Path.to_dyn dir ]
         | Some dir -> Path.Build.Set.add acc dir)
      | _ -> acc
    in
    fold_one_step t ~init:acc ~f:loop
  in
  fun t -> loop Path.Build.Set.empty t
;;

let empty = Progn []

let rec is_dynamic = function
  | Dynamic_run _ -> true
  | Chdir (_, t)
  | Setenv (_, _, t)
  | Redirect_out (_, _, _, t)
  | Redirect_in (_, _, t)
  | Ignore (_, t)
  | With_accepted_exit_codes (_, t) -> is_dynamic t
  | Progn l | Pipe (_, l) | Concurrent l -> List.exists l ~f:is_dynamic
  | Run _
  | Bash _
  | Echo _
  | Cat _
  | Copy _
  | Symlink _
  | Hardlink _
  | Write_file _
  | Rename _
  | Remove_tree _
  | Mkdir _
  | Extension _
  | Needed_deps _ -> false
;;

let maybe_sandbox_path sandbox p =
  match Path.as_in_build_dir p with
  | None -> p
  | Some p -> Path.build (Sandbox.map_path sandbox p)
;;

let sandbox t sandbox : t =
  map
    t
    ~dir:Path.root
    ~f_string:(fun ~dir:_ x -> x)
    ~f_path:(fun ~dir:_ p -> maybe_sandbox_path sandbox p)
    ~f_target:(fun ~dir:_ p -> Sandbox.map_path sandbox p)
    ~f_program:(fun ~dir:_ p -> Result.map p ~f:(maybe_sandbox_path sandbox))
    ~f_ext:(fun ~dir:_ (module A) ->
      let module A = struct
        include A

        let v = Spec.bimap v (maybe_sandbox_path sandbox) (Sandbox.map_path sandbox)
      end
      in
      (module A))
;;

type is_useful =
  | Clearly_not
  | Maybe

let is_useful_to memoize =
  let rec loop t =
    match t with
    | Chdir (_, t) -> loop t
    | Setenv (_, _, t) -> loop t
    | Redirect_out (_, _, _, t) -> memoize || loop t
    | Redirect_in (_, _, t) -> loop t
    | Ignore (_, t) | With_accepted_exit_codes (_, t) -> loop t
    | Progn l | Pipe (_, l) | Concurrent l -> List.exists l ~f:loop
    | Echo _ -> false
    | Cat _ -> memoize
    | Copy _ -> memoize
    | Symlink _ -> false
    | Hardlink _ -> false
    | Write_file _ -> true
    | Rename _ -> memoize
    | Remove_tree _ -> false
    | Mkdir _ -> false
    | Run _ -> true
    | Dynamic_run _ -> true
    | Bash _ -> true
    | Extension (module A) -> A.Spec.is_useful_to ~memoize
    | Needed_deps _ -> false
  in
  fun t ->
    match loop t with
    | true -> Maybe
    | false -> Clearly_not
;;

let is_useful_to_distribute = is_useful_to false
let is_useful_to_memoize = is_useful_to true

module Full = struct
  module T = struct
    type nonrec t =
      { action : t
      ; env : Env.t
      ; locks : Path.t list
      ; can_go_in_shared_cache : bool
      ; sandbox : Sandbox_config.t
      }

    let empty =
      { action = Progn []
      ; env = Env.empty
      ; locks = []
      ; can_go_in_shared_cache = true
      ; sandbox = Sandbox_config.default
      }
    ;;

    let combine { action; env; locks; can_go_in_shared_cache; sandbox } x =
      { action = combine action x.action
      ; env = Env.extend_env env x.env
      ; locks = locks @ x.locks
      ; can_go_in_shared_cache = can_go_in_shared_cache && x.can_go_in_shared_cache
      ; sandbox = Sandbox_config.inter sandbox x.sandbox
      }
    ;;
  end

  include T
  include Monoid.Make (T)

  let make
    ?(env = Env.empty)
    ?(locks = [])
    ?(can_go_in_shared_cache = !Clflags.can_go_in_shared_cache_default)
    ?(sandbox = Sandbox_config.default)
    action
    =
    { action; env; locks; can_go_in_shared_cache; sandbox }
  ;;

  let map t ~f = { t with action = f t.action }
  let add_env e t = { t with env = Env.extend_env t.env e }
  let add_locks l t = { t with locks = t.locks @ l }

  let add_can_go_in_shared_cache b t =
    { t with can_go_in_shared_cache = t.can_go_in_shared_cache && b }
  ;;

  let add_sandbox s t = { t with sandbox = Sandbox_config.inter t.sandbox s }
end
