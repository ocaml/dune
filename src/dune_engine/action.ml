open Import
open Stdune.Action_types
module Ext = Action_intf.Ext
include Action_intf.Exec

module Prog = struct
  module Not_found = struct
    type t =
      { context : Context_name.t
      ; program : Filename.t
      ; hint : string option
      ; loc : Loc.t option
      }

    let create ?hint ~context ~program ~loc () = { hint; context; program; loc }
    let program t = t.program

    let raise { context; program; hint; loc } =
      Utils.program_not_found ?hint ~loc ~context (Filename.to_string program)
    ;;

    let to_dyn { context; program; hint; loc = _ } =
      let open Dyn in
      record
        [ "context", Context_name.to_dyn context
        ; "program", Filename.to_dyn program
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

type t =
  | Run of
      { prog : Prog.t
      ; args : string Appendable_list.t
      ; can_run_in_action_runner : bool
      }
  | With_accepted_exit_codes of int Predicate_lang.t * t
  | Chdir of Path.t * t
  | Setenv of string * string * t
  | Redirect_out of Outputs.t * Path.Build.t * File_perm.t * t
  | Redirect_in of Inputs.t * Path.t * t
  | Ignore of Outputs.t * t
  | Progn of t list
  | Concurrent of t list
  | Echo of string list
  | Cat of Path.t list
  | Copy of Path.t * Path.Build.t
  | Symlink of Path.t * Path.Build.t
  | Hardlink of Path.t * Path.Build.t
  | System of string
  | Bash of
      { script : string
      ; can_run_in_action_runner : bool
      }
  | Write_file of Path.Build.t * File_perm.t * string
  | Rename of Path.Build.t * Path.Build.t
  | Remove_tree of Path.Build.t
  | Mkdir of Path.Build.t
  | Pipe of Outputs.t * t list
  | Diff of (Path.t, Path.Build.t) Diff.t
  | Extension of
      (module Ext.Instance with type target = Path.Build.t and type path = Path.t)

let run prog args =
  Run { prog; args = Appendable_list.of_list args; can_run_in_action_runner = true }
;;

let chdir path t = Chdir (path, t)
let with_stdout_to ?(perm = File_perm.Normal) path t = Redirect_out (Stdout, path, perm, t)

let with_outputs_to ?(perm = File_perm.Normal) path t =
  Redirect_out (Outputs, path, perm, t)
;;

let progn ts = Progn ts
let concurrent ts = Concurrent ts
let echo s = Echo s
let cat ps = Cat ps
let copy a b = Copy (a, b)
let write_file ?(perm = File_perm.Normal) p s = Write_file (p, perm, s)
let remove_tree path = Remove_tree path
let mkdir path = Mkdir path

let diff
      ?(optional = false)
      ?(mode = Diff.Mode.Text)
      ?(directory_diffs = true)
      file1
      file2
  =
  Diff { optional; mode; directory_diffs; file1; file2 }
;;

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

let digest =
  let open Dune_digest.Manual in
  let digest_outputs d outputs = repr d Outputs.repr outputs in
  let digest_inputs d inputs = repr d Inputs.repr inputs in
  let digest_file_perm d perm = repr d File_perm.repr perm in
  let digest_mode d mode = repr d Diff.Mode.repr mode in
  let digest_program d ~dir (program : Prog.t) =
    match program with
    | Ok p -> string d (Path.reach p ~from:dir)
    | Error e -> string d (Filename.to_string e.program)
  in
  let digest_path d ~dir path = string d (Path.reach path ~from:dir) in
  let digest_target d ~dir target = string d (Path.reach (Path.build target) ~from:dir) in
  let digest_ext
        d
        ~dir
        (module A : Ext.Instance with type target = Path.Build.t and type path = Path.t)
    =
    repr
      d
      Sexp.repr
      (A.Spec.encode
         A.v
         (fun p -> Sexp.Atom (Path.reach p ~from:dir))
         (fun p -> Sexp.Atom (Path.reach (Path.build p) ~from:dir)))
  in
  let rec loop d t ~dir =
    match t with
    | Run { prog; args; can_run_in_action_runner = _ } ->
      int d 0;
      digest_program d ~dir prog;
      int d (Appendable_list.length args);
      Appendable_list.iter args ~f:(string d)
    | With_accepted_exit_codes (pred, t) ->
      int d 1;
      repr d (Predicate_lang.repr Repr.int) pred;
      loop d t ~dir
    | Chdir (path, t) ->
      int d 2;
      digest_path d ~dir path;
      loop d t ~dir:path
    | Setenv (var, value, t) ->
      int d 3;
      string d var;
      string d value;
      loop d t ~dir
    | Redirect_out (outputs, target, perm, t) ->
      int d 4;
      digest_outputs d outputs;
      digest_target d ~dir target;
      digest_file_perm d perm;
      loop d t ~dir
    | Redirect_in (inputs, path, t) ->
      int d 5;
      digest_inputs d inputs;
      digest_path d ~dir path;
      loop d t ~dir
    | Ignore (outputs, t) ->
      int d 6;
      digest_outputs d outputs;
      loop d t ~dir
    | Progn ts ->
      int d 7;
      list d ts ~f:(fun d t -> loop d t ~dir)
    | Concurrent ts ->
      int d 8;
      list d ts ~f:(fun d t -> loop d t ~dir)
    | Echo xs ->
      int d 9;
      list d xs ~f:string
    | Cat paths ->
      int d 10;
      list d paths ~f:(fun d path -> digest_path d ~dir path)
    | Copy (src, dst) ->
      int d 11;
      digest_path d ~dir src;
      digest_target d ~dir dst
    | Symlink (src, dst) ->
      int d 12;
      let src =
        match Path.Build.parent dst with
        | None -> Path.to_string src
        | Some from -> Path.reach ~from:(Path.build from) src
      in
      string d src;
      digest_target d ~dir dst
    | Hardlink (src, dst) ->
      int d 13;
      digest_path d ~dir src;
      digest_target d ~dir dst
    | Bash { script; can_run_in_action_runner = _ } ->
      int d 14;
      string d script
    | Write_file (target, perm, contents) ->
      int d 15;
      digest_target d ~dir target;
      digest_file_perm d perm;
      string d contents
    | Rename (src, dst) ->
      int d 16;
      digest_target d ~dir src;
      digest_target d ~dir dst
    | Remove_tree target ->
      int d 17;
      digest_target d ~dir target
    | Mkdir target ->
      int d 18;
      digest_target d ~dir target
    | Pipe (outputs, ts) ->
      int d 19;
      digest_outputs d outputs;
      list d ts ~f:(fun d t -> loop d t ~dir)
    | Diff { optional; mode; directory_diffs; file1; file2 } ->
      int d 20;
      bool d optional;
      digest_mode d mode;
      bool d directory_diffs;
      digest_path d ~dir file1;
      digest_target d ~dir file2
    | Extension ext ->
      int d 21;
      digest_ext d ~dir ext
    | System command ->
      int d 22;
      string d command
  in
  fun d t -> loop d t ~dir:Path.root
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
  | Echo _
  | Cat _
  | Copy _
  | Symlink _
  | Hardlink _
  | System _
  | Bash _
  | Write_file _
  | Rename _
  | Remove_tree _
  | Mkdir _
  | Diff _
  | Extension _ -> acc
;;

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

let exists t ~leaf ~extension =
  let rec loop = function
    | Chdir (_, t)
    | Setenv (_, _, t)
    | Redirect_out (_, _, _, t)
    | Redirect_in (_, _, t)
    | Ignore (_, t)
    | With_accepted_exit_codes (_, t) -> loop t
    | Progn l | Pipe (_, l) | Concurrent l -> List.exists l ~f:loop
    | Extension extension_ -> extension extension_
    | t -> leaf t
  in
  loop t
;;

let is_dynamic t =
  exists t ~leaf:(fun _ -> false) ~extension:(fun (module A) -> A.Spec.is_dynamic)
;;

let runs_process t =
  exists
    t
    ~leaf:(function
      | Run _ | System _ | Bash _ | Diff _ -> true
      | _ -> false)
    ~extension:(fun (module A) -> A.Spec.runs_process)
;;

let sandbox t sandbox_dir =
  let target p = Path.Build.append sandbox_dir p in
  let path p =
    match Path.as_in_build_dir p with
    | None -> p
    | Some p -> Path.build (target p)
  in
  let rec loop = function
    | Run ({ prog; _ } as run) -> Run { run with prog = Result.map prog ~f:path }
    | With_accepted_exit_codes (pred, t) -> With_accepted_exit_codes (pred, loop t)
    | Chdir (dir, t) -> Chdir (path dir, loop t)
    | Setenv (var, value, t) -> Setenv (var, value, loop t)
    | Redirect_out (outputs, fn, perm, t) ->
      Redirect_out (outputs, target fn, perm, loop t)
    | Redirect_in (inputs, fn, t) -> Redirect_in (inputs, path fn, loop t)
    | Ignore (outputs, t) -> Ignore (outputs, loop t)
    | Progn l -> Progn (List.map l ~f:loop)
    | Concurrent l -> Concurrent (List.map l ~f:loop)
    | (Echo _ | System _ | Bash _) as t -> t
    | Cat xs -> Cat (List.map xs ~f:path)
    | Copy (src, dst) -> Copy (path src, target dst)
    | Symlink (src, dst) -> Symlink (path src, target dst)
    | Hardlink (src, dst) -> Hardlink (path src, target dst)
    | Write_file (fn, perm, contents) -> Write_file (target fn, perm, contents)
    | Rename (src, dst) -> Rename (target src, target dst)
    | Remove_tree dir -> Remove_tree (target dir)
    | Mkdir dir -> Mkdir (target dir)
    | Pipe (outputs, l) -> Pipe (outputs, List.map l ~f:loop)
    | Diff ({ file1; file2; _ } as diff) ->
      Diff { diff with file1 = path file1; file2 = target file2 }
    | Extension (module A) ->
      Extension
        (module struct
          include A

          let v = Spec.bimap v path target
        end)
  in
  loop t
;;

type is_useful =
  | Clearly_not
  | Maybe

let is_useful_to_memoize t =
  let rec loop t =
    match t with
    | Chdir (_, t) -> loop t
    | Setenv (_, _, t) -> loop t
    | Redirect_out _ -> true
    | Redirect_in (_, _, t) -> loop t
    | Ignore (_, t) | With_accepted_exit_codes (_, t) -> loop t
    | Progn l | Pipe (_, l) | Concurrent l -> List.exists l ~f:loop
    | Echo _ -> false
    | Cat _ -> true
    | Copy _ -> true
    | Symlink _ -> false
    | Hardlink _ -> false
    | Write_file _ -> true
    | Rename _ -> true
    | Remove_tree _ -> false
    | Diff _ -> false
    | Mkdir _ -> false
    | Run _ -> true
    | System _ -> true
    | Bash _ -> true
    | Extension (module A) -> A.Spec.is_useful_to_memoize
  in
  match loop t with
  | true -> Maybe
  | false -> Clearly_not
;;

module Full = struct
  type action = t

  module Props = struct
    type t =
      { env : Env.t
      ; locks : Path.Set.t
      ; can_go_in_shared_cache : bool
      ; can_use_sandbox_policy : bool
      ; sandbox : Sandbox_config.t
      ; corrections : Corrections.t option
      ; job_slots : action option
      }

    let empty =
      { env = Env.empty
      ; locks = Path.Set.empty
      ; can_go_in_shared_cache = true
      ; can_use_sandbox_policy = true
      ; sandbox = Sandbox_config.default
      ; corrections = None
      ; job_slots = None
      }
    ;;

    let combine_corrections (x : Corrections.t option) (y : Corrections.t option) =
      match x, y with
      | None, x -> x
      | x, None -> x
      | Some Produce, Some Produce -> Some Produce
      | Some Ignore, Some Ignore -> Some Ignore
      | Some x, Some y ->
        Code_error.raise
          "incompatible corrections settings for action"
          [ "x", Corrections.to_dyn x; "y", Corrections.to_dyn y ]
    ;;

    let combine_job_slots x y =
      match x, y with
      | None, x -> x
      | x, None -> x
      | Some _, Some _ ->
        Code_error.raise "an action can only have one job_slots command" []
    ;;

    let combine
          { env
          ; locks
          ; can_go_in_shared_cache
          ; can_use_sandbox_policy
          ; sandbox
          ; corrections
          ; job_slots
          }
          t
      =
      { env = Env.extend_env env t.env
      ; locks = Path.Set.union locks t.locks
      ; can_go_in_shared_cache = can_go_in_shared_cache && t.can_go_in_shared_cache
      ; can_use_sandbox_policy = can_use_sandbox_policy && t.can_use_sandbox_policy
      ; sandbox = Sandbox_config.inter sandbox t.sandbox
      ; corrections = combine_corrections corrections t.corrections
      ; job_slots = combine_job_slots job_slots t.job_slots
      }
    ;;

    let make ~env ~locks ~can_go_in_shared_cache ~sandbox ~corrections =
      { env
      ; locks = Path.Set.of_list locks
      ; can_go_in_shared_cache
      ; can_use_sandbox_policy = true
      ; sandbox
      ; corrections
      ; job_slots = None
      }
    ;;

    let add_env t env = { t with env = Env.extend_env t.env env }

    let add_locks t locks =
      { t with locks = Path.Set.union t.locks (Path.Set.of_list locks) }
    ;;

    let add_can_go_in_shared_cache t can_go_in_shared_cache =
      { t with
        can_go_in_shared_cache = t.can_go_in_shared_cache && can_go_in_shared_cache
      }
    ;;

    let disable_sandbox_policy t = { t with can_use_sandbox_policy = false }

    let add_sandbox t sandbox =
      { t with sandbox = Sandbox_config.inter t.sandbox sandbox }
    ;;

    let add_corrections t corrections =
      { t with corrections = combine_corrections (Some corrections) t.corrections }
    ;;

    let add_job_slots t job_slots =
      { t with job_slots = combine_job_slots (Some job_slots) t.job_slots }
    ;;
  end

  module T = struct
    type nonrec t =
      { action : t
      ; props : Props.t
      }

    let empty = { action = Progn []; props = Props.empty }

    let combine { action; props } t =
      { action = combine action t.action; props = Props.combine props t.props }
    ;;
  end

  include T
  include Monoid.Make (T)

  let make
        ?(env = Env.empty)
        ?(locks = [])
        ?(can_go_in_shared_cache = !Clflags.can_go_in_shared_cache_default)
        ?(sandbox = Sandbox_config.default)
        ?corrections
        action
    =
    let props = Props.make ~env ~locks ~can_go_in_shared_cache ~sandbox ~corrections in
    { action; props }
  ;;

  let map t ~f = { t with action = f t.action }

  let add_env env t =
    if Env.is_empty env then t else { t with props = Props.add_env t.props env }
  ;;

  let add_locks locks t = { t with props = Props.add_locks t.props locks }

  let add_can_go_in_shared_cache can_go_in_shared_cache t =
    { t with props = Props.add_can_go_in_shared_cache t.props can_go_in_shared_cache }
  ;;

  let disable_sandbox_policy t = { t with props = Props.disable_sandbox_policy t.props }
  let add_sandbox sandbox t = { t with props = Props.add_sandbox t.props sandbox }

  let add_corrections corrections t =
    { t with props = Props.add_corrections t.props corrections }
  ;;

  let add_job_slots job_slots t = { t with props = Props.add_job_slots t.props job_slots }
end
