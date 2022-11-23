open Import
module Ext = Action_intf.Ext
module File_perm = File_perm
module Outputs = Outputs
module Inputs = Inputs

module type Encoder = sig
  type t

  val encode : t -> Dune_lang.t
end

module Make
    (Program : Encoder)
    (Path : Encoder)
    (Target : Encoder)
    (String : Encoder)
    (Extension : Encoder)
    (Ast : Action_intf.Ast
             with type program := Program.t
             with type path := Path.t
             with type target := Target.t
             with type string := String.t
              and type ext := Extension.t) =
struct
  include Ast

  let rec encode =
    let open Dune_lang in
    let program = Program.encode in
    let string = String.encode in
    let path = Path.encode in
    let target = Target.encode in
    function
    | Run (a, xs) -> List (atom "run" :: program a :: List.map xs ~f:string)
    | With_accepted_exit_codes (pred, t) ->
      List
        [ atom "with-accepted-exit-codes"
        ; Predicate_lang.encode Dune_lang.Encoder.int pred
        ; encode t
        ]
    | Dynamic_run (a, xs) ->
      List (atom "run_dynamic" :: program a :: List.map xs ~f:string)
    | Chdir (a, r) -> List [ atom "chdir"; path a; encode r ]
    | Setenv (k, v, r) -> List [ atom "setenv"; string k; string v; encode r ]
    | Redirect_out (outputs, fn, perm, r) ->
      List
        [ atom
            (sprintf "with-%s-to%s"
               (Outputs.to_string outputs)
               (File_perm.suffix perm))
        ; target fn
        ; encode r
        ]
    | Redirect_in (inputs, fn, r) ->
      List
        [ atom (sprintf "with-%s-from" (Inputs.to_string inputs))
        ; path fn
        ; encode r
        ]
    | Ignore (outputs, r) ->
      List [ atom (sprintf "ignore-%s" (Outputs.to_string outputs)); encode r ]
    | Progn l -> List (atom "progn" :: List.map l ~f:encode)
    | Echo xs -> List (atom "echo" :: List.map xs ~f:string)
    | Cat xs -> List (atom "cat" :: List.map xs ~f:path)
    | Copy (x, y) -> List [ atom "copy"; path x; target y ]
    | Symlink (x, y) -> List [ atom "symlink"; path x; target y ]
    | Hardlink (x, y) -> List [ atom "hardlink"; path x; target y ]
    | System x -> List [ atom "system"; string x ]
    | Bash x -> List [ atom "bash"; string x ]
    | Write_file (x, perm, y) ->
      List [ atom ("write-file" ^ File_perm.suffix perm); target x; string y ]
    | Rename (x, y) -> List [ atom "rename"; target x; target y ]
    | Remove_tree x -> List [ atom "remove-tree"; target x ]
    | Mkdir x -> List [ atom "mkdir"; target x ]
    | Diff { optional; file1; file2; mode = Binary } ->
      assert (not optional);
      List [ atom "cmp"; path file1; target file2 ]
    | Diff { optional = false; file1; file2; mode = _ } ->
      List [ atom "diff"; path file1; target file2 ]
    | Diff { optional = true; file1; file2; mode = _ } ->
      List [ atom "diff?"; path file1; target file2 ]
    | Merge_files_into (srcs, extras, into) ->
      List
        [ atom "merge-files-into"
        ; List (List.map ~f:path srcs)
        ; List (List.map ~f:string extras)
        ; target into
        ]
    | Pipe (outputs, l) ->
      List
        (atom (sprintf "pipe-%s" (Outputs.to_string outputs))
        :: List.map l ~f:encode)
    | Extension ext -> List [ atom "ext"; Extension.encode ext ]

  let run prog args = Run (prog, args)

  let chdir path t = Chdir (path, t)

  let setenv var value t = Setenv (var, value, t)

  let with_stdout_to ?(perm = File_perm.Normal) path t =
    Redirect_out (Stdout, path, perm, t)

  let with_stderr_to ?(perm = File_perm.Normal) path t =
    Redirect_out (Stderr, path, perm, t)

  let with_outputs_to ?(perm = File_perm.Normal) path t =
    Redirect_out (Outputs, path, perm, t)

  let with_stdin_from path t = Redirect_in (Stdin, path, t)

  let ignore_stdout t = Ignore (Stdout, t)

  let ignore_stderr t = Ignore (Stderr, t)

  let ignore_outputs t = Ignore (Outputs, t)

  let progn ts = Progn ts

  let echo s = Echo s

  let cat ps = Cat ps

  let copy a b = Copy (a, b)

  let symlink a b = Symlink (a, b)

  let system s = System s

  let bash s = Bash s

  let write_file ?(perm = File_perm.Normal) p s = Write_file (p, perm, s)

  let rename a b = Rename (a, b)

  let remove_tree path = Remove_tree path

  let mkdir path = Mkdir path

  let diff ?(optional = false) ?(mode = Diff.Mode.Text) file1 file2 =
    Diff { optional; file1; file2; mode }
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

    let user_message { context; program; hint; loc } =
      let hint =
        match program with
        | "refmt" -> Some (Option.value ~default:"opam install reason" hint)
        | "rescript_syntax" ->
          Some (Option.value ~default:"opam install mel" hint)
        | _ -> hint
      in
      Utils.program_not_found_message ?hint ~loc ~context program

    let raise t = raise (User_error.E (user_message t))

    let to_dyn { context; program; hint; loc = _ } =
      let open Dyn in
      record
        [ ("context", Context_name.to_dyn context)
        ; ("program", string program)
        ; ("hint", option string hint)
        ]
  end

  type t = (Path.t, Not_found.t) result

  let encode = function
    | Ok s -> Dpath.encode s
    | Error (e : Not_found.t) -> Dune_lang.Encoder.string e.program

  let to_dyn t = Result.to_dyn Path.to_dyn Not_found.to_dyn t

  let ok_exn = function
    | Ok s -> s
    | Error e -> Not_found.raise e
end

module Encode_ext = struct
  type t =
    (module Ext.Instance
       with type target = Dpath.Build.t
        and type path = Dpath.t)

  let encode = Dune_lang.Encoder.unknown
end

module type Ast =
  Action_intf.Ast
    with type program = Prog.t
    with type path = Path.t
    with type target = Path.Build.t
    with type string = String.t
     and type ext = Encode_ext.t

module rec Ast : Ast = Ast

module String_with_sexp = struct
  type t = string

  let encode = Dune_lang.Encoder.string
end

include Make (Prog) (Dpath) (Dpath.Build) (String_with_sexp) (Encode_ext) (Ast)

include Monoid.Make (struct
  type nonrec t = t

  let empty = Progn []

  let combine a b =
    match (a, b) with
    | Progn [], x | x, Progn [] -> x
    | Progn xs, Progn ys -> Progn (xs @ ys)
    | x, y -> Progn [ x; y ]
end)

type string = String.t

module For_shell = struct
  module type Ast =
    Action_intf.Ast
      with type program = string
      with type path = string
      with type target = string
      with type string = string
      with type ext = Dune_lang.t

  module rec Ast : Ast = Ast

  include
    Make (String_with_sexp) (String_with_sexp) (String_with_sexp)
      (String_with_sexp)
      (struct
        type t = Dune_lang.t

        let encode = Dune_lang.Encoder.sexp
      end)
      (Ast)
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
      Relativise.map_one_step loop t ~dir ~f_program ~f_string ~f_path ~f_target
        ~f_ext
  in
  let f_path ~dir x = Path.reach x ~from:dir in
  let f_target ~dir x = Path.reach (Path.build x) ~from:dir in
  loop t ~dir:Path.root
    ~f_string:(fun ~dir:_ x -> x)
    ~f_path ~f_target
    ~f_ext:(fun ~dir (module A) ->
      A.Spec.encode A.v
        (fun p -> Dune_lang.atom_or_quoted_string (f_path p ~dir))
        (fun p -> Dune_lang.atom_or_quoted_string (f_target p ~dir)))
    ~f_program:(fun ~dir x ->
      match x with
      | Ok p -> Path.reach p ~from:dir
      | Error e -> e.program)

let fold_one_step t ~init:acc ~f =
  match t with
  | Chdir (_, t)
  | Setenv (_, _, t)
  | Redirect_out (_, _, _, t)
  | Redirect_in (_, _, t)
  | Ignore (_, t)
  | With_accepted_exit_codes (_, t) -> f acc t
  | Progn l | Pipe (_, l) -> List.fold_left l ~init:acc ~f
  | Run _
  | Dynamic_run _
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
  | Merge_files_into _
  | Extension _ -> acc

include Action_mapper.Make (Ast) (Ast)

(* TODO change [chdir] to use [Path.Build.t] in the directory. This will be
   easier once [Action.t] is split off from the dune lang's action *)
let chdirs =
  let rec loop acc t =
    let acc =
      match t with
      | Chdir (dir, _) -> (
        match Path.as_in_build_dir dir with
        | None ->
          Code_error.raise "chdir outside the build directory"
            [ ("dir", Path.to_dyn dir) ]
        | Some dir -> Path.Build.Set.add acc dir)
      | _ -> acc
    in
    fold_one_step t ~init:acc ~f:loop
  in
  fun t -> loop Path.Build.Set.empty t

let empty = Progn []

let rec is_dynamic = function
  | Dynamic_run _ -> true
  | Chdir (_, t)
  | Setenv (_, _, t)
  | Redirect_out (_, _, _, t)
  | Redirect_in (_, _, t)
  | Ignore (_, t)
  | With_accepted_exit_codes (_, t) -> is_dynamic t
  | Progn l | Pipe (_, l) -> List.exists l ~f:is_dynamic
  | Run _
  | System _
  | Bash _
  | Echo _
  | Cat _
  | Copy _
  | Symlink _
  | Hardlink _
  | Write_file _
  | Rename _
  | Remove_tree _
  | Diff _
  | Mkdir _
  | Merge_files_into _
  | Extension _ -> false

let maybe_sandbox_path sandbox p =
  match Path.as_in_build_dir p with
  | None -> p
  | Some p -> Path.build (Sandbox.map_path sandbox p)

let sandbox t sandbox : t =
  map t ~dir:Path.root
    ~f_string:(fun ~dir:_ x -> x)
    ~f_path:(fun ~dir:_ p -> maybe_sandbox_path sandbox p)
    ~f_target:(fun ~dir:_ p -> Sandbox.map_path sandbox p)
    ~f_program:(fun ~dir:_ p -> Result.map p ~f:(maybe_sandbox_path sandbox))
    ~f_ext:(fun ~dir:_ (module A) ->
      let module A = struct
        include A

        let v =
          Spec.bimap v (maybe_sandbox_path sandbox) (Sandbox.map_path sandbox)
      end in
      (module A))

type is_useful =
  | Clearly_not
  | Maybe

let is_useful_to distribute memoize =
  let rec loop t =
    match t with
    | Chdir (_, t) -> loop t
    | Setenv (_, _, t) -> loop t
    | Redirect_out (_, _, _, t) -> memoize || loop t
    | Redirect_in (_, _, t) -> loop t
    | Ignore (_, t) | With_accepted_exit_codes (_, t) -> loop t
    | Progn l | Pipe (_, l) -> List.exists l ~f:loop
    | Echo _ -> false
    | Cat _ -> memoize
    | Copy _ -> memoize
    | Symlink _ -> false
    | Hardlink _ -> false
    | Write_file _ -> distribute
    | Rename _ -> memoize
    | Remove_tree _ -> false
    | Diff _ -> distribute
    | Mkdir _ -> false
    | Merge_files_into _ -> distribute
    | Run _ -> true
    | Dynamic_run _ -> true
    | System _ -> true
    | Bash _ -> true
    | Extension (module A) -> A.Spec.is_useful_to ~distribute ~memoize
  in
  fun t ->
    match loop t with
    | true -> Maybe
    | false -> Clearly_not

let is_useful_to_sandbox = is_useful_to false false

let is_useful_to_distribute = is_useful_to true false

let is_useful_to_memoize = is_useful_to true true

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

    let combine { action; env; locks; can_go_in_shared_cache; sandbox } x =
      { action = combine action x.action
      ; env = Env.extend_env env x.env
      ; locks = locks @ x.locks
      ; can_go_in_shared_cache =
          can_go_in_shared_cache && x.can_go_in_shared_cache
      ; sandbox = Sandbox_config.inter sandbox x.sandbox
      }
  end

  include T
  include Monoid.Make (T)

  let make ?(env = Env.empty) ?(locks = []) ?(can_go_in_shared_cache = true)
      ?(sandbox = Sandbox_config.default) action =
    { action; env; locks; can_go_in_shared_cache; sandbox }

  let map t ~f = { t with action = f t.action }

  let add_env e t = { t with env = Env.extend_env t.env e }

  let add_locks l t = { t with locks = t.locks @ l }

  let add_can_go_in_shared_cache b t =
    { t with can_go_in_shared_cache = t.can_go_in_shared_cache && b }

  let add_sandbox s t = { t with sandbox = Sandbox_config.inter t.sandbox s }
end
