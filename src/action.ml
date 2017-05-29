open Import
open Sexp.Of_sexp

module Env_var_map = Context.Env_var_map

type var_expansion =
  | Not_found
  | Path  of Path.t
  | Paths of Path.t list
  | Str   of string

let expand_str ~dir ~f template =
  String_with_vars.expand template ~f:(fun var ->
    match f var with
    | Not_found -> None
    | Path path -> Some (Path.reach ~from:dir path)
    | Paths l -> Some (List.map l ~f:(Path.reach ~from:dir) |> String.concat ~sep:" ")
    | Str s -> Some s)

let expand_path ~dir ~f template =
  match String_with_vars.just_a_var template with
  | None -> expand_str ~dir ~f template |> Path.relative dir
  | Some v ->
    match f v with
    | Not_found -> expand_str ~dir ~f template |> Path.relative dir
    | Path p
    | Paths [p] -> p
    | Str s -> Path.relative dir s
    | Paths l ->
      List.map l ~f:(Path.reach ~from:dir)
      |> String.concat ~sep:" "
      |> Path.relative dir

let expand_prog ctx ~dir ~f template =
  let resolve s =
    if String.contains s '/' then
      Path.relative dir s
    else
      match Context.which ctx s with
      | Some p -> p
      | None -> Utils.program_not_found ~context:ctx.name s
  in
  match String_with_vars.just_a_var template with
  | None -> resolve (expand_str ~dir ~f template)
  | Some v ->
    match f v with
    | Not_found -> resolve (expand_str ~dir ~f template)
    | Path p
    | Paths [p] -> p
    | Str s -> resolve s
    | Paths l ->
      List.map l ~f:(Path.reach ~from:dir)
      |> String.concat ~sep:" "
      |> resolve

module Outputs = struct
  include Action_intf.Outputs

  let to_string = function
    | Stdout -> "stdout"
    | Stderr -> "stderr"
    | Outputs -> "outputs"
end

module type Sexpable = sig
  type t
  val t : t Sexp.Of_sexp.t
  val sexp_of_t : t Sexp.To_sexp.t
end

module Make_ast
    (Path   : Sexpable)
    (String : Sexpable)
    (Ast : Action_intf.Ast
     with type path   := Path.t
     with type string := String.t) =
struct
  include Ast

  let rec t sexp =
    let path = Path.t and string = String.t in
    sum
      [ cstr_rest "run" (path @> nil) string             (fun prog args -> Run (prog, args))
      ; cstr "chdir"    (path @> t @> nil)        (fun dn t -> Chdir (dn, t))
      ; cstr "setenv"   (string @> string @> t @> nil)   (fun k v t -> Setenv (k, v, t))
      ; cstr "with-stdout-to"  (path @> t @> nil) (fun fn t -> Redirect (Stdout, fn, t))
      ; cstr "with-stderr-to"  (path @> t @> nil) (fun fn t -> Redirect (Stderr, fn, t))
      ; cstr "with-outputs-to" (path @> t @> nil) (fun fn t -> Redirect (Outputs, fn, t))
      ; cstr "ignore-stdout"   (t @> nil)      (fun t -> Ignore (Stdout, t))
      ; cstr "ignore-stderr"   (t @> nil)      (fun t -> Ignore (Stderr, t))
      ; cstr "ignore-outputs"  (t @> nil)      (fun t -> Ignore (Outputs, t))
      ; cstr_rest "progn"      nil t         (fun l -> Progn l)
      ; cstr "echo"           (string @> nil)         (fun x -> Echo x)
      ; cstr "cat"            (path @> nil)         (fun x -> Cat x)
      ; cstr "create-file"    (path @> nil)         (fun x -> Create_file x)
      ; cstr "copy" (path @> path @> nil)              (fun src dst -> Copy (src, dst))
      (*
         (* We don't expose symlink to the user yet since this might complicate things *)
         ; cstr "symlink" (a @> a @> nil) (fun src dst -> Symlink (dst, Cat src))
      *)
      ; cstr "copy-and-add-line-directive" (path @> path @> nil) (fun src dst ->
          Copy_and_add_line_directive (src, dst))
      ; cstr "system" (string @> nil) (fun cmd -> System cmd)
      ; cstr "bash"   (string @> nil) (fun cmd -> Bash   cmd)
      ]
      sexp

  let rec sexp_of_t : _ -> Sexp.t =
    let path = Path.sexp_of_t and string = String.sexp_of_t in
    function
    | Run (a, xs) -> List (Atom "run" :: path a :: List.map xs ~f:string)
    | Chdir (a, r) -> List [Atom "chdir" ; path a ; sexp_of_t r]
    | Setenv (k, v, r) -> List [Atom "setenv" ; string k ; string v ; sexp_of_t r]
    | Redirect (outputs, fn, r) ->
      List [ Atom (sprintf "with-%s-to" (Outputs.to_string outputs))
           ; path fn
           ; sexp_of_t r
           ]
    | Ignore (outputs, r) ->
      List [ Atom (sprintf "ignore-%s" (Outputs.to_string outputs))
           ; sexp_of_t r
           ]
    | Progn l -> List (Atom "progn" :: List.map l ~f:sexp_of_t)
    | Echo x -> List [Atom "echo"; string x]
    | Cat x -> List [Atom "cat"; path x]
    | Create_file x -> List [Atom "create-file"; path x]
    | Copy (x, y) ->
      List [Atom "copy"; path x; path y]
    | Symlink (x, y) ->
      List [Atom "symlink"; path x; path y]
    | Copy_and_add_line_directive (x, y) ->
      List [Atom "copy-and-add-line-directive"; path x; path y]
    | System x -> List [Atom "system"; string x]
    | Bash   x -> List [Atom "bash"; string x]
    | Update_file (x, y) -> List [Atom "update-file"; path x; string y]
    | Rename (x, y) -> List [Atom "rename"; path x; path y]
    | Remove_tree x -> List [Atom "remove-tree"; path x]
    | Mkdir x       -> List [Atom "mkdir"; path x]
end

module type Ast = Action_intf.Ast
  with type path   := Path.t
  with type string := String.t
module rec Ast : Ast = Ast

include Make_ast
    (Path)
    (struct
      type t = string
      let t = Sexp.Of_sexp.string
      let sexp_of_t = Sexp.To_sexp.string
    end)
    (Ast)

type action = t

module Unexpanded = struct
  module type Ast = Action_intf.Ast
    with type path   := String_with_vars.t
    with type string := String_with_vars.t
  module rec Ast : Ast = Ast

  include Make_ast(String_with_vars)(String_with_vars)(Ast)

  let t sexp =
    match sexp with
    | Atom _ ->
      of_sexp_errorf sexp
        "if you meant for this to be executed with bash, write (bash \"...\") instead"
    | List _ -> t sexp

  let rec fold t ~init:acc ~f =
    match t with
    | Run (prog, args) -> List.fold_left args ~init:(f acc prog) ~f
    | Chdir (fn, t) -> fold t ~init:(f acc fn) ~f
    | Setenv (var, value, t) -> fold t ~init:(f (f acc var) value) ~f
    | Redirect (_, fn, t) -> fold t ~init:(f acc fn) ~f
    | Ignore (_, t) -> fold t ~init:acc ~f
    | Progn l -> List.fold_left l ~init:acc ~f:(fun init t -> fold t ~init ~f)
    | Echo x -> f acc x
    | Cat x -> f acc x
    | Create_file x -> f acc x
    | Copy (x, y) -> f (f acc x) y
    | Symlink (x, y) -> f (f acc x) y
    | Copy_and_add_line_directive (x, y) -> f (f acc x) y
    | System x -> f acc x
    | Bash x -> f acc x
    | Update_file (x, y) -> f (f acc x) y
    | Rename (x, y) -> f (f acc x) y
    | Remove_tree x
    | Mkdir x -> f acc x

  let fold_vars t ~init ~f =
    fold t ~init ~f:(fun acc pat ->
      String_with_vars.fold ~init:acc pat ~f)

  let rec expand ctx dir t ~f : action =
    match t with
    | Run (prog, args) ->
      Run (expand_prog ctx ~dir ~f prog,
           List.map args ~f:(fun arg -> expand_str ~dir ~f arg))
    | Chdir (fn, t) ->
      let fn = expand_path ~dir ~f fn in
      Chdir (fn, expand ctx fn t ~f)
    | Setenv (var, value, t) ->
      Setenv (expand_str ~dir ~f var, expand_str ~dir ~f value,
              expand ctx dir t ~f)
    | Redirect (outputs, fn, t) ->
      Redirect (outputs, expand_path ~dir ~f fn, expand ctx dir t ~f)
    | Ignore (outputs, t) ->
      Ignore (outputs, expand ctx dir t ~f)
    | Progn l -> Progn (List.map l ~f:(fun t -> expand ctx dir t ~f))
    | Echo x -> Echo (expand_str ~dir ~f x)
    | Cat x -> Cat (expand_path ~dir ~f x)
    | Create_file x -> Create_file (expand_path ~dir ~f x)
    | Copy (x, y) ->
      Copy (expand_path ~dir ~f x, expand_path ~dir ~f y)
    | Symlink (x, y) ->
      Symlink (expand_path ~dir ~f x, expand_path ~dir ~f y)
    | Copy_and_add_line_directive (x, y) ->
      Copy_and_add_line_directive (expand_path ~dir ~f x, expand_path ~dir ~f y)
    | System x -> System (expand_str ~dir ~f x)
    | Bash x -> Bash (expand_str ~dir ~f x)
    | Update_file (x, y) -> Update_file (expand_path ~dir ~f x, expand_str ~dir ~f y)
    | Rename (x, y) ->
      Rename (expand_path ~dir ~f x, expand_path ~dir ~f y)
    | Remove_tree x ->
      Remove_tree (expand_path ~dir ~f x)
    | Mkdir x ->
      Mkdir (expand_path ~dir ~f x)
end

let fold_one_step t ~init:acc ~f =
  match t with
  | Chdir (_, t)
  | Setenv (_, _, t)
  | Redirect (_, _, t)
  | Ignore (_, t) -> f acc t
  | Progn l -> List.fold_left l ~init:acc ~f
  | Run _
  | Echo _
  | Cat _
  | Create_file _
  | Copy _
  | Symlink _
  | Copy_and_add_line_directive _
  | System _
  | Bash _
  | Update_file _
  | Rename _
  | Remove_tree _
  | Mkdir _ -> acc

let rec map t ~fs ~fp =
    match t with
    | Run (prog, args) ->
      Run (fp prog, List.map args ~f:fs)
    | Chdir (fn, t) ->
      Chdir (fp fn, map t ~fs ~fp)
    | Setenv (var, value, t) ->
      Setenv (fs var, fs value, map t ~fs ~fp)
    | Redirect (outputs, fn, t) ->
      Redirect (outputs, fp fn, map t ~fs ~fp)
    | Ignore (outputs, t) ->
      Ignore (outputs, map t ~fs ~fp)
    | Progn l -> Progn (List.map l ~f:(fun t -> map t ~fs ~fp))
    | Echo x -> Echo (fs x)
    | Cat x -> Cat (fp x)
    | Create_file x -> Create_file (fp x)
    | Copy (x, y) -> Copy (fp x, fp y)
    | Symlink (x, y) ->
      Symlink (fp x, fp y)
    | Copy_and_add_line_directive (x, y) ->
      Copy_and_add_line_directive (fp x, fp y)
    | System x -> System (fs x)
    | Bash x -> Bash (fs x)
    | Update_file (x, y) -> Update_file (fp x, fs y)
    | Rename (x, y) -> Rename (fp x, fp y)
    | Remove_tree x -> Remove_tree (fp x)
    | Mkdir x -> Mkdir (fp x)

let updated_files =
  let rec loop acc t =
    let acc =
      match t with
      | Update_file (fn, _) -> Path.Set.add fn acc
      | _ -> acc
    in
    fold_one_step t ~init:acc ~f:loop
  in
  fun t -> loop Path.Set.empty t

let chdirs =
  let rec loop acc t =
    let acc =
      match t with
      | Chdir (dir, _) -> Path.Set.add dir acc
      | _ -> acc
    in
    fold_one_step t ~init:acc ~f:loop
  in
  fun t -> loop Path.Set.empty t

open Future

let get_std_output : _ -> Future.std_output_to = function
  | None          -> Terminal
  | Some (fn, oc) -> Opened_file { filename = fn; tail = false; desc = Channel oc }

let run ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to prog args =
  let stdout_to = get_std_output stdout_to in
  let stderr_to = get_std_output stderr_to in
  let env = Context.extend_env ~vars:env_extra ~env in
  Future.run Strict ~dir:(Path.to_string dir) ~env ~stdout_to ~stderr_to ~purpose
    (Path.reach_for_running ~from:dir prog) args

let rec exec t ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to =
  match t with
  | Run (prog, args) ->
    run ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to prog args
  | Chdir (dir, t) ->
    exec t ~purpose ~env ~env_extra ~stdout_to ~stderr_to ~dir
  | Setenv (var, value, t) ->
    exec t ~purpose ~dir ~env ~stdout_to ~stderr_to
      ~env_extra:(Env_var_map.add env_extra ~key:var ~data:value)
  | Redirect (outputs, fn, t) ->
    redirect ~purpose outputs fn t ~dir ~env ~env_extra ~stdout_to ~stderr_to
  | Ignore (outputs, t) ->
    redirect ~purpose outputs Config.dev_null t ~dir ~env ~env_extra ~stdout_to ~stderr_to
  | Progn l ->
    exec_list l ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to
  | Echo str ->
    return
      (match stdout_to with
       | None -> print_string str; flush stdout
       | Some (_, oc) -> output_string oc str)
  | Cat fn ->
    Io.with_file_in (Path.to_string fn) ~f:(fun ic ->
      let oc =
        match stdout_to with
        | None -> stdout
        | Some (_, oc) -> oc
      in
      Io.copy_channels ic oc);
    return ()
  | Create_file fn ->
    let fn = Path.to_string fn in
    if Sys.file_exists fn then Sys.remove fn;
    Unix.close (Unix.openfile fn [O_CREAT; O_TRUNC; O_WRONLY] 0o666);
    return ()
  | Copy (src, dst) ->
    Io.copy_file ~src:(Path.to_string src) ~dst:(Path.to_string dst);
    return ()
  | Symlink (src, dst) ->
    if Sys.win32 then
      Io.copy_file ~src:(Path.to_string src) ~dst:(Path.to_string dst)
    else begin
      let src =
        if Path.is_root dst then
          Path.to_string src
        else
          Path.reach ~from:(Path.parent dst) src
      in
      let dst = Path.to_string dst in
      match Unix.readlink dst with
      | target ->
        if target <> src then begin
          Unix.unlink dst;
          Unix.symlink src dst
        end
      | exception _ ->
        Unix.symlink src dst
    end;
    return ()
  | Copy_and_add_line_directive (src, dst) ->
    Io.with_file_in (Path.to_string src) ~f:(fun ic ->
      Io.with_file_out (Path.to_string dst) ~f:(fun oc ->
        let fn = Path.drop_build_context src in
        Printf.fprintf oc "# 1 %S\n" (Path.to_string fn);
        Io.copy_channels ic oc));
    return ()
  | System cmd ->
    let path, arg =
      Utils.system_shell_exn ~needed_to:"interpret (system ...) actions"
    in
    run ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to path [arg; cmd]
  | Bash cmd ->
    run ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to
      (Utils.bash_exn ~needed_to:"interpret (bash ...) actions")
      ["-e"; "-u"; "-o"; "pipefail"; "-c"; cmd]
  | Update_file (fn, s) ->
    let fn = Path.to_string fn in
    if Sys.file_exists fn && Io.read_file fn = s then
      ()
    else
      Io.write_file fn s;
    return ()
  | Rename (src, dst) ->
    Unix.rename (Path.to_string src) (Path.to_string dst);
    return ()
  | Remove_tree path ->
    Path.rm_rf path;
    return ()
  | Mkdir path ->
    (match Path.kind path with
     | External _ ->
       (* CR-someday jdimino: we need to keep locations here *)
       die "(mkdir ...) is not supported for paths outside of the workspace:\n\
           \  %a\n"
         Sexp.pp (List [Atom "mkdir"; Path.sexp_of_t path])
     | Local path ->
       Path.Local.mkdir_p path);
    return ()

and redirect outputs fn t ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to =
  let fn = Path.to_string fn in
  let oc = Io.open_out fn in
  let out = Some (fn, oc) in
  let stdout_to, stderr_to =
    match outputs with
    | Stdout -> (out, stderr_to)
    | Stderr -> (stdout_to, out)
    | Outputs -> (out, out)
  in
  exec t ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to >>| fun () ->
  close_out oc

and exec_list l ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to =
  match l with
  | [] ->
    Future.return ()
  | [t] ->
    exec t ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to
  | t :: rest ->
    exec t ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to >>= fun () ->
    exec_list rest ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to

let exec ~targets ?context t =
  let env =
    match (context : Context.t option) with
    | None -> Lazy.force Context.initial_env
    | Some c -> c.env
  in
  let targets = Path.Set.elements targets in
  let purpose = Future.Build_job targets in
  exec t ~purpose ~dir:Path.root ~env ~env_extra:Env_var_map.empty
    ~stdout_to:None ~stderr_to:None

let sandbox t ~sandboxed ~deps ~targets =
  Progn
    [ Progn (List.filter_map deps ~f:(fun path ->
        if Path.is_local path then
          Some (Ast.Symlink (path, sandboxed path))
        else
          None))
    ; map t ~fs:(fun x -> x) ~fp:sandboxed
    ; Progn (List.filter_map targets ~f:(fun path ->
        if Path.is_local path then
          Some (Ast.Rename (sandboxed path, path))
        else
          None))
    ]

module Infer = struct
  module S = Path.Set
  module Outcome = struct
    type t =
      { deps    : S.t
      ; targets : S.t
      }
  end
  open Outcome

  let ( +@ ) acc fn = { acc with targets = S.add fn acc.targets }
  let ( +< ) acc fn =
    if S.mem fn acc.targets then
      acc
    else
      { acc with deps = S.add fn acc.deps }
  let ( -@ ) acc fn = { acc with targets = S.remove fn acc.targets }

  let rec infer acc t =
    match t with
    | Run (prog, _)       -> acc +< prog
    | Redirect (_, fn, t) -> infer (acc +@ fn) t
    | Cat fn              -> acc +< fn
    | Create_file fn      -> acc +@ fn
    | Update_file (fn, _) -> acc +@ fn
    | Rename (src, dst)   -> acc +< src +@ dst -@ src
    | Copy (src, dst)
    | Copy_and_add_line_directive (src, dst)
    | Symlink (src, dst) -> acc +< src +@ dst
    | Chdir (_, t)
    | Setenv (_, _, t)
    | Ignore (_, t) -> infer acc t
    | Progn l -> List.fold_left l ~init:acc ~f:infer
    | Echo _
    | System _
    | Bash _ -> acc
    | Remove_tree dir ->
      { acc with targets = S.filter acc.targets ~f:(fun fn ->
          not (Path.is_descendant fn ~of_:dir))
      }
    | Mkdir _ -> acc

  let infer t =
    infer { deps = S.empty; targets = S.empty } t
end
