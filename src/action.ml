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

module Mini_shexp = struct
  module Ast = struct
    type outputs =
      | Stdout
      | Stderr
      | Outputs (* Both Stdout and Stderr *)

    let string_of_outputs = function
      | Stdout -> "stdout"
      | Stderr -> "stderr"
      | Outputs -> "outputs"

    type ('a, 'path) t =
      | Run            of 'path * 'a list
      | Chdir          of 'path * ('a, 'path) t
      | Setenv         of 'a * 'a * ('a, 'path) t
      | Redirect       of outputs * 'path * ('a, 'path) t
      | Ignore         of outputs * ('a, 'path) t
      | Progn          of ('a, 'path) t list
      | Echo           of 'a
      | Create_file    of 'path
      | Cat            of 'path
      | Copy           of 'path * 'path
      | Symlink        of 'path * 'path
      | Copy_and_add_line_directive of 'path * 'path
      | System         of 'a
      | Bash           of 'a
      | Update_file     of 'path * 'a
      | Rename         of 'path * 'path

    let rec t a p sexp =
      sum
        [ cstr_rest "run" (p @> nil) a             (fun prog args -> Run (prog, args))
        ; cstr "chdir"    (p @> t a p @> nil)        (fun dn t -> Chdir (dn, t))
        ; cstr "setenv"   (a @> a @> t a p @> nil)   (fun k v t -> Setenv (k, v, t))
        ; cstr "with-stdout-to"  (p @> t a p @> nil) (fun fn t -> Redirect (Stdout, fn, t))
        ; cstr "with-stderr-to"  (p @> t a p @> nil) (fun fn t -> Redirect (Stderr, fn, t))
        ; cstr "with-outputs-to" (p @> t a p @> nil) (fun fn t -> Redirect (Outputs, fn, t))
        ; cstr "ignore-stdout"   (t a p @> nil)      (fun t -> Ignore (Stdout, t))
        ; cstr "ignore-stderr"   (t a p @> nil)      (fun t -> Ignore (Stderr, t))
        ; cstr "ignore-outputs"  (t a p @> nil)      (fun t -> Ignore (Outputs, t))
        ; cstr_rest "progn"      nil (t a p)         (fun l -> Progn l)
        ; cstr "echo"           (a @> nil)         (fun x -> Echo x)
        ; cstr "cat"            (p @> nil)         (fun x -> Cat x)
        ; cstr "create-file"    (p @> nil)         (fun x -> Create_file x)
        ; cstr "copy" (p @> p @> nil)              (fun src dst -> Copy (src, dst))
      (*
           (* We don't expose symlink to the user yet since this might complicate things *)
           ; cstr "symlink" (a @> a @> nil) (fun src dst -> Symlink (dst, Cat src))
        *)
        ; cstr "copy-and-add-line-directive" (p @> p @> nil) (fun src dst ->
            Copy_and_add_line_directive (src, dst))
        ; cstr "system" (a @> nil) (fun cmd -> System cmd)
        ; cstr "bash"   (a @> nil) (fun cmd -> Bash   cmd)
        ]
        sexp

    let rec sexp_of_t f g : _ -> Sexp.t = function
      | Run (a, xs) -> List (Atom "run" :: g a :: List.map xs ~f)
      | Chdir (a, r) -> List [Atom "chdir" ; g a ; sexp_of_t f g r]
      | Setenv (k, v, r) -> List [Atom "setenv" ; f k ; f v ; sexp_of_t f g r]
      | Redirect (outputs, fn, r) ->
        List [ Atom (sprintf "with-%s-to" (string_of_outputs outputs))
             ; g fn
             ; sexp_of_t f g r
             ]
      | Ignore (outputs, r) ->
        List [ Atom (sprintf "ignore-%s" (string_of_outputs outputs))
             ; sexp_of_t f g r
             ]
      | Progn l -> List (Atom "progn" :: List.map l ~f:(sexp_of_t f g))
      | Echo x -> List [Atom "echo"; f x]
      | Cat x -> List [Atom "cat"; g x]
      | Create_file x -> List [Atom "create-file"; g x]
      | Copy (x, y) ->
        List [Atom "copy"; g x; g y]
      | Symlink (x, y) ->
        List [Atom "symlink"; g x; g y]
      | Copy_and_add_line_directive (x, y) ->
        List [Atom "copy-and-add-line-directive"; g x; g y]
      | System x -> List [Atom "system"; f x]
      | Bash   x -> List [Atom "bash"; f x]
      | Update_file (x, y) -> List [Atom "update-file"; g x; f y]
      | Rename (x, y) -> List [Atom "rename"; g x; g y]

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
      | Rename _ -> acc

    let rec map
      : 'a 'b 'c 'd. ('a, 'b) t -> f1:('a -> 'c) -> f2:('b -> 'd) -> ('c, 'd) t
      = fun t ~f1 ~f2 ->
        match t with
        | Run (prog, args) ->
          Run (f2 prog, List.map args ~f:f1)
        | Chdir (fn, t) ->
          Chdir (f2 fn, map t ~f1 ~f2)
        | Setenv (var, value, t) ->
          Setenv (f1 var, f1 value, map t ~f1 ~f2)
        | Redirect (outputs, fn, t) ->
          Redirect (outputs, f2 fn, map t ~f1 ~f2)
        | Ignore (outputs, t) ->
          Ignore (outputs, map t ~f1 ~f2)
        | Progn l -> Progn (List.map l ~f:(fun t -> map t ~f1 ~f2))
        | Echo x -> Echo (f1 x)
        | Cat x -> Cat (f2 x)
        | Create_file x -> Create_file (f2 x)
        | Copy (x, y) -> Copy (f2 x, f2 y)
        | Symlink (x, y) ->
          Symlink (f2 x, f2 y)
        | Copy_and_add_line_directive (x, y) ->
          Copy_and_add_line_directive (f2 x, f2 y)
        | System x -> System (f1 x)
        | Bash x -> Bash (f1 x)
        | Update_file (x, y) -> Update_file (f2 x, f1 y)
        | Rename (x, y) -> Rename (f2 x, f2 y)
  end
  open Ast

  type t = (string, Path.t) Ast.t
  let t = Ast.t string Path.t
  let sexp_of_t = Ast.sexp_of_t Sexp.To_sexp.string Path.sexp_of_t

  let updated_files =
    let rec loop acc t =
      let acc =
        match t with
        | Update_file (fn, _) -> Path.Set.add fn acc
        | _ -> acc
      in
      Ast.fold_one_step t ~init:acc ~f:loop
    in
    fun t -> loop Path.Set.empty t

  module Unexpanded = struct
    type t = (String_with_vars.t, String_with_vars.t) Ast.t
    let sexp_of_t = Ast.sexp_of_t String_with_vars.sexp_of_t String_with_vars.sexp_of_t

    let t sexp =
      match sexp with
      | Atom _ ->
        of_sexp_errorf sexp
          "if you meant for this to be executed with bash, write (bash \"...\") instead"
      | List _ -> Ast.t String_with_vars.t String_with_vars.t sexp

    let fold_vars t ~init ~f =
      Ast.fold t ~init ~f:(fun acc pat ->
        String_with_vars.fold ~init:acc pat ~f)

    let rec expand ctx dir t ~f : (string, Path.t) Ast.t =
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
  end

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
      with_file_in (Path.to_string fn) ~f:(fun ic ->
        let oc =
          match stdout_to with
          | None -> stdout
          | Some (_, oc) -> oc
        in
        copy_channels ic oc);
      return ()
    | Create_file fn ->
      let fn = Path.to_string fn in
      if Sys.file_exists fn then Sys.remove fn;
      Unix.close (Unix.openfile fn [O_CREAT; O_TRUNC; O_WRONLY] 0o666);
      return ()
    | Copy (src, dst) ->
      copy_file ~src:(Path.to_string src) ~dst:(Path.to_string dst);
      return ()
    | Symlink (src, dst) ->
      if Sys.win32 then
        copy_file ~src:(Path.to_string src) ~dst:(Path.to_string dst)
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
      with_file_in (Path.to_string src) ~f:(fun ic ->
        with_file_out (Path.to_string dst) ~f:(fun oc ->
          let fn = Path.drop_build_context src in
          Printf.fprintf oc "# 1 %S\n" (Path.to_string fn);
          copy_channels ic oc));
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
      if Sys.file_exists fn && read_file fn = s then
        ()
      else
        write_file fn s;
      return ()
    | Rename (src, dst) ->
      Unix.rename (Path.to_string src) (Path.to_string dst);
      return ()

  and redirect outputs fn t ~purpose ~dir ~env ~env_extra ~stdout_to ~stderr_to =
    let fn = Path.to_string fn in
    let oc = open_out_bin fn in
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
end

type t =
  { context : Context.t option
  ; dir     : Path.t
  ; action  : Mini_shexp.t
  }

let t contexts sexp =
  let open Sexp.Of_sexp in
  let context sexp =
    let name = string sexp in
    match String_map.find name contexts with
    | None   -> of_sexp_errorf sexp "Context %s not found" name
    | Some c -> c
  in
  record
    (field_o "context" context      >>= fun context ->
     field   "dir"     Path.t       >>= fun dir ->
     field   "action"  Mini_shexp.t >>= fun action ->
     return { context; dir; action })
    sexp

let sexp_of_t { context; dir; action } =
  let fields : Sexp.t list =
    [ List [ Atom "dir"    ; Path.sexp_of_t dir          ]
    ; List [ Atom "action" ; Mini_shexp.sexp_of_t action ]
    ]
  in
  let fields =
    match context with
    | None -> fields
    | Some { name; _ } -> List [ Atom "context"; Atom name ] :: fields
  in
  Sexp.List fields

let exec ~targets { action; dir; context } =
  let env =
    match context with
    | None -> Lazy.force Context.initial_env
    | Some c -> c.env
  in
  let targets = Path.Set.elements targets in
  let purpose = Future.Build_job targets in
  Mini_shexp.exec action ~purpose ~dir ~env ~env_extra:Env_var_map.empty
    ~stdout_to:None ~stderr_to:None

let sandbox t ~sandboxed ~deps ~targets =
  let action =
    let module M = Mini_shexp.Ast in
    M.Progn
      [ M.Progn (List.filter_map deps ~f:(fun path ->
          if Path.is_local path then
            Some (M.Symlink (path, sandboxed path))
          else
            None))
      ; M.map t.action ~f1:(fun x -> x) ~f2:sandboxed
      ; M.Progn (List.filter_map targets ~f:(fun path ->
          if Path.is_local path then
            Some (M.Rename (sandboxed path, path))
          else
            None))
      ]
  in
  { t with
    action
  ; dir = sandboxed t.dir
  }

type for_hash = string option * Path.t * Mini_shexp.t

let for_hash { context; dir; action; _ } =
  (Option.map context ~f:(fun c -> c.name),
   dir,
   action)
