open Import
open Sexp.Of_sexp

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
      | None ->
        die "@{<error>Error@}: Program %s not found in PATH (context: %s)" s ctx.name
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
  end
  open Ast

  type t = (string, Path.t) Ast.t
  let t = Ast.t string Path.t
  let sexp_of_t = Ast.sexp_of_t Sexp.To_sexp.string Path.sexp_of_t

  let updated_files =
    let rec loop acc t =
      match t with
      | Update_file (fn, _) -> Path.Set.add fn acc
      | Chdir (_, t)
      | Setenv (_, _, t)
      | Redirect (_, _, t)
      | Ignore (_, t) -> loop acc t
      | Progn l -> List.fold_left l ~init:acc ~f:loop
      | Run _ -> acc
      | Echo _
      | Cat _
      | Create_file _
      | Copy _
      | Symlink _
      | Copy_and_add_line_directive _
      | System _
      | Bash _ -> acc
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
  end

  open Future

  let get_std_output : _ -> Future.std_output_to = function
    | None          -> Terminal
    | Some (fn, oc) -> Opened_file { filename = fn; tail = false; desc = Channel oc }

  let run ~dir ~env ~env_extra ~stdout_to ~stderr_to prog args =
    let stdout_to = get_std_output stdout_to in
    let stderr_to = get_std_output stderr_to in
    let env = Context.extend_env ~vars:env_extra ~env in
    Future.run Strict ~dir:(Path.to_string dir) ~env ~stdout_to ~stderr_to
      (Path.reach_for_running ~from:dir prog) args

  let rec exec t ~dir ~env ~env_extra ~stdout_to ~stderr_to =
    match t with
    | Run (prog, args) ->
      run ~dir ~env ~env_extra ~stdout_to ~stderr_to prog args
    | Chdir (dir, t) ->
      exec t ~env ~env_extra ~stdout_to ~stderr_to ~dir
    | Setenv (var, value, t) ->
      exec t ~dir ~env ~stdout_to ~stderr_to
        ~env_extra:(String_map.add env_extra ~key:var ~data:value)
    | Redirect (outputs, fn, t) ->
      redirect outputs fn t ~dir ~env ~env_extra ~stdout_to ~stderr_to
    | Ignore (outputs, t) ->
      redirect outputs Config.dev_null t ~dir ~env ~env_extra ~stdout_to ~stderr_to
    | Progn l ->
      exec_list l ~dir ~env ~env_extra ~stdout_to ~stderr_to
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
          let fn =
            match Path.extract_build_context src with
            | None -> src
            | Some (_, rem) -> rem
          in
          Printf.fprintf oc "# 1 %S\n" (Path.to_string fn);
          copy_channels ic oc));
      return ()
    | System cmd -> begin
      let path, arg, err =
        Utils.system_shell ~needed_to:"interpret (system ...) actions"
      in
      match err with
      | Some err -> err.fail ()
      | None ->
        run ~dir ~env ~env_extra ~stdout_to ~stderr_to path [arg; cmd]
      end
    | Bash cmd ->
      run ~dir ~env ~env_extra ~stdout_to ~stderr_to
        (Path.absolute "/bin/bash")
        ["-e"; "-u"; "-o"; "pipefail"; "-c"; cmd]
    | Update_file (fn, s) ->
      let fn = Path.to_string fn in
      if Sys.file_exists fn && read_file fn = s then
        ()
      else
        write_file fn s;
      return ()

  and redirect outputs fn t ~dir ~env ~env_extra ~stdout_to ~stderr_to =
    let fn = Path.to_string fn in
    let oc = open_out_bin fn in
    let out = Some (fn, oc) in
    let stdout_to, stderr_to =
      match outputs with
      | Stdout -> (out, stderr_to)
      | Stderr -> (stdout_to, out)
      | Outputs -> (out, out)
    in
    exec t ~dir ~env ~env_extra ~stdout_to ~stderr_to >>| fun () ->
    close_out oc

  and exec_list l ~dir ~env ~env_extra ~stdout_to ~stderr_to =
    match l with
    | [] ->
      Future.return ()
    | [t] ->
      exec t ~dir ~env ~env_extra ~stdout_to ~stderr_to
    | t :: rest ->
      exec t ~dir ~env ~env_extra ~stdout_to ~stderr_to >>= fun () ->
      exec_list rest ~dir ~env ~env_extra ~stdout_to ~stderr_to
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

let exec { action; dir; context } =
  let env =
    match context with
    | None -> Lazy.force Context.initial_env
    | Some c -> c.env
  in
  Mini_shexp.exec action ~dir ~env ~env_extra:String_map.empty
    ~stdout_to:None ~stderr_to:None

type for_hash = string option * Path.t * Mini_shexp.t

let for_hash { context; dir; action } =
  (Option.map context ~f:(fun c -> c.name),
   dir,
   action)
