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

module Mini_shexp = struct
  module Ast = struct
    type ('a, 'path) t =
      | Run            of 'path * 'a list
      | Chdir          of 'path * ('a, 'path) t
      | Setenv         of 'a * 'a * ('a, 'path) t
      | With_stdout_to of 'path * ('a, 'path) t
      | Progn          of ('a, 'path) t list
      | Echo           of 'a
      | Create_file    of 'path
      | Cat            of 'path
      | Copy           of 'path * 'path
      | Symlink        of 'path * 'path
      | Copy_and_add_line_directive of 'path * 'path
      | System         of 'a
      | Bash           of 'a

    let rec t a p sexp =
      sum
        [ cstr_rest "run" (p @> nil) a             (fun prog args -> Run (prog, args))
        ; cstr "chdir"    (p @> t a p @> nil)        (fun dn t -> Chdir (dn, t))
        ; cstr "setenv"   (a @> a @> t a p @> nil)   (fun k v t -> Setenv (k, v, t))
        ; cstr "with-stdout-to" (p @> t a p @> nil)  (fun fn t -> With_stdout_to (fn, t))
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
      | With_stdout_to (fn, r) -> List [Atom "with-stdout-to"; g fn; sexp_of_t f g r]
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

    let rec fold t ~init:acc ~f =
      match t with
      | Run (prog, args) -> List.fold_left args ~init:(f acc prog) ~f
      | Chdir (fn, t) -> fold t ~init:(f acc fn) ~f
      | Setenv (var, value, t) -> fold t ~init:(f (f acc var) value) ~f
      | With_stdout_to (fn, t) -> fold t ~init:(f acc fn) ~f
      | Progn l -> List.fold_left l ~init:acc ~f:(fun init t -> fold t ~init ~f)
      | Echo x -> f acc x
      | Cat x -> f acc x
      | Create_file x -> f acc x
      | Copy (x, y) -> f (f acc x) y
      | Symlink (x, y) -> f (f acc x) y
      | Copy_and_add_line_directive (x, y) -> f (f acc x) y
      | System x -> f acc x
      | Bash x -> f acc x
  end
  open Ast

  type t = (string, Path.t) Ast.t
  let t = Ast.t string Path.t
  let sexp_of_t = Ast.sexp_of_t Sexp.To_sexp.string Path.sexp_of_t

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

    let rec expand dir t ~f : (string, Path.t) Ast.t =
      match t with
      | Run (prog, args) ->
        Run (expand_path ~dir ~f prog,
             List.map args ~f:(fun arg -> expand_str ~dir ~f arg))
      | Chdir (fn, t) ->
        let fn = expand_path ~dir ~f fn in
        Chdir (fn, expand fn t ~f)
      | Setenv (var, value, t) ->
        Setenv (expand_str ~dir ~f var, expand_str ~dir ~f value,
                expand dir t ~f)
      | With_stdout_to (fn, t) ->
        With_stdout_to (expand_path ~dir ~f fn, expand dir t ~f)
      | Progn l -> Progn (List.map l ~f:(fun t -> expand dir t ~f))
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
  end

  open Future

  let run ~dir ~env ~env_extra ~stdout_to ~tail prog args =
    let stdout_to : Future.stdout_to =
      match stdout_to with
      | None          -> Terminal
      | Some (fn, oc) -> Opened_file { filename = fn; tail; desc = Channel oc }
    in
    let env = Context.extend_env ~vars:env_extra ~env in
    Future.run Strict ~dir:(Path.to_string dir) ~env ~stdout_to
      (Path.reach_for_running ~from:dir prog) args

  let rec exec t ~dir ~env ~env_extra ~stdout_to ~tail =
    match t with
    | Run (prog, args) ->
      run ~dir ~env ~env_extra ~stdout_to ~tail prog args
    | Chdir (dir, t) ->
      exec t ~env ~env_extra ~stdout_to ~tail ~dir
    | Setenv (var, value, t) ->
      exec t ~dir ~env ~stdout_to ~tail
        ~env_extra:(String_map.add env_extra ~key:var ~data:value)
    | With_stdout_to (fn, t) ->
      if tail then Option.iter stdout_to ~f:(fun (_, oc) -> close_out oc);
      let fn = Path.to_string fn in
      exec t ~dir ~env ~env_extra ~tail
        ~stdout_to:(Some (fn, open_out_bin fn))
    | Progn l ->
      exec_list l ~dir ~env ~env_extra ~stdout_to ~tail
    | Echo str ->
      return
        (match stdout_to with
         | None -> print_string str; flush stdout
         | Some (_, oc) ->
           output_string oc str;
           if tail then close_out oc)
    | Cat fn ->
      with_file_in (Path.to_string fn) ~f:(fun ic ->
        match stdout_to with
        | None -> copy_channels ic stdout
        | Some (_, oc) ->
          copy_channels ic oc;
          if tail then close_out oc);
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
        run ~dir ~env ~env_extra ~stdout_to ~tail path [arg; cmd]
      end
    | Bash cmd ->
      run ~dir ~env ~env_extra ~stdout_to ~tail
        (Path.absolute "/bin/bash")
        ["-e"; "-u"; "-o"; "pipefail"; "-c"; cmd]

  and exec_list l ~dir ~env ~env_extra ~stdout_to ~tail =
    match l with
    | [] ->
      if tail then Option.iter stdout_to ~f:(fun (_, oc) -> close_out oc);
      Future.return ()
    | [t] ->
      exec t ~dir ~env ~env_extra ~stdout_to ~tail
    | t :: rest ->
      exec t ~dir ~env ~env_extra ~stdout_to ~tail:false >>= fun () ->
      exec_list rest ~dir ~env ~env_extra ~stdout_to ~tail
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
    ~stdout_to:None ~tail:true
