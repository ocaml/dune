open Import
open Sexp.Of_sexp

module Env_var_map = Context.Env_var_map

module Program = struct
  type t =
    | This      of Path.t
    | Not_found of string

  let sexp_of_t = function
    | This p -> Path.sexp_of_t p
    | Not_found s -> List [Atom "not_found"; Atom s]

  let t sexp =
    match sexp with
    | Atom _ -> This (Path.t sexp)
    | List (_, [Atom (_, "not_found"); Atom (_, s)]) -> Not_found s
    | _ ->
      Loc.fail (Sexp.Ast.loc sexp)
        "S-expression of the form <atom> or (not_found <atom>) expected"

  let resolve ctx ~dir s =
    if s = "" then
      Not_found ""
    else if String.contains s '/' then
      This (Path.relative dir s)
    else
      match Context.which ctx s with
      | Some p -> This p
      | None -> Not_found s
end

module Var_expansion = struct
  module Concat_or_split = struct
    type t =
      | Concat (* default *)
      | Split  (* ${!...} *)
  end

  open Concat_or_split

  type t =
    | Paths   of Path.t list * Concat_or_split.t
    | Strings of string list * Concat_or_split.t

  let concat = function
    | [s] -> s
    | l -> String.concat ~sep:" " l

  let string_of_path ~dir p = Path.reach ~from:dir p
  let path_of_string ~dir s = Path.relative dir s

  let to_strings ~dir = function
    | Strings (l, Split ) -> l
    | Strings (l, Concat) -> [concat l]
    | Paths   (l, Split ) -> List.map l ~f:(string_of_path ~dir)
    | Paths   (l, Concat) -> [concat (List.map l ~f:(string_of_path ~dir))]

  let to_string ~dir = function
    | Strings (_, Split) | Paths (_, Split) -> assert false
    | Strings (l, Concat) -> concat l
    | Paths   (l, Concat) -> concat (List.map l ~f:(string_of_path ~dir))

  let to_path ~dir = function
    | Strings (_, Split) | Paths (_, Split) -> assert false
    | Strings (l, Concat) -> path_of_string ~dir (concat l)
    | Paths ([p], Concat) -> p
    | Paths (l,   Concat) ->
      path_of_string ~dir (concat (List.map l ~f:(string_of_path ~dir)))

  let to_prog_and_args ctx ~dir exp : Program.t * string list =
    let resolve = Program.resolve in
    match exp with
    | Paths   ([p], _) -> (This p, [])
    | Strings ([s], _) -> (resolve ctx ~dir s, [])
    | Paths ([], _) | Strings ([], _) -> (Not_found "", [])
    | Paths (l, Concat) ->
      (This
         (path_of_string ~dir
            (concat (List.map l ~f:(string_of_path ~dir)))),
       [])
    | Strings (l, Concat) ->
      (resolve ~dir ctx (concat l), l)
    | Paths (p :: l, Split) ->
      (This p, List.map l ~f:(string_of_path ~dir))
    | Strings (s :: l, Split) ->
      (resolve ~dir ctx s, l)
end

module VE = Var_expansion
module SW = String_with_vars

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
    (Program : Sexpable)
    (Path    : Sexpable)
    (String  : Sexpable)
    (Ast : Action_intf.Ast
     with type program := Program.t
     with type path    := Path.t
     with type string  := String.t) =
struct
  include Ast

  let rec t sexp =
    let path = Path.t and string = String.t in
    sum
      [ cstr_rest "run" (Program.t @> nil) string (fun prog args -> Run (prog, args))
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
      ; cstr "copy#" (path @> path @> nil) (fun src dst ->
          Copy_and_add_line_directive (src, dst))
      ; cstr "system" (string @> nil) (fun cmd -> System cmd)
      ; cstr "bash"   (string @> nil) (fun cmd -> Bash   cmd)
      ]
      sexp

  let rec sexp_of_t : _ -> Sexp.t =
    let path = Path.sexp_of_t and string = String.sexp_of_t in
    function
    | Run (a, xs) -> List (Atom "run" :: Program.sexp_of_t a :: List.map xs ~f:string)
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
  with type program = Program.t
  with type path    = Path.t
  with type string  = String.t
module rec Ast : Ast = Ast

include Make_ast
    (Program)
    (Path)
    (struct
      type t = string
      let t = Sexp.Of_sexp.string
      let sexp_of_t = Sexp.To_sexp.string
    end)
    (Ast)

module Unexpanded = struct
  module type Uast = Action_intf.Ast
    with type program = String_with_vars.t
    with type path    = String_with_vars.t
    with type string  = String_with_vars.t
  module rec Uast : Uast = Uast

  include Make_ast(String_with_vars)(String_with_vars)(String_with_vars)(Uast)

  let t sexp =
    match sexp with
    | Atom _ ->
      of_sexp_errorf sexp
        "if you meant for this to be executed with bash, write (bash \"...\") instead"
    | List _ -> t sexp

  let check_mkdir loc path =
    if not (Path.is_local path) then
      Loc.fail loc
        "(mkdir ...) is not supported for paths outside of the workspace:\n\
        \  %a\n"
        Sexp.pp (List [Atom "mkdir"; Path.sexp_of_t path])

  module Partial = struct
    module type Past = Action_intf.Ast
      with type program = (Program.t, String_with_vars.t) either
      with type path    = (Path.t   , String_with_vars.t) either
      with type string  = (string   , String_with_vars.t) either
    module rec Past : Past = Past

    include Past

    module E = struct
      let string ~dir ~f = function
        | Inl x -> x
        | Inr template ->
          SW.expand template ~f:(fun loc var ->
            match f loc var with
            | None   -> None
            | Some e -> Some (VE.to_string ~dir e))

      let expand ~generic ~special ~map ~dir ~f = function
        | Inl x -> map x
        | Inr template as x ->
          match SW.just_a_var template with
          | None -> generic ~dir (string ~dir ~f x)
          | Some var ->
            match f (SW.loc template) var with
            | None   -> generic ~dir (SW.to_string template)
            | Some e -> special ~dir e
      [@@inlined always]

      let strings ~dir ~f x =
        expand ~dir ~f x
          ~generic:(fun ~dir:_ x -> [x])
          ~special:VE.to_strings
          ~map:(fun x -> [x])

      let path ~dir ~f x =
        expand ~dir ~f x
          ~generic:VE.path_of_string
          ~special:VE.to_path
          ~map:(fun x -> x)

      let prog_and_args ctx ~dir ~f x =
        expand ~dir ~f x
          ~generic:(fun ~dir:_ s -> (Program.resolve ctx ~dir s, []))
          ~special:(VE.to_prog_and_args ctx)
          ~map:(fun x -> (x, []))
    end

    let rec expand ctx dir t ~f : Ast.t =
      match t with
      | Run (prog, args) ->
        let args = List.concat_map args ~f:(E.strings ~dir ~f) in
        let prog, more_args = E.prog_and_args ctx ~dir ~f prog in
        Run (prog, more_args @ args)
      | Chdir (fn, t) ->
        let fn = E.path ~dir ~f fn in
        Chdir (fn, expand ctx fn t ~f)
      | Setenv (var, value, t) ->
        Setenv (E.string ~dir ~f var, E.string ~dir ~f value,
                expand ctx dir t ~f)
      | Redirect (outputs, fn, t) ->
        Redirect (outputs, E.path ~dir ~f fn, expand ctx dir t ~f)
      | Ignore (outputs, t) ->
        Ignore (outputs, expand ctx dir t ~f)
      | Progn l -> Progn (List.map l ~f:(fun t -> expand ctx dir t ~f))
      | Echo x -> Echo (E.string ~dir ~f x)
      | Cat x -> Cat (E.path ~dir ~f x)
      | Create_file x -> Create_file (E.path ~dir ~f x)
      | Copy (x, y) ->
        Copy (E.path ~dir ~f x, E.path ~dir ~f y)
      | Symlink (x, y) ->
        Symlink (E.path ~dir ~f x, E.path ~dir ~f y)
      | Copy_and_add_line_directive (x, y) ->
        Copy_and_add_line_directive (E.path ~dir ~f x, E.path ~dir ~f y)
      | System x -> System (E.string ~dir ~f x)
      | Bash x -> Bash (E.string ~dir ~f x)
      | Update_file (x, y) -> Update_file (E.path ~dir ~f x, E.string ~dir ~f y)
      | Rename (x, y) ->
        Rename (E.path ~dir ~f x, E.path ~dir ~f y)
      | Remove_tree x ->
        Remove_tree (E.path ~dir ~f x)
      | Mkdir x ->
        match x with
        | Inl path -> Mkdir path
        | Inr tmpl ->
          let path = E.path ~dir ~f x in
          check_mkdir (SW.loc tmpl) path;
          Mkdir path
  end

  module E = struct
    let string ~dir ~f template =
      SW.partial_expand template ~f:(fun loc var ->
        match f loc var with
        | None   -> None
        | Some e -> Some (VE.to_string ~dir e))

    let expand ~generic ~special ~dir ~f template =
      match SW.just_a_var template with
      | None -> begin
          match string ~dir ~f template with
          | Inl x -> Inl (generic ~dir x)
          | Inr _ as x -> x
        end
      | Some var ->
        match f (SW.loc template) var with
        | None   -> Inr template
        | Some e -> Inl (special ~dir e)

    let strings ~dir ~f x =
      expand ~dir ~f x
        ~generic:(fun ~dir:_ x -> [x])
        ~special:VE.to_strings

    let path ~dir ~f x =
      expand ~dir ~f x
        ~generic:VE.path_of_string
        ~special:VE.to_path

    let prog_and_args ctx ~dir ~f x =
      expand ~dir ~f x
        ~generic:(fun ~dir s -> (Program.resolve ctx ~dir s, []))
        ~special:(VE.to_prog_and_args ctx)

    let simple x =
      match SW.just_text x with
      | Some s -> Inl s
      | None   -> Inr x
  end

  (* Like [partial_expand] except we keep everything as a template. This is for when we
     can't determine a chdir statically *)
  let rec simple_expand t ~f : Partial.t =
    match t with
    | Run (prog, args) ->
      SW.iter prog ~f;
      List.iter args ~f:(SW.iter ~f);
      Run (Inr prog, List.map args ~f:E.simple)
    | Chdir (fn, t) ->
      SW.iter fn ~f;
      Chdir (Inr fn, simple_expand t ~f)
    | Setenv (var, value, t) ->
      SW.iter var ~f;
      SW.iter value ~f;
      Setenv (E.simple var, E.simple value, simple_expand t  ~f)
    | Redirect (outputs, fn, t) ->
      SW.iter fn ~f;
      Redirect (outputs, Inr fn, simple_expand t ~f)
    | Ignore (outputs, t) ->
      Ignore (outputs, simple_expand t ~f)
    | Progn l -> Progn (List.map l ~f:(simple_expand ~f))
    | Echo x -> SW.iter x ~f; Echo (E.simple x)
    | Cat x -> SW.iter x ~f; Cat (Inr x)
    | Create_file x -> SW.iter x ~f; Create_file (Inr x)
    | Copy (x, y) ->
      SW.iter x ~f;
      SW.iter y ~f;
      Copy (Inr x, Inr y)
    | Copy_and_add_line_directive (x, y) ->
      SW.iter x ~f;
      SW.iter y ~f;
      Copy_and_add_line_directive (Inr x, Inr y)
    | Symlink (x, y) ->
      SW.iter x ~f;
      SW.iter y ~f;
      Symlink (Inr x, Inr y)
    | Rename (x, y) ->
      SW.iter x ~f;
      SW.iter y ~f;
      Rename (Inr x, Inr y)
    | System x -> SW.iter x ~f; System (E.simple x)
    | Bash x -> SW.iter x ~f; Bash (E.simple x)
    | Update_file (x, y) ->
      SW.iter x ~f;
      SW.iter y ~f;
      Update_file (Inr x, E.simple y)
    | Remove_tree x -> SW.iter x ~f; Remove_tree (Inr x)
    | Mkdir x -> SW.iter x ~f; Mkdir (Inr x)

  let rec partial_expand ctx dir t ~f : Partial.t =
    match t with
    | Run (prog, args) ->
      let args =
        List.concat_map args ~f:(fun arg ->
          match E.strings ~dir ~f arg with
          | Inl args -> List.map args ~f:(fun x -> Inl x)
          | Inr _ as x -> [x])
      in
      begin
        match E.prog_and_args ctx ~dir ~f prog with
        | Inl (prog, more_args) ->
          let more_args = List.map more_args ~f:(fun x -> Inl x) in
          Run (Inl prog, more_args @ args)
        | Inr _ as prog ->
          Run (prog, args)
      end
    | Chdir (fn, t) -> begin
        let res = E.path ~dir ~f fn in
        match res with
        | Inl dir ->
          Chdir (res, partial_expand ctx dir t ~f)
        | Inr _ ->
          let f loc x = ignore (f loc x : _ option) in
          Chdir (res, simple_expand t ~f)
      end
    | Setenv (var, value, t) ->
      Setenv (E.string ~dir ~f var, E.string ~dir ~f value,
              partial_expand ctx dir t ~f)
    | Redirect (outputs, fn, t) ->
      Redirect (outputs, E.path ~dir ~f fn, partial_expand ctx dir t ~f)
    | Ignore (outputs, t) ->
      Ignore (outputs, partial_expand ctx dir t ~f)
    | Progn l -> Progn (List.map l ~f:(fun t -> partial_expand ctx dir t ~f))
    | Echo x -> Echo (E.string ~dir ~f x)
    | Cat x -> Cat (E.path ~dir ~f x)
    | Create_file x -> Create_file (E.path ~dir ~f x)
    | Copy (x, y) ->
      Copy (E.path ~dir ~f x, E.path ~dir ~f y)
    | Symlink (x, y) ->
      Symlink (E.path ~dir ~f x, E.path ~dir ~f y)
    | Copy_and_add_line_directive (x, y) ->
      Copy_and_add_line_directive (E.path ~dir ~f x, E.path ~dir ~f y)
    | System x -> System (E.string ~dir ~f x)
    | Bash x -> Bash (E.string ~dir ~f x)
    | Update_file (x, y) -> Update_file (E.path ~dir ~f x, E.string ~dir ~f y)
    | Rename (x, y) ->
      Rename (E.path ~dir ~f x, E.path ~dir ~f y)
    | Remove_tree x ->
      Remove_tree (E.path ~dir ~f x)
    | Mkdir x ->
      let res = E.path ~dir ~f x in
      (match res with
       | Inl path -> check_mkdir (SW.loc x) path
       | Inr _    -> ());
      Mkdir res
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
    | Run (This prog, args) ->
      Run (This (fp prog), List.map args ~f:fs)
    | Run (Not_found _ as nf, args) ->
      Run (nf, List.map args ~f:fs)
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

type exec_context =
  { context : Context.t option
  ; purpose : Future.purpose
  ; env     : string array
  }

let run ~ectx ~dir ~env_extra ~stdout_to ~stderr_to prog args =
  let stdout_to = get_std_output stdout_to in
  let stderr_to = get_std_output stderr_to in
  let env = Context.extend_env ~vars:env_extra ~env:ectx.env in
  Future.run Strict ~dir:(Path.to_string dir) ~env ~stdout_to ~stderr_to
    ~purpose:ectx.purpose
    (Path.reach_for_running ~from:dir prog) args

let rec exec t ~ectx ~dir ~env_extra ~stdout_to ~stderr_to =
  match t with
  | Run (This prog, args) ->
    run ~ectx ~dir ~env_extra ~stdout_to ~stderr_to prog args
  | Run (Not_found prog, _) ->
    Utils.program_not_found prog ?context:(Option.map ectx.context ~f:(fun c -> c.name))
  | Chdir (dir, t) ->
    exec t ~ectx ~dir ~env_extra ~stdout_to ~stderr_to
  | Setenv (var, value, t) ->
    exec t ~ectx ~dir ~stdout_to ~stderr_to
      ~env_extra:(Env_var_map.add env_extra ~key:var ~data:value)
  | Redirect (outputs, fn, t) ->
    redirect ~ectx ~dir outputs fn t ~env_extra ~stdout_to ~stderr_to
  | Ignore (outputs, t) ->
    redirect ~ectx ~dir outputs Config.dev_null t ~env_extra ~stdout_to ~stderr_to
  | Progn l ->
    exec_list l ~ectx ~dir ~env_extra ~stdout_to ~stderr_to
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
    run ~ectx ~dir ~env_extra ~stdout_to ~stderr_to path [arg; cmd]
  | Bash cmd ->
    run ~ectx ~dir ~env_extra ~stdout_to ~stderr_to
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
       (* Internally we make sure never to do that, and [Unexpanded.*expand] check that *)
       Sexp.code_error
         "(mkdir ...) is not supported for paths outside of the workspace"
         [ "mkdir", Path.sexp_of_t path ]
     | Local path ->
       Path.Local.mkdir_p path);
    return ()

and redirect outputs fn t ~ectx ~dir ~env_extra ~stdout_to ~stderr_to =
  let fn = Path.to_string fn in
  let oc = Io.open_out fn in
  let out = Some (fn, oc) in
  let stdout_to, stderr_to =
    match outputs with
    | Stdout -> (out, stderr_to)
    | Stderr -> (stdout_to, out)
    | Outputs -> (out, out)
  in
  exec t ~ectx ~dir ~env_extra ~stdout_to ~stderr_to >>| fun () ->
  close_out oc

and exec_list l ~ectx ~dir ~env_extra ~stdout_to ~stderr_to =
  match l with
  | [] ->
    Future.return ()
  | [t] ->
    exec t ~ectx ~dir ~env_extra ~stdout_to ~stderr_to
  | t :: rest ->
    exec t ~ectx ~dir ~env_extra ~stdout_to ~stderr_to >>= fun () ->
    exec_list rest ~ectx ~dir ~env_extra ~stdout_to ~stderr_to

let exec ~targets ?context t =
  let env =
    match (context : Context.t option) with
    | None -> Lazy.force Context.initial_env
    | Some c -> c.env
  in
  let targets = Path.Set.elements targets in
  let purpose = Future.Build_job targets in
  let ectx = { purpose; context; env } in
  exec t ~ectx ~dir:Path.root ~env_extra:Env_var_map.empty
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
  let ( +< ) acc fn = { acc with deps    = S.add fn acc.deps    }

  let rec infer acc t =
    match t with
    | Run (This prog, _)   -> acc +< prog
    | Run (Not_found _, _) -> acc
    | Redirect (_, fn, t)  -> infer (acc +@ fn) t
    | Cat fn               -> acc +< fn
    | Create_file fn       -> acc +@ fn
    | Update_file (fn, _)  -> acc +@ fn
    | Rename (src, dst)    -> acc +< src +@ dst
    | Copy (src, dst)
    | Copy_and_add_line_directive (src, dst)
    | Symlink (src, dst) -> acc +< src +@ dst
    | Chdir (_, t)
    | Setenv (_, _, t)
    | Ignore (_, t) -> infer acc t
    | Progn l -> List.fold_left l ~init:acc ~f:infer
    | Echo _
    | System _
    | Bash _
    | Remove_tree _
    | Mkdir _ -> acc

  let infer t =
    let { deps; targets } = infer { deps = S.empty; targets = S.empty } t in
    (* A file can be inferred as both a dependency and a target, for instance:

       {[
         (progn (copy a b) (copy b c))
       ]}
    *)
    { deps = S.diff deps targets; targets }

  let ( +@? ) acc fn =
    match fn with
    | Inl fn -> { acc with targets = S.add fn acc.targets }
    | Inr _  -> acc
  let ( +<? ) acc fn =
    match fn with
    | Inl fn -> { acc with deps    = S.add fn acc.deps    }
    | Inr _  -> acc

  let rec partial acc (t : Unexpanded.Partial.t) =
    match t with
    | Run (Inl (This prog), _)   -> acc +< prog
    | Run (_, _) -> acc
    | Redirect (_, fn, t)  -> partial (acc +@? fn) t
    | Cat fn               -> acc +<? fn
    | Create_file fn       -> acc +@? fn
    | Update_file (fn, _)  -> acc +@? fn
    | Rename (src, dst)    -> acc +<? src +@? dst
    | Copy (src, dst)
    | Copy_and_add_line_directive (src, dst)
    | Symlink (src, dst) -> acc +<? src +@? dst
    | Chdir (_, t)
    | Setenv (_, _, t)
    | Ignore (_, t) -> partial acc t
    | Progn l -> List.fold_left l ~init:acc ~f:partial
    | Echo _
    | System _
    | Bash _
    | Remove_tree _
    | Mkdir _ -> acc

  let ( +@? ) acc fn =
    match fn with
    | Inl fn -> { acc with targets = S.add fn acc.targets }
    | Inr sw -> Loc.fail (SW.loc sw) "Cannot determine this target statically."

  let rec partial_with_all_targets acc (t : Unexpanded.Partial.t) =
    match t with
    | Run (Inl (This prog), _)   -> acc +< prog
    | Run (_, _) -> acc
    | Redirect (_, fn, t)  -> partial_with_all_targets (acc +@? fn) t
    | Cat fn               -> acc +<? fn
    | Create_file fn       -> acc +@? fn
    | Update_file (fn, _)  -> acc +@? fn
    | Rename (src, dst)    -> acc +<? src +@? dst
    | Copy (src, dst)
    | Copy_and_add_line_directive (src, dst)
    | Symlink (src, dst) -> acc +<? src +@? dst
    | Chdir (_, t)
    | Setenv (_, _, t)
    | Ignore (_, t) -> partial_with_all_targets acc t
    | Progn l -> List.fold_left l ~init:acc ~f:partial_with_all_targets
    | Echo _
    | System _
    | Bash _
    | Remove_tree _
    | Mkdir _ -> acc

  let partial ~all_targets t =
    let acc = { deps = S.empty; targets = S.empty } in
    let { deps; targets } =
      if all_targets then
        partial_with_all_targets acc t
      else
        partial acc t
    in
    { deps = S.diff deps targets; targets }
end
