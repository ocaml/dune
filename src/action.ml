open Import
open Sexp.Of_sexp

module Env_var_map = Context.Env_var_map

module Outputs = struct
  include Action_intf.Outputs

  let to_string = function
    | Stdout -> "stdout"
    | Stderr -> "stderr"
    | Outputs -> "outputs"
end

module Promote_mode = Action_intf.Promote_mode

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

  let promoted_file sexp =
    match sexp with
    | List (_, [src; Atom (_, "as"); dst]) ->
      { Promote. src = Path.t src; dst = Path.t dst }
    | _ ->
      of_sexp_error sexp
        "(<file1> as <file2>) expected"

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
      ; cstr "copy" (path @> path @> nil)              (fun src dst -> Copy (src, dst))
      (*
         (* We don't expose symlink to the user yet since this might complicate things *)
         ; cstr "symlink" (a @> a @> nil) (fun src dst -> Symlink (dst, Cat src))
      *)
      ; cstr "copy#" (path @> path @> nil) (fun src dst ->
          Copy_and_add_line_directive (src, dst))
      ; cstr_loc "copy-and-add-line-directive" (path @> path @> nil) (fun loc src dst ->
          Loc.warn loc "copy-and-add-line-directive is deprecated, use copy# instead";
          Copy_and_add_line_directive (src, dst))
      ; cstr "copy#" (path @> path @> nil) (fun src dst ->
          Copy_and_add_line_directive (src, dst))
      ; cstr "system" (string @> nil) (fun cmd -> System cmd)
      ; cstr "bash"   (string @> nil) (fun cmd -> Bash   cmd)
      ; cstr "write-file" (path @> string @> nil) (fun fn s -> Write_file (fn, s))
      ; cstr_rest "promote" nil promoted_file
          (fun files -> Promote { mode = Always; files })
      ; cstr_rest "promote-if" nil promoted_file
          (fun files -> Promote { mode = If_corrected_file_exists; files })
      ]
      sexp

  let sexp_of_promoted_file (file : Promote.file) =
    Sexp.List [Path.sexp_of_t file.src; Atom "as"; Path.sexp_of_t file.dst]

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
    | Copy (x, y) ->
      List [Atom "copy"; path x; path y]
    | Symlink (x, y) ->
      List [Atom "symlink"; path x; path y]
    | Copy_and_add_line_directive (x, y) ->
      List [Atom "copy#"; path x; path y]
    | System x -> List [Atom "system"; string x]
    | Bash   x -> List [Atom "bash"; string x]
    | Write_file (x, y) -> List [Atom "write-file"; path x; string y]
    | Rename (x, y) -> List [Atom "rename"; path x; path y]
    | Remove_tree x -> List [Atom "remove-tree"; path x]
    | Mkdir x       -> List [Atom "mkdir"; path x]
    | Digest_files paths -> List [Atom "digest-files"; List (List.map paths ~f:path)]
    | Promote { mode = Always; files } ->
      List (Atom "promote" :: List.map files ~f:sexp_of_promoted_file)
    | Promote { mode = If_corrected_file_exists; files } ->
      List (Atom "promote-if" :: List.map files ~f:sexp_of_promoted_file)

  let run prog args = Run (prog, args)
  let chdir path t = Chdir (path, t)
  let setenv var value t = Setenv (var, value, t)
  let with_stdout_to path t = Redirect (Stdout, path, t)
  let with_stderr_to path t = Redirect (Stderr, path, t)
  let with_outputs_to path t = Redirect (Outputs, path, t)
  let ignore_stdout t = Ignore (Stdout, t)
  let ignore_stderr t = Ignore (Stderr, t)
  let ignore_outputs t = Ignore (Outputs, t)
  let progn ts = Progn ts
  let echo s = Echo s
  let cat path = Cat path
  let copy a b = Copy (a, b)
  let symlink a b = Symlink (a, b)
  let copy_and_add_line_directive a b = Copy_and_add_line_directive (a, b)
  let system s = System s
  let bash s = Bash s
  let write_file p s = Write_file (p, s)
  let rename a b = Rename (a, b)
  let remove_tree path = Remove_tree path
  let mkdir path = Mkdir path
  let digest_files files = Digest_files files
end

module Make_mapper
    (Src : Action_intf.Ast)
    (Dst : Action_intf.Ast)
= struct
  let rec map (t : Src.t) ~f_program ~f_string ~f_path : Dst.t =
    match t with
    | Run (prog, args) ->
      Run (f_program prog, List.map args ~f:f_string)
    | Chdir (fn, t) ->
      Chdir (f_path fn, map t ~f_program ~f_string ~f_path)
    | Setenv (var, value, t) ->
      Setenv (f_string var, f_string value, map t ~f_program ~f_string ~f_path)
    | Redirect (outputs, fn, t) ->
      Redirect (outputs, f_path fn, map t ~f_program ~f_string ~f_path)
    | Ignore (outputs, t) ->
      Ignore (outputs, map t ~f_program ~f_string ~f_path)
    | Progn l -> Progn (List.map l ~f:(fun t -> map t ~f_program ~f_string ~f_path))
    | Echo x -> Echo (f_string x)
    | Cat x -> Cat (f_path x)
    | Copy (x, y) -> Copy (f_path x, f_path y)
    | Symlink (x, y) ->
      Symlink (f_path x, f_path y)
    | Copy_and_add_line_directive (x, y) ->
      Copy_and_add_line_directive (f_path x, f_path y)
    | System x -> System (f_string x)
    | Bash x -> Bash (f_string x)
    | Write_file (x, y) -> Write_file (f_path x, f_string y)
    | Rename (x, y) -> Rename (f_path x, f_path y)
    | Remove_tree x -> Remove_tree (f_path x)
    | Mkdir x -> Mkdir (f_path x)
    | Digest_files x -> Digest_files (List.map x ~f:f_path)
    | Promote p ->
      let files =
        List.map p.files ~f:(fun { Src.Promote. src; dst } ->
          { Dst.Promote.src = f_path src; dst = f_path dst })
      in
      Promote { mode = p.mode; files }
end

module Prog = struct
  module Not_found = struct
    type t =
      { context : string
      ; program : string
      ; hint    : string option
      }

    let raise { context ; program ; hint } =
      Utils.program_not_found ?hint ~context program
  end

  type t = (Path.t, Not_found.t) result

  let t sexp = Ok (Path.t sexp)

  let sexp_of_t = function
    | Ok s -> Path.sexp_of_t s
    | Error (e : Not_found.t) -> Sexp.To_sexp.string e.program
end

module type Ast = Action_intf.Ast
  with type program = Prog.t
  with type path    = Path.t
  with type string  = String.t
module rec Ast : Ast = Ast

include Make_ast
    (Prog)
    (Path)
    (struct
      type t = string
      let t = Sexp.Of_sexp.string
      let sexp_of_t = Sexp.To_sexp.string
    end)
    (Ast)

module Unresolved = struct
  module Program = struct
    type t =
      | This   of Path.t
      | Search of string

    let of_string ~dir s =
      if String.contains s '/' then
        This (Path.relative dir s)
      else
        Search s
  end

  module type Uast = Action_intf.Ast
    with type program = Program.t
    with type path    = Path.t
    with type string  = String.t
  module rec Uast : Uast = Uast
  include Uast

  include Make_mapper(Uast)(Ast)

  let resolve t ~f =
    map t ~f_path:(fun x -> x) ~f_string:(fun x -> x)
      ~f_program:(function
        | This p -> Ok p
        | Search s -> Ok (f s))
end

module Var_expansion = struct
  module Concat_or_split = struct
    type t =
      | Concat (* default *)
      | Split  (* the variable is a "split" list of items *)
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
    | Strings (l, _) -> concat l
    | Paths   (l, _) -> concat (List.map l ~f:(string_of_path ~dir))

  let to_path ~dir = function
    | Strings (l, _) -> path_of_string ~dir (concat l)
    | Paths ([p], _) -> p
    | Paths (l,   _) ->
      path_of_string ~dir (concat (List.map l ~f:(string_of_path ~dir)))

  let to_prog_and_args ~dir exp : Unresolved.Program.t * string list =
    let module P = Unresolved.Program in
    match exp with
    | Paths   ([p], _) -> (This p, [])
    | Strings ([s], _) -> (P.of_string ~dir s, [])
    | Paths ([], _) | Strings ([], _) -> (Search "", [])
    | Paths (l, Concat) ->
      (This
         (path_of_string ~dir
            (concat (List.map l ~f:(string_of_path ~dir)))),
       [])
    | Strings (l, Concat) ->
      (P.of_string ~dir (concat l), l)
    | Paths (p :: l, Split) ->
      (This p, List.map l ~f:(string_of_path ~dir))
    | Strings (s :: l, Split) ->
      (P.of_string ~dir s, l)
end

module VE = Var_expansion
module SW = String_with_vars

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
    module Program = Unresolved.Program

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

      let prog_and_args ~dir ~f x =
        expand ~dir ~f x
          ~generic:(fun ~dir:_ s -> (Program.of_string ~dir s, []))
          ~special:VE.to_prog_and_args
          ~map:(fun x -> (x, []))
    end

    let rec expand t ~dir ~map_exe ~f : Unresolved.t =
      match t with
      | Run (prog, args) ->
        let args = List.concat_map args ~f:(E.strings ~dir ~f) in
        let prog, more_args = E.prog_and_args ~dir ~f prog in
        let prog =
          match prog with
          | Search _ -> prog
          | This path -> This (map_exe path)
        in
        Run (prog, more_args @ args)
      | Chdir (fn, t) ->
        let fn = E.path ~dir ~f fn in
        Chdir (fn, expand t ~dir:fn ~map_exe ~f)
      | Setenv (var, value, t) ->
        Setenv (E.string ~dir ~f var, E.string ~dir ~f value,
                expand t ~dir ~map_exe ~f)
      | Redirect (outputs, fn, t) ->
        Redirect (outputs, E.path ~dir ~f fn, expand t ~dir ~map_exe ~f)
      | Ignore (outputs, t) ->
        Ignore (outputs, expand t ~dir ~map_exe ~f)
      | Progn l -> Progn (List.map l ~f:(fun t -> expand t ~dir ~map_exe ~f))
      | Echo x -> Echo (E.string ~dir ~f x)
      | Cat x -> Cat (E.path ~dir ~f x)
      | Copy (x, y) ->
        Copy (E.path ~dir ~f x, E.path ~dir ~f y)
      | Symlink (x, y) ->
        Symlink (E.path ~dir ~f x, E.path ~dir ~f y)
      | Copy_and_add_line_directive (x, y) ->
        Copy_and_add_line_directive (E.path ~dir ~f x, E.path ~dir ~f y)
      | System x -> System (E.string ~dir ~f x)
      | Bash x -> Bash (E.string ~dir ~f x)
      | Write_file (x, y) -> Write_file (E.path ~dir ~f x, E.string ~dir ~f y)
      | Rename (x, y) ->
        Rename (E.path ~dir ~f x, E.path ~dir ~f y)
      | Remove_tree x ->
        Remove_tree (E.path ~dir ~f x)
      | Mkdir x -> begin
          match x with
          | Inl path -> Mkdir path
          | Inr tmpl ->
            let path = E.path ~dir ~f x in
            check_mkdir (SW.loc tmpl) path;
            Mkdir path
        end
      | Digest_files x ->
        Digest_files (List.map x ~f:(E.path ~dir ~f))
      | Promote p ->
        let files =
          List.map p.files ~f:(fun { Promote.src; dst } ->
            { Unresolved.Promote.
              src = E.path ~dir ~f src
            ; dst = Path.drop_build_context (E.path ~dir ~f dst)
            })
        in
        Promote { mode = p.mode; files }
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

    let prog_and_args ~dir ~f x =
      expand ~dir ~f x
        ~generic:(fun ~dir s -> (Unresolved.Program.of_string ~dir s, []))
        ~special:VE.to_prog_and_args
  end

  let rec partial_expand t ~dir ~map_exe ~f : Partial.t =
    match t with
    | Run (prog, args) ->
      let args =
        List.concat_map args ~f:(fun arg ->
          match E.strings ~dir ~f arg with
          | Inl args -> List.map args ~f:(fun x -> Inl x)
          | Inr _ as x -> [x])
      in
      begin
        match E.prog_and_args ~dir ~f prog with
        | Inl (prog, more_args) ->
          let more_args = List.map more_args ~f:(fun x -> Inl x) in
          let prog =
            match prog with
            | Search _ -> prog
            | This path -> This (map_exe path)
          in
          Run (Inl prog, more_args @ args)
        | Inr _ as prog ->
          Run (prog, args)
      end
    | Chdir (fn, t) -> begin
        let res = E.path ~dir ~f fn in
        match res with
        | Inl dir ->
          Chdir (res, partial_expand t ~dir ~map_exe ~f)
        | Inr fn ->
          let loc = SW.loc fn in
          Loc.fail loc
            "This directory cannot be evaluated statically.\n\
             This is not allowed by jbuilder"
      end
    | Setenv (var, value, t) ->
      Setenv (E.string ~dir ~f var, E.string ~dir ~f value,
              partial_expand t ~dir ~map_exe ~f)
    | Redirect (outputs, fn, t) ->
      Redirect (outputs, E.path ~dir ~f fn, partial_expand t ~dir ~map_exe ~f)
    | Ignore (outputs, t) ->
      Ignore (outputs, partial_expand t ~dir ~map_exe ~f)
    | Progn l -> Progn (List.map l ~f:(fun t -> partial_expand t ~dir ~map_exe ~f))
    | Echo x -> Echo (E.string ~dir ~f x)
    | Cat x -> Cat (E.path ~dir ~f x)
    | Copy (x, y) ->
      Copy (E.path ~dir ~f x, E.path ~dir ~f y)
    | Symlink (x, y) ->
      Symlink (E.path ~dir ~f x, E.path ~dir ~f y)
    | Copy_and_add_line_directive (x, y) ->
      Copy_and_add_line_directive (E.path ~dir ~f x, E.path ~dir ~f y)
    | System x -> System (E.string ~dir ~f x)
    | Bash x -> Bash (E.string ~dir ~f x)
    | Write_file (x, y) -> Write_file (E.path ~dir ~f x, E.string ~dir ~f y)
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
    | Digest_files x ->
      Digest_files (List.map x ~f:(E.path ~dir ~f))
    | Promote p ->
      let files =
        List.map p.files ~f:(fun { Promote.src; dst } ->
          { Partial.Promote.
            src = E.path ~dir ~f src
          ; dst = E.path ~dir ~f dst
          })
      in
      Promote { mode = p.mode; files }
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
  | Copy _
  | Symlink _
  | Copy_and_add_line_directive _
  | System _
  | Bash _
  | Write_file _
  | Rename _
  | Remove_tree _
  | Mkdir _
  | Digest_files _
  | Promote _ -> acc

include Make_mapper(Ast)(Ast)

let updated_files =
  let rec loop acc t =
    let acc =
      match t with
      | Write_file (fn, _) -> Path.Set.add fn acc
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

let exec_run ~ectx ~dir ~env_extra ~stdout_to ~stderr_to prog args =
  begin match ectx.context with
   | None
   | Some { Context.for_host = None; _ } -> ()
   | Some ({ Context.for_host = Some host; _ } as target) ->
     let invalid_prefix prefix =
       match Path.descendant prog ~of_:(Path.of_string prefix) with
       | None -> ()
       | Some _ ->
         die "Context %s has a host %s.@.It's not possible to execute binary %a \
              in it.@.@.This is a bug and should be reported upstream."
           target.name host.name Path.pp prog in
     invalid_prefix ("_build/" ^ target.name);
     invalid_prefix ("_build/install/" ^ target.name);
  end;
  let stdout_to = get_std_output stdout_to in
  let stderr_to = get_std_output stderr_to in
  let env = Context.extend_env ~vars:env_extra ~env:ectx.env in
  Future.run Strict ~dir:(Path.to_string dir) ~env ~stdout_to ~stderr_to
    ~purpose:ectx.purpose
    (Path.reach_for_running ~from:dir prog) args

let exec_echo stdout_to str =
  return
    (match stdout_to with
     | None -> print_string str; flush stdout
     | Some (_, oc) -> output_string oc str)

let rec exec t ~ectx ~dir ~env_extra ~stdout_to ~stderr_to =
  match t with
  | Run (Error e, _) ->
    Prog.Not_found.raise e
  | Run (Ok prog, args) ->
    exec_run ~ectx ~dir ~env_extra ~stdout_to ~stderr_to prog args
  | Chdir (dir, t) ->
    exec t ~ectx ~dir ~env_extra ~stdout_to ~stderr_to
  | Setenv (var, value, t) ->
    exec t ~ectx ~dir ~stdout_to ~stderr_to
      ~env_extra:(Env_var_map.add env_extra ~key:var ~data:value)
  | Redirect (Stdout, fn, Echo s) ->
    Io.write_file (Path.to_string fn) s;
    return ()
  | Redirect (outputs, fn, t) ->
    redirect ~ectx ~dir outputs fn t ~env_extra ~stdout_to ~stderr_to
  | Ignore (outputs, t) ->
    redirect ~ectx ~dir outputs Config.dev_null t ~env_extra ~stdout_to ~stderr_to
  | Progn l ->
    exec_list l ~ectx ~dir ~env_extra ~stdout_to ~stderr_to
  | Echo str -> exec_echo stdout_to str
  | Cat fn ->
    Io.with_file_in (Path.to_string fn) ~f:(fun ic ->
      let oc =
        match stdout_to with
        | None -> stdout
        | Some (_, oc) -> oc
      in
      Io.copy_channels ic oc);
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
        let directive =
          if List.mem (Path.extension fn) ~set:[".c"; ".cpp"; ".h"] then
            "line"
          else
            ""
        in
        Printf.fprintf oc "#%s 1 %S\n" directive (Path.to_string fn);
        Io.copy_channels ic oc));
    return ()
  | System cmd ->
    let path, arg =
      Utils.system_shell_exn ~needed_to:"interpret (system ...) actions"
    in
    exec_run ~ectx ~dir ~env_extra ~stdout_to ~stderr_to path [arg; cmd]
  | Bash cmd ->
    exec_run ~ectx ~dir ~env_extra ~stdout_to ~stderr_to
      (Utils.bash_exn ~needed_to:"interpret (bash ...) actions")
      ["-e"; "-u"; "-o"; "pipefail"; "-c"; cmd]
  | Write_file (fn, s) ->
    Io.write_file (Path.to_string fn) s;
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
  | Digest_files paths ->
    let s =
      let data =
        List.map paths ~f:(fun fn ->
          (fn, Utils.Cached_digest.file fn))
      in
      Digest.string
        (Marshal.to_string data [])
    in
    exec_echo stdout_to s
  | Promote { mode; files } ->
    let promote_mode = !Clflags.promote_mode in
    if promote_mode = Ignore then
      return ()
    else begin
      let files =
        match mode with
        | Always -> files
        | If_corrected_file_exists ->
          List.filter files ~f:(fun file -> Path.exists file.Promote.src)
      in
      let not_ok =
        List.filter files ~f:(fun { Promote. src; dst } ->
          let src_contents = Io.read_file (Path.to_string src) in
          let dst_contents = Io.read_file (Path.to_string dst) in
          src_contents <> dst_contents)
      in
      match not_ok with
      | [] -> return ()
      | _ ->
        if promote_mode = Copy then
          Future.Scheduler.at_exit_after_waiting_for_commands (fun () ->
            List.iter not_ok ~f:(fun { Promote. src; dst } ->
              Format.eprintf "Promoting %s to %s.@."
                (Path.to_string_maybe_quoted src)
                (Path.to_string_maybe_quoted dst);
              Io.copy_file ~src:(Path.to_string src) ~dst:(Path.to_string dst)));
        Future.all_unit (List.map not_ok ~f:(fun { Promote. src; dst } ->
          Diff.print dst src))
    end

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
    ; map t ~f_string:(fun x -> x) ~f_path:sandboxed
        ~f_program:(function
        | Ok p -> Ok (sandboxed p)
        | Error _ as e -> e)
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

  let infer_promote mode files ~init ~f =
    if mode = Promote_mode.If_corrected_file_exists ||
       !Clflags.promote_mode = Ignore then
      init
    else
      List.fold_left files ~init ~f

  let ( +@ ) acc fn = { acc with targets = S.add fn acc.targets }
  let ( +< ) acc fn = { acc with deps    = S.add fn acc.deps    }

  let rec infer acc t =
    match t with
    | Run (Ok prog, _)        -> acc +< prog
    | Run (Error _, _) -> acc
    | Redirect (_, fn, t)  -> infer (acc +@ fn) t
    | Cat fn               -> acc +< fn
    | Write_file (fn, _)  -> acc +@ fn
    | Rename (src, dst)    -> acc +< src +@ dst
    | Copy (src, dst)
    | Copy_and_add_line_directive (src, dst)
    | Symlink (src, dst) -> acc +< src +@ dst
    | Chdir (_, t)
    | Setenv (_, _, t)
    | Ignore (_, t) -> infer acc t
    | Progn l -> List.fold_left l ~init:acc ~f:infer
    | Digest_files l -> List.fold_left l ~init:acc ~f:(+<)
    | Promote { mode; files } ->
      infer_promote mode files ~init:acc ~f:(fun acc file -> acc +< file.Promote.src)
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
    | Write_file (fn, _)  -> acc +@? fn
    | Rename (src, dst)    -> acc +<? src +@? dst
    | Copy (src, dst)
    | Copy_and_add_line_directive (src, dst)
    | Symlink (src, dst) -> acc +<? src +@? dst
    | Chdir (_, t)
    | Setenv (_, _, t)
    | Ignore (_, t) -> partial acc t
    | Progn l -> List.fold_left l ~init:acc ~f:partial
    | Digest_files l -> List.fold_left l ~init:acc ~f:(+<?)
    | Promote { mode; files } ->
      infer_promote mode files ~init:acc ~f:(fun acc file ->
        acc +<? file.Unexpanded.Partial.Promote.src)
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
    | Write_file (fn, _)  -> acc +@? fn
    | Rename (src, dst)    -> acc +<? src +@? dst
    | Copy (src, dst)
    | Copy_and_add_line_directive (src, dst)
    | Symlink (src, dst) -> acc +<? src +@? dst
    | Chdir (_, t)
    | Setenv (_, _, t)
    | Ignore (_, t) -> partial_with_all_targets acc t
    | Progn l -> List.fold_left l ~init:acc ~f:partial_with_all_targets
    | Digest_files l -> List.fold_left l ~init:acc ~f:(+<?)
    | Promote { mode; files } ->
      infer_promote mode files ~init:acc ~f:(fun acc file ->
        acc +<? file.Unexpanded.Partial.Promote.src)
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
