open Import
open Sexp.Of_sexp

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
      ; cstr "diff" (path @> path @> nil)
          (fun file1 file2 -> Diff { optional = false; file1; file2 })
      ; cstr "diff?" (path @> path @> nil)
          (fun file1 file2 -> Diff { optional = true ; file1; file2 })
      ]
      sexp

  let rec sexp_of_t : _ -> Sexp.t =
    let path = Path.sexp_of_t and string = String.sexp_of_t in
    function
    | Run (a, xs) -> List (Sexp.unsafe_atom_of_string "run"
                           :: Program.sexp_of_t a :: List.map xs ~f:string)
    | Chdir (a, r) -> List [Sexp.unsafe_atom_of_string "chdir" ;
                            path a ; sexp_of_t r]
    | Setenv (k, v, r) -> List [Sexp.unsafe_atom_of_string "setenv" ;
                                string k ; string v ; sexp_of_t r]
    | Redirect (outputs, fn, r) ->
      List [ Sexp.atom (sprintf "with-%s-to" (Outputs.to_string outputs))
           ; path fn
           ; sexp_of_t r
           ]
    | Ignore (outputs, r) ->
      List [ Sexp.atom (sprintf "ignore-%s" (Outputs.to_string outputs))
           ; sexp_of_t r
           ]
    | Progn l -> List (Sexp.unsafe_atom_of_string "progn"
                       :: List.map l ~f:sexp_of_t)
    | Echo x -> List [Sexp.unsafe_atom_of_string "echo"; string x]
    | Cat x -> List [Sexp.unsafe_atom_of_string "cat"; path x]
    | Copy (x, y) ->
      List [Sexp.unsafe_atom_of_string "copy"; path x; path y]
    | Symlink (x, y) ->
      List [Sexp.unsafe_atom_of_string "symlink"; path x; path y]
    | Copy_and_add_line_directive (x, y) ->
      List [Sexp.unsafe_atom_of_string "copy#"; path x; path y]
    | System x -> List [Sexp.unsafe_atom_of_string "system"; string x]
    | Bash   x -> List [Sexp.unsafe_atom_of_string "bash"; string x]
    | Write_file (x, y) -> List [Sexp.unsafe_atom_of_string "write-file";
                                 path x; string y]
    | Rename (x, y) -> List [Sexp.unsafe_atom_of_string "rename";
                             path x; path y]
    | Remove_tree x -> List [Sexp.unsafe_atom_of_string "remove-tree"; path x]
    | Mkdir x       -> List [Sexp.unsafe_atom_of_string "mkdir"; path x]
    | Digest_files paths -> List [Sexp.unsafe_atom_of_string "digest-files";
                                  List (List.map paths ~f:path)]
    | Diff { optional = false; file1; file2 } ->
      List [Sexp.unsafe_atom_of_string "diff"; path file1; path file2]
    | Diff { optional = true; file1; file2 } ->
      List [Sexp.unsafe_atom_of_string "diff?"; path file1; path file2]

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
  let diff ?(optional=false) file1 file2 = Diff { optional; file1; file2 }
end

module Make_mapper
    (Src : Action_intf.Ast)
    (Dst : Action_intf.Ast)
= struct
  let rec map (t : Src.t) ~dir ~f_program ~f_string ~f_path : Dst.t =
    match t with
    | Run (prog, args) ->
      Run (f_program ~dir prog, List.map args ~f:(f_string ~dir))
    | Chdir (fn, t) ->
      Chdir (f_path ~dir fn, map t ~dir:fn ~f_program ~f_string ~f_path)
    | Setenv (var, value, t) ->
      Setenv (f_string ~dir var, f_string ~dir value, map t ~dir ~f_program ~f_string ~f_path)
    | Redirect (outputs, fn, t) ->
      Redirect (outputs, f_path ~dir fn, map t ~dir ~f_program ~f_string ~f_path)
    | Ignore (outputs, t) ->
      Ignore (outputs, map t ~dir ~f_program ~f_string ~f_path)
    | Progn l -> Progn (List.map l ~f:(fun t -> map t ~dir ~f_program ~f_string ~f_path))
    | Echo x -> Echo (f_string ~dir x)
    | Cat x -> Cat (f_path ~dir x)
    | Copy (x, y) -> Copy (f_path ~dir x, f_path ~dir y)
    | Symlink (x, y) ->
      Symlink (f_path ~dir x, f_path ~dir y)
    | Copy_and_add_line_directive (x, y) ->
      Copy_and_add_line_directive (f_path ~dir x, f_path ~dir y)
    | System x -> System (f_string ~dir x)
    | Bash x -> Bash (f_string ~dir x)
    | Write_file (x, y) -> Write_file (f_path ~dir x, f_string ~dir y)
    | Rename (x, y) -> Rename (f_path ~dir x, f_path ~dir y)
    | Remove_tree x -> Remove_tree (f_path ~dir x)
    | Mkdir x -> Mkdir (f_path ~dir x)
    | Digest_files x -> Digest_files (List.map x ~f:(f_path ~dir))
    | Diff { optional; file1; file2 } ->
      Diff { optional; file1 = f_path ~dir file1; file2 = f_path ~dir file2 }
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

module String_with_sexp = struct
  type t = string
  let t = Sexp.Of_sexp.string
  let sexp_of_t = Sexp.To_sexp.string
end

include Make_ast
    (Prog)
    (Path)
    (String_with_sexp)
    (Ast)

module For_shell = struct
  module type Ast = Action_intf.Ast
    with type program = string
    with type path    = string
    with type string  = string
  module rec Ast : Ast = Ast

  include Make_ast
      (String_with_sexp)
      (String_with_sexp)
      (String_with_sexp)
      (Ast)
end

module Relativise = Make_mapper(Ast)(For_shell.Ast)

let for_shell t =
  Relativise.map t
    ~dir:Path.root
    ~f_string:(fun ~dir:_ x -> x)
    ~f_path:(fun ~dir x -> Path.reach x ~from:dir)
    ~f_program:(fun ~dir x ->
      match x with
      | Ok p -> Path.reach p ~from:dir
      | Error e -> e.program)

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
    map t
      ~dir:Path.root
      ~f_path:(fun ~dir:_ x -> x)
      ~f_string:(fun ~dir:_ x -> x)
      ~f_program:(fun ~dir:_ -> function
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

  let is_multivalued = function
    | Paths (_, Split) | Strings (_, Split) -> true
    | Paths (_, Concat) | Strings (_, Concat) -> false

  type context = Path.t (* For String_with_vars.Expand_to *)

  let concat = function
    | [s] -> s
    | l -> String.concat ~sep:" " l

  let string_of_path ~dir p = Path.reach ~from:dir p
  let path_of_string dir s = Path.relative dir s

  let to_strings dir = function
    | Strings (l, Split ) -> l
    | Strings (l, Concat) -> [concat l]
    | Paths   (l, Split ) -> List.map l ~f:(string_of_path ~dir)
    | Paths   (l, Concat) -> [concat (List.map l ~f:(string_of_path ~dir))]

  let to_string (dir: context) = function
    | Strings (l, _) -> concat l
    | Paths   (l, _) -> concat (List.map l ~f:(string_of_path ~dir))

  let to_path dir = function
    | Strings (l, _) -> path_of_string dir (concat l)
    | Paths ([p], _) -> p
    | Paths (l,   _) ->
      path_of_string dir (concat (List.map l ~f:(string_of_path ~dir)))

  let to_prog_and_args dir exp : Unresolved.Program.t * string list =
    let module P = Unresolved.Program in
    match exp with
    | Paths   ([p], _) -> (This p, [])
    | Strings ([s], _) -> (P.of_string ~dir s, [])
    | Paths ([], _) | Strings ([], _) -> (Search "", [])
    | Paths (l, Concat) ->
      (This
         (path_of_string dir
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
module To_VE = String_with_vars.Expand_to(VE)
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
    | Atom _ | Quoted_string _ ->
      of_sexp_errorf sexp
        "if you meant for this to be executed with bash, write (bash \"...\") instead"
    | List _ -> t sexp

  let check_mkdir loc path =
    if not (Path.is_local path) then
      Loc.fail loc
        "(mkdir ...) is not supported for paths outside of the workspace:\n\
        \  %a\n"
        Sexp.pp (List [Sexp.unsafe_atom_of_string "mkdir"; Path.sexp_of_t path])

  module Partial = struct
    module Program = Unresolved.Program

    module type Past = Action_intf.Ast
      with type program = (Program.t, String_with_vars.t) either
      with type path    = (Path.t   , String_with_vars.t) either
      with type string  = (string   , String_with_vars.t) either
    module rec Past : Past = Past

    include Past

    module E = struct
      let expand ~generic ~special ~map ~dir ~f = function
        | Left x -> map x
        | Right template ->
           match To_VE.expand dir template ~f with
           | Left  e -> special dir e
           | Right s -> generic dir s
      [@@inlined always]

      let string ~dir ~f x =
        expand ~dir ~f x
          ~generic:(fun _dir x -> x)
          ~special:VE.to_string
          ~map:(fun x -> x)

      let strings ~dir ~f x =
        expand ~dir ~f x
          ~generic:(fun _dir x -> [x])
          ~special:VE.to_strings
          ~map:(fun x -> [x])

      let path ~dir ~f x =
        expand ~dir ~f x
          ~generic:VE.path_of_string
          ~special:VE.to_path
          ~map:(fun x -> x)

      let prog_and_args ~dir ~f x =
        expand ~dir ~f x
          ~generic:(fun _dir s -> (Program.of_string ~dir s, []))
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
          | Left  path -> Mkdir path
          | Right tmpl ->
            let path = E.path ~dir ~f x in
            check_mkdir (SW.loc tmpl) path;
            Mkdir path
        end
      | Digest_files x ->
        Digest_files (List.map x ~f:(E.path ~dir ~f))
      | Diff { optional; file1; file2 } ->
        Diff { optional
             ; file1 = E.path ~dir ~f file1
             ; file2 = E.path ~dir ~f file2
             }
  end

  module E = struct
    let expand ~generic ~special ~dir ~f template =
      match To_VE.partial_expand dir template ~f with
      | Left (Left  e) -> Left (special dir e)
      | Left (Right s) -> Left (generic dir s)
      | Right _ as x -> x

    let string ~dir ~f x =
      expand ~dir ~f x
        ~generic:(fun _dir x -> x)
        ~special:VE.to_string

    let strings ~dir ~f x =
      expand ~dir ~f x
        ~generic:(fun _dir x -> [x])
        ~special:VE.to_strings

    let path ~dir ~f x =
      expand ~dir ~f x
        ~generic:VE.path_of_string
        ~special:VE.to_path

    let prog_and_args ~dir ~f x =
      expand ~dir ~f x
        ~generic:(fun dir s -> (Unresolved.Program.of_string ~dir s, []))
        ~special:VE.to_prog_and_args
  end

  let rec partial_expand t ~dir ~map_exe ~f : Partial.t =
    match t with
    | Run (prog, args) ->
      let args =
        List.concat_map args ~f:(fun arg ->
          match E.strings ~dir ~f arg with
          | Left args -> List.map args ~f:(fun x -> Left x)
          | Right _ as x -> [x])
      in
      begin
        match E.prog_and_args ~dir ~f prog with
        | Left (prog, more_args) ->
          let more_args = List.map more_args ~f:(fun x -> Left x) in
          let prog =
            match prog with
            | Search _ -> prog
            | This path -> This (map_exe path)
          in
          Run (Left prog, more_args @ args)
        | Right _ as prog ->
          Run (prog, args)
      end
    | Chdir (fn, t) -> begin
        let res = E.path ~dir ~f fn in
        match res with
        | Left dir ->
          Chdir (res, partial_expand t ~dir ~map_exe ~f)
        | Right fn ->
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
       | Left path -> check_mkdir (SW.loc x) path
       | Right _   -> ());
      Mkdir res
    | Digest_files x ->
      Digest_files (List.map x ~f:(E.path ~dir ~f))
    | Diff { optional; file1; file2 } ->
      Diff { optional
           ; file1 = E.path ~dir ~f file1
           ; file2 = E.path ~dir ~f file2
           }
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
  | Diff _ -> acc

include Make_mapper(Ast)(Ast)

let chdirs =
  let rec loop acc t =
    let acc =
      match t with
      | Chdir (dir, _) -> Path.Set.add acc dir
      | _ -> acc
    in
    fold_one_step t ~init:acc ~f:loop
  in
  fun t -> loop Path.Set.empty t

open Fiber.O

let get_std_output : _ -> Process.std_output_to = function
  | None          -> Terminal
  | Some (fn, oc) -> Opened_file { filename = fn; tail = false; desc = Channel oc }

module Promotion = struct
  module File = struct
    type t =
      { src : Path.t
      ; dst : Path.t
      }

    let t = function
      | Sexp.Ast.List (_, [src; Atom (_, A "as"); dst]) ->
        { src = Path.t src
        ; dst = Path.t dst
        }
      | sexp ->
        Sexp.Of_sexp.of_sexp_errorf sexp "(<file> as <file>) expected"

    let sexp_of_t { src; dst } =
      Sexp.List [Path.sexp_of_t src; Sexp.unsafe_atom_of_string "as";
                 Path.sexp_of_t dst]

    let db : t list ref = ref []

    let register t = db := t :: !db

    let promote { src; dst } =
      Format.eprintf "Promoting %s to %s.@."
        (Path.to_string_maybe_quoted src)
        (Path.to_string_maybe_quoted dst);
      Io.copy_file
        ~src:(Path.to_string src)
        ~dst:(Path.to_string dst)
  end

  let db_file = "_build/.to-promote"

  let dump_db db =
    if Sys.file_exists "_build" then begin
      match db with
      | [] -> if Sys.file_exists db_file then Sys.remove db_file
      | l ->
        Io.write_file db_file
          (String.concat ~sep:""
             (List.map l ~f:(fun x -> Sexp.to_string (File.sexp_of_t x) ^ "\n")))
    end

  let load_db () =
    if Sys.file_exists db_file then
      Sexp.load ~fname:db_file ~mode:Many
      |> List.map ~f:File.t
    else
      []

  let group_by_targets db =
    List.map db ~f:(fun { File. src; dst } ->
      (dst, src))
    |> Path.Map.of_list_multi
    (* Sort the list of possible sources for deterministic behavior *)
    |> Path.Map.map ~f:(List.sort ~compare:Path.compare)

  let do_promote db =
    let by_targets = group_by_targets db  in
    let potential_build_contexts =
      match Path.readdir Path.build_dir with
      | exception _ -> []
      | files ->
        List.filter_map files ~f:(fun fn ->
          if fn = "" || fn.[0] = '.' || fn = "install" then
            None
          else
            let path = Path.(relative build_dir) fn in
            Option.some_if (Path.is_directory path) path)
    in
    let dirs_to_clear_from_cache = Path.root :: potential_build_contexts in
    Path.Map.iteri by_targets ~f:(fun dst srcs ->
      match srcs with
      | [] -> assert false
      | src :: others ->
        (* We remove the files from the digest cache to force a rehash
           on the next run. We do this because on OSX [mtime] is not
           precise enough and if a file is modified and promoted
           quickly, it will look like it hasn't changed even though it
           might have. *)
        List.iter dirs_to_clear_from_cache ~f:(fun dir ->
          Utils.Cached_digest.remove (Path.append dir dst));
        File.promote { src; dst };
        List.iter others ~f:(fun path ->
          Format.eprintf " -> ignored %s.@."
            (Path.to_string_maybe_quoted path)))

  let finalize () =
    let db =
      if !Clflags.auto_promote then
        (do_promote !File.db; [])
      else
        !File.db
    in
    dump_db db

  let promote_files_registered_in_last_run () =
    let db = load_db () in
    do_promote db;
    dump_db []
end

type exec_context =
  { context : Context.t option
  ; purpose : Process.purpose
  }

let exec_run_direct ~ectx ~dir ~env ~stdout_to ~stderr_to prog args =
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
  Process.run Strict ~dir:(Path.to_string dir) ~env
    ~stdout_to ~stderr_to
    ~purpose:ectx.purpose
    (Path.reach_for_running ~from:dir prog) args

let exec_run ~stdout_to ~stderr_to =
  let stdout_to = get_std_output stdout_to in
  let stderr_to = get_std_output stderr_to in
  exec_run_direct ~stdout_to ~stderr_to

let exec_echo stdout_to str =
  Fiber.return
    (match stdout_to with
     | None -> print_string str; flush stdout
     | Some (_, oc) -> output_string oc str)

let rec exec t ~ectx ~dir ~env ~stdout_to ~stderr_to =
  match t with
  | Run (Error e, _) ->
    Prog.Not_found.raise e
  | Run (Ok prog, args) ->
    exec_run ~ectx ~dir ~env ~stdout_to ~stderr_to prog args
  | Chdir (dir, t) ->
    exec t ~ectx ~dir ~env ~stdout_to ~stderr_to
  | Setenv (var, value, t) ->
    exec t ~ectx ~dir ~stdout_to ~stderr_to
      ~env:(Env.add env ~var ~value)
  | Redirect (Stdout, fn, Echo s) ->
    Io.write_file (Path.to_string fn) s;
    Fiber.return ()
  | Redirect (outputs, fn, Run (Ok prog, args)) ->
    let out = Process.File (Path.to_string fn) in
    let stdout_to, stderr_to =
      match outputs with
      | Stdout -> (out, get_std_output stderr_to)
      | Stderr -> (get_std_output stdout_to, out)
      | Outputs -> (out, out)
    in
    exec_run_direct ~ectx ~dir ~env ~stdout_to ~stderr_to prog args
  | Redirect (outputs, fn, t) ->
    redirect ~ectx ~dir outputs fn t ~env ~stdout_to ~stderr_to
  | Ignore (outputs, t) ->
    redirect ~ectx ~dir outputs Config.dev_null t ~env ~stdout_to ~stderr_to
  | Progn l ->
    exec_list l ~ectx ~dir ~env ~stdout_to ~stderr_to
  | Echo str -> exec_echo stdout_to str
  | Cat fn ->
    Io.with_file_in (Path.to_string fn) ~f:(fun ic ->
      let oc =
        match stdout_to with
        | None -> stdout
        | Some (_, oc) -> oc
      in
      Io.copy_channels ic oc);
    Fiber.return ()
  | Copy (src, dst) ->
    Io.copy_file ~src:(Path.to_string src) ~dst:(Path.to_string dst);
    Fiber.return ()
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
          (* @@DRA Win32 remove read-only attribute needed when symlinking enabled *)
          Unix.unlink dst;
          Unix.symlink src dst
        end
      | exception _ ->
        Unix.symlink src dst
    end;
    Fiber.return ()
  | Copy_and_add_line_directive (src, dst) ->
    Io.with_file_in (Path.to_string src) ~f:(fun ic ->
      Io.with_file_out (Path.to_string dst) ~f:(fun oc ->
        let fn = Path.drop_optional_build_context src in
        let directive =
          if List.mem (Path.extension fn) ~set:[".c"; ".cpp"; ".h"] then
            "line"
          else
            ""
        in
        Printf.fprintf oc "#%s 1 %S\n" directive (Path.to_string fn);
        Io.copy_channels ic oc));
    Fiber.return ()
  | System cmd ->
    let path, arg =
      Utils.system_shell_exn ~needed_to:"interpret (system ...) actions"
    in
    exec_run ~ectx ~dir ~env ~stdout_to ~stderr_to path [arg; cmd]
  | Bash cmd ->
    exec_run ~ectx ~dir ~env ~stdout_to ~stderr_to
      (Utils.bash_exn ~needed_to:"interpret (bash ...) actions")
      ["-e"; "-u"; "-o"; "pipefail"; "-c"; cmd]
  | Write_file (fn, s) ->
    Io.write_file (Path.to_string fn) s;
    Fiber.return ()
  | Rename (src, dst) ->
    Unix.rename (Path.to_string src) (Path.to_string dst);
    Fiber.return ()
  | Remove_tree path ->
    Path.rm_rf path;
    Fiber.return ()
  | Mkdir path ->
    (match Path.kind path with
     | External _ ->
       (* Internally we make sure never to do that, and [Unexpanded.*expand] check that *)
       Sexp.code_error
         "(mkdir ...) is not supported for paths outside of the workspace"
         [ "mkdir", Path.sexp_of_t path ]
     | Local path ->
       Path.Local.mkdir_p path);
    Fiber.return ()
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
  | Diff { optional; file1; file2 } ->
    if (optional && not (Path.exists file1 && Path.exists file2)) ||
       Io.compare_files (Path.to_string file1) (Path.to_string file2) = Eq then
      Fiber.return ()
    else begin
      let is_copied_from_source_tree file =
        match Path.drop_build_context file with
        | None -> false
        | Some file -> Path.exists file
      in
      if is_copied_from_source_tree file1 &&
         not (is_copied_from_source_tree file2) then begin
        Promotion.File.register
          { src = file2
          ; dst = Option.value_exn (Path.drop_build_context file1)
          }
      end;
      Print_diff.print file1 file2
    end

and redirect outputs fn t ~ectx ~dir ~env ~stdout_to ~stderr_to =
  let fn = Path.to_string fn in
  let oc = Io.open_out fn in
  let out = Some (fn, oc) in
  let stdout_to, stderr_to =
    match outputs with
    | Stdout -> (out, stderr_to)
    | Stderr -> (stdout_to, out)
    | Outputs -> (out, out)
  in
  exec t ~ectx ~dir ~env ~stdout_to ~stderr_to >>| fun () ->
  close_out oc

and exec_list l ~ectx ~dir ~env ~stdout_to ~stderr_to =
  match l with
  | [] ->
    Fiber.return ()
  | [t] ->
    exec t ~ectx ~dir ~env ~stdout_to ~stderr_to
  | t :: rest ->
    exec t ~ectx ~dir ~env ~stdout_to ~stderr_to >>= fun () ->
    exec_list rest ~ectx ~dir ~env ~stdout_to ~stderr_to

let exec ~targets ~context t =
  let env =
    match (context : Context.t option) with
    | None   -> Env.initial
    | Some c -> c.env
  in
  let targets = Path.Set.to_list targets in
  let purpose = Process.Build_job targets in
  let ectx = { purpose; context } in
  exec t ~ectx ~dir:Path.root ~env ~stdout_to:None ~stderr_to:None

let sandbox t ~sandboxed ~deps ~targets =
  Progn
    [ Progn (List.filter_map deps ~f:(fun path ->
        if Path.is_local path then
          Some (Ast.Symlink (path, sandboxed path))
        else
          None))
    ; map t
        ~dir:Path.root
        ~f_string:(fun ~dir:_ x -> x)
        ~f_path:(fun ~dir:_ p -> sandboxed p)
        ~f_program:(fun ~dir:_ x -> Result.map x ~f:sandboxed)
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

  module type Pset = sig
    type t
    val empty : t
    val diff : t -> t -> t
  end

  module type Outcome = sig
    type path_set
    type t =
      { deps    : path_set
      ; targets : path_set
      }
  end

  module type Primitives = sig
    type path
    type program
    type outcome
    val ( +@ ) : outcome -> path -> outcome
    val ( +< ) : outcome -> path -> outcome
    val ( +<! ) : outcome -> program -> outcome
  end

  module Make
      (Ast : Action_intf.Ast)
      (Pset : Pset)
      (Out : Outcome with type path_set := Pset.t)
      (Prim : Primitives
       with type path := Ast.path
       with type program := Ast.program
       with type outcome := Out.t) =
  struct
    open Ast
    open Out
    open Prim
    let rec infer acc t =
      match t with
      | Run (prog, _) -> acc +<! prog
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
      | Diff { optional; file1; file2 } ->
        if optional then acc else acc +< file1 +< file2
      | Echo _
      | System _
      | Bash _
      | Remove_tree _
      | Mkdir _ -> acc

    let infer t =
      let { deps; targets } =
        infer { deps = Pset.empty; targets = Pset.empty } t
      in
      (* A file can be inferred as both a dependency and a target,
         for instance:

         {[
           (progn (copy a b) (copy b c))
         ]}
      *)
      { deps = Pset.diff deps targets; targets }
  end [@@inline always]

  include Make(Ast)(S)(Outcome)(struct
      let ( +@ ) acc fn = { acc with targets = S.add acc.targets fn }
      let ( +< ) acc fn = { acc with deps    = S.add acc.deps    fn }
      let ( +<! ) acc prog =
        match prog with
        | Ok p -> acc +< p
        | Error _ -> acc
    end)

  module Partial = Make(Unexpanded.Partial.Past)(S)(Outcome)(struct
      let ( +@ ) acc fn =
        match fn with
        | Left  fn -> { acc with targets = S.add acc.targets fn }
        | Right _  -> acc
      let ( +< ) acc fn =
        match fn with
        | Left  fn -> { acc with deps    = S.add acc.deps fn }
        | Right _  -> acc
      let ( +<! ) acc fn =
        match (fn : Unexpanded.Partial.program) with
        | Left  (This fn) -> { acc with deps = S.add acc.deps fn }
        | Left  (Search _) | Right _ -> acc
    end)

  module Partial_with_all_targets = Make(Unexpanded.Partial.Past)(S)(Outcome)(struct
      let ( +@ ) acc fn =
        match fn with
        | Left  fn -> { acc with targets = S.add acc.targets fn }
        | Right sw ->
          Loc.fail (SW.loc sw) "Cannot determine this target statically."
      let ( +< ) acc fn =
        match fn with
        | Left  fn -> { acc with deps    = S.add acc.deps fn }
        | Right _  -> acc
      let ( +<! ) acc fn =
        match (fn : Unexpanded.Partial.program) with
        | Left  (This fn) -> { acc with deps = S.add acc.deps fn }
        | Left  (Search _) | Right _ -> acc
    end)

  let partial ~all_targets t =
    if all_targets then
      Partial_with_all_targets.infer t
    else
      Partial.infer t

  module S_unexp = struct
    type t = String_with_vars.t list
    let empty = []
    let diff a _ = a
  end

  module Outcome_unexp = struct
    type t =
      { deps    : S_unexp.t
      ; targets : S_unexp.t
      }
  end

  module Unexp = Make(Unexpanded.Uast)(S_unexp)(Outcome_unexp)(struct
      open Outcome_unexp
      let ( +@ ) acc fn = { acc with targets = fn :: acc.targets }
      let ( +< ) acc _ = acc
      let ( +<! )= ( +< )
    end)

  let unexpanded_targets t =
    (Unexp.infer t).targets
end
