open! Stdune
open Import
open Dsexp.Of_sexp

let ignore_loc k ~loc:_ = k

module Outputs = struct
  include Action_intf.Outputs

  let to_string = function
    | Stdout -> "stdout"
    | Stderr -> "stderr"
    | Outputs -> "outputs"
end

module Diff_mode = Action_intf.Diff_mode

module Make_ast
    (Program : Dsexp.Sexpable)
    (Path    : Dsexp.Sexpable)
    (String  : Dsexp.Sexpable)
    (Ast : Action_intf.Ast
     with type program := Program.t
     with type path    := Path.t
     with type string  := String.t) =
struct
  include Ast

  let dparse =
    let path = Path.dparse and string = String.dparse in
    Dsexp.Of_sexp.fix (fun t ->
      sum
        [ "run",
          (let%map prog = Program.dparse
           and args = repeat String.dparse
           in
           Run (prog, args))
        ; "chdir",
          (let%map dn = path
           and t = t
           in
           Chdir (dn, t))
        ; "setenv",
          (let%map k = string
           and v = string
           and t = t
           in
           Setenv (k, v, t))
        ; "with-stdout-to",
          (let%map fn = path
           and t = t
           in
           Redirect (Stdout, fn, t))
        ; "with-stderr-to",
          (let%map fn = path
           and t = t
           in
           Redirect (Stderr, fn, t))
        ; "with-outputs-to",
          (let%map fn = path
           and t = t
           in
           Redirect (Outputs, fn, t))
        ; "ignore-stdout",
          (t >>| fun t -> Ignore (Stdout, t))
        ; "ignore-stderr",
          (t >>| fun t -> Ignore (Stderr, t))
        ; "ignore-outputs",
          (t >>| fun t -> Ignore (Outputs, t))
        ; "progn",
          (repeat t >>| fun l -> Progn l)
        ; "echo",
          (let%map x = string
           and xs = repeat string
           in
           Echo (x :: xs))
        ; "cat",
          (path >>| fun x -> Cat x)
        ; "copy",
          (let%map src = path
           and dst = path
           in
           Copy (src, dst))
        ; "copy#",
          (let%map src = path
           and dst = path
           in
           Copy_and_add_line_directive (src, dst))
        ; "copy-and-add-line-directive",
          (let%map src = path
           and dst = path
           in
           Copy_and_add_line_directive (src, dst))
        ; "system",
          (string >>| fun cmd -> System cmd)
        ; "bash",
          (string >>| fun cmd -> Bash cmd)
        ; "write-file",
          (let%map fn = path
           and s = string
           in
           Write_file (fn, s))
        ; "diff",
          (let%map file1 = path
           and file2 = path
           and kind = Stanza.file_kind ()
           in
           let mode =
             match kind with
             | Jbuild -> Diff_mode.Text_jbuild
             | Dune   -> Text
           in
           Diff { optional = false; file1; file2; mode })
        ; "diff?",
          (let%map file1 = path
           and file2 = path
           and kind = Stanza.file_kind ()
           in
           let mode =
             match kind with
             | Jbuild -> Diff_mode.Text_jbuild
             | Dune   -> Text
           in
           Diff { optional = true; file1; file2; mode })
        ; "cmp",
          (let%map () = Syntax.since Stanza.syntax (1, 0)
           and file1 = path
           and file2 = path
           in
           Diff { optional = false; file1; file2; mode = Binary })
        ])

  let rec dgen =
    let open Dsexp in
    let program = Program.dgen in
    let string = String.dgen in
    let path = Path.dgen in
    function
    | Run (a, xs) ->
      List (atom "run" :: program a :: List.map xs ~f:string)
    | Chdir (a, r) -> List [atom "chdir" ; path a ; dgen r]
    | Setenv (k, v, r) -> List [atom "setenv" ; string k ; string v ; dgen r]
    | Redirect (outputs, fn, r) ->
      List [ atom (sprintf "with-%s-to" (Outputs.to_string outputs))
           ; path fn
           ; dgen r
           ]
    | Ignore (outputs, r) ->
      List [ atom (sprintf "ignore-%s" (Outputs.to_string outputs))
           ; dgen r
           ]
    | Progn l -> List (atom "progn" :: List.map l ~f:dgen)
    | Echo xs ->
      List (atom "echo" :: List.map xs ~f:string)
    | Cat x -> List [atom "cat"; path x]
    | Copy (x, y) ->
      List [atom "copy"; path x; path y]
    | Symlink (x, y) ->
      List [atom "symlink"; path x; path y]
    | Copy_and_add_line_directive (x, y) ->
      List [atom "copy#"; path x; path y]
    | System x -> List [atom "system"; string x]
    | Bash   x -> List [atom "bash"; string x]
    | Write_file (x, y) -> List [atom "write-file"; path x; string y]
    | Rename (x, y) -> List [atom "rename"; path x; path y]
    | Remove_tree x -> List [atom "remove-tree"; path x]
    | Mkdir x       -> List [atom "mkdir"; path x]
    | Digest_files paths -> List [atom "digest-files";
                                  List (List.map paths ~f:path)]
    | Diff { optional; file1; file2; mode = Binary} ->
      assert (not optional);
      List [atom "cmp"; path file1; path file2]
    | Diff { optional = false; file1; file2; mode = _ } ->
      List [atom "diff"; path file1; path file2]
    | Diff { optional = true; file1; file2; mode = _ } ->
      List [atom "diff?"; path file1; path file2]
    | Merge_files_into (srcs, extras, target) ->
      List
        [ atom "merge-files-into"
        ; List (List.map ~f:path srcs)
        ; List (List.map ~f:string extras)
        ; path target
        ]

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
  let diff ?(optional=false) ?(mode=Diff_mode.Text) file1 file2 =
    Diff { optional; file1; file2; mode }
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
    | Echo xs -> Echo (List.map xs ~f:(f_string ~dir))
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
    | Diff { optional; file1; file2; mode } ->
      Diff { optional
           ; file1 = f_path ~dir file1
           ; file2 = f_path ~dir file2
           ; mode
           }
    | Merge_files_into (sources, extras, target) ->
      Merge_files_into
        (List.map sources ~f:(f_path ~dir),
         List.map extras ~f:(f_string ~dir),
         f_path ~dir target)
end

module Prog = struct
  module Not_found = struct
    type t =
      { context : string
      ; program : string
      ; hint    : string option
      ; loc     : Loc.t option
      }

    let raise { context ; program ; hint ; loc } =
      Utils.program_not_found ?hint ~loc ~context program
  end

  type t = (Path.t, Not_found.t) result

  let dparse : t Dsexp.Of_sexp.t =
    Dsexp.Of_sexp.map Path_dsexp.dparse ~f:Result.ok

  let dgen = function
    | Ok s -> Path_dsexp.dgen s
    | Error (e : Not_found.t) -> Dsexp.To_sexp.string e.program
end

module type Ast = Action_intf.Ast
  with type program = Prog.t
  with type path    = Path.t
  with type string  = String.t
module rec Ast : Ast = Ast

module String_with_sexp = struct
  type t = string
  let dparse = Dsexp.Of_sexp.string
  let dgen = Dsexp.To_sexp.string
end

include Make_ast
    (Prog)
    (Path_dsexp)
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
      | Search of Loc.t option * string

    let of_string ~dir ~loc s =
      if String.contains s '/' then
        This (Path.relative dir s)
      else
        Search (loc, s)
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
        | Search (loc, s) -> Ok (f loc s))
end

let prog_and_args_of_values ~loc p ~dir =
  match p with
  | [] -> (Unresolved.Program.Search (loc, ""), [])
  | Value.Dir p :: _ ->
    die "%s is a directory and cannot be used as an executable"
      (Path.to_string_maybe_quoted p)
  | Value.Path p :: xs -> (This p, Value.L.to_strings ~dir xs)
  | String s :: xs ->
    ( Unresolved.Program.of_string ~loc ~dir s
    , Value.L.to_strings ~dir xs
    )

module Unexpanded = struct
  module type Uast = Action_intf.Ast
    with type program = String_with_vars.t
    with type path    = String_with_vars.t
    with type string  = String_with_vars.t
  module rec Uast : Uast = Uast

  include Make_ast(String_with_vars)(String_with_vars)(String_with_vars)(Uast)

  module Mapper = Make_mapper(Uast)(Uast)

  let remove_locs =
    let no_loc_template = String_with_vars.make_text Loc.none "" in
    fun t ->
      Mapper.map t ~dir:no_loc_template
        ~f_program:(fun ~dir:_ -> String_with_vars.remove_locs)
        ~f_path:(fun ~dir:_ -> String_with_vars.remove_locs)
        ~f_string:(fun ~dir:_ -> String_with_vars.remove_locs)

  let dparse =
    if_list
      ~then_:dparse
      ~else_:
        (loc >>| fun loc ->
         of_sexp_errorf
           loc
           "if you meant for this to be executed with bash, write (bash \"...\") instead")

  let check_mkdir loc path =
    if not (Path.is_managed path) then
      Errors.fail loc
        "(mkdir ...) is not supported for paths outside of the workspace:\n\
        \  %a\n"
        (Dsexp.pp Dune)
        (List [Dsexp.unsafe_atom_of_string "mkdir"; Path_dsexp.dgen path])

  module Partial = struct
    module Program = Unresolved.Program

    module type Past = Action_intf.Ast
      with type program = (Program.t, String_with_vars.t) either
      with type path    = (Path.t   , String_with_vars.t) either
      with type string  = (string   , String_with_vars.t) either
    module rec Past : Past = Past

    include Past

    module E = struct
      let expand ~dir ~mode ~f ~l ~r =
        Either.map ~l
          ~r:(fun s ->
            r ~loc:(Some (String_with_vars.loc s))
              (String_with_vars.expand s ~dir ~f ~mode) ~dir)

      let string =
        expand ~mode:Single
          ~l:(fun x -> x)
          ~r:(ignore_loc Value.to_string)

      let strings =
        expand ~mode:Many
          ~l:(fun x -> [x])
          ~r:(ignore_loc Value.L.to_strings)

      let path e =
        let error_loc =
          match e with
          | Left _ -> None
          | Right r -> Some (String_with_vars.loc r) in
        expand ~mode:Single
          ~l:(fun x -> x)
          ~r:(ignore_loc (Value.(to_path ?error_loc))) e

      let prog_and_args =
        expand ~mode:Many
          ~l:(fun x -> (x, []))
          ~r:prog_and_args_of_values
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
      | Echo xs -> Echo (List.concat_map xs ~f:(E.strings ~dir ~f))
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
            check_mkdir (String_with_vars.loc tmpl) path;
            Mkdir path
        end
      | Digest_files x ->
        Digest_files (List.map x ~f:(E.path ~dir ~f))
      | Diff { optional; file1; file2; mode } ->
        Diff { optional
             ; file1 = E.path ~dir ~f file1
             ; file2 = E.path ~dir ~f file2
             ; mode
             }
      | Merge_files_into (sources, extras, target) ->
        Merge_files_into
          (List.map ~f:(E.path ~dir ~f) sources,
           List.map ~f:(E.string ~dir ~f) extras,
           E.path ~dir ~f target)
  end

  module E = struct
    let expand ~dir ~mode ~f ~map x =
      match String_with_vars.partial_expand ~mode ~dir ~f x with
      | Expanded e ->
        let loc = Some (String_with_vars.loc x) in
        Left (map ~loc e ~dir)
      | Unexpanded x -> Right x

    let string = expand ~mode:Single ~map:(ignore_loc Value.to_string)
    let strings = expand ~mode:Many ~map:(ignore_loc Value.L.to_strings)
    let cat_strings = expand ~mode:Many ~map:(ignore_loc Value.L.concat)
    let path x =
      expand ~mode:Single ~map:(fun ~loc v ~dir ->
        Value.to_path ?error_loc:loc v ~dir) x
    let prog_and_args = expand ~mode:Many ~map:prog_and_args_of_values
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
          let loc = String_with_vars.loc fn in
          Errors.fail loc
            "This directory cannot be evaluated statically.\n\
             This is not allowed by dune"
      end
    | Setenv (var, value, t) ->
      Setenv (E.string ~dir ~f var, E.string ~dir ~f value,
              partial_expand t ~dir ~map_exe ~f)
    | Redirect (outputs, fn, t) ->
      Redirect (outputs, E.path ~dir ~f fn, partial_expand t ~dir ~map_exe ~f)
    | Ignore (outputs, t) ->
      Ignore (outputs, partial_expand t ~dir ~map_exe ~f)
    | Progn l -> Progn (List.map l ~f:(partial_expand ~dir ~map_exe ~f))
    | Echo xs -> Echo (List.map xs ~f:(E.cat_strings ~dir ~f))
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
       | Left path -> check_mkdir (String_with_vars.loc x) path
       | Right _   -> ());
      Mkdir res
    | Digest_files x ->
      Digest_files (List.map x ~f:(E.path ~dir ~f))
    | Diff { optional; file1; file2; mode } ->
      Diff { optional
           ; file1 = E.path ~dir ~f file1
           ; file2 = E.path ~dir ~f file2
           ; mode
           }
    | Merge_files_into (sources, extras, target) ->
      Merge_files_into
        (List.map sources ~f:(E.path ~dir ~f),
         List.map extras ~f:(E.string ~dir ~f),
         E.path ~dir ~f target)
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
  | Diff _
  | Merge_files_into _ -> acc

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
module Infer = struct
  module Outcome = struct
    type t =
      { deps    : Path.Set.t
      ; targets : Path.Set.t
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
      | Diff { optional; file1; file2; mode = _ } ->
        if optional then acc else acc +< file1 +< file2
      | Merge_files_into (sources, _extras, target) ->
        List.fold_left sources ~init:acc ~f:(+<) +@ target
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

  include Make(Ast)(Path.Set)(Outcome)(struct
      let ( +@ ) acc fn = { acc with targets = Path.Set.add acc.targets fn }
      let ( +< ) acc fn = { acc with deps    = Path.Set.add acc.deps    fn }
      let ( +<! ) acc prog =
        match prog with
        | Ok p -> acc +< p
        | Error _ -> acc
    end)

  module Partial = Make(Unexpanded.Partial.Past)(Path.Set)(Outcome)(struct
      let ( +@ ) acc fn =
        match fn with
        | Left  fn -> { acc with targets = Path.Set.add acc.targets fn }
        | Right _  -> acc
      let ( +< ) acc fn =
        match fn with
        | Left  fn -> { acc with deps    = Path.Set.add acc.deps fn }
        | Right _  -> acc
      let ( +<! ) acc fn =
        match (fn : Unexpanded.Partial.program) with
        | Left  (This fn) -> { acc with deps = Path.Set.add acc.deps fn }
        | Left  (Search _) | Right _ -> acc
    end)

  module Partial_with_all_targets = Make(Unexpanded.Partial.Past)(Path.Set)(Outcome)(struct
      let ( +@ ) acc fn =
        match fn with
        | Left  fn -> { acc with targets = Path.Set.add acc.targets fn }
        | Right sw ->
          Errors.fail (String_with_vars.loc sw)
            "Cannot determine this target statically."
      let ( +< ) acc fn =
        match fn with
        | Left  fn -> { acc with deps    = Path.Set.add acc.deps fn }
        | Right _  -> acc
      let ( +<! ) acc fn =
        match (fn : Unexpanded.Partial.program) with
        | Left  (This fn) -> { acc with deps = Path.Set.add acc.deps fn }
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
      let ( +@ ) acc fn =
        if String_with_vars.is_var fn ~name:"null" then
          acc
        else
          { acc with targets = fn :: acc.targets }
      let ( +< ) acc _ = acc
      let ( +<! )= ( +< )
    end)

  let unexpanded_targets t =
    (Unexp.infer t).targets
end

let symlink_managed_paths sandboxed deps =
  let steps =
    Path.Set.fold (Deps.paths deps)
      ~init:[]
      ~f:(fun path acc ->
        if Path.is_managed path then
          Symlink (path, sandboxed path)::acc
        else
          acc
      )
  in
  Progn steps

let sandbox t ~sandboxed ~deps ~targets : t =
  Progn
    [ symlink_managed_paths sandboxed deps
    ; map t
        ~dir:Path.root
        ~f_string:(fun ~dir:_ x -> x)
        ~f_path:(fun ~dir:_ p -> sandboxed p)
        ~f_program:(fun ~dir:_ x -> Result.map x ~f:sandboxed)
    ; Progn (List.filter_map targets ~f:(fun path ->
        if Path.is_managed path then
          Some (Rename (sandboxed path, path))
        else
          None))
    ]
