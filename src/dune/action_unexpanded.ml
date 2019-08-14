open! Stdune
open Import
include Action_dune_lang
module Unresolved = Action.Unresolved
module Mapper = Action_mapper.Make (Action_dune_lang) (Action_dune_lang)

let ignore_loc k ~loc:_ = k

let remove_locs =
  let no_loc_template = String_with_vars.make_text Loc.none "" in
  fun t ->
    Mapper.map t ~dir:no_loc_template
      ~f_program:(fun ~dir:_ -> String_with_vars.remove_locs)
      ~f_path:(fun ~dir:_ -> String_with_vars.remove_locs)
      ~f_target:(fun ~dir:_ -> String_with_vars.remove_locs)
      ~f_string:(fun ~dir:_ -> String_with_vars.remove_locs)

let check_mkdir loc path =
  if not (Path.is_managed path) then
    User_error.raise ~loc
      [ Pp.text
        "(mkdir ...) is not supported for paths outside of the workspace:"
      ; Pp.seq (Pp.verbatim "  ")
        (Dune_lang.pp
          (List [ Dune_lang.unsafe_atom_of_string "mkdir"; Dpath.encode path ]))
      ]

let as_in_build_dir ~loc p =
  match Path.as_in_build_dir p with
  | Some p -> p
  | None ->
    User_error.raise ?loc
      [ Pp.textf
        "target %s is outside the build directory. This is not allowed."
          (Path.to_string_maybe_quoted p)
      ]

module Partial = struct
  module Program = Unresolved.Program

  module type Past =
    Action_intf.Ast
      with type program = (Program.t, String_with_vars.t) either
      with type path = (Path.t, String_with_vars.t) either
      with type target = (Path.Build.t, String_with_vars.t) either
      with type string = (String.t, String_with_vars.t) either

  module rec Past : Past = Past

  include Past

  module E = struct
    let expand ~expander ~mode ~l ~r =
      Either.map ~l ~r:(fun s ->
        let dir = Path.build (Expander.dir expander) in
        r
          ~loc:(Some (String_with_vars.loc s))
          (Expander.expand expander ~template:s ~mode)
          ~dir)

    let string = expand ~mode:Single ~l:Fn.id ~r:(ignore_loc Value.to_string)

    let strings =
      expand ~mode:Many ~l:List.singleton ~r:(ignore_loc Value.L.to_strings)

    let loc = function
      | Left _ -> None
      | Right r -> Some (String_with_vars.loc r)

    let path e =
      let error_loc = loc e in
      expand ~mode:Single ~l:Fn.id ~r:(ignore_loc (Value.to_path ?error_loc)) e

    let target e =
      let error_loc = loc e in
      expand e ~mode:Single ~l:Fn.id
        ~r:
          (ignore_loc (fun v ~dir ->
            Value.to_path ?error_loc v ~dir |> as_in_build_dir ~loc:error_loc))

    let prog_and_args_of_values ~loc p ~dir =
      match p with
      | [] -> (Unresolved.Program.Search (loc, ""), [])
      | Value.Dir p :: _ ->
        User_error.raise ?loc
          [ Pp.textf "%s is a directory and cannot be used as an executable"
            (Path.to_string_maybe_quoted p)
          ]
      | Value.Path p :: xs -> (This p, Value.L.to_strings ~dir xs)
      | String s :: xs ->
        (Unresolved.Program.of_string ~loc ~dir s, Value.L.to_strings ~dir xs)

    let prog_and_args =
      expand ~mode:Many ~l:(fun x -> (x, [])) ~r:prog_and_args_of_values
  end

  let rec expand t ~map_exe ~expander : Unresolved.t =
    match t with
    | Run (prog, args) ->
      let args = List.concat_map args ~f:(E.strings ~expander) in
      let prog, more_args = E.prog_and_args ~expander prog in
      let prog =
        match prog with
        | Search _ -> prog
        | This path -> This (map_exe path)
      in
      Run (prog, more_args @ args)
    | Chdir (fn, t) ->
      let fn = E.path ~expander fn in
      let expander =
        (* TODO this conversion doesn't look safe. It's possible to chdir
          outside the build dir *)
        Expander.set_dir expander ~dir:(Path.as_in_build_dir_exn fn)
      in
      Chdir (fn, expand t ~expander ~map_exe)
    | Setenv (var, value, t) ->
      let var = E.string ~expander var in
      let value = E.string ~expander value in
      let expander = Expander.set_env expander ~var ~value in
      Setenv (var, value, expand t ~expander ~map_exe)
    | Redirect_out (outputs, fn, t) ->
      Redirect_out (outputs, E.target ~expander fn, expand t ~map_exe ~expander)
    | Redirect_in (inputs, fn, t) ->
      Redirect_in (inputs, E.path ~expander fn, expand t ~map_exe ~expander)
    | Ignore (outputs, t) -> Ignore (outputs, expand t ~expander ~map_exe)
    | Progn l -> Progn (List.map l ~f:(expand ~expander ~map_exe))
    | Echo xs -> Echo (List.concat_map xs ~f:(E.strings ~expander))
    | Cat x -> Cat (E.path ~expander x)
    | Copy (x, y) -> Copy (E.path ~expander x, E.target ~expander y)
    | Symlink (x, y) -> Symlink (E.path ~expander x, E.target ~expander y)
    | Copy_and_add_line_directive (x, y) ->
      Copy_and_add_line_directive (E.path ~expander x, E.target ~expander y)
    | System x -> System (E.string ~expander x)
    | Bash x -> Bash (E.string ~expander x)
    | Write_file (x, y) ->
      Write_file (E.target ~expander x, E.string ~expander y)
    | Rename (x, y) -> Rename (E.target ~expander x, E.target ~expander y)
    | Remove_tree x -> Remove_tree (E.target ~expander x)
    | Mkdir x -> (
      match x with
      | Left path -> Mkdir path
      | Right tmpl ->
        let path = E.path ~expander x in
        check_mkdir (String_with_vars.loc tmpl) path;
        Mkdir path )
    | Digest_files x -> Digest_files (List.map x ~f:(E.path ~expander))
    | Diff { optional; file1; file2; mode } ->
      Diff
        { optional
        ; file1 = E.path ~expander file1
        ; file2 = E.path ~expander file2
        ; mode
        }
    | Merge_files_into (sources, extras, target) ->
      Merge_files_into
        ( List.map ~f:(E.path ~expander) sources
        , List.map ~f:(E.string ~expander) extras
        , E.target ~expander target )
end

module E = struct
  let expand ~expander ~mode ~map x =
    let dir = Path.build (Expander.dir expander) in
    let f = Expander.expand_var_exn expander in
    match String_with_vars.partial_expand ~mode ~dir ~f x with
    | Expanded e ->
      let loc = Some (String_with_vars.loc x) in
      Left (map ~loc e ~dir)
    | Unexpanded x -> Right x

  let string = expand ~mode:Single ~map:(ignore_loc Value.to_string)

  let strings = expand ~mode:Many ~map:(ignore_loc Value.L.to_strings)

  let cat_strings = expand ~mode:Many ~map:(ignore_loc Value.L.concat)

  let path x =
    expand ~mode:Single
      ~map:(fun ~loc v ~dir -> Value.to_path ?error_loc:loc v ~dir)
      x

  let target x =
    expand ~mode:Single
      ~map:(fun ~loc v ~dir ->
        Value.to_path ?error_loc:loc v ~dir |> as_in_build_dir ~loc)
      x

  let prog_and_args = expand ~mode:Many ~map:Partial.E.prog_and_args_of_values
end

let rec partial_expand t ~map_exe ~expander : Partial.t =
  match t with
  | Run (prog, args) -> (
    let args =
      List.concat_map args ~f:(fun arg ->
        match E.strings ~expander arg with
        | Left args -> List.map args ~f:Either.left
        | Right _ as x -> [ x ])
    in
    match E.prog_and_args ~expander prog with
    | Left (prog, more_args) ->
      let more_args = List.map more_args ~f:Either.left in
      let prog =
        match prog with
        | Search _ -> prog
        | This path -> This (map_exe path)
      in
      Run (Left prog, more_args @ args)
    | Right _ as prog -> Run (prog, args) )
  | Chdir (fn, t) -> (
    let res = E.path ~expander fn in
    match res with
    | Left dir ->
      let expander =
        (* TODO this conversion doesn't look safe. It's possible to chdir
          outside the build dir *)
        Expander.set_dir expander ~dir:(Path.as_in_build_dir_exn dir)
      in
      Chdir (res, partial_expand t ~expander ~map_exe)
    | Right fn ->
      let loc = String_with_vars.loc fn in
      User_error.raise ~loc
        [ Pp.text "This directory cannot be evaluated statically."
        ; Pp.text "This is not allowed by dune"
        ] )
  | Setenv (var, value, t) ->
    let var =
      match E.string ~expander var with
      | Left l -> l
      | Right sw ->
        User_error.raise ~loc:(String_with_vars.loc sw)
          [ Pp.text "environment variable names must be static" ]
    in
    let value = E.string ~expander value in
    let expander =
      match value with
      | Left value -> Expander.set_env expander ~var ~value
      | Right _ -> Expander.hide_env expander ~var
    in
    Setenv (Left var, value, partial_expand t ~expander ~map_exe)
  | Redirect_out (outputs, fn, t) ->
    Redirect_out
      (outputs, E.target ~expander fn, partial_expand t ~expander ~map_exe)
  | Redirect_in (inputs, fn, t) ->
    Redirect_in
      (inputs, E.path ~expander fn, partial_expand t ~expander ~map_exe)
  | Ignore (outputs, t) -> Ignore (outputs, partial_expand t ~expander ~map_exe)
  | Progn l -> Progn (List.map l ~f:(partial_expand ~map_exe ~expander))
  | Echo xs -> Echo (List.map xs ~f:(E.cat_strings ~expander))
  | Cat x -> Cat (E.path ~expander x)
  | Copy (x, y) -> Copy (E.path ~expander x, E.target ~expander y)
  | Symlink (x, y) -> Symlink (E.path ~expander x, E.target ~expander y)
  | Copy_and_add_line_directive (x, y) ->
    Copy_and_add_line_directive (E.path ~expander x, E.target ~expander y)
  | System x -> System (E.string ~expander x)
  | Bash x -> Bash (E.string ~expander x)
  | Write_file (x, y) -> Write_file (E.target ~expander x, E.string ~expander y)
  | Rename (x, y) -> Rename (E.target ~expander x, E.target ~expander y)
  | Remove_tree x -> Remove_tree (E.target ~expander x)
  | Mkdir x ->
    let res = E.path ~expander x in
    ( match res with
    | Left path -> check_mkdir (String_with_vars.loc x) path
    | Right _ -> () );
    Mkdir res
  | Digest_files x -> Digest_files (List.map x ~f:(E.path ~expander))
  | Diff { optional; file1; file2; mode } ->
    Diff
      { optional
      ; file1 = E.path ~expander file1
      ; file2 = E.path ~expander file2
      ; mode
      }
  | Merge_files_into (sources, extras, target) ->
    Merge_files_into
      ( List.map sources ~f:(E.path ~expander)
      , List.map extras ~f:(E.string ~expander)
      , E.target ~expander target )

module Infer = struct
  module Outcome = struct
    type t =
      { deps : Path.Set.t
      ; targets : Path.Build.Set.t
      }
  end

  open Outcome

  module type Sets = sig
    module Targets : sig
      type t

      val empty : t
    end

    module Deps : sig
      type t

      val empty : t

      val diff : t -> Targets.t -> t
    end
  end

  module type Outcome = sig
    type path_set

    type target_set

    type t =
      { deps : path_set
      ; targets : target_set
      }
  end

  module type Primitives = sig
    type path

    type target

    type program

    type outcome

    val ( +@+ ) : outcome -> target -> outcome

    val ( +< ) : outcome -> path -> outcome

    val ( +<+ ) : outcome -> target -> outcome

    val ( +<! ) : outcome -> program -> outcome
  end

  module Make
    (Ast : Action_intf.Ast)
      (Sets : Sets)
      (Out : Outcome
        with type path_set := Sets.Deps.t
          and type target_set := Sets.Targets.t)
      (Prim : Primitives
        with type path := Ast.path
          with type target := Ast.target
            with type program := Ast.program
              with type outcome := Out.t) =
  struct
    open Ast
    open Out
    open Prim

    let rec infer acc t =
      match t with
      | Run (prog, _) -> acc +<! prog
      | Redirect_out (_, fn, t) -> infer (acc +@+ fn) t
      | Redirect_in (_, fn, t) -> infer (acc +< fn) t
      | Cat fn -> acc +< fn
      | Write_file (fn, _) -> acc +@+ fn
      | Rename (src, dst) -> acc +<+ src +@+ dst
      | Copy (src, dst)
       |Copy_and_add_line_directive (src, dst)
       |Symlink (src, dst) ->
        acc +< src +@+ dst
      | Chdir (_, t)
       |Setenv (_, _, t)
       |Ignore (_, t) ->
        infer acc t
      | Progn l -> List.fold_left l ~init:acc ~f:infer
      | Digest_files l -> List.fold_left l ~init:acc ~f:( +< )
      | Diff { optional; file1; file2; mode = _ } ->
        if optional then
          acc +< file1
        else
          acc +< file1 +< file2
      | Merge_files_into (sources, _extras, target) ->
        List.fold_left sources ~init:acc ~f:( +< ) +@+ target
      | Echo _
       |System _
       |Bash _
       |Remove_tree _
       |Mkdir _ ->
        acc

    let infer t =
      let { deps; targets } =
        infer { deps = Sets.Deps.empty; targets = Sets.Targets.empty } t
      in
      (* A file can be inferred as both a dependency and a target, for
        instance:

         {[ (progn (copy a b) (copy b c)) ]} *)
      { deps = Sets.Deps.diff deps targets; targets }
  end
  [@@inline always]

  module Sets = struct
    module Targets = Path.Build.Set

    module Deps = struct
      include Path.Set

      let diff deps targets =
        Path.Build.Set.fold targets ~init:deps ~f:(fun target acc ->
          Path.Set.remove acc (Path.build target))
    end
  end

  include Make (Action) (Sets) (Outcome)
    (struct
      let ( +@+ ) acc fn =
        { acc with targets = Path.Build.Set.add acc.targets fn }

      let ( +< ) acc fn = { acc with deps = Path.Set.add acc.deps fn }

      let ( +<+ ) acc fn =
        { acc with deps = Path.Set.add acc.deps (Path.build fn) }

      let ( +<! ) acc prog =
        match prog with
        | Ok p -> acc +< p
        | Error _ -> acc
          end)

  module Partial_with_all_targets =
    Make (Partial.Past) (Sets) (Outcome)
      (struct
        let ( +@+ ) acc fn =
          match fn with
          | Left fn -> { acc with targets = Path.Build.Set.add acc.targets fn }
          | Right sw ->
            User_error.raise ~loc:(String_with_vars.loc sw)
              [ Pp.text "Cannot determine this target statically." ]

        let ( +< ) acc fn =
          match fn with
          | Left fn -> { acc with deps = Path.Set.add acc.deps fn }
          | Right _ -> acc

        let ( +<+ ) acc fn =
          match fn with
          | Left fn ->
            { acc with deps = Path.Set.add acc.deps (Path.build fn) }
          | Right _ -> acc

        let ( +<! ) acc fn =
          match (fn : Partial.program) with
          | Left (This fn) -> { acc with deps = Path.Set.add acc.deps fn }
          | Left (Search _)
           |Right _ ->
            acc
      end)

  module Partial =
    Make (Partial.Past) (Sets) (Outcome)
      (struct
        let ( +@+ ) acc fn =
          match fn with
          | Left fn -> { acc with targets = Path.Build.Set.add acc.targets fn }
          | Right _ -> acc

        let ( +< ) acc fn =
          match fn with
          | Left fn -> { acc with deps = Path.Set.add acc.deps fn }
          | Right _ -> acc

        let ( +<+ ) acc fn =
          match fn with
          | Left fn ->
            { acc with deps = Path.Set.add acc.deps (Path.build fn) }
          | Right _ -> acc

        let ( +<! ) acc fn =
          match (fn : Partial.program) with
          | Left (This fn) -> { acc with deps = Path.Set.add acc.deps fn }
          | Left (Search _)
           |Right _ ->
            acc
      end)

  let partial ~all_targets t =
    if all_targets then
      Partial_with_all_targets.infer t
    else
      Partial.infer t

  module S_unexp = struct
    module Targets = struct
      type t = String_with_vars.t list

      let empty = []
    end

    module Deps = struct
      include Targets

      let diff a _ = a
    end
  end

  module Outcome_unexp = struct
    type t =
      { deps : S_unexp.Deps.t
      ; targets : S_unexp.Targets.t
      }
  end

  module Unexp =
    Make (Action_dune_lang) (S_unexp) (Outcome_unexp)
      (struct
        open Outcome_unexp

        let ( +@+ ) acc fn =
          if String_with_vars.is_var fn ~name:"null" then
            acc
          else
            { acc with targets = fn :: acc.targets }

        let ( +< ) acc _ = acc

        let ( +<+ ) acc _ = acc

        let ( +<! ) = ( +< )
      end)

  let unexpanded_targets t = (Unexp.infer t).targets
end
