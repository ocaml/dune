open Import

let isn't_allowed_in_this_position ~(source : Dune_lang.Template.Pform.t) =
  let exn =
    User_error.make
      ~loc:source.loc
      [ Pp.textf
          "%s isn't allowed in this position."
          (Dune_lang.Template.Pform.describe source)
      ]
  in
  raise (User_error.E exn)
;;

let as_in_build_dir ~what ~loc p =
  match Path.as_in_build_dir p with
  | Some p -> p
  | None ->
    User_error.raise
      ~loc
      [ Pp.textf
          "%s %s is outside the build directory. This is not allowed."
          what
          (Path.to_string_maybe_quoted p)
      ]
;;

module Deps = struct
  module T = struct
    type 'a t =
      | Without of 'a Memo.t
      | With of 'a Action_builder.t

    let return x = Without (Memo.return x)

    let map t ~f =
      match t with
      | Without t -> Without (Memo.map t ~f)
      | With t -> With (Action_builder.map t ~f)
    ;;

    let both a b =
      match a, b with
      | Without a, Without b -> Without (Memo.both a b)
      | With a, With b -> With (Action_builder.both a b)
      | Without a, With b -> With (Action_builder.both (Action_builder.of_memo a) b)
      | With a, Without b -> With (Action_builder.both a (Action_builder.of_memo b))
    ;;
  end

  include T
  include Applicative.Make (T)

  let action_builder = function
    | Without x -> Action_builder.of_memo x
    | With x -> x
  ;;

  let dep p =
    let open Action_builder.O in
    let+ () = Action_builder.path p in
    [ Value.Path p ]
  ;;
end

module Expanding_what = struct
  type t =
    | Nothing_special
    | Deps_like_field
    | User_action of Path.Build.t Targets_spec.t
    | User_action_without_targets of { what : string }
end

type value = Value.t list Deps.t

type t =
  { dir : Path.Build.t
  ; context : Context_name.t
  ; expanding_what : Expanding_what.t
  }

let make ~dir expanding_what =
  { dir; context = Install.Context.of_path dir |> Option.value_exn; expanding_what }
;;

let dir t = t.dir
let context t = t.context
let expanding_what t = t.expanding_what

module Expansion_result = struct
  type nonrec t =
    | Direct of value
    | Need_full_expander of (t -> value)
end

module Source = struct
  type t =
    { vars : (Loc.t -> Expansion_result.t) Pform.Var.Map.t
    ; forms : (Loc.t -> Pform.Macro_invocation.t -> Expansion_result.t) Pform.Macro.Map.t
    }

  let empty = { vars = Pform.Var.Map.empty; forms = Pform.Macro.Map.empty }

  let union_exn t { vars; forms } =
    { vars = Pform.Var.Map.union_exn t.vars vars
    ; forms = Pform.Macro.Map.union_exn t.forms forms
    }
  ;;

  let all = Ref.With_freeze.create empty

  let make vars forms =
    Ref.With_freeze.set all (union_exn (Ref.With_freeze.get all) { vars; forms })
  ;;

  let expand loc pform =
    Ref.With_freeze.freeze all;
    let all = Ref.With_freeze.get all in
    match (pform : Pform.t) with
    | Var var ->
      (match Pform.Var.Map.find all.vars var with
       | None -> None
       | Some p -> Some (p loc))
    | Macro macro_invocation ->
      (match Pform.Macro.Map.find all.forms macro_invocation.macro with
       | None -> None
       | Some p -> Some (p loc macro_invocation))
  ;;
end

let dev_null =
  Expansion_result.Direct (Without (Memo.return [ Value.Path Dev_null.path ]))
;;

let nothing = Expansion_result.Direct (Without (Memo.return []))

let ignoring_promoted_rules =
  Expansion_result.Direct
    (Without
       (let open Memo.O in
        let+ () = Memo.return () in
        [ Value.String (Bool.to_string !Clflags.ignore_promoted_rules) ]))
;;

let no_loc f _loc = f

let () =
  let forms = Pform.Macro.Map.empty in
  let vars =
    Pform.Var.Map.of_list_exn
      [ Dev_null, no_loc dev_null
      ; Nothing, no_loc nothing
      ; Ignoring_promoted_rules, no_loc ignoring_promoted_rules
      ]
  in
  Source.make vars forms
;;
