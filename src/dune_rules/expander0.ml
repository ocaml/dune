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

module type S = sig
  type t

  val expand
    :  t
    -> mode:('deferred, 'value) String_with_vars.Mode.t
    -> String_with_vars.t
    -> 'value Action_builder.t

  val project : t -> Dune_project.t
  val eval_blang : t -> Blang.t -> bool Memo.t
  val expand_str : t -> String_with_vars.t -> string Action_builder.t
  val expand_str_partial : t -> String_with_vars.t -> String_with_vars.t Action_builder.t
end

open Memo.O

type t = E : 'a Memo.t * (module S with type t = 'a) -> t

let db = Fdecl.create Dyn.opaque
let set_db = Fdecl.set db
let create e m = E (e, m)
let project (E (e, (module E))) = Memo.map e ~f:E.project

let eval_blang (E (e, (module E))) blang =
  let* e = e in
  E.eval_blang e blang
;;

let expand_str (E (e, (module E))) sw =
  let open Action_builder.O in
  let* e = Action_builder.of_memo e in
  E.expand_str e sw
;;

let expand_str_partial (E (e, (module E))) sw =
  let open Action_builder.O in
  let* e = Action_builder.of_memo e in
  E.expand_str_partial e sw
;;

let expand (E (e, (module E))) ~mode sw =
  let open Action_builder.O in
  let* e = Action_builder.of_memo e in
  E.expand e ~mode sw
;;

let expand_str_and_build_deps (E (e, (module E))) sw =
  let* e = e in
  E.expand_str e sw |> Action_builder.evaluate_and_collect_facts >>| fst
;;

let get ~dir = (Fdecl.get db) ~dir
