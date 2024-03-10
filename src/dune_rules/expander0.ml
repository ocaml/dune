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

  val project : t -> Dune_project.t
  val eval_blang : t -> Blang.t -> bool Memo.t
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

let get ~dir = (Fdecl.get db) ~dir
