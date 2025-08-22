module Pp = Root.Pp
include Pp

(** This version of [Pp.compare] uses [Ordering.t] rather than returning an [int]. *)
let compare ~compare x y =
  Ordering.of_int (Pp.compare (fun a b -> Ordering.to_int (compare a b)) x y)
;;

let to_dyn tag_to_dyn t =
  let rec to_dyn t =
    let open Dyn in
    match (t : _ Pp.Ast.t) with
    | Nop -> variant "Nop" []
    | Seq (x, y) -> variant "Seq" [ to_dyn x; to_dyn y ]
    | Concat (x, y) -> variant "Concat" [ to_dyn x; list to_dyn y ]
    | Box (i, t) -> variant "Box" [ int i; to_dyn t ]
    | Vbox (i, t) -> variant "Vbox" [ int i; to_dyn t ]
    | Hbox t -> variant "Hbox" [ to_dyn t ]
    | Hvbox (i, t) -> variant "Hvbox" [ int i; to_dyn t ]
    | Hovbox (i, t) -> variant "Hovbox" [ int i; to_dyn t ]
    | Verbatim s -> variant "Verbatim" [ string s ]
    | Char c -> variant "Char" [ char c ]
    | Break (x, y) ->
      variant "Break" [ triple string int string x; triple string int string y ]
    | Newline -> variant "Newline" []
    | Text s -> variant "Text" [ string s ]
    | Tag (s, t) -> variant "Tag" [ tag_to_dyn s; to_dyn t ]
  in
  to_dyn (Pp.to_ast t)
;;
