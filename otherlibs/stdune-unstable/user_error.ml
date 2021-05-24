module Annot = struct
  type t = ..

  let format = ref (fun _ -> assert false)

  module type S = sig
    type payload

    val make : payload -> t

    val check : t -> (payload -> 'a) -> (unit -> 'a) -> 'a
  end

  module Make (M : sig
    type payload

    val to_dyn : payload -> Dyn.t
  end) : S with type payload = M.payload = struct
    type payload = M.payload

    type t += A of M.payload

    let make t = A t

    let check t on_match on_failure =
      match t with
      | A t -> on_match t
      | _ -> on_failure ()

    let () =
      let f = function
        | A t -> Dyn.pp (M.to_dyn t)
        | other -> !format other
      in
      format := f
  end

  module Has_embedded_location = Make (struct
    type payload = unit

    let to_dyn = Unit.to_dyn
  end)
end

exception E of User_message.t * Annot.t list

let prefix =
  Pp.seq (Pp.tag User_message.Style.Error (Pp.verbatim "Error")) (Pp.char ':')

let make ?loc ?hints paragraphs =
  User_message.make ?loc ?hints paragraphs ~prefix

let raise ?loc ?hints ?(annots = []) paragraphs =
  raise (E (make ?loc ?hints paragraphs, annots))

let is_loc_none loc =
  match loc with
  | None -> true
  | Some loc -> loc = Loc0.none

let has_embed_location annots =
  List.exists annots ~f:(fun annot ->
      Annot.Has_embedded_location.check annot (fun () -> true) (fun () -> false))

let has_location (msg : User_message.t) annots =
  (not (is_loc_none msg.loc)) || has_embed_location annots

let () =
  Printexc.register_printer (function
    | E (t, []) -> Some (Format.asprintf "%a@?" Pp.to_fmt (User_message.pp t))
    | E (t, annots) ->
      let open Pp.O in
      let pp =
        User_message.pp t
        ++ Pp.vbox
             (Pp.concat_map annots ~f:(fun annot ->
                  Pp.box (!Annot.format annot) ++ Pp.cut))
      in
      Some (Format.asprintf "%a" Pp.to_fmt pp)
    | _ -> None)
