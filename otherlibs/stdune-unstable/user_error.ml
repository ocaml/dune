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
end

exception E of User_message.t * Annot.t option

let prefix =
  Pp.seq (Pp.tag User_message.Style.Error (Pp.verbatim "Error")) (Pp.char ':')

let make ?loc ?hints paragraphs =
  User_message.make ?loc ?hints paragraphs ~prefix

let raise ?loc ?hints ?annot paragraphs =
  raise (E (make ?loc ?hints paragraphs, annot))

let () =
  Printexc.register_printer (function
    | E (t, None) -> Some (Format.asprintf "%a@?" Pp.to_fmt (User_message.pp t))
    | E (t, Some annot) ->
      Some
        (Format.asprintf "%a (annotations: %a)@?" Pp.to_fmt (User_message.pp t)
           Pp.to_fmt (!Annot.format annot))
    | _ -> None)
