open! Stdune

module Kind = struct
  type t =
    | Optional
    | Required

  let merge a b =
    match a, b with
    | Optional, Optional -> Optional
    | _ -> Required

  let of_optional b = if b then Optional else Required

  let to_sexp t =
    Sexp.Atom
      (match t with
       | Optional -> "optional"
       | Required -> "required")
end

type t = Kind.t Lib_name.Map.t

let merge a b =
  Lib_name.Map.merge a b ~f:(fun _ a b ->
    match a, b with
    | None, None -> None
    | x, None | None, x -> x
    | Some a, Some b -> Some (Kind.merge a b))

let to_sexp l =
  Sexp.List
    (Lib_name.Map.to_list l
     |> List.map ~f:(fun (name, kind) ->
       Sexp.List [Kind.to_sexp kind; Lib_name.to_sexp name]))
