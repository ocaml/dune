open Stdune

module Ast = struct
  type 'a t =
    | Element of 'a
    | Compl of 'a t
    | Standard
    | Union of 'a t list
    | Inter of 'a t list

  let diff a = function
    | Union []
     |Inter [] ->
      a
    | b -> Inter [ a; Compl b ]

  let inter a = Inter a

  let compl a = Compl a

  let union a = Union a

  let not_union a = compl (union a)

  let decode elt =
    let open Dune_lang.Decoder in
    let elt =
      let+ e = elt in
      Element e
    in
    let rec one (kind : Dune_lang.File_syntax.t) =
      peek_exn
      >>= function
      | Atom (loc, A "\\") -> User_error.raise ~loc [ Pp.text "unexpected \\" ]
      | Atom (_, A "")
       |Quoted_string (_, _)
       |Template _ ->
        elt
      | Atom (loc, A s) -> (
        match s with
        | ":standard" -> junk >>> return Standard
        | ":include" ->
          User_error.raise ~loc
            [ Pp.text ":include isn't supported in the predicate language" ]
        | _ when s.[0] = ':' ->
          User_error.raise ~loc [ Pp.textf "undefined symbol %s" s ]
        | _ -> elt )
      | List (_, Atom (loc, A s) :: _) -> (
        match (s, kind) with
        | ":include", _ ->
          User_error.raise ~loc
            [ Pp.text ":include isn't supported in the predicate language" ]
        | ("or" | "and" | "not"), _ -> bool_ops kind
        | s, Dune when s <> "" && s.[0] <> '-' && s.[0] <> ':' ->
          User_error.raise ~loc
            [ Pp.text
              "This atom must be quoted because it is the first element of a \
               list and doesn't start with - or:"
            ]
        | _ -> enter (many union [] kind) )
      | List _ -> enter (many union [] kind)
    and bool_ops kind =
      sum
        [ ("or", many union [] kind)
        ; ("and", many inter [] kind)
        ; ("not", many not_union [] kind)
        ]
    and many k acc kind =
      peek
      >>= function
      | None -> return (k (List.rev acc))
      | Some (Atom (_, A "\\")) ->
        junk >>> many union [] kind
        >>| fun to_remove -> diff (k (List.rev acc)) to_remove
      | Some _ ->
        let* x = one kind in
        many k (x :: acc) kind
    in
    let* kind = Stanza.file_kind () in
    match kind with
    | Dune -> many union [] kind
    | Jbuild -> one kind

  let rec to_dyn f =
    let open Dyn.Encoder in
    function
    | Element a -> f a
    | Compl a -> constr "compl" [ to_dyn f a ]
    | Standard -> string ":standard"
    | Union xs -> constr "or" (List.map ~f:(to_dyn f) xs)
    | Inter xs -> constr "and" (List.map ~f:(to_dyn f) xs)
end

type t = (string -> bool) Ast.t

let to_dyn t = Ast.to_dyn (fun _ -> Dyn.Encoder.string "opaque") t

let decode : t Dune_lang.Decoder.t =
  let open Dune_lang.Decoder in
  Ast.decode (Glob.decode >>| Glob.test)

let empty = Ast.Union []

let rec exec t ~standard elem =
  match (t : _ Ast.t) with
  | Compl t -> not (exec t ~standard elem)
  | Element f -> f elem
  | Union xs -> List.exists ~f:(fun t -> exec t ~standard elem) xs
  | Inter xs -> List.for_all ~f:(fun t -> exec t ~standard elem) xs
  | Standard -> exec standard ~standard elem

let filter (t : t) ~standard elems =
  match t with
  | Inter []
   |Union [] ->
    []
  | _ -> List.filter elems ~f:(fun elem -> exec t ~standard elem)

let union t = Ast.Union t

let of_glob g = Ast.Element (Glob.test g)

let of_pred p = Ast.Element p

let true_ = of_pred (fun _ -> true)

let false_ = of_pred (fun _ -> false)

let of_string_set s = Ast.Element (String.Set.mem s)

let compl t = Ast.Compl t

let diff = Ast.diff
