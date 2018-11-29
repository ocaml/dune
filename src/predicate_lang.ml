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
    | Inter [] -> a
    | b -> Inter [a; Compl b]

  let inter a = Inter a
  let compl a = Compl a
  let union a = Union a
  let not_union a = compl (union a)

  let decode elt =
    let open Stanza.Decoder in
    let elt = elt >>| fun e -> Element e in
    let rec one (kind : Dune_lang.Syntax.t) =
      peek_exn >>= function
      | Atom (loc, A "\\") -> Errors.fail loc "unexpected \\"
      | (Atom (_, A "") | Quoted_string (_, _)) | Template _ ->
        elt
      | Atom (loc, A s) -> begin
          match s with
          | ":standard" ->
            junk >>> return Standard
          | ":include" ->
            Errors.fail loc ":include isn't supported in the predicate language"
          | _ when s.[0] = ':' ->
            Errors.fail loc "undefined symbol %s" s
          | _ ->
            elt
        end
      | List (_, Atom (loc, A s) :: _) -> begin
          match s, kind with
          | ":include", _ ->
            Errors.fail loc ":include isn't supported in the predicate language"
          | ("or" | "and" | "not"), _ -> bool_ops kind
          | s, Dune when s <> "" && s.[0] <> '-' && s.[0] <> ':' ->
            Errors.fail loc
              "This atom must be quoted because it is the first element \
               of a list and doesn't start with - or :"
          | _ -> enter (many union [] kind)
        end
      | List _ -> enter (many union [] kind)
    and bool_ops kind =
      sum [ "or", many union [] kind
          ; "and", many inter [] kind
          ; "not", many not_union [] kind
          ]
    and many k acc kind =
      peek >>= function
      | None -> return (k (List.rev acc))
      | Some (Atom (_, A "\\")) ->
        junk >>> many union [] kind >>| fun to_remove ->
        diff (k (List.rev acc)) to_remove
      | Some _ ->
        one kind >>= fun x ->
        many k (x :: acc) kind
    in
    Stanza.file_kind () >>= fun kind ->
    match kind with
    | Dune -> many union [] kind
    | Jbuild -> one kind

  let rec to_sexp f =
    let open Sexp.Encoder in
    function
    | Element a -> f a
    | Compl a -> constr "compl" [to_sexp f a]
    | Standard -> string ":standard"
    | Union xs -> constr "or" (List.map ~f:(to_sexp f) xs)
    | Inter xs -> constr "and" (List.map ~f:(to_sexp f) xs)
end

type t = (string -> bool) Ast.t

let pp ppf t =
  Sexp.pp ppf (Ast.to_sexp (fun _ -> Sexp.Encoder.string "opaque") t)

let decode : t Dune_lang.Decoder.t =
  let open Stanza.Decoder in
  Ast.decode (Glob.decode >>| Glob.test)

let empty = Ast.Union []

let rec mem t ~standard ~elem =
  match (t : _ Ast.t) with
  | Compl t -> not (mem t ~standard ~elem)
  | Element f -> f elem
  | Union xs -> List.exists ~f:(mem ~standard ~elem) xs
  | Inter xs -> List.for_all ~f:(mem ~standard ~elem) xs
  | Standard -> mem standard ~standard ~elem

let filter (t : t) ~standard elems =
  match t with
  | Inter []
  | Union [] -> []
  | _ ->
    (List.filter elems ~f:(fun elem -> mem t ~standard ~elem))

let union t = Ast.Union t

let of_glob g = Ast.Element (Glob.test g)

let of_pred p = Ast.Element p

let of_string_set s = Ast.Element (String.Set.mem s)

let compl t = Ast.Compl t

let diff = Ast.diff
