open Import

type 'a t =
  | String : string t
  | List   : 'a t -> 'a list t
  | Pair   : 'a t * 'b t -> ('a * 'b) t

let rec eq : type a b. a t -> b t -> (a, b) eq = fun a b ->
  match a, b with
  | String, String -> Eq
  | List a, List b -> begin
      match eq a b with
      | Eq -> Eq
      | Ne -> Ne
    end
  | Pair (a1, a2), Pair (b1, b2) -> begin
    match eq a1 b1 with
    | Ne -> Ne
    | Eq ->
      match eq a2 b2 with
      | Eq -> Eq
      | Ne -> Ne
  end
  | _ -> Ne

let rec to_sexp : type a. a t -> a -> Sexp.t =
  let open Sexp.To_sexp in
  function
  | String -> string
  | List t -> list (to_sexp t)
  | Pair (a, b) -> pair (to_sexp a) (to_sexp b)

let rec of_sexp : type a. a t -> Sexp.t -> a =
  let open Sexp.Of_sexp in
  function
  | String -> string
  | List t -> list (of_sexp t)
  | Pair (a, b) -> pair (of_sexp a) (of_sexp b)

let save kind ~filename x =
  let s = to_sexp kind x |> Sexp.to_string in
  let oc = open_out filename in
  output_string oc s;
  close_out oc

let load kind ~filename =
  let sexp, _locs =
    with_lexbuf_from_file filename ~f:Sexp_lexer.single
  in
  of_sexp kind sexp
