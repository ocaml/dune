open Stdune
open Dune_lang.Decoder

type 'a one =
  | Unnamed of 'a
  | Named of string * 'a list

type 'a t = 'a one list

let fold t ~f ~init = List.fold_left ~f:(fun acc x -> f x acc) ~init t

let map t ~f =
  List.map t ~f:(function
    | Unnamed a ->
      Unnamed (f a)
    | Named (s, xs) ->
      Named (s, List.map ~f xs))

let to_list =
  List.concat_map ~f:(function Unnamed x -> [ x ] | Named (_, xs) -> xs)

let find t k =
  List.find_map t ~f:(function
    | Unnamed _ ->
      None
    | Named (k', x) ->
      Option.some_if (k = k') x)

let empty = []

let singleton x = [ Unnamed x ]

let to_dyn dyn_of_a bindings =
  let open Dyn.Encoder in
  Dyn.List
    (List.map bindings ~f:(function
      | Unnamed a ->
        dyn_of_a a
      | Named (name, bindings) ->
        Dyn.List (string (":" ^ name) :: List.map ~f:dyn_of_a bindings)))

let decode elem =
  let+ l =
    repeat
      (if_paren_colon_form
        ~then_:
          (let+ values = repeat elem in
           fun (loc, name) -> Left (loc, name, values))
         ~else_:(elem >>| Either.right))
  in
  let rec loop vars acc = function
    | [] ->
      List.rev acc
    | Right x :: l ->
      loop vars (Unnamed x :: acc) l
    | Left (loc, name, values) :: l ->
      let vars =
        if not (String.Set.mem vars name) then
          String.Set.add vars name
        else
          User_error.raise ~loc
            [ Pp.textf "Variable %s is defined for the second time." name ]
      in
      loop vars (Named (name, values) :: acc) l
  in
  loop String.Set.empty [] l

let encode encode bindings =
  Dune_lang.List
    (List.map bindings ~f:(function
      | Unnamed a ->
        encode a
      | Named (name, bindings) ->
        Dune_lang.List
          (Dune_lang.atom (":" ^ name) :: List.map ~f:encode bindings)))
