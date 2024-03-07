open Stdune
open Dune_sexp.Decoder

type 'a one =
  | Unnamed of 'a
  | Named of string * 'a list

type 'a t = 'a one list

let fold t ~f ~init = List.fold_left ~f:(fun acc x -> f x acc) ~init t

let map t ~f =
  List.map t ~f:(function
    | Unnamed a -> Unnamed (f a)
    | Named (s, xs) -> Named (s, List.map ~f xs))
;;

let to_list =
  List.concat_map ~f:(function
    | Unnamed x -> [ x ]
    | Named (_, xs) -> xs)
;;

let find t k =
  List.find_map t ~f:(function
    | Unnamed _ -> None
    | Named (k', x) -> Option.some_if (k = k') x)
;;

let empty = []
let singleton x = [ Unnamed x ]

let to_dyn dyn_of_a bindings =
  let binding_to_dyn = function
    | Unnamed a -> dyn_of_a a
    | Named (name, bindings) ->
      Dyn.List (Dyn.string (":" ^ name) :: List.map ~f:dyn_of_a bindings)
  in
  Dyn.list binding_to_dyn bindings
;;

let decode elem =
  let+ l =
    repeat
      (either
         (enter
            (let+ loc, name =
               located
                 (atom_matching
                    ~desc:"Atom of the form :<name>"
                    (String.drop_prefix ~prefix:":"))
             and+ values = repeat elem in
             loc, name, values))
         elem)
  in
  let rec loop vars acc = function
    | [] -> List.rev acc
    | Right x :: l -> loop vars (Unnamed x :: acc) l
    | Left (loc, name, values) :: l ->
      let vars =
        if not (String.Set.mem vars name)
        then String.Set.add vars name
        else
          User_error.raise
            ~loc
            [ Pp.textf "Variable %s is defined for the second time." name ]
      in
      loop vars (Named (name, values) :: acc) l
  in
  loop String.Set.empty [] l
;;

let encode encode bindings =
  Dune_sexp.List
    (List.map bindings ~f:(function
      | Unnamed a -> encode a
      | Named (name, bindings) ->
        Dune_sexp.List (Dune_sexp.atom (":" ^ name) :: List.map ~f:encode bindings)))
;;

let var_names t =
  List.filter_map t ~f:(function
    | Unnamed _ -> None
    | Named (s, _) -> Some s)
;;

let to_pform_map t =
  Pform.Map.of_list_exn
    (List.filter_map t ~f:(function
      | Unnamed _ -> None
      | Named (name, l) -> Some (Pform.Var (User_var name), l)))
;;
