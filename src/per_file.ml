open Import
open Sexp.Of_sexp
open Jbuild_helpers

type 'a t =
  | For_all  of 'a
  | Per_file of 'a String_map.t

let t a sexp =
  match sexp with
  | List (_, Atom (_, "per_file") :: rest) -> begin
      List.concat_map rest ~f:(fun sexp ->
        let pp, names = pair a module_names sexp in
        List.map (String_set.elements names) ~f:(fun name -> (name, pp)))
      |> String_map.of_alist
      |> function
      | Ok map -> Per_file map
      | Error (name, _, _) ->
        of_sexp_error sexp (sprintf "module %s present in two different sets" name)
    end
  | sexp -> For_all (a sexp)

let pure a = For_all a

let fmap per_file f = 
  match per_file with
  | For_all a ->
    For_all (f a)
  | Per_file a_map ->
    Per_file (String_map.map a_map ~f)

let (>>|) = fmap

let get per_file ~target ~default =
    match per_file with
    | For_all a -> a
    | Per_file a_map -> String_map.find_default ~default target a_map 
