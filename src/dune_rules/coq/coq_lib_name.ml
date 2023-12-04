open! Import

(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2020                              *)
(* Written by: Emilio Jesús Gallego Arias *)

(* Coq Directory Path *)
type t = string list

let compare = List.compare ~compare:String.compare
let equal x y = Ordering.is_eq (compare x y)
let to_string x = String.concat ~sep:"." x
let empty = []
let stdlib = [ "Coq" ]
let to_list x = x
let append x y = x @ [ y ]
let to_dir x = String.concat ~sep:"/" x
let wrapper x = to_string x
let dir x = to_dir x

(* We should add some further validation to Coq library names; the rules in Coq
   itself have been tweaked due to Unicode, etc... so this is not trivial *)
let decode : (Loc.t * t) Dune_lang.Decoder.t =
  Dune_lang.Decoder.plain_string (fun ~loc s -> loc, String.split ~on:'.' s)
;;

let encode : t Dune_lang.Encoder.t = fun lib -> Dune_lang.Encoder.string (to_string lib)
let pp x = Pp.text (to_string x)
let to_dyn = Dyn.(list string)

module Rep = struct
  type nonrec t = t

  let compare = List.compare ~compare:String.compare
  let to_dyn = to_dyn
end

module Map = Map.Make (Rep)
