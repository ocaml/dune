(***********************************************)
(* This file is licensed under The MIT License *)
(* (c) MINES ParisTech 2018-2019               *)
(* (c) INRIA 2019-2024                         *)
(* (c) Emilio J. Gallego Arias 2024-2025       *)
(* (c) CNRS 2025                               *)
(***********************************************)
(* Written by: Ali Caglayan                    *)
(* Written by: Emilio Jesús Gallego Arias      *)
(* Written by: Rudi Grinberg                   *)
(* Written by: Rodolphe Lepigre                *)
(***********************************************)

open! Import

(* Rocq Directory Path, example [Stdlib.List] *)
type t = string list

let compare = List.compare ~compare:String.compare
let equal x y = Ordering.is_eq (compare x y)
let to_string x = String.concat ~sep:"." x
let empty = []
let corelib = [ "Corelib" ]
let to_list x = x
let append x y = x @ [ y ]
let to_dir x = String.concat ~sep:"/" x
let wrapper x = to_string x
let dir x = to_dir x

(* We should add some further validation to Rocq library names; the rules in Rocq
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
