(** Very small tooling for format printers. *)

include Format

type 'a t = Format.formatter -> 'a -> unit

let list = pp_print_list
let array ?pp_sep f fmt arr = list ?pp_sep f fmt (Array.to_list arr)
let str = pp_print_string
let sexp fmt s pp x = fprintf fmt "@[<3>(%s@ %a)@]" s pp x
let bytes fmt t = Format.fprintf fmt "%S" (Bytes.to_string t)

let pair pp1 pp2 fmt (v1, v2) =
  pp1 fmt v1;
  pp_print_space fmt ();
  pp2 fmt v2
;;

let triple pp1 pp2 pp3 fmt (v1, v2, v3) =
  pp1 fmt v1;
  pp_print_space fmt ();
  pp2 fmt v2;
  pp_print_space fmt ();
  pp3 fmt v3
;;

let opt f fmt x =
  match x with
  | None -> pp_print_string fmt "<None>"
  | Some x -> fprintf fmt "%a" f x
;;

let int = pp_print_int

let optint fmt = function
  | None -> ()
  | Some i -> fprintf fmt "@ %d" i
;;

let quote fmt s = Format.fprintf fmt "\"%s\"" s

let pp_olist pp_elem fmt =
  Format.fprintf
    fmt
    "@[<3>[@ %a@ ]@]"
    (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") pp_elem)
;;

let char fmt c = Format.fprintf fmt "%c" c
let bool = Format.pp_print_bool
let pp_str_list = pp_olist quote
let lit s fmt () = pp_print_string fmt s

let to_to_string pp x =
  let b = Buffer.create 16 in
  let fmt = Format.formatter_of_buffer b in
  pp fmt x;
  Buffer.contents b
;;

let quoted_string fmt s = Format.fprintf fmt "%S" s
