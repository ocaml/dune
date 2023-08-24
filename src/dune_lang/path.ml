open Stdune

(* to trick ocamldep *)
module Path = Path

module Local = struct
  let encode ~dir:from p =
    let open Dune_sexp.Encoder in
    string (Path.reach ~from p)
  ;;

  let decode ~dir =
    let open Dune_sexp.Decoder in
    let+ error_loc, path = located string in
    Path.relative ~error_loc dir path
  ;;
end
