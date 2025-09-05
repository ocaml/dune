open Import

(* to trick ocamldep *)
module Path = Path

module Local = struct
  let encode ~dir:from p =
    let open Encoder in
    string (Path.reach ~from p)
  ;;

  let decode ~dir =
    let open Decoder in
    let+ error_loc, path = located string in
    Path.relative ~error_loc dir path
  ;;
end
