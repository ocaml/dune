open! Stdune

type t = ..

module Parser = struct
  type nonrec t = string * t list Dune_lang.Decoder.t
end

(* The actual latest version is defined in the rpc library. This is because rpc
   client needs to know the version of dune to use to connect. *)
let latest_version = Dune_rpc_private.Version.latest

let since v = (v, `Since v)

let all_minors (major, minor) =
  List.init (minor + 1) ~f:(fun i -> since (major, i))

let syntax =
  Dune_lang.Syntax.create ~name:"dune" ~desc:"the dune language"
    (List.concat
       [ all_minors (1, 12); all_minors (2, 9); all_minors latest_version ])
