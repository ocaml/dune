open Import
open Memo.O

let programs_for_which_we_prefer_opt_ext =
  [ "ocamlc"; "ocamldep"; "ocamlmklib"; "ocamlobjinfo"; "ocamlopt" ]
;;

let with_opt p = p ^ ".opt"

let candidates =
  let make p = p ^ Bin.exe in
  fun prog ->
    let base = [ make prog ] in
    if List.mem programs_for_which_we_prefer_opt_ext prog ~equal:String.equal
    then make (with_opt prog) :: base
    else base
;;

let best_in_dir ~dir program =
  candidates program
  |> Memo.List.find_map ~f:(fun fn ->
    let path = Path.relative dir fn in
    Fs_memo.file_exists (Path.as_outside_build_dir_exn path)
    >>| function
    | false -> None
    | true -> Some path)
;;

module rec Rec : sig
  val which : path:Path.t list -> string -> Path.t option Memo.t
end = struct
  open Rec

  let which_impl (path, program) =
    match path with
    | [] -> Memo.return None
    | dir :: path ->
      best_in_dir ~dir program
      >>= (function
       | None -> which ~path program
       | Some prog -> Memo.return (Some prog))
  ;;

  let which =
    let memo =
      let module Input = struct
        type t = Path.t list * string

        let equal = Tuple.T2.equal (List.equal Path.equal) String.equal
        let hash = Tuple.T2.hash (List.hash Path.hash) String.hash
        let to_dyn = Dyn.opaque
      end
      in
      Memo.create
        "which"
        ~input:(module Input)
        ~cutoff:(Option.equal Path.equal)
        which_impl
    in
    fun ~path prog -> Memo.exec memo (path, prog)
  ;;
end

include Rec
