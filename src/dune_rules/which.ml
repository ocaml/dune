open Import
open Memo.O

let programs_for_which_we_prefer_opt_ext =
  [ "ocamlc"; "ocamldep"; "ocamlmklib"; "ocamlobjinfo"; "ocamlopt" ]
;;

let with_opt p = p ^ ".opt"

let candidates prog =
  let prog = Filename.to_string prog in
  let base = [ Filename.of_string_exn (Bin.add_exe prog) ] in
  if List.mem programs_for_which_we_prefer_opt_ext prog ~equal:String.equal
  then Filename.of_string_exn (Bin.add_exe (with_opt prog)) :: base
  else base
;;

let best_in_dir ~dir program =
  candidates program
  |> Memo.List.find_map ~f:(fun fn ->
    let path = Path.relative_fname dir fn in
    Fs_memo.file_exists (Path.as_outside_build_dir_exn path)
    >>| function
    | false -> None
    | true -> Some path)
;;

module rec Rec : sig
  val which : path:Path.t list -> Filename.t -> Path.t option Memo.t
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
        type t = Path.t list * Filename.t

        let equal = Tuple.T2.equal (List.equal Path.equal) Filename.equal
        let hash = Tuple.T2.hash (List.hash Path.hash) Filename.hash
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
