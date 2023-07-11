open Import
open Memo.O

let programs_for_which_we_prefer_opt_ext =
  [ "ocamlc"; "ocamldep"; "ocamlmklib"; "ocamlobjinfo"; "ocamlopt" ]

module Best_path = struct
  module Make
      (Monad : Monad.S) (Fs : sig
        val file_exists : Path.t -> bool Monad.t
      end) =
  struct
    open Monad.O

    let best_path ~dir program =
      let exe_path program =
        let fn = Path.relative dir (program ^ Bin.exe) in
        let+ exists = Fs.file_exists fn in
        Option.some_if exists fn
      in
      if
        List.mem programs_for_which_we_prefer_opt_ext program
          ~equal:String.equal
      then
        let* path = exe_path (program ^ ".opt") in
        match path with
        | None -> exe_path program
        | Some _ as path -> Monad.return path
      else exe_path program
  end

  let memo =
    let module M =
      Make
        (Memo)
        (struct
          let file_exists f =
            Fs_memo.file_exists (Path.as_outside_build_dir_exn f)
        end)
    in
    M.best_path
end

module rec Rec : sig
  val which : path:Path.t list -> string -> Path.t option Memo.t
end = struct
  open Rec

  let which_impl (path, program) =
    match path with
    | [] -> Memo.return None
    | dir :: path -> (
      let* res = Best_path.memo ~dir program in
      match res with
      | None -> which ~path program
      | Some prog -> Memo.return (Some prog))

  let which =
    let memo =
      let module Input = struct
        type t = Path.t list * string

        let equal = Tuple.T2.equal (List.equal Path.equal) String.equal

        let hash = Tuple.T2.hash (List.hash Path.hash) String.hash

        let to_dyn = Dyn.opaque
      end in
      Memo.create "which"
        ~input:(module Input)
        ~cutoff:(Option.equal Path.equal) which_impl
    in
    fun ~path prog -> Memo.exec memo (path, prog)
end

include Rec
