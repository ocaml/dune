open Import

module Error = struct
  type t =
    | No_such_file
    | Broken_symlink
    | Cyclic_symlink
    | Unexpected_kind of File_kind.t
    | Unix_error of Unix_error.Detailed.t
    | Unrecognized of exn

  let no_such_file = function
    | No_such_file -> true
    | _ -> false
  ;;

  let pp t path =
    match t with
    | No_such_file -> Pp.verbatim "No such file"
    | Broken_symlink -> Pp.verbatim "Broken symbolic link"
    | Cyclic_symlink -> Pp.verbatim "Cyclic symbolic link"
    | Unexpected_kind file_kind ->
      Pp.verbatimf
        "Unexpected file kind %S (%s)"
        (File_kind.to_string file_kind)
        (File_kind.to_string_hum file_kind)
    | Unix_error (error, syscall, arg) ->
      let unix_error = Unix_error.Detailed.create error ~syscall ~arg in
      Unix_error.Detailed.pp unix_error
    | Unrecognized exn ->
      Pp.verbatim
        (match exn with
         | Sys_error msg ->
           let prefix =
             let expected_syscall_path = Path.to_string path in
             expected_syscall_path ^ ": "
           in
           String.drop_prefix_if_exists ~prefix msg
         | exn -> Printexc.to_string exn)
  ;;

  let equal x y =
    match x, y with
    | No_such_file, No_such_file -> true
    | No_such_file, _ | _, No_such_file -> false
    | Broken_symlink, Broken_symlink -> true
    | Broken_symlink, _ | _, Broken_symlink -> false
    | Cyclic_symlink, Cyclic_symlink -> true
    | Cyclic_symlink, _ | _, Cyclic_symlink -> false
    | Unexpected_kind x, Unexpected_kind y -> File_kind.equal x y
    | Unexpected_kind _, _ | _, Unexpected_kind _ -> false
    | Unix_error x, Unix_error y ->
      Tuple.T3.equal Unix_error.equal String.equal String.equal x y
    | Unix_error _, _ | _, Unix_error _ -> false
    | Unrecognized x, Unrecognized y ->
      (* Falling back to polymorphic equality check seems OK for this rare case.
           We could also just return [false] but that would break the reflexivity
           of the equality check, which doesn't seem nice. *)
      x = y
  ;;

  let to_dyn =
    let open Dyn in
    function
    | No_such_file -> Variant ("No_such_file", [])
    | Broken_symlink -> Variant ("Broken_symlink", [])
    | Cyclic_symlink -> Variant ("Cyclic_symlink", [])
    | Unexpected_kind kind -> Variant ("Unexpected_kind", [ File_kind.to_dyn kind ])
    | Unix_error error -> Variant ("Unix_error", [ Unix_error.Detailed.to_dyn error ])
    | Unrecognized exn -> Variant ("Unrecognized", [ String (Printexc.to_string exn) ])
  ;;

  let of_exn = function
    | Unix.Unix_error (ELOOP, _, _) -> Cyclic_symlink
    | Unix.Unix_error (error, syscall, arg) -> Unix_error (error, syscall, arg)
    | exn -> Unrecognized exn
  ;;
end

type t = (Digest.t, Error.t) result

let equal = Result.equal Digest.equal Error.equal
let to_option = Result.to_option
let to_dyn = Result.to_dyn Digest.to_dyn Error.to_dyn

let catch_fs_errors f =
  match f () with
  | result -> result
  | exception exn -> Error (Error.of_exn exn)
;;
