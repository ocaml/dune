open Stdune
open Sexpable_intf

module Deserialization_error = struct
  type t =
    | Version_mismatch of int
    | Parse_error
end

module type Serializable = sig
  type t

  val serialize : t -> String.t

  val deserialize : String.t -> (t, Deserialization_error.t) Result.t
end

module type S = sig
  include Sexpable

  val version : int
end

module Make (TypeToSerialize : S) :
  Serializable with type t := TypeToSerialize.t = struct
  open TypeToSerialize

  let parsing_error_of_option :
      _ Option.t -> (_, Deserialization_error.t) Result.t = function
    | Some data -> Ok data
    | None -> Error Parse_error

  let serialize t =
    Sexp.(List [ Atom "version"; Atom (Int.to_string version); sexp_of_t t ])
    |> Csexp.to_string

  let deserialize data =
    let open Result.O in
    let* sexp =
      Csexp.parse_string data
      |> Result.map_error ~f:(fun _message -> Deserialization_error.Parse_error)
    in
    let* format_version, data =
      match sexp with
      | List [ Atom "version"; Atom version; data ] -> Ok (version, data)
      | _ -> Error Deserialization_error.Parse_error
    in
    let* format_version =
      Int.of_string format_version |> parsing_error_of_option
    in
    if format_version <> version then
      Error (Deserialization_error.Version_mismatch format_version)
    else t_of_sexp data |> parsing_error_of_option
end
