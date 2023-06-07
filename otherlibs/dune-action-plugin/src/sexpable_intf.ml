open Import

module Error = struct
  type t =
    | Version_mismatch of int
    | Parse_error
end

module type Sexpable = sig
  type t

  val to_sexp : t -> Sexp.t

  val of_sexp : Sexp.t -> (t, Error.t) result
end

module type S = sig
  type t

  val conv : t Conv.value

  val version : int
end

module Make (Type : S) = struct
  let conv =
    let open Conv in
    pair int Type.conv

  let of_sexp sexp : (_, Error.t) result =
    match Conv.of_sexp Conv.(pair int sexp) ~version:(0, 0) sexp with
    | Error _ -> Error Parse_error
    | Ok (version, sexp) -> (
      match Int.equal version Type.version with
      | false -> Error (Version_mismatch version)
      | true -> (
        match Conv.of_sexp Type.conv ~version:(0, 0) sexp with
        | Error _ -> Error Parse_error
        | Ok v -> Ok v))

  let to_sexp t = Conv.to_sexp conv (Type.version, t)
end
