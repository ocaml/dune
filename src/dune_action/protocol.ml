open Stdune

let run_by_dune_env_variable = "DUNE_DYNAMIC_RUN_CLIENT"

let sexp_of_list sexp_of_t list : Sexp.t = List (List.map ~f:sexp_of_t list)

let list_of_sexp (t_of_sexp : Sexp.t -> 'a Option.t) : Sexp.t -> _ = function
  | List sexps -> List.map sexps ~f:t_of_sexp |> Option.List.all
  | _ -> None

let sexp_of_string string : Sexp.t = Atom string

let string_of_sexp : Sexp.t -> _ = function
  | Atom string -> Some string
  | _ -> None

module Dependency = struct
  module T = struct
    type t =
      | File of string
      | Directory of string

    let sexp_of_t : _ -> Sexp.t = function
      | File path -> List [ Atom "file"; Atom path ]
      | Directory path -> List [ Atom "directory"; Atom path ]

    let t_of_sexp : Sexp.t -> _ = function
      | List [ Atom "file"; Atom path ] -> Some (File path)
      | List [ Atom "directory"; Atom path ] -> Some (Directory path)
      | _ -> None

    let compare x y =
      match (x, y) with
      | File x, File y -> String.compare x y
      | File _, _ -> Lt
      | _, File _ -> Gt
      | Directory x, Directory y -> String.compare x y

    let to_dyn _ = Dyn.opaque
  end

  include T
  module O = Comparable.Make (T)
  module Map = O.Map

  module Set = struct
    include O.Set

    let sexp_of_t (t : t) = to_list t |> sexp_of_list T.sexp_of_t

    let t_of_sexp sexp = Option.O.(list_of_sexp T.t_of_sexp sexp >>| of_list)
  end
end

module Greeting = struct
  type t =
    { run_arguments_fn : string
    ; response_fn : string
    }

  let sexp_of_t { run_arguments_fn; response_fn } : Sexp.t =
    List [ Atom run_arguments_fn; Atom response_fn ]

  let t_of_sexp : Sexp.t -> _ = function
    | List [ Atom run_arguments_fn; Atom response_fn ] ->
      Some { run_arguments_fn; response_fn }
    | _ -> None
end

module Run_arguments = struct
  type t =
    { prepared_dependencies : Dependency.Set.t
    ; targets : String.Set.t
    }

  let sexp_of_t { prepared_dependencies; targets } : Sexp.t =
    List
      [ Dependency.Set.sexp_of_t prepared_dependencies
      ; targets |> String.Set.to_list |> sexp_of_list sexp_of_string
      ]

  let t_of_sexp : Sexp.t -> _ = function
    | List [ prepared_dependencies; targets ] ->
      let open Option.O in
      let* prepared_dependencies =
        Dependency.Set.t_of_sexp prepared_dependencies
      in
      let+ targets = list_of_sexp string_of_sexp targets in
      let targets = String.Set.of_list targets in
      { prepared_dependencies; targets }
    | _ -> None
end

module Response = struct
  type t =
    | Done
    | Need_more_deps of Dependency.Set.t

  let sexp_of_t : _ -> Sexp.t = function
    | Done -> List [ Atom "done" ]
    | Need_more_deps deps ->
      List [ Atom "need_more_deps"; Dependency.Set.sexp_of_t deps ]

  let t_of_sexp : Sexp.t -> _ = function
    | List [ Atom "done" ] -> Some Done
    | List [ Atom "need_more_deps"; sexp ] ->
      Option.O.(Dependency.Set.t_of_sexp sexp >>| fun xs -> Need_more_deps xs)
    | _ -> None
end

module Context = struct
  type t =
    { response_fn : string
    ; prepared_dependencies : Dependency.Set.t
    ; targets : String.Set.t
    }

  type create_result =
    | Ok of t
    | Run_outside_of_dune
    | Error of string

  let cannot_parse_error = Error "Can not parse dune message."

  let cannot_read_file = Error "Cannot read file containing dune message."

  let file_not_found_error = Error "Cannot find file containing dune message."

  let create () =
    match Sys.getenv_opt run_by_dune_env_variable with
    | None -> Run_outside_of_dune
    | Some value -> (
      match
        Option.O.(
          value |> Csexp.parse_string |> Result.to_option
          >>= Greeting.t_of_sexp)
      with
      | None -> cannot_parse_error
      | Some greeting -> (
        match
          ( Result.try_with (fun () ->
            Io.String_path.read_file greeting.run_arguments_fn)
          , Sys.file_exists greeting.response_fn )
        with
        | _, false -> file_not_found_error
        | Error _, _ -> cannot_read_file
        | Ok data, true -> (
          match
            Option.O.(
              data |> Csexp.parse_string |> Result.to_option
              >>= Run_arguments.t_of_sexp)
          with
          | None -> cannot_parse_error
          | Some { prepared_dependencies; targets } ->
            Ok
              { response_fn = greeting.response_fn
              ; prepared_dependencies
              ; targets
              } ) ) )

  let prepared_dependencies (t : t) = t.prepared_dependencies

  let targets (t : t) = t.targets

  let respond (t : t) response =
    let data = response |> Response.sexp_of_t |> Csexp.to_string in
    Io.String_path.write_file t.response_fn data
end
