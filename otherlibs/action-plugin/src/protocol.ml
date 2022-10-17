open Import

let run_by_dune_env_variable = "DUNE_DYNAMIC_RUN_CLIENT"

module Error = Sexpable_intf.Error

module Dependency = struct
  module T = struct
    type t =
      | File of string
      | Directory of string
      | Glob of
          { path : string
          ; glob : string
          }

    let conv =
      let open Conv in
      let file = constr "File" string (fun s -> File s) in
      let directory = constr "Directory" string (fun s -> Directory s) in
      let glob_cstr =
        constr "Glob" (pair string string) (fun (path, glob) ->
            Glob { path; glob })
      in
      sum
        [ econstr file; econstr directory; econstr glob_cstr ]
        (function
          | File s -> case s file
          | Directory s -> case s directory
          | Glob { path; glob } -> case (path, glob) glob_cstr)

    let compare x y =
      match (x, y) with
      | File x, File y -> String.compare x y
      | File _, _ -> Lt
      | _, File _ -> Gt
      | Directory x, Directory y -> String.compare x y
      | Directory _, _ -> Lt
      | _, Directory _ -> Gt
      | Glob { path; glob }, Glob t ->
        let open Ordering.O in
        let= () = String.compare path t.path in
        String.compare glob t.glob

    let to_dyn = Dyn.opaque
  end

  include T
  module O = Comparable.Make (T)
  module Map = O.Map

  module Set = struct
    include O.Set

    let conv : t Conv.value = Conv.iso (Conv.list conv) of_list to_list
  end
end

module Greeting = struct
  module T = struct
    type t =
      { run_arguments_fn : string
      ; response_fn : string
      }

    let conv =
      let open Conv in
      let to_ (run_arguments_fn, response_fn) =
        { run_arguments_fn; response_fn }
      in
      let from { run_arguments_fn; response_fn } =
        (run_arguments_fn, response_fn)
      in
      iso (pair string string) to_ from

    let version = 0
  end

  include T
  include Sexpable_intf.Make (T)
end

module Run_arguments = struct
  module T = struct
    type t =
      { prepared_dependencies : Dependency.Set.t
      ; targets : String.Set.t
      }

    let conv =
      let from { prepared_dependencies; targets } =
        (prepared_dependencies, targets)
      in
      let to_ (prepared_dependencies, targets) =
        { prepared_dependencies; targets }
      in
      let string_set =
        Conv.iso Conv.(list string) String.Set.of_list String.Set.to_list
      in
      let conv = Conv.pair Dependency.Set.conv string_set in
      Conv.iso conv to_ from

    let version = 0
  end

  include T
  include Sexpable_intf.Make (T)
end

module Response = struct
  module T = struct
    type t =
      | Done
      | Need_more_deps of Dependency.Set.t

    let conv =
      let open Conv in
      let done_ = constr "Done" unit (fun () -> Done) in
      let need_more_deps =
        constr "Need_more_deps" Dependency.Set.conv (fun deps ->
            Need_more_deps deps)
      in
      sum
        [ econstr done_; econstr need_more_deps ]
        (function
          | Done -> case () done_
          | Need_more_deps deps -> case deps need_more_deps)

    let version = 0
  end

  include T
  include Sexpable_intf.Make (T)
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

  let version_mismatch_error =
    Error
      "Dune version is incompatible with dune-action-plugin library version \
       that was used to build this executable."

  let cannot_read_file = Error "Cannot read file containing dune message."

  let file_not_found_error = Error "Cannot find file containing dune message."

  let create () =
    match Sys.getenv_opt run_by_dune_env_variable with
    | None -> Run_outside_of_dune
    | Some value -> (
      match Csexp.parse_string value with
      | Error _ -> cannot_parse_error
      | Ok sexp -> (
        match Greeting.of_sexp sexp with
        | Error (Version_mismatch _) -> version_mismatch_error
        | Error Parse_error -> cannot_parse_error
        | Ok greeting -> (
          match
            ( Result.try_with (fun () ->
                  Io.String_path.read_file greeting.run_arguments_fn)
            , Sys.file_exists greeting.response_fn )
          with
          | _, false -> file_not_found_error
          | Error _, _ -> cannot_read_file
          | Ok data, true -> (
            match Csexp.parse_string data with
            | Error _ -> cannot_parse_error
            | Ok sexp -> (
              match Run_arguments.of_sexp sexp with
              | Error (Version_mismatch _) -> version_mismatch_error
              | Error Parse_error -> cannot_parse_error
              | Ok { prepared_dependencies; targets } ->
                Ok
                  { response_fn = greeting.response_fn
                  ; prepared_dependencies
                  ; targets
                  })))))

  let prepared_dependencies (t : t) = t.prepared_dependencies

  let targets (t : t) = t.targets

  let respond (t : t) response =
    let data = Response.to_sexp response |> Csexp.to_string in
    Io.String_path.write_file t.response_fn data
end
