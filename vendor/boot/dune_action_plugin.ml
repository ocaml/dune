let unimplemented () =
  failwith "Dune_action_plugin is not available during bootstrap"

module Deserialization_error = struct
  type t =
    | Version_mismatch of int
    | Parse_error
end

module Private = struct
  module Protocol = struct
    module Dependency = struct
      type t =
        | File of string
        | Directory of string
        | Glob of
            { path : string
            ; glob : string
            }

      module Set = struct
        type t = T
        let to_list _ = unimplemented ()
        let union _ _ = unimplemented ()
        let empty = T
      end
    end


    module Run_arguments = struct
      type t =
        { prepared_dependencies : Dependency.Set.t
        ; targets : Stdune.String.Set.t
        }

      let serialize _ = unimplemented ()
    end

    module Greeting = struct
      type t =
        { run_arguments_fn : string
        ; response_fn : string
        }

      let serialize _ = unimplemented ()
    end

    module Response = struct
      type t =
        | Done
        | Need_more_deps of Dependency.Set.t

      let deserialize _ =
        (unimplemented () : (t, Deserialization_error.t) result)
    end
    let run_by_dune_env_variable = "<NONE>"
  end
end
