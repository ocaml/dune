let version () = Option.map Build_info.V1.Version.to_string (Build_info.V1.version ())
