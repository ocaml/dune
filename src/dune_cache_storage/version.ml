module File = struct
  type t =
    | V3
    | V4

  let current = V4
  let all = [ V3; V4 ]

  let to_string = function
    | V3 -> "v3"
    | V4 -> "v4"
  ;;
end

module Value = struct
  type t = V3

  let current = V3
  let all = [ V3 ]

  let to_string = function
    | V3 -> "v3"
  ;;
end

module Metadata = struct
  type t =
    | V3
    | V4
    | V5

  let current = V5
  let all = [ V3; V4; V5 ]

  let to_string = function
    | V3 -> "v3"
    | V4 -> "v4"
    | V5 -> "v5"
  ;;

  let file_version = function
    | V3 -> File.V3
    | V4 | V5 -> File.V4
  ;;

  let value_version = function
    | V3 | V4 | V5 -> Value.V3
  ;;
end
