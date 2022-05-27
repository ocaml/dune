module Outputs = struct
  type t =
    | Stdout
    | Stderr
    | Outputs

  let to_string = function
    | Stdout -> "stdout"
    | Stderr -> "stderr"
    | Outputs -> "outputs"
end

module Inputs = struct
  type t = Stdin

  let to_string = function
    | Stdin -> "stdin"
end

module File_perm = struct
  type t =
    | Normal
    | Executable

  let suffix = function
    | Normal -> ""
    | Executable -> "-executable"

  let to_unix_perm = function
    | Normal -> 0o666
    | Executable -> 0o777
end
