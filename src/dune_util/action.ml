module Diff = struct
  module Mode = struct
    type t =
      | Binary
      | Text
  end

  type ('path, 'target) t =
    { optional : bool
    ; mode : Mode.t
    ; file1 : 'path
    ; file2 : 'target
    }

  let map t ~path ~target = { t with file1 = path t.file1; file2 = target t.file2 }
end

module Outputs = struct
  type t =
    | Stdout
    | Stderr
    | Outputs

  let to_string = function
    | Stdout -> "stdout"
    | Stderr -> "stderr"
    | Outputs -> "outputs"
  ;;
end

module Inputs = struct
  type t = Stdin

  let to_string = function
    | Stdin -> "stdin"
  ;;
end

module File_perm = struct
  type t =
    | Normal
    | Executable

  let suffix = function
    | Normal -> ""
    | Executable -> "-executable"
  ;;

  let to_unix_perm = function
    | Normal -> 0o666
    | Executable -> 0o777
  ;;
end
