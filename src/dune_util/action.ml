open Stdune

module Diff = struct
  module Mode = struct
    type t =
      | Binary
      | Text

    let repr =
      Repr.variant
        "diff-mode"
        [ Repr.case0 "Binary" ~test:(function
            | Binary -> true
            | Text -> false)
        ; Repr.case0 "Text" ~test:(function
            | Text -> true
            | Binary -> false)
        ]
    ;;

    let to_dyn = Repr.to_dyn repr
  end

  type ('path, 'target) t =
    { optional : bool
    ; mode : Mode.t
    ; directory_diffs : bool
    ; file1 : 'path
    ; file2 : 'target
    }

  let to_dyn path target { optional; mode; directory_diffs; file1; file2 } =
    let open Dyn in
    record
      [ "optional", bool optional
      ; "mode", Mode.to_dyn mode
      ; "directory_diffs", bool directory_diffs
      ; "file1", path file1
      ; "file2", target file2
      ]
  ;;

  let map t ~path ~target = { t with file1 = path t.file1; file2 = target t.file2 }
end

module Outputs = struct
  type t =
    | Stdout
    | Stderr
    | Outputs

  let repr =
    Repr.variant
      "outputs"
      [ Repr.case0 "Stdout" ~test:(function
          | Stdout -> true
          | Stderr | Outputs -> false)
      ; Repr.case0 "Stderr" ~test:(function
          | Stderr -> true
          | Stdout | Outputs -> false)
      ; Repr.case0 "Outputs" ~test:(function
          | Outputs -> true
          | Stdout | Stderr -> false)
      ]
  ;;

  let to_string = function
    | Stdout -> "stdout"
    | Stderr -> "stderr"
    | Outputs -> "outputs"
  ;;
end

module Inputs = struct
  type t = Stdin

  let repr = Repr.variant "inputs" [ Repr.case0 "Stdin" ~test:(fun Stdin -> true) ]

  let to_string = function
    | Stdin -> "stdin"
  ;;
end

module File_perm = struct
  type t =
    | Normal
    | Executable

  let repr =
    Repr.variant
      "file-perm"
      [ Repr.case0 "Normal" ~test:(function
          | Normal -> true
          | Executable -> false)
      ; Repr.case0 "Executable" ~test:(function
          | Executable -> true
          | Normal -> false)
      ]
  ;;

  let suffix = function
    | Normal -> ""
    | Executable -> "-executable"
  ;;

  let to_unix_perm = function
    | Normal -> 0o666
    | Executable -> 0o777
  ;;
end
