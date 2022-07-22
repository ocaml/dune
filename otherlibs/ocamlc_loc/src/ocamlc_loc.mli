[@@@alert
unstable "The API of this library is not stable and may change without notice."]

type source =
  | Code of
      { code : int
      ; name : string
      }
  | Alert of string

type lines =
  | Single of int
  | Range of int * int

type loc =
  { chars : (int * int) option
  ; lines : lines
  ; path : string
  }

type severity =
  | Error of source option
  | Warning of source

type report =
  { loc : loc
  ; severity : severity
  ; message : string
  ; related : (loc * string) list
  }

val dyn_of_report : report -> Dyn.t

val parse : string -> report list
