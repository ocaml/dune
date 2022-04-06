[@@@alert
unstable "The API of this library is not stable and may change without notice."]

type warning =
  { code : int
  ; name : string
  }

type loc =
  { path : string
  ; line : [ `Single of int | `Range of int * int ]
  ; chars : (int * int) option
  }

type severity =
  | Error
  | Warning of warning option

type message =
  | Raw of string
  | Structured of
      { file_excerpt : string option
      ; message : string
      ; severity : severity
      }

type report =
  { loc : loc
  ; message : message
  ; related : (loc * message) list
  }

val dyn_of_report : report -> Dyn.t

val parse : string -> report list

val parse_raw :
  string -> [ `Loc of [ `Related | `Parent ] * loc | `Message of message ] list
