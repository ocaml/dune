type loc =
  { path : string
  ; line : [ `Single of int | `Range of int * int ]
  ; chars : (int * int) option
  }

type severity =
  | Error
  | Warning of
      { code : int option
      ; name : string
      }

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

val dyn_of_report : report -> Stdune.Dyn.t

val parse : string -> report list
