exception Unknown_format

type lines =
  | Single of int
  | Range of int * int

type source =
  | Code of
      { code : int
      ; name : string
      }
  | Alert of string

type severity =
  | Error of source option
  | Warning of source

type loc =
  { chars : (int * int) option
  ; lines : lines
  ; path : string
  }

type line =
  { indent : int
  ; contents : string
  }

type token =
  | Loc of
      { indent : int
      ; loc : loc
      ; message : string
      }
  | Line of line
  | Eof

val severity : Lexing.lexbuf -> (severity * string) option

val skip_excerpt : Lexing.lexbuf -> [ `Stop | `Continue ]

val token : Lexing.lexbuf -> token
