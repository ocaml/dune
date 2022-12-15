exception Unknown_format

type lines =
  | Single of int
  | Range of int * int

type code =
  { code : int
  ; name : string
  }

type source =
  | Code of code
  | Alert of string

type severity =
  | Error of source option
  | Warning of code
  | Alert of
      { name : string
      ; source : string
      }

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
