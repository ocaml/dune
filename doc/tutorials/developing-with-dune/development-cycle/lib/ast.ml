type op =
  | Add
  | Mul
  | Div

type exp =
  | Int of int
  | Float of float
  | Ident of string
  | Op of op * exp * exp
  | Call of string * exp
