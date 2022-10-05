
let add x y = x + y

let rec factorial = function
  | 0 -> 1
  | n -> n * factorial (n - 1)
