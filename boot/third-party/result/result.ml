type nonrec ('a, 'b) result = ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

type ('a, 'b) t = ('a, 'b) result
