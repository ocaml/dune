include struct
  [@@@warning "-33"]

  open Result_compat
  open Pervasives [@@warning "-3"]

  type ('a, 'error) t = ('a, 'error) result =
    | Ok of 'a
    | Error of 'error
end

type ('a, 'error) result = ('a, 'error) t =
  | Ok of 'a
  | Error of 'error
