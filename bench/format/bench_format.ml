module Value = struct
  type t =
    | F of float
    | I of int
    | L of t list

  let rec t_of_yojson = function
    | `Float f -> F f
    | `Int n -> I n
    | j -> L ([%of_yojson: t list] j)

  let rec yojson_of_t = function
    | F f -> [%yojson_of: float] f
    | I n -> [%yojson_of: int] n
    | L l -> `List (List.map yojson_of_t l)
end

module Metric = struct
  type t =
    { name : string
    ; value : Value.t
    ; units : string
    }
  [@@deriving yojson]
end

module Bench_result = struct
  type t =
    { name : string
    ; metrics : Metric.t list
    }
  [@@deriving yojson]
end

module Version = struct
  type t = V2

  let yojson_of_t V2 = `Int 2

  let t_of_yojson j =
    match [%of_yojson: int] j with
    | 2 -> V2
    | _ -> failwith "unknown version"
end

type t =
  { version : Version.t
  ; results : Bench_result.t list
  }
[@@deriving yojson]
