module Value = struct
  type t =
    | F of float
    | I of int
    | L of t list

  let rec yojson_of_t = function
    | F f -> [%yojson_of: float] f
    | I n -> [%yojson_of: int] n
    | L l -> [%yojson_of: t list] l
end

module Metric = struct
  type t =
    { name : string
    ; value : Value.t
    ; units : string
    }
  [@@deriving yojson_of]
end

module Bench_result = struct
  type t =
    { name : string
    ; metrics : Metric.t list
    }
  [@@deriving yojson_of]
end

module Version = struct
  type t = V2

  let yojson_of_t V2 = `Int 2
end

type t =
  { version : Version.t
  ; results : Bench_result.t list
  }
[@@deriving yojson_of]
