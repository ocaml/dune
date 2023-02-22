open Stdune

module T = struct
  type t =
    { ino : int
    ; dev : int
    }

  let to_dyn { ino; dev } =
    let open Dyn in
    record [ ("ino", Int.to_dyn ino); ("dev", Int.to_dyn dev) ]

  let compare { ino; dev } t =
    let open Ordering.O in
    let= () = Int.compare ino t.ino in
    Int.compare dev t.dev
end

include T

let dummy = { ino = 0; dev = 0 }

module Map = Map.Make (T)

module Make (Reduced_stats : Reduced_stats_intf.S) = struct
  let of_stats (st : Reduced_stats.t) = { ino = st.st_ino; dev = st.st_dev }
end
