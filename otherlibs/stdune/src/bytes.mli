include module type of struct
  include StdLabels.Bytes
end

(** [index_in_range_unchecked bytes ~pos ~len char] returns the first index of
    [char] between [pos] and [pos + len], excluding the latter, or [-1] if it is
    absent. The range must be within [bytes]. *)
val index_in_range_unchecked : t -> pos:int -> len:int -> char -> int
