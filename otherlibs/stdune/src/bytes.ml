include StdLabels.Bytes

external index_in_range_unchecked
  :  t
  -> pos:int
  -> len:int
  -> char
  -> int
  = "dune_bytes_index_in_range"
[@@noalloc]
