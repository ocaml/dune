(** A type isomorphic to [Result], but without the negative connotations
    associated with the word "error". *)

type ('hit, 'miss) t =
  | Hit of 'hit
  | Miss of 'miss
