Definition nb (b : bool) : bool :=
  match b with
  | false => true
  | true => false
  end.

Require Extraction.
Separate Extraction nb.
