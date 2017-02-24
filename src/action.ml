type t =
  { prog      : Path.t
  ; args      : string list
  ; dir       : Path.t
  ; env       : string array
  ; stdout_to : Path.t option
  ; touches   : Path.t list
  }
