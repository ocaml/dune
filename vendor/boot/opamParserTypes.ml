type value =
  | String of unit * string
  | List of unit * value list
  | Other

type opamfile_item =
  | Variable of unit * string * value
  | Other

type opamfile =
  { file_contents : opamfile_item list
  ; file_name     : string
  }
