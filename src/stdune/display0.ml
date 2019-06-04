type t =
  | Progress
  | Short
  | Verbose
  | Quiet

let all =
  [ "progress" , Progress
  ; "verbose"  , Verbose
  ; "short"    , Short
  ; "quiet"    , Quiet
  ]
