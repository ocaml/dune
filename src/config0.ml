module Display = struct
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
end
