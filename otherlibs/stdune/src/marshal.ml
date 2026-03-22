include Stdlib.Marshal

let make_sharing f = if f then [] else [ No_sharing ]
let to_string t ~sharing = to_string t (make_sharing sharing)
let to_channel chan t ~sharing = to_channel chan t (make_sharing sharing)
let from_string s = from_string s 0
