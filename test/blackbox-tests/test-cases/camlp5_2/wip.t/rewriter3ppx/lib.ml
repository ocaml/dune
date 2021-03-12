
let rec list_length = function
    [] ->  0
  | _ :: tl -> 1 + list_length tl
  
