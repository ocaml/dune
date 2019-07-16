type lock_type = Read | Write

type whence = Set | Cur | End

let lock ?(whence = Set) ?(start = 0) ?(len = 0) _ _ = assert false

let lock_try ?(whence = Set) ?(start = 0) ?(len = 0) _ _ = assert false

let lock_get ?(whence = Set) ?(start = 0) ?(len = 0) _ _ = assert false

let unlock ?(whence = Set) ?(start = 0) ?(len = 0) _ = assert false
