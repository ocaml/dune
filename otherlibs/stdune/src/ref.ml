type 'a t = 'a ref

module With_freeze = struct
  type 'a t =
    { mutable cell : 'a
    ; mutable frozen : bool
    }

  let create initial = { cell = initial; frozen = false }

  let set t v =
    if t.frozen then Code_error.raise "reference is already frozen" [];
    t.cell <- v
  ;;

  let get t = t.cell
  let freeze t = t.frozen <- true
end
