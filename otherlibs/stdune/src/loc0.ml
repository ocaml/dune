include Lexbuf.Loc

let to_lexbuf_loc x = x

let of_lexbuf_loc x = x

let start t = t.start

let stop t = t.stop

let set_stop t stop = { t with stop }

let set_start t start = { t with start }

let create ~start ~stop = { start; stop }

let map_pos { start; stop } ~f = { start = f start; stop = f stop }

let is_none = equal none
