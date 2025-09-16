include Stdune

include struct
  open Dune_engine
  module Process = Process
  module Scheduler = Scheduler
  module Display = Display
end

include Fiber.O

module Cmdliner = struct
  include Cmdliner

  let ( let+ ) t f = Term.(const f $ t)
  let ( and+ ) a b = Term.(const (fun x y -> x, y) $ a $ b)
end
