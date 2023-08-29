include Stdune
module Ui = Nottui.Ui
module A = Notty.A
module I = Notty.I
module Renderer = Nottui.Renderer

module Lwd = struct
  module O = struct
    let ( let+ ) x f = Lwd.map x ~f
    let ( let* ) x f = Lwd.bind x ~f
    let ( >>| ) x f = Lwd.map x ~f
    let ( and+ ) x y = Lwd.pair x y
  end

  include Lwd
end
