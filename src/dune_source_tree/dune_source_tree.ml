module type Reduced_stats = Reduced_stats_intf.S

module Make (Reduced_stats : Reduced_stats) = struct
  module Readdir = struct
    type t = Readdir.t

    module type S = Readdir.S with type reduced_stats := Reduced_stats.t

    module Make = Readdir.Make (Reduced_stats)
  end

  module Source_tree = struct
    module Dir = Source_tree.Dir
    module Make = Source_tree.Make (Reduced_stats)
  end
end
