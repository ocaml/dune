let reset () = ()

module type Stat = sig
  val count : Counter.t
  val bytes : Counter.t
  val time : Counter.Timer.t
end

module File_read = struct
  let count = Counter.create ()
  let bytes = Counter.create ()
  let time = Counter.Timer.create ()
end

module File_write = struct
  let count = Counter.create ()
  let bytes = Counter.create ()
  let time = Counter.Timer.create ()
end

module Directory_read = struct
  let count = Counter.create ()
  let time = Counter.Timer.create ()
end

module Digest = struct
  module File = struct
    let count = Counter.create ()
    let bytes = Counter.create ()
    let time = Counter.Timer.create ()
  end

  module Value = struct
    let count = Counter.create ()
    let bytes = Counter.create ()
    let time = Counter.Timer.create ()
  end
end
