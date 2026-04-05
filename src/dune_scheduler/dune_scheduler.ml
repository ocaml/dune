module Scheduler = Scheduler
module Async_io = Async_io
module File_watcher = File_watcher

module For_tests = struct
  module Async_inotify = Async_inotify
end

module For_benchmarks = struct
  module Thread_pool = Thread_pool
end
