module Fiber = struct
  include Fiber

  let collect_errors f =
    let open Fiber.O in
    Fiber.collect_errors f
    >>| function
    | Ok _ as s -> s
    | Error exns ->
      Error (List.map (fun { Stdune.Exn_with_backtrace.exn; backtrace = _ } -> exn) exns)
  ;;

  let parallel_iter t ~f =
    let stream = Fiber.Stream.In.create t in
    Fiber.Stream.In.parallel_iter stream ~f
  ;;
end
