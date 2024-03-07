open! Stdune

module Temp = Temp.Monad (struct
    type 'a t = 'a Fiber.t

    let protect ~f ~finally =
      Fiber.finalize f ~finally:(fun () -> finally () |> Fiber.return)
    ;;
  end)
