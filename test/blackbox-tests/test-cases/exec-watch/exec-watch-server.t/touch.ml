let touch path =
  let fd = Unix.openfile path [ Unix.O_CREAT ] 777 in
  Unix.close fd
