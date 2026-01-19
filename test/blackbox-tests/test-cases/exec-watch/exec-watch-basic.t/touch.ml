let touch path =
  let fd = Unix.openfile path [ Unix.O_CREAT ; O_CLOEXEC ] 777 in
  Unix.close fd
