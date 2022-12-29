let dirname = [%bs.raw "__dirname"]
let file_path = "assets/file.txt"
let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) `utf8
let it = file_content
