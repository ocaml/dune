(try 
  Sys.remove "config/testdb.db"
with 
  _ -> ());

let dbm =
  Dbm.opendbm "config/testdb" [ Dbm.Dbm_rdwr; Dbm.Dbm_create ] 0o777 in

print_string "OK\n";;

