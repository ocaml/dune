Rpc connection fails when dune isn't running
  $ dune rpc init
  Error: rpc server not running
  [1]

Test is disabled beacuse it's not used deterministic
#  $ dune rpc test <<EOF
#  > ((id init) (method initialize) (params ((id test) (version (1 0)))))
#  > EOF
#  ((id init) (result (ok ())))
