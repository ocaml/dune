`dune internal with-bwrap` runs commands in Dune's bubblewrap wrapper.

  $ readlink /proc/self/ns/mnt > host-ns
  $ dune internal with-bwrap -- sh -c 'echo wrapped; readlink /proc/self/ns/mnt > wrapped-ns'
  wrapped
  $ cmp -s host-ns wrapped-ns && echo same || echo different
  different
