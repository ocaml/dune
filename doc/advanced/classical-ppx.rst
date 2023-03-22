Classical PPX
=============

*Classical PPX* refers to running PPX using the ```-ppx`` compiler option, which is
composed using Findlib. Even though this is useful to run some (usually old)
PPXs that don't support drivers, Dune doesn't support preprocessing with
PPX this way. However, a workaround exists using the `ppxfind
<https://github.com/kandu/ppxfind>`_ tool.
