.. _menhir-main:

******
Menhir
******

The basic form for defining menhir_ parsers (analogous to ocamlyacc) is:

.. code:: scheme

    (menhir
     ((modules (<parser1> <parser2> ...))))

Modular parsers can be defined by adding a ``merge_into`` field. This correspond
to the ``--base`` command line option of ``menhir``. With this option, a single
parser named ``base_name`` is generated.

.. code:: scheme

    (menhir
     ((merge_into <base_name>)
      (modules (<parser1> <parser2> ...))))

Extra flags can be passed to menhir using the ``flags`` flag:

.. code:: scheme

    (menhir
     ((flags (<option1> <option2> ...))
      (modules (<parser1> <parser2> ...))))

.. _menhir: https://gitlab.inria.fr/fpottier/menhir
