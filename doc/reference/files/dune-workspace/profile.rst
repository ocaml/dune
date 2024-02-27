#########
 profile
#########

The build profile can be selected in the ``dune-workspace`` file by
write a ``(profile ...)`` stanza. For instance:

.. code:: dune

   (profile release)

Note that the command line option ``--profile`` has precedence over this
stanza.
