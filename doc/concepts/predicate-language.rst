Predicate Language
==================

The predicate language allows the user to define simple predicates
(Boolean-valued functions) that Dune can evaluate. Here is a semi-formal
specification of the predicate language:

.. productionlist::
   pred : (and `pred` `pred`)
        : (or `pred` `pred`)
        : (not `pred`)
        : :standard
        : `element`

The exact meaning of ``:standard`` and the nature of :token:`element` depends
on the context. For example, in the case of the :ref:`dune-subdirs`, an
:token:`element` corresponds to file glob patterns. Another example is the user
action :ref:`(with-accepted-exit-codes ...) <user-actions>`, where an
:token:`element` corresponds to a literal integer.
