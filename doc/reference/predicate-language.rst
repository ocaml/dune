####################
 Predicate Language
####################

The predicate language allows the user to define simple predicates
(Boolean-valued functions) that Dune can evaluate. Here is a semiformal
specification of the predicate language:

.. productionlist:: pred : (and `pred` `pred`) : (or `pred` `pred`) : (not `pred`) : :standard : `element`

The exact meaning of ``:standard`` and the nature of :token:`element`
depend on the context. For example, in the case of the
:doc:`/reference/files/dune/subdir`, an :token:`element` corresponds to
file glob patterns. Another example is the user action
:doc:`actions/with-accepted-exit-codes`, where an :token:`element`
corresponds to a literal integer.
