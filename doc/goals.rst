##############
 Goal of Dune
##############

..
   TODO(diataxis)
   This is an important page and should go in a sort of meta section together
   with the history of the project and the glossary for example.

The Dune project strives to provide the best possible build tool for the
entire OCaml community, including individual developers contributing to
open source projects in their free time, larger companies (such as Jane
Street), and communities, like MirageOS and Irmin. Additionally, we aim
to provide the same features for other neighbouring communities, such as
Coq and possibly Reason/Bucklescript, in the future.

We haven't reached this goal yet, as Dune still requires development in
some areas to be such a tool, but we're steadily working towards that
goal. On a practical level, a few boxes must be checked, and a
considerable number of details needs to be sorted out. At a high-level,
we think a tool that works for everyone in the OCaml community should at
least:

#. have excellent backward compatibility properties
#. have a robust and scalable core
#. remain a no-brainer dependency
#. remain accessible
#. have very good support for the OCaml language
#. be extensible

At this point, we've done a good job at 1, 3, 4, and 5. We're currently
working towards 2 and are doing the preparatory work for 6. Once all
these boxes have been checked, we'll consider the Dune project complete.

Below, we develop each point and give some insights into our current and
future focuses.

**************************************************
 Have Excellent Backward-Compatibility Properties
**************************************************

In an open source community, two types of groups exist: those with
enough resources to continuously bring their projects up-to-date and
those who work on them in their free time. The latter obviously can't
provide the same level of continuous support and updates as the former.

From the Dune point of view, we consider every released project with
``dune`` files a precious piece that will potentially never change, so
we discourage changing Dune in a way where it could no longer understand
a released project.

Of course, we can't give a 100% guarantee that Dune will always behave
exactly the same. That would be unrealistic and would prevent the
project from moving forward. In order to provide good
backward-compatibility properties while still keeping the project fresh
and dynamic, we need to properly delimit, document, and version the set
of behaviours on which users rely. For this to be manageable, the
surface Dune API must remain small.

A distinguishing feature of Dune allows the user to declare which
version of the ``dune`` tool they wrote the project against, and
``dune`` will morph itself to behave the same as this version of the
``dune`` binary, even if it's a newer version. As a result, a recent
``dune`` binary version can understand a wide range of Dune projects
written against many different versions of Dune, and while we strictly
follow `semantic versioning`_, new major versions of Dune effectively
introduce very few breaking changes. Most projects don't need upper
bounds on Dune.

This guarantee is of course limited to documented behaviours.

.. _semantic versioning: https://semver.org/

*********************************
 Have a Robust and Scalable Core
*********************************

Tech companies tend be fond of big mono repositories, so for
compatibility, Dune must consume large repositories without blinking. It
not only needs to build fast, but more importantly, it must not impede
fast feedback during development, no matter the size of the repository.

Note that we'll only test Dune on repositories as large as people
participating in Dune's development require. Currently, the largest user
is Jane Street. If someone wanted to use Dune on much larger
repositories than the ones used at Jane Street, and this required a
significant amount of effort on Dune, this wouldn't be considered unless
we get some help to do so and we can keep the other promises.

In particular, while making Dune scalable, we must also ensure Dune
doesn't turn into a monster, because no one wants to force their users
to install a monster to build their project. This brings us to the next
point of Dune being a no-brainer dependency:

********************************
 Remain a No-Brainer Dependency
********************************

Dune is a hard dependency of any Dune project. Anyone using Dune to
develop their project will have to ask their user to install Dune. For
this reason, it is very important to keep Dune as lean as possible.

We need to be careful when we start relying on an external piece of
software or when we introduce new concepts. We must not introduce
duplication or useless stuff. The overall projects has to remain lean.

It's also important to keep Dune as easy to install as possible.
Currently, the only requirement to build Dune is a working OCaml
compiler. Nothing else is required, not even a shell, and we should keep
it this way.

*******************
 Remain Accessible
*******************

Since Dune aims to be the best possible tool for the whole OCaml
community, it's important to keep Dune accessible. Getting started and
learning Dune should be straightforward.

For that purpose, when designing the language (the command line
interface or the documentation), we must take on the new-user
perspective, one who just discovered Dune and its features, because Dune
should be suitable for everyone! It also needs to provide advanced and
more complex features for expert users. However, the documentation
should always flow from the simpler concepts and common tasks to the
more complex ones, even if the simpler features can be explained as
instances of the more general ones.

***********************************************
 Have Excellent Support for the OCaml Language
***********************************************

There are many, many build systems out there. Dune stands out because it
primarily targets the OCaml community, so Dune must come with excellent
support for the OCaml language and OCaml projects in general.

If it didn't, Dune would just be yet another generic build system.

Perhaps in the future some of the general build system will take over,
and Dune might just become a plugin in this system. It could even
disappear into the language, if the compiler gains significant
high-level features. But for now, Dune is a standalone build system that
primarily serves the OCaml community's needs, and to the extent that is
reasonably possible, the needs of other functional language communities.

***************
 Be Extensible
***************

No matter the quality of the OCaml language's support, it will never be
enough to cover every single project need. For this reason, Dune must
provide some form of openness for projects with need that don't
completely fit in the Dune model.

In the long run, extensibility tends to obstruct innovation, and we
should always strive to ensure that we cover all the general needs of
the main Dune language; however, we'll always need an escape hatch for
Dune to remain a practical choice.

It's pretty clear that extensibility must be done via OCaml code, and
currently it's a bit difficult to use OCaml as a proper extension
language, though some work is being done to help on that front.
