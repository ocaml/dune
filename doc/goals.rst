************
Goal of Dune
************

The main goal of the Dune project is to provide the best possible
build tool for the whole of the OCaml community; from individual
developers who work on open source projects during their free time all
the way to larger companies such as Jane Street passing by communities
such as MirageOS. And to the extent that is reasonably possible, help
provide the same features for friend communities such as Coq and
possibly Reason/Bucklescript in the future.

We haven't reached this goal yet and Dune still lacks in some areas in
order to be such a tool, but we are steadily working towards that goal.
On a practical level, there are a few boxes to check. There are in
fact a ton details to sort out, but at a high-level a tool that works
for everyone in the OCaml community should at least:

1. have excellent backward compatibility properties
2. have a robust and scalable core
3. remain a no-brainer dependency
4. remain accessible
5. have very good support for the OCaml language
6. be extensible

At this point, we have done a good job at 1, 3, 4 and 5, we are
working towards 2 and are doing the preparatory work for 6. Once all
these boxes have been checked, we will consider that the Dune project
is feature complete.

In the rest of this page, we develop these points and give some
insights into our current and future focuses.

Have excellent backward compatibility properties
================================================

In an open-source community, there will always be groups of people
with enough resources to continuously bring their projects up to date
as well as people who work on their free time and cannot provide the
same level of continuous support and updates.

From the point of view of Dune, we have to assume that a released
project with ``dune`` files is a precious piece that will potentially
never change. So changing Dune in a way that it could no longer
understand a released project is by default a no-no.

Of course, we can't give a 100% guarantee that Dune will always behave
exactly the same. That would be unrealistic and would prevent the
project from moving forward.  In order for us to provide good backward
compatibility properties while still keeping the project fresh and
dynamic, we have to make sure to properly delimit, document and
version the set of behaviours on which users should rely. And for this
to be manageable, the surface API of Dune has to remain small.

A distinguishing feature of Dune is that it lets the user declare which
version of the ``dune`` tool their project was written against, and
``dune`` will morph itself to behave the same as this version of the
``dune`` binary, even if it is a newer version. As a result, a recent
version of the ``dune`` binary is able to understand a wide range of
dune projects written against many different version of Dune. And
while we strictly follow `semantic versioning`_, new major versions of
Dune effectively introduce very few breaking changes and most projects
do not need upper bounds on Dune.

This guarantee is of course limited to documented behaviours.

.. _semantic versioning: https://semver.org/

Have a robust and scalable core
===============================

Tech companies tend be fond of big mono repositories, so to be able to
work for them Dune needs to be able to eat up large repositories
without blinking. It not only needs to build fast, but more
importantly it must not get in the way of fast feedback during
development, no matter the size of the repository.

Note that Dune will only be tested on repositories as large as people
participating in the development of Dune require. Currently, the
largest user is Jane Street. If someone wanted to use Dune on much
larger repositories than the ones used at Jane Street and this
required a significant amount of effort on Dune, this wouldn't be
considered unless we get some help to do so and we can keep the other
promises.

In particular, while making Dune scalable we must also make sure to
not turn Dune into a monster because no one wants to force their users
to install a monster to build their project. This brings us to the
next point of Dune being a no-brainer dependency.

Remain a no-brainer dependency
==============================

Dune is a hard dependency of any Dune project. Anyone using Dune
to develop their project will have to ask their user to install
Dune. For this reason, it is very important to keep Dune as lean as
possible.

This is why we have to be careful when we start relying on an external
piece of software, or when we introduce new concepts. We must make
sure to not introduce duplication or useless stuff. The overall
projects has to remain lean.

It is also important to keep Dune as easy to install as
possible. Currently, the only requirement to build Dune is a
working OCaml compiler. Nothing else is required, not even a shell and
we should keep it this way.

Remain accessible
=================

Since Dune aims to be the best possible tool for the whole OCaml
community, it is important for Dune to remain accessible. Getting
started and learning Dune should be straightforward.

For that purpose, when designing the language, the command line
interface or the documentation, we must take on the perspective 
of a user who is just discovering Dune and its features.

Because Dune must be suitable for everyone, it must also provide
advanced and more complex features for expert users. However, the
documentation should always flow from the simpler concepts and common
tasks to the more complex ones. Even if the simpler features can be
explained as instances of the more general ones.

Have very good support for the OCaml language
=============================================

There are many many build systems out there. What makes Dune different
is that it primarily targets the OCaml community. So Dune must come
with excellent support for the OCaml language and OCaml projects in
general.

If it didn't, then Dune would just be yet another build system.

Perhaps in the future some of the general build system will take over
and Dune might just become a plugin in this system, or even disappear
into the language if the compiler gains a lot of higher level
features. But for now, Dune is a standalone build system that is
primarily serving the needs of the OCaml community, and to the extent
that is reasonably possible the needs of friend communities.

Be extensible
=============

No matter how good the support for the OCaml language is, it will
never be enough to cover every single project need. For this reason,
Dune needs to provide some form of openness for projects that need
something that doesn't completely fit in the model provided by Dune.

In the long run, extensibility tends to get in the way of innovation
and we should always strive to make sure that all the general needs
are covered by the main Dune language, but we will always need an
escape hatch for Dune to remain a practical choice.

It is pretty clear to us that extensibility must be done via OCaml
code, and currently it is a bit difficult to use OCaml as a proper
extension language, though some work is being done to help on that
front.
