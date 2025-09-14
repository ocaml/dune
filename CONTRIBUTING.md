Dune is an community orientated open source project. It was originally developed
at [Jane Street][js] and is now maintained by Jane Street and [Tarides][tarides]
together with several developers from the OCaml community.

Sharing feedback
================

The easiest way to contribute is to share feedback. 

We discuss *bugs reports* and *feature requests* via our [issues][issues].  If
you don't find a preexisting issue discussing your topic, then please [file a
new issue][file an issue].

For other kinds of support, feedback, or questions, please contribute to our
[discussions][discussions].

[file an issue]: https://github.com/ocaml/dune/issues/new/choose
[issues]: https://github.com/ocaml/dune/issues
[discussions]: https://github.com/ocaml/dune/discussions

Developing Dune
===============

Contributions to the Dune code base are welcome!

Our development process is as follows:

- Non-trivial submissions should be proceeded by an issue communicating the
  rationale for the intended change.
- Substantial changes should be preceded by upfront design and planning,
  proportional to the scope and impact of the intended change.
  - This design should be reviewed by at least one other party.
  - Github issues are an appropriate venue for most design discussions, but more
    involved design work may warrant an RFC or ADR.
- Github is the preferred venue for discussing architectural and design decisions.
  Exchanges in meetings and chat are valuable, but we insist on deliberating and
  recording the what and why of our work in the persistent record of GitHub.
- Changes are submitted via GitHub pull requests against the `main` branch.

If you are looking to get started, check our [issues tagged with good first
issue][good first issue].

[good first issue]: https://github.com/ocaml/dune/issues?q=is%3Aissue%20state%3Aopen%20label%3A%22good%20first%20issue%22

Developer documentation
-----------------------

See our developer documentation at [./doc/hacking.rst][hack] or
[online][devdocs] for technical guidance about contributing to the code base.

[devdocs]: https://dune.readthedocs.io/en/stable/hacking.html

Developer meetings
------------------

See our [developer wiki][developer wiki] for the latest information about our
recurring developer meetings. All interested attendees are welcome.

[developer wiki]: https://github.com/ocaml/dune/wiki/

Signing contributions
---------------------

Dune is distributed under the MIT license and contributors are required to sign
their work in order to certify that they have the right to submit it under this
license.

Your signature certifies that you wrote the patch or otherwise have the right to
pass it on as an open-source patch. The rules are pretty simple: if you can
certify the below (from [developercertificate.org][dco]):

```
Developer Certificate of Origin
Version 1.1

Copyright (C) 2004, 2006 The Linux Foundation and its contributors.
1 Letterman Drive
Suite D4700
San Francisco, CA, 94129

Everyone is permitted to copy and distribute verbatim copies of this
license document, but changing it is not allowed.


Developer's Certificate of Origin 1.1

By making a contribution to this project, I certify that:

(a) The contribution was created in whole or in part by me and I
    have the right to submit it under the open source license
    indicated in the file; or

(b) The contribution is based upon previous work that, to the best
    of my knowledge, is covered under an appropriate open source
    license and I have the right under that license to submit that
    work with modifications, whether created in whole or in part
    by me, under the same open source license (unless I am
    permitted to submit under a different license), as indicated
    in the file; or

(c) The contribution was provided directly to me by some other
    person who certified (a), (b) or (c) and I have not modified
    it.

(d) I understand and agree that this project and the contribution
    are public and that a record of the contribution (including all
    personal information I submit with it, including my sign-off) is
    maintained indefinitely and may be redistributed consistent with
    this project or the open source license(s) involved.
```

Then you just add a line to every git commit message:

```
Signed-off-by: Joe Smith <joe.smith@email.com>
```

Use your real name (sorry, no pseudonyms or anonymous contributions.)

If you set your `user.name` and `user.email` git configs, you can sign
your commit automatically with `git commit -s`.

It is possible to set up `git` so that it signs off automatically by using a
prepare-commit-msg hook in git. See <https://stackoverflow.com/a/46536244> for
details. As noted in the manual for `format.signOff`, note that adding the
`Signed-off-by` trailer should be a conscious act and means that you certify
you have the rights to submit this work under the same open source license.

[dco]: http://developercertificate.org/
[js]: https://www.janestreet.com/
[tarides]: https://tarides.com/
[hack]: ./doc/hacking.rst
