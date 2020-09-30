Issue and PR management
=======================

Notes:
- clarify when should someone feel confident everyone has read a PR
- clarify that someone opening a PR should have read it

The Dune team handles issues and pull requests in a way that is
manageable for developers and provides a good user experience.

Our workflow ensures that all issues and pull requests are discussed,
triaged and reporters get feedback in a timely manner. It also
encourages simple issues and pull requests to be dealt with
quickly. Finally, it is designed so that developers don't have to
watch the bulk of github notifications.

## One person assigned to each issue or PR

For each issue or PR, there must be one person who is responsible for
it. It is this person responsibility to ensure that:

- the issue/PR eventually gets closed or merged
- the issue/PR never stalls

If the conclusion of a discussion is that some work needs to be done,
it is not necessarily the responsibility of this person to do this
work. Though if the work is a small bugfix that would benefit Dune
users and no one has volunteered to write the fix, it is expected that
the assignee will write the fix and submit a pull request.

For bug reports, it is the responsibility of this person to make sure
the bug is reproducible with a minimal example. It is not their
responsibility to come up with the minimal example, however it is
their responsibility to lead the discussion to obtain such a minimal
example. For instance, by asking questions.

While the assignee is responsible for the issue, other developers are
still free to participate in the discussion and help it move
forward. In fact, the assignee doesn’t necessarily have to be the
person talking the most and they can even not participate. However, if
the discussion stalls they are responsible for making it move forward.

If the assignee doesn’t have enough technical knowledge, they should
ask on the #dune-dev slack channel.

We will use the `Assignees` field on github to indicate who is
assigned to the issue or PR.

## When it is not clear how to proceed

Sometimes, the discussion reaches a point where it is not clear what
to do. For instance it can be because controversial features are being
discussed, or because the scope of the discussion becomes wider than
the original subject. In any case, while some issues or PRs can be
dealt with by a single person, some other require wider team
discussions.

When the discussion reaches such a point, the assignee should mark it
as such. We will do that by adding the label
“requires-team-discussion”.

When the discussion is premature

Sometimes, while a feature request or pull request is valid on its
own, it doesn’t play nicely with other planned or on-going work and it
is better to wait before dealing with it. It can also be that we
simply don’t know to move forward at this point.

In such cases, we should decide whether:

- the feature/bugfix discussed is something we think would improve
  Dune in general
- the feature/bugfix discussed is more marginal or unclear

In the first case, we should add the label “postponed” to the
issue. In the latter, we should close it.  Regular team meetings

During our regular team meetings, we will look at the list of issues
and PRs:

- that are unassigned
- that have the label `requires-team-discussion`

For each of them, we will have a short discussion and decide what to
do. For each issue or PR, the outcome can be one of:

- we assign it to someone
- we close it
- we add the label `postponed`

In between meetings, if someone sees an issue that is un-assigned and
they know how to deal with it and have the bandwidth to do it, they
should still feel free to assign it to themselves immediately.

The main goal of the meetings will be to clear the list issues that
are unassigned or have the label
“requires-team-discussion”. Initially, we will continue with one
meeting every two weeks. However, if the rate of such issues and PRs
increases and we can’t go through the stack in one meeting, we will
increase the frequency of the meetings to one every week.  Reviewing
postponed issues and PRs

Every 6 months, we will review the list of issues and PRs with the
label “postponed” and decide whether:

- to assign it to someone or change the assignment
- to close it
- to keep the “postponed” label

## Assigning issues and PRs fairly

When there is no clear person who should deal with an issue or PR, we
will pick one person from the team. The Dune team is composed of
various people from different organizations who do not have the same
time budget to work on Dune. We will keep a list of the team members
and for each of them indicate how many days a week they feel they can
spend working on Dune. We will keep this list in the Dune wiki.

When assigning issues or PRs, we will try to keep the number of
issues/PRs assigned to each person proportional to the amount of time
they can spend on Dune each week.

When new members join in and after an integration period, we can
rebalance the assignments.
