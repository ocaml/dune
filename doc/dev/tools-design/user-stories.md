# Dune Tools: User Stories

The following are workflows for Dune tools described from a user's perspective. They are intentionally kept short, and don't touch upon implementation details. This is expected to be complement the [Requirements Document](/requirements.md).

## Lifecycle

1. Install a tool

I can install a binary available in [opam-repository](https://github.com/ocaml/opam-repository/) as a tool with an intuitive command.

I see sensible errors when it's not possible to install a tool for any reason.

2. Run a tool

I can run a binary which has been installed via the command-line. I don't need to know where or how Dune installed the tool.

3. Remove a tool

If I no longer need to use a tool, I can remove it via command line. I should no longer be able to run the tool.

Optimisation: The removed tool should no longer take space on my machine.

4. List all tools

Dune can show me a list of all available tools in the current project.

5. Default Version

When a version of the tool being installed is not mentioned, I want Dune to pick a sensible default version of the tool. In case of tools like ocaml-lsp-server that require the tool to be built with the same compiler, or utop with similar requirements, Dune should pick it up or it should be possible to specify it.

6. Updating tools

I can explicitly request updating a tool's version. The version doesn't change implicitly.

## Solving & Reproducibility

7. Version Pinning

I have the option to pin my tool and its dependencies to specific versions or development branches.

8. Constraints

I can specify constraints for my tool without needing pin it to a branch.

9. Reproducibility

My collaborators and the CI pick up the exact set of tools I've been using, without any extra setup on their end.

10. Conflicting tools

Two tools, or a tool with a project dependency conflicting with each others' dependencies should not stop them from being installed.

## Compiler Interaction

11. Compiler caching

Every time I install a tool, I shouldn't be building the same compiler again. The tool build shouldn't take unreasonably long.

12. Working without a compiler

For tools that don't interact with the project dependencies (I3), the tool should build and run even if my project is not built.

13. Compiler matching

Dune should sensibly pick and install a compiler toolchain for a given tool.

- By default, this could be that tools use the same compiler version as the
  project.
- If a tool is incompatible with the project's compiler, it can be upgraded or
  downgraded

14. Upgrading the compiler

When my project's compiler version is upgraded, I want to have a straightforward way to update all the tools to be compatible.

15. Switching contexts

When I switch contexts, I want my tools to continue working. Compiler independent tools shouldn't be rebuilt when I change the context.

## Dune Integration

16. Build Aliases

I want the familar build aliases (dune fmt, dune build @doc, etc.) to continue working, and to automatically invoke the right tool.

17. Editor integration

Installing editor tools (ocaml-lsp-server, and whatever) via dune should work well with my editor (Emacs, Vim, VSCode).

18. Optional Integration

When a tool has an optional feature powered by another package, I want to enable that feature with the tool, so that I get the capability without a second tool to manage.
