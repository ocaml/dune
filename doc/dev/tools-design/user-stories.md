# Dune Tools: User Stories

The following are workflows for Dune tools described from a user's perspective.
They are intentionally kept short and don't touch upon implementation details.
This is expected to complement the [Requirements Document](requirements.md).

## Lifecycle

1. Install a Tool

I can install a binary available in
[opam-repository](https://github.com/ocaml/opam-repository/) as a tool with an
intuitive command.

I see sensible errors when it's not possible to install a tool for any reason.

2. Run a Tool

I can run a binary which has been installed via the command line. I don't need
to know where or how Dune installed the tool.

3. Remove a Tool

If I no longer need to use a tool, I can remove it via the command line. I
should no longer be able to run the tool.

Optimisation: The removed tool should no longer take up space on my machine.

4. List all Tools

Dune can show me a list of all available tools in the current project.

5. Default Version

When no version is specified for the tool being installed, I want Dune to pick a
sensible default version of the tool. In the case of tools like
`ocaml-lsp-server` that must be built with the same compiler as the project, or
`utop`, which has similar requirements, Dune should ensure the required compiler
is used, or it should be possible to specify the required compiler.

6. Updating Tools

I can explicitly request updating a tool's version. The version doesn't change
implicitly.

## Solving & Reproducibility

7. Version Pinning

I have the option to pin my tool and its dependencies to specific versions or
development branches.

8. Constraints

I can specify constraints for my tool without needing to pin it to a branch.

9. Reproducibility

My collaborators and the CI pick up the exact set of tools I've been using,
without any extra setup on their end.

10. Conflicting Tools

A conflict between the dependencies of two tools, or between a tool's
dependencies and the project's dependencies, should not prevent either from
being installed.

## Compiler Interaction

11. Compiler Caching

Every time I install a tool, I shouldn't be building the same compiler again.
The tool build shouldn't take unreasonably long.

12. Working without a Compiler

For tools that don't interact with the project's dependencies (I3), the tool
should build and run even if my project is not built.

13. Compiler Matching

I want Dune to pick and install a compatible compiler toolchain for each tool,
so that I don't have to manage compiler versions myself.

- I want tools to reuse my project's compiler when possible, to ensure
  predictable behaviour and fast builds.
- If a tool is incompatible with the project's compiler, I want the tool to
  still work without changing my project setup.

14. Upgrading the Compiler

When my project's compiler version is upgraded, I want to have a straightforward
way to update all the tools to be compatible. Dune need not upgrade unless
explicitly asked.

15. Switching Contexts

When I switch contexts, I want my tools to continue working.
Compiler-independent tools shouldn't be rebuilt when I change the context.

## Dune Integration

16. Build Aliases

I want the familiar build aliases (dune fmt, dune build @doc, etc.) to continue
working, and to automatically invoke the right tool.

17. Editor Integration

Installing editor tools (`ocaml-lsp-server`, `ocamlformat`, etc.) via Dune
should work well with my editor (Emacs, Vim, VS Code).

18. Optional Integration

When a tool has an optional feature powered by another package, I want to be
able to enable that feature when installing the tool.
