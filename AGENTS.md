# Agent Guide - elm-cqrs

## About this repository

This repository is the `jamesrweb/elm-cqrs` Elm package. It provides types and
utilities for command/query responsibility segregation (CQRS) in Elm.

## Layout

```
./
├── .git/                  # git internals
├── .agents/                # agent configuration (DotAgents protocol)
├── .github/
│   ├── actions/           # composite actions for lint, test, and publish
│   │   ├── lint/
│   │   ├── publish/
│   │   └── test/
│   └── workflows/         # CI and CD workflows
├── .coverage/             # generated coverage output, gitignored
├── .idea/                 # JetBrains IDE state (only run configurations are tracked)
├── elm-home/              # generated patched Elm build, gitignored
├── elm-stuff/             # generated Elm build artefacts, gitignored
├── node_modules/          # installed dependencies, gitignored
├── review/                # elm-review project
├── src/                   # exposed package source
├── tests/                 # test suites and generated verify-examples output
├── AGENTS.md              # this file
├── CLAUDE.md              # symlink to AGENTS.md
├── LICENSE                # BSD 3-Clause licence
├── README.md              # human-facing documentation
├── biome.json             # Biome configuration
├── elm.json               # Elm manifest
├── package.json           # pnpm manifest and scripts
├── pnpm-lock.yaml         # pnpm lockfile
├── pnpm-workspace.yaml    # pnpm workspace settings
├── .gitattributes         # git attributes
└── .gitignore             # git ignore rules
```

## Commands

| Command              | Purpose                                             |
| -------------------- | --------------------------------------------------- |
| `pnpm format`        | Applies formatting fixes                            |
| `pnpm lint`          | Runs the lint checks                                |
| `pnpm test`          | Runs the test suite                                 |
| `pnpm coverage`      | Opens the coverage report (not a gate)              |
| `pnpm documentation` | Opens the documentation preview (not a gate)        |
| `pnpm install`       | Installs dependencies and runs the postinstall step |

Every change must pass `pnpm lint` and `pnpm test` before it is complete.

## Standards

The rules in this file and in the `.agents/` tree are authoritative for this
repository. Change them deliberately, in those files, rather than working around
them.

## Tooling

Formatting is done via `elm-format` and Biome. Linting is done via `elm-review`
and Biome.

## Conventions

Use module and value documentation for the public API. Do not add explanatory
comments to files in `src/`. Write functional, type-driven code with full type
annotations on exposed functions. Documentation examples are compiled by the
test gate, so they must compile.

## CI/CD and releases

CI runs on pushes to `main`, on all pull requests, and on manual dispatch. CD
publishes the package on pushes to `main`. Dependabot runs monthly.

## Guardrails

Never publish or create releases locally. Publishing happens through CD only.
Never disable or skip a lint, format, or test gate to make a change pass. Fix
the code, not the gate. The dependency constraints in `elm.json` are the
package's compatibility contract. Do not edit them casually.

## Pointers

- `.agents/agents.md` - the slim agent guidelines for this repository
- `.agents/knowledge/repo-overview/` - the injected repo overview note
- `README.md` - the human-facing documentation

## Open topics

- Lift coverage on the exposed modules as the APIs mature.
- Evaluate converging the custom review rules into a shared review package.
